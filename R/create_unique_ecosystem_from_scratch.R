#' Create all the unique ecosystem from sratch
#'
#' This function create all the unique ecosystem derived
#'
#' @param dsn data source name (interpretation varies by driver - for some drivers, dsn is a file name, but may also be a folder, or contain the name and access credentials of a database); in case of GeoJSON, dsn may be the character string holding the geojson data. It can also be an open database connection.
#' @param vri_dsn dsn for vri layer, default to dsn if empty
#' @param bem_dsn dsn for bem layer, default to dsn if empty
#' @param rivers_dsn dsn for rivers layer, default to dsn if empty
#' @param wetlands_dsn dsn for wetlands layer, default to dsn if empty
#' @param layers_names_list named list that contains the layer name for the vri, bem, rivers and wetlands layers
#' @param beu_bec_csv dsn for beu bec csv
#' @param beu_wetland_update_csv dsn for wetlands correction csv
#' @param clear_site_ma boolean, if TRUE variable SITE_M1A, SITE_M2A will be cleared
#' @param use_ifelse boolean, if TRUE correction done after the combine_duplicated_BEUMC will only be applied on rows that were not affected by the correction of duplicated BEUMC
#' @param wkt_filter character; WKT representation of a spatial filter (may be used as bounding box, selecting overlapping geometries)
#' @param n_iterations integer number of iterations, usefull when running out of RAM to load smaller areas and iterate over them one at a time
#' @param verbose boolean , if TRUE function will return message to indicate progress throughout the function execution
#' @return data.table that contains the frequency of each unique ecosystem and generates the following empty column to be feed later on:
#'
#'   * Forested (Y/N),
#'   * Strict_Climax,
#'   * Stand_Climax,
#'   * Stand_Age_0-15, Stand_Age_16-30, Stand_Age_31-50, Stand_Age_51-80, Stand_Age_80+,
#'   * Struct_Age_0-3, Struct_Age_4-10, Struct_Age_11-30, Struct_Age_31-40, Struct_Age_41-60, Struct_Age_61-80, Struct_Age_81-139, Struct_Age_140_249, Struct_Age_250+,
#'   * Snow_code
#'
#'
#' @import data.table
#' @import sf
#' @export
#'
create_unique_ecosystem_from_scratch <- function(dsn, vri_dsn = dsn, bem_dsn = dsn, rivers_dsn = dsn, wetlands_dsn = dsn,
                                                 layers_names_list = list(vri = "VEG_R1_PLY_polygon", bem = "BEM", rivers = "FWA_RIVERS_POLY", wetlands = "FWA_WETLANDS_POLY"),
                                                 beu_bec_csv = system.file("csv/Allowed_BEC_BEUs_NE_ALL.csv", package = "SSGBM-VRI-BEM"),
                                                 beu_wetland_update_csv = system.file("csv/beu_wetland_updates.csv", package = "SSGBM-VRI-BEM"),
                                                 clear_site_ma = TRUE, use_ifelse = TRUE, wkt_filter = character(0), n_iterations = 1, verbose = TRUE) {

  if (FALSE) {
    .<-BEU_MC<-BGC_PHASE<-BGC_SUBZON<-BGC_VRT<-BGC_ZONE<-FREQ<-NULL
  }

  # TODO add default wkt_filter when no filter is passed but number of iterations is greater than 1 (maybe default to the whole skeena region, store it as part of the package)
  # TODO check what appends when wkt_filter cover an area where there is no polygon
  # then maybe add a if in the loop to check if the vri is empty just go to the next iteration

  # make grid with wkt filter based on number of iterations
  grid <- create_grid_from_iteration_number(n_iterations, wkt_filter, as.text = TRUE)

  # reading csv once before the loop
  beu_bec_csv <- fread(beu_bec_csv)
  beu_wetland_update_csv <- fread(beu_wetland_update_csv)

  # initialize an empty list for result of each iterations
  unique_ecosystem_dt_list <- list()

  for (iteration in seq.int(length.out = n_iterations)) {

    if (verbose) {
      message(paste0("iteration ", iteration, " out of ", n_iterations))
    }

    # read layers into sf objects
    if (verbose) {
      message("reading layers into sf objects")
    }
    filter <- grid[iteration]
    vri <- read_vri(vri_dsn, layer = layers_names_list$vri, wkt_filter = filter)
    bem <- read_bem(bem_dsn, layer = layers_names_list$bem, wkt_filter = filter)
    rivers <- read_rivers(rivers_dsn, layer = layers_names_list$rivers, wkt_filter = filter)
    wetlands <- read_wetlands(wetlands_dsn, layer = layers_names_list$wetlands, wkt_filter = filter)


    # create the vri-bem with bem attributes updated based on vri, wetlands and rivers
    vri_bem <- create_updated_vri_bem(vri = vri,
                                      bem = bem,
                                      rivers = rivers,
                                      wetlands = wetlands,
                                      beu_bec_csv = beu_bec_csv,
                                      beu_wetland_update_csv = beu_wetland_update_csv,
                                      clear_site_ma = TRUE,
                                      use_ifelse = TRUE,
                                      verbose = verbose)

    # create unique ecosystem
    if (verbose) {
      message(paste0("Creating unique ecosystem output for iteration ", iteration))
    }
    unique_ecosystem_dt_list[[iteration]] <- summarize_unique_ecosystem(vri_bem_dt = setDT(vri_bem))
  }

  # combine all iteration together, sum the frequency for same ecosystems and create empty forest structure variables
  if (verbose) {
    message("Combining all unique ecosystem output together to create final ouput")
  }

  if (n_iterations == 1) {
    return(unique_ecosystem_dt_list[[1]])
  }
  else {
    return(create_empty_forest_structure_variables(rbindlist(unique_ecosystem_dt_list)[ , .(FREQ = sum(FREQ)) , by = .(BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEU_MC)]))
  }
}
