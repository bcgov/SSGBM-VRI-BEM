#' Creates the RRM ecosystem outpout for skeena bear and moose model
#'
#'
#' @param dsn data source name (interpretation varies by driver - for some drivers, dsn is a file name, but may also be a folder, or contain the name and access credentials of a database); in case of GeoJSON, dsn may be the character string holding the geojson data. It can also be an open database connection.
#' @param vri_dsn dsn for vri layer, default to dsn if empty
#' @param bem_dsn dsn for bem layer, default to dsn if empty
#' @param rivers_dsn dsn for rivers layer, default to dsn if empty
#' @param wetlands_dsn dsn for wetlands layer, default to dsn if empty
#' @param ccb_dsn dsn for ccb layer, default to dsn if empty
#' @param elevation_dsn dsn for elevation raster, must be provided because cannot be part of a .gbd , needs to be .tif
#' @param layers_names_list named list that contains the layer name for the vri, bem, rivers, wetlands and ccb layers
#' @param beu_bec_csv dsn for beu bec csv
#' @param beu_wetland_update_csv dsn for wetlands correction csv
#' @param unique_ecosystem dsn for unique ecosystem csv filled with values
#' @param clear_site_ma boolean, if TRUE variable SITE_M1A, SITE_M2A will be cleared
#' @param use_ifelse boolean, if TRUE correction done after the combine_duplicated_BEUMC will only be applied on rows that were not affected by the correction of duplicated BEUMC
#' @param most_recent_harvest_year integer that represent the most recent harvest year
#' @param wkt_filter character; WKT representation of a spatial filter (may be used as bounding box, selecting overlapping geometries)
#' @param n_iterations integer number of iterations, usefull when running out of RAM to load smaller areas and iterate over them one at a time
#' @details
#' For each of the following combination compute the sum of the area
#'   * ECO_SEC
#'   * BGC_ZONE
#'   * BGC_SUBZON
#'   * BGC_VRT
#'   * BGC_PHASE
#'   * BEUMC  (BEUMC_S1, BEUMC_S2, BEUMC_S3)
#'   * SLOPE_MOD
#'   * SITE_M3A
#'   * SNOW_CODE
#'   * ABOVE_ELEV
#'   * CROWN_MOOSE (CROWN_MOOSE_1, CROWN_MOOSE_2, CROWN_MOOSE_3)
#'   * STRCT (STRCT_S1, STRCT_S2, STRCT_S3)
#'   * STAND (STAND_A1, STAND_A2, STAND_A3)
#'   * FORESTED (FORESTED_1, FORESTED_2, FORESTED_3)
#'
#' For STRCT and STAND every combination is also made with the projected age values.
#'
#' @return Summary of Area by unique ecosystem
#' @import data.table
#' @import sf
#' @importFrom terra terrain rast
#' @export
#'
create_RRM_ecosystem_from_scratch <- function(dsn, vri_dsn = dsn, bem_dsn = dsn, rivers_dsn = dsn, wetlands_dsn = dns, ccb_dsn = dsn, elevation_dsn,
                                              layers_names_list = list(vri = "VEG_R1_PLY_polygon", bem = "BEM", rivers = "FWA_RIVERS_POLY", wetlands = "FWA_WETLANDS_POLY", ccb = "CNS_CUT_BL_polygon"),
                                              beu_bec_csv = "csv/Allowed_BEC_BEUs_NE_ALL.csv", beu_wetland_update_csv = "csv/beu_wetland_updates.csv", unique_ecosystem = "csv/Skeena_VRIBEM_LUT.csv",
                                              clear_site_ma = TRUE, use_ifelse = TRUE, most_recent_harvest_year, wkt_filter = character(0), n_iterations = 1) {

  # TODO add default wkt_filter when no filter is passed but number of iterations is greater than 1 (maybe default to the whole skeena region, store it as part of the package)
  # TODO check what appends when wkt_filter cover an area where there is no polygon
  # then maybe add a if in the loop to check if the vri is empty just go to the next iteration

  # make grid with wkt filter based on number of iterations
  if (n_iterations %% 1 != 0) {
    n_iterations <- round(n_iterations)
    warning(paste0("n_iterations must be an integer, n_iterations was rounder to ", n_iterations))
  }

  grid <- create_grid_from_iteration_number(n_iterations, wkt_filter, as.text = TRUE)

  # compute slope and aspect only once before the loop
  terrain_raster <- terrain(elev_raster, v = c("slope", "aspect"), unit = "radians")

  # initialize empty list for RRM ecosystem which will be filled when iterating
  RRM_ecosystem_list <- list()

  for (iteration in n_interactions) {

    # read sf objects and csv files
    filter <- grid[iteration]
    vri <- read_vri(vri_dsn, layer = layers_names_list$vri, wkt_filter = filter)
    bem <- read_bem(bem_dsn, layer = layers_names_list$bem, wkt_filter = filter)
    rivers <- read_rivers(rivers_dsn, layer = layers_names_list$rivers, wkt_filter = filter)
    wetlands <- read_wetlands(wetlands_dsn, layer = layers_names_list$wetlands, filter = filter)
    ccb <- read_ccb(ccb_dsn, layer = layers_names_list$ccb, wkt_filter = filter)
    elevation <- terra::rast(elevation_dsn)

    beu_bec_csv <- fread(beu_bec_csv)
    beu_wetland_update_csv <- fread(beu_wetland_update_csv)
    unique_ecosystem_dt <- read_unique_ecosystem_dt(unique_ecosystem)

    # create the vri-bem with bem attributes updated based on vri, wetlands and rivers
    vri_bem <- create_updated_vri_bem(vri = vri, bem = bem, rivers = rivers, wetlands = wetlands, beu_bec_csv = beu_bec_csv, beu_wetland_update_csv = beu_wetland_update_csv,
                                      clear_site_ma = TRUE, use_ifelse = TRUE, return_intersection_dt = TRUE)

    vri_bem_intersection_dt <- vri_bem$intersection_dt
    vri_bem <- vri_bem$vri_bem

    # merge elevation, slope and and aspect information
    vri_bem <- merge_elevation_raster_on_sf(elev_raster = elevation,
                                            vri_bem = vri_bem,
                                            terrain_raster = terrain_raster)

    # merge consolidated cutblocks info
    vri_bem <- merge_ccb_on_vri(vri_bem = vri_bem,
                                ccb = ccb)

    # calc forest age
    vri_bem <- calc_forest_age_class(vri_bem = vri_bem,
                                     most_recent_harvest_year = most_recent_harvest_year)


    # merge forest structural informations on vri bem
    vri_bem <- merge_unique_ecosystem_fields(vri_bem = vri_bem,
                                             unique_ecosystem_dt = unique_ecosystem_dt)

    # find dominant bear and moose values
    vri_bem <- find_crown_area_dominant_values(vri = vri_bem,
                                               bem = bem,
                                               intersection_dt = vri_bem_intersection_dt)

    RRM_ecosystem_list[[iteration]] <- create_RRM_ecosystem(vri_bem = vri_bem)

  }

  return(rbindlist(RRM_ecosystem_list)[, .(Hectares = sum(Hectares)),
                                       by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC, SLOPE_MOD, SITE_M3A,
                                                 SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE, STRCT, STAND, FORESTED)]
  )

}
