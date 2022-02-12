create_unique_ecosystem_from_scratch <- function(dsn, vri_dsn = dsn, bem_dsn = dsn, rivers_dsn = dsn, wetlands_dsn = dns,
                                                 layers_names_list = list(vri = "VEG_R1_PLY_polygon", bem = "BEM", rivers = "FWA_RIVERS_POLY", wetlands = "FWA_WETLANDS_POLY"),
                                                 beu_bec_csv = "csv/Allowed_BEC_BEUs_NE_ALL.csv", beu_wetland_update_csv = "csv/beu_wetland_updates.csv",
                                                 clear_site_ma = TRUE, use_ifelse = TRUE, wkt_filter = character(0), n_iterations = 1) {

  # TODO add default wkt_filter when no filter is passed but number of iterations is greater than 1 (maybe default to the whole skeena region, store it as part of the package)
  # TODO check what appends when wkt_filter cover an area where there is no polygon
  # then maybe add a if in the loop to check if the vri is empty just go to the next iteration

  # make grid with wkt filter based on number of iterations
  grid <- create_grid_from_iteration_number(n_iterations, wkt_filter, as.text = TRUE)

  # initialize an empty list for result of each iterations
  unique_ecosystem_dt_list <- list()

  for (iteration in n_interactions) {
    # read sf objects and csv files
    filter <- grid[iteration]
    vri <- read_vri(vri_dsn, layer = layers_names_list$vri, wkt_filter = filter)
    bem <- read_bem(bem_dsn, layer = layers_names_list$bem, wkt_filter = filter)
    rivers <- read_rivers(rivers_dsn, layer = layers_names_list$rivers, wkt_filter = filter)
    wetlands <- read_wetlands(wetlands_dsn, layer = layers_names_list$wetlands, wkt_filter = filter)
    beu_bec_csv <- fread(beu_bec_csv)
    beu_wetland_update_csv <- fread(csv/beu_wetland_updates.csv)

    # create the vri-bem with bem attributes updated based on vri, wetlands and rivers
    vri_bem <- create_updated_vri_bem(vri = vri, bem = bem, rivers = rivers, wetlands = wetlands, beu_bec_csv = beu_bec_csv, beu_wetland_update_csv = beu_wetland_update_csv,  clear_site_ma = TRUE, use_ifelse = TRUE)

    # create unique ecosystem
    unique_ecosystem_dt_list[[iteration]] <- summarize_unique_ecosystem(vri_bem = vri_bem)
  }

  # combine all iteration together, sum the frequency for same ecosystems and create empty forest structure variables
  return(create_empty_forest_structure_variables(rbindlist(unique_ecosystem_dt_list)[ , .(FREQ = sum(FREQ)) , by = .(BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEU_MC)]))

}
