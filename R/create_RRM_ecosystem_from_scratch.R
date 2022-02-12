create_RRM_ecosystem_from_scratch <- function(dsn, vri_dsn = dsn, bem_dsn = dsn, rivers_dsn = dsn, wetlands_dsn = dns, ccb_dsn = dsn, elevation_dsn,
                                              layers_names_list = list(vri = "VEG_R1_PLY_polygon", bem = "BEM", rivers = "FWA_RIVERS_POLY", wetlands = "FWA_WETLANDS_POLY", ccb = "CNS_CUT_BL_polygon"),
                                              beu_bec_csv = "csv/Allowed_BEC_BEUs_NE_ALL.csv", beu_wetland_update_csv = "csv/beu_wetland_updates.csv", unique_ecosystem = "csv/Skeena_VRIBEM_LUT.csv",
                                              clear_site_ma = TRUE, use_ifelse = TRUE, return_intersection_dt = FALSE, wkt_filter = character(0), n_iterations = 1) {

  # TODO add default wkt_filter when no filter is passed but number of iterations is greater than 1 (maybe default to the whole skeena region, store it as part of the package)
  # TODO check what appends when wkt_filter cover an area where there is no polygon
  # then maybe add a if in the loop to check if the vri is empty just go to the next iteration

  # make grid with wkt filter based on number of iterations
  grid <- create_grid_from_iteration_number(n_iterations, wkt_filter, as.text = TRUE)

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
    beu_wetland_update_csv <- fread(csv/beu_wetland_updates.csv)
    unique_ecosystem_dt <- fread(unique_ecosystem)

    # create the vri-bem with bem attributes updated based on vri, wetlands and rivers
    vri_bem <- create_updated_vri_bem(vri = vri, bem = bem, rivers = rivers, wetlands = wetlands, beu_bec_csv = beu_bec_csv, beu_wetland_update_csv = beu_wetland_update_csv,
                                      clear_site_ma = TRUE, use_ifelse = TRUE, return_intersection_dt = TRUE)

    vri_bem_intersection_dt <- vri_bem$intersection_dt
    vri_bem <- vri_bem$vri_bem

    # merge elevation, slope and and aspect information
    vri_bem <- merge_elevation_raster_on_sf(elev_raster = elevation,
                                            vri_bem = vri_bem)

    # merge consolidated cutblocks info
    vri_bem <- merge_ccb_on_vri(vri_bem = vri_bem,
                                ccb = ccb)

    # calc forest age
    vri_bem <- calc_forest_age_class(vri_bem = vri_bem,
                                     most_recent_harvest_year = 2020)


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