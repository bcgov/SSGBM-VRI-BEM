#' Read VRI spatial dataset
#'
#' Read the vegetation resources inventory  (VRI) layer
#'
#' @param dsn data source name (interpretation varies by driver - for some drivers, dsn is a file name, but may also be a folder, or contain the name and access credentials of a database); in case of GeoJSON, dsn may be the character string holding the geojson data. It can also be an open database connection.
#' When dsn is left empty/NULL the polygons will be fetched from the BC Data Catalog.
#' @param layer layer name (varies by driver, may be a file name without extension); in case layer is missing, st_read will read the first layer of dsn, give a warning and (unless quiet = TRUE) print a message when there are multiple layers, or give an error if there are no layers in dsn. If dsn is a database connection, then layer can be a table name or a database identifier (see Id). It is also possible to omit layer and rather use the query argument.
#' @param wkt_filter character; WKT representation of a spatial filter (may be used as bounding box, selecting overlapping geometries)
#' @return sf object
#' @import sf
#' @importFrom bcdata bcdc_query_geodata filter collect INTERSECTS select
#' @importFrom wk as_wkt
#' @export
read_vri <- function(dsn = NULL, layer = "VEG_COMP_LYR_R1_POLY", wkt_filter = character(0)) {

  vri_record <- "2ebb35d8-c82f-4a17-9c96-612ac3532d55"
  vri_resources <- bcdata::bcdc_tidy_resources(vri_record)
  vri_vars <- c("FEATURE_ID","BCLCS_LEVEL_1","BCLCS_LEVEL_2","BCLCS_LEVEL_3","BCLCS_LEVEL_4","BCLCS_LEVEL_5",
    "SPECIES_CD_1","SPECIES_CD_2","SPECIES_CD_3","SPECIES_CD_4","SPECIES_CD_5","SPECIES_CD_6",
    "SPECIES_PCT_1","SPECIES_PCT_2","SPECIES_PCT_3","SPECIES_PCT_4","SPECIES_PCT_5","SPECIES_PCT_6",
    "CROWN_CLOSURE","LAND_COVER_CLASS_CD_1","EST_COVERAGE_PCT_1","LINE_5_VEGETATION_COVER",
    "HARVEST_DATE","PROJ_AGE_1","SOIL_MOISTURE_REGIME_1","SOIL_NUTRIENT_REGIME","INVENTORY_STANDARD_CD",
    "BEC_ZONE_CODE","BEC_SUBZONE","BEC_VARIANT","BEC_PHASE","REFERENCE_YEAR","SITE_POSITION_MESO","SITE_INDEX","EST_SITE_INDEX")

  vri_query <- bcdata::bcdc_query_geodata(record = vri_record) |>
    bcdata::select(.include = vri_vars)

  if (!inherits(wkt_filter, "wk_wkt")) {
    wkt_filter <- wk::as_wkt(wkt_filter)
  }

  if(length(wkt_filter) > 0 ){
    vri_query <- vri_query |> bcdata::filter(local(bcdata::INTERSECTS(sf::st_as_sfc(wkt_filter))))
  }

  collect <- number_of_records(vri_query) <= 50000L

  if (!collect && is.null(dsn)) {
    dsn <- resolve_resource(vri_resources)
  }

  if (collect) {

    rlang::with_options(
      vri <- bcdata::collect(vri_query) |> sf::st_transform(3005),
      bcdata.chunk_limit = 9999
    )

  } else {

    vri <- sf::st_read(
      dsn = dsn,
      quiet = TRUE,
      query = "SELECT %s FROM %s" |> sprintf(paste0(vri_vars, collapse = ","), layer),
      wkt_filter = wkt_filter,
      stringsAsFactors = FALSE,
      as_tibble = TRUE
    )

    if (sf::st_crs(vri) != albers) {
      vri <- vri |> sf::st_transform(3005)
    }

  }

  data.table::setnames(
    x = vri,
    old = c("BCLCS_LEVEL_1", "BCLCS_LEVEL_2", "BCLCS_LEVEL_3", "BCLCS_LEVEL_4", "BCLCS_LEVEL_5",
            "SPECIES_CD_1", "SPECIES_CD_2", "SPECIES_CD_3", "SPECIES_CD_4", "SPECIES_CD_5", "SPECIES_CD_6",
            "SPECIES_PCT_1", "SPECIES_PCT_2", "SPECIES_PCT_3", "SPECIES_PCT_4", "SPECIES_PCT_5"
            ,"SPECIES_PCT_6","CROWN_CLOSURE", "LAND_COVER_CLASS_CD_1", "EST_COVERAGE_PCT_1",
            "LINE_5_VEGETATION_COVER","HARVEST_DATE","BEC_ZONE_CODE","BEC_SUBZONE",
            "BEC_VARIANT","BEC_PHASE","REFERENCE_YEAR"),
    new = c("BCLCS_LV_1", "BCLCS_LV_2", "BCLCS_LV_3", "BCLCS_LV_4", "BCLCS_LV_5",
            "SPEC_CD_1", "SPEC_CD_2", "SPEC_CD_3", "SPEC_CD_4", "SPEC_CD_5", "SPEC_CD_6",
            "SPEC_PCT_1", "SPEC_PCT_2", "SPEC_PCT_3", "SPEC_PCT_4", "SPEC_PCT_5", "SPEC_PCT_6",
            "CR_CLOSURE", "LAND_CD_1", "COV_PCT_1", "LBL_VEGCOV", "HRVSTDT",
            "VRI_BEC_ZONE","VRI_BEC_SUBZON","VRI_BEC_VRT","VRI_BEC_PHASE","VRI_SURVEY_YEAR"),
    skip_absent = TRUE
  )

  vri <- rename_geometry(vri, "Shape")

  # if we have a filter cut all the shapes that are outside of the aoi area
  if (!is.null(wkt_filter)) {
    sf::st_agr(vri) <- "constant"
    vri <- sf::st_intersection(vri, sf::st_as_sfc(wkt_filter, crs = sf::st_crs(vri))) |>
     sf::st_collection_extract() #updated
    vri$Shape <- sf::st_cast(vri$Shape,"MULTIPOLYGON")
  }

  #make shape valid because ARCGIS draw polygon differently than sf
  vri$Shape <- sf::st_cast(sf::st_make_valid(vri$Shape),"MULTIPOLYGON")

  return(vri)
}


#' Read BEM spatial dataset
#'
#' Read the broad ecosystem mapping (BEM) layer
#'
#' @inheritParams read_vri
#' @return sf object
#' @import sf
#' @export
read_bem <- function(dsn, layer = "BEM", wkt_filter = character(0)) {

  if (!inherits(wkt_filter, "wk_wkt")) {
    wkt_filter <- wk::as_wkt(wkt_filter)
  }

  bem <- sf::st_read(
    dsn = dsn,
    layer = layer,
    quiet = TRUE,
    wkt_filter = wkt_filter,
    stringsAsFactors = FALSE,
    as_tibble = TRUE
  )

  if (sf::st_crs(bem) != albers) {
    bem <- bem |> sf::st_transform(3005)
  }

  if(length(wkt_filter) > 0 ){
    sf::st_agr(bem) <- "constant"
    bem <- bem |> sf::st_intersection(sf::st_as_sfc(wkt_filter, crs = sf::st_crs(bem)))
  }

  bem <- rename_geometry(bem, "Shape")
  #make sure all missing BEM values = NA
  bem <- bem |> dplyr::mutate_if(is.character,function(x) ifelse(x %in% c(""," "),NA,x))
  #make shape valid because ARCGIS draw polygon differently than sf
  bem$Shape <- sf::st_make_valid(bem$Shape)

  return(bem)
}


#' Read Wetlands polygons
#'
#' Read the wetlands polygons layer
#'
#' @inheritParams read_vri
#' @return sf object
#' @import sf
#' @importFrom bcdata bcdc_query_geodata filter collect INTERSECTS select
#' @export
read_wetlands <- function(dsn = NULL, layer = "FWA_WETLANDS_POLY", wkt_filter = character(0)) {

  wetlands_record <- "93b413d8-1840-4770-9629-641d74bd1cc6"

  if (!inherits(wkt_filter, "wk_wkt")) {
    wkt_filter <- wk::as_wkt(wkt_filter)
  }

  #If dsn is null read information from bcdata
  if (is.null(dsn)) {
    wl_query <- bcdata::bcdc_query_geodata(record = wetlands_record) |>
      bcdata::select()

    if(length(wkt_filter) > 0 ){
      wl_query <- wl_query |> bcdata::filter(local(bcdata::INTERSECTS(sf::st_as_sfc(wkt_filter))))
    }

    rlang::with_options(
      wetlands <- bcdata::collect(wl_query) |> sf::st_transform(3005),
      bcdata.chunk_limit = 9999
    )

  } else {

    wetlands <- sf::st_read(
      dsn = dsn,
      layer = layer,
      quiet = TRUE,
      wkt_filter = wkt_filter,
      stringsAsFactors = FALSE,
      as_tibble = TRUE
    )

    if (sf::st_crs(wetlands) != albers) {
      wetlands <- wetlands |> sf::st_transform(3005)
    }

  }

  wetlands <- rename_geometry(wetlands, "Shape")
  #make shape valid because ARCGIS draw polygon differently than sf
  wetlands$Shape <- sf::st_make_valid(wetlands$Shape)

  return(wetlands)
}


#' Read rivers polygons
#'
#' Read the rivers layer
#'
#' @inheritParams read_vri
#' @return sf object
#' @import sf
#' @importFrom bcdata bcdc_query_geodata filter collect INTERSECTS select
#' @export
read_rivers <- function(dsn = NULL, layer = "FWA_RIVERS_POLY", wkt_filter = character(0)) {

  rivers_record <- "f7dac054-efbf-402f-ab62-6fc4b32a619e"

  if (!inherits(wkt_filter, "wk_wkt")) {
    wkt_filter <- wk::as_wkt(wkt_filter)
  }

  if (is.null(dsn)) {
    rivers_query <- bcdata::bcdc_query_geodata(record = rivers_record) |>
      bcdata::select()

    if(length(wkt_filter) > 0 ){
      rivers_query <- rivers_query |> bcdata::filter(local(bcdata::INTERSECTS(sf::st_as_sfc(wkt_filter))))
    }

    rlang::with_options(
      rivers <- bcdata::collect(rivers_query) |> sf::st_transform(3005),
      bcdata.chunk_limit = 9999
    )

  } else {

    rivers <- sf::st_read(
      dsn = dsn,
      layer = layer,
      quiet = TRUE,
      wkt_filter = wkt_filter,
      stringsAsFactors = FALSE,
      as_tibble = TRUE
    )

    if (sf::st_crs(rivers) != albers) {
      rivers <- rivers |> sf::st_transform(3005)
    }

  }

  rivers <- rename_geometry(rivers, "GEOMETRY")
  #make shape valid because ARCGIS draw polygon differently than sf
  rivers$GEOMETRY <- sf::st_make_valid(rivers$GEOMETRY)
  return(rivers)
}


#' Read lakes polygons
#'
#' @inheritParams read_vri
#' @return sf object
#' @import sf
#' @importFrom bcdata bcdc_query_geodata filter collect INTERSECTS select
#' @export
read_lakes <- function(dsn = NULL, layer = "FWA_LAKES_POLY", wkt_filter = character(0)) {

  lakes_record <- "cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6"

  if (!inherits(wkt_filter, "wk_wkt")) {
    wkt_filter <- wk::as_wkt(wkt_filter)
  }

  if (is.null(dsn)) {
    lakes_query <- bcdata::bcdc_query_geodata(record = lakes_record) |>
      bcdata::select()

    if(length(wkt_filter) > 0 ){
      lakes_query <- lakes_query |> bcdata::filter(local(bcdata::INTERSECTS(sf::st_as_sfc(wkt_filter))))
    }

    rlang::with_options(
      lakes <- bcdata::collect(lakes_query) |> sf::st_transform(3005),
      bcdata.chunk_limit = 9999
    )

  } else {

    lakes <- sf::st_read(
      dsn = dsn,
      layer = layer,
      quiet = TRUE,
      wkt_filter = wkt_filter,
      stringsAsFactors = FALSE,
      as_tibble = TRUE
    )

    if (sf::st_crs(lakes) != albers) {
      lakes <- lakes |> sf::st_transform(3005)
    }

  }

  lakes <- rename_geometry(lakes, "Shape")
  #make shape valid because ARCGIS draw polygon differently than sf
  lakes$Shape <- sf::st_make_valid(lakes$Shape)

  return(lakes)
}


#' Read glaciers and snow polygons
#'
#' @inheritParams read_vri
#' @return sf object
#' @import sf
#' @importFrom bcdata bcdc_query_geodata filter collect INTERSECTS select
#' @export
# from WHSE_BASEMAPPING.BTM_PRESENT_LAND_USE_V1_SVW, which CEF Human Disturbance 2021 BTM glaciers and snow is based on
read_glaciers <- function(dsn = NULL, layer = "BTM_PLU_V1",  wkt_filter = character(0)) {

  if (!inherits(wkt_filter, "wk_wkt")) {
    wkt_filter <- wk::as_wkt(wkt_filter)
  }

  glaciers_record <- "134fdc69-7b0c-4c50-b77c-e8f2553a1d40"

  if (is.null(dsn)) {
    glaciers_query <- bcdata::bcdc_query_geodata(record = glaciers_record) |>
      bcdata::filter(PRESENT_LAND_USE_LABEL == "Glaciers and Snow") |>
      bcdata::select()

    if(length(wkt_filter) > 0 ){
      glaciers_query <- glaciers_query |> bcdata::filter(local(bcdata::INTERSECTS(sf::st_as_sfc(wkt_filter))))
    }

    rlang::with_options(
      glaciers <- bcdata::collect(glaciers_query) |> sf::st_transform(3005),
      bcdata.chunk_limit = 9999
    )

  } else {

    glaciers <- sf::st_read(
      dsn = dsn,
      layer = layer,
      quiet = TRUE,
      wkt_filter = wkt_filter,
      stringsAsFactors = FALSE,
      as_tibble = TRUE
    )

    if (sf::st_crs(glaciers) != albers) {
      glaciers <- glaciers |> sf::st_transform(3005)
    }

  }

  glaciers <- rename_geometry(glaciers, "Shape")
  #make shape valid because ARCGIS draw polygon differently than sf
  glaciers$Shape <- sf::st_make_valid(glaciers$Shape)

  return(glaciers)
}


#' Read CCB polygons
#'
#' Read the consolidated cutblocks (CCB) layer
#'
#' @inheritParams read_vri
#' @return sf object
#' @import sf
#' @importFrom bcdata bcdc_query_geodata filter collect INTERSECTS select
#' @export
read_ccb <- function(dsn = NULL, layer = "Cut_Block_all_BC",  wkt_filter = character(0)) {

  if (!inherits(wkt_filter, "wk_wkt")) {
    wkt_filter <- wk::as_wkt(wkt_filter)
  }

  ccb_record <- "b1b647a6-f271-42e0-9cd0-89ec24bce9f7"
  ccb_resources <- bcdata::bcdc_tidy_resources(ccb_record)
  ccb_vars <- "HARVEST_YEAR"

  ccb_query <- bcdata::bcdc_query_geodata(record = ccb_record) |>
    bcdata::select(.include = ccb_vars)

  if(length(wkt_filter) > 0 ){
    ccb_query <- ccb_query |> bcdata::filter(local(bcdata::INTERSECTS(sf::st_as_sfc(wkt_filter))))
  }

  collect <- number_of_records(ccb_query) <= 10000L

  if (!collect && is.null(dsn)) {
    dsn <- resolve_resource(ccb_resources)
  }

  if (collect) {

    rlang::with_options(
      ccb <- bcdata::collect(ccb_query) |> sf::st_transform(3005),
      bcdata.chunk_limit = 9999
    )

  } else {

    ccb <- sf::st_read(
      dsn = dsn,
      quiet = TRUE,
      query = "SELECT %s FROM %s" |> sprintf(paste0(ccb_vars, collapse = ","), layer),
      wkt_filter = wkt_filter,
      stringsAsFactors = FALSE,
      as_tibble = TRUE
    )

    if (sf::st_crs(ccb) != albers) {
      ccb <- ccb |> sf::st_transform(3005)
    }

  }

  ccb <- rename_geometry(ccb, "Shape")
  #make shape valid because ARCGIS draw polygon differently than sf
  ccb$Shape <- sf::st_make_valid(ccb$Shape)

  return(ccb)
}

#' Read burn severity polygons
#'
#' Read the forest fire severity layer
#'
#' @inheritParams read_vri
#' @return sf object
#' @import sf
#' @importFrom bcdata bcdc_query_geodata filter collect INTERSECTS select
#' @export
read_burn <- function(dsn = NULL, layer = "WHSE_FOREST_VEGETATION_VEG_BURN_SEVERITY_SP",  wkt_filter = character(0)) {

  burn_record <- "c58a54e5-76b7-4921-94a7-b5998484e697"
  burn_vars <- "BURN_SEVERITY_RATING"

  if (!inherits(wkt_filter, "wk_wkt")) {
    wkt_filter <- wk::as_wkt(wkt_filter)
  }

  if (is.null(dsn)) {
    burn_query <- bcdata::bcdc_query_geodata(record = burn_record) |>
      bcdata::filter(BURN_SEVERITY_RATING %in% c("High", "Low", "Medium")) |>
      bcdata::select(.include = burn_vars)

    if(length(wkt_filter) > 0 ){
      burn_query <- burn_query |> bcdata::filter(local(bcdata::INTERSECTS(sf::st_as_sfc(wkt_filter))))
    }

    rlang::with_options(
      burn <- bcdata::collect(burn_query) |> sf::st_transform(3005),
      bcdata.chunk_limit = 9999
    )

  } else {

    burn <- sf::st_read(
      dsn = dsn,
      layer = layer,
      quiet = TRUE,
      wkt_filter = wkt_filter,
      stringsAsFactors = FALSE,
      as_tibble = TRUE
    )

    if (sf::st_crs(burn) != albers) {
      burn <- burn |> sf::st_transform(3005)
    }

  }

  burn <- rename_geometry(burn, "Shape")
  #make shape valid because ARCGIS draw polygon differently than sf
  burn$Shape <- sf::st_make_valid(burn$Shape)

  return(burn)
}


#' Read fire polygons (fire perimeters)
#'
#' Read the forest fire perimeter layer (WHSE_LAND_AND_NATURAL_RESOURCE.PROT_HISTORICAL_FIRE_POLYS_SP)
#'
#' @inheritParams read_vri
#' @return sf object
#' @import sf
#' @importFrom bcdata bcdc_query_geodata filter collect INTERSECTS select
#' @export
read_fire <- function(dsn = NULL, layer = "WHSE_LAND_AND_NATURAL_RESOURCE.PROT_HISTORICAL_FIRE_POLYS_SP",  wkt_filter = character(0)) {

  fire_record <- "22c7cb44-1463-48f7-8e47-88857f207702"

  if (!inherits(wkt_filter, "wk_wkt")) {
    wkt_filter <- wk::as_wkt(wkt_filter)
  }

  if (is.null(dsn)) {
    fire_query <- bcdata::bcdc_query_geodata(record = fire_record) |>
      bcdata::select()

    if(length(wkt_filter) > 0) {
      fire_query <- fire_query |> bcdata::filter(local(bcdata::INTERSECTS(sf::st_as_sfc(wkt_filter))))
    }

    rlang::with_options(
      fire <- bcdata::collect(fire_query) |> sf::st_transform(3005),
      bcdata.chunk_limit = 9999
    )

  } else {

    fire <- sf::st_read(
      dsn = dsn,
      layer = layer,
      quiet = TRUE,
      wkt_filter = wkt_filter,
      stringsAsFactors = FALSE,
      as_tibble = TRUE
    )

    if (sf::st_crs(fire) != albers) {
      fire <- fire |> sf::st_transform(3005)
    }

  }

  fire <- rename_geometry(fire, "Shape")
  #make shape valid
  fire$Shape <- sf::st_make_valid(fire$Shape)

  return(fire)
}


#'
#'
#'Read aoi (TSAs)
#'
#'Read in the TSA of interest as an aoi
#'Filter to Skeena region (for TSAs that extend past Skeena region boundary). If not required, Skeena_boundary=FALSE
#'
#' @param tsa_name Character vector. A vector of TSA_NUMBER_DESCRIPTION values to filter.
#' @param Skeena_boundary Boolean. Restrict tsa to Skeena region.
#' @return sf object
#' @import sf
#' @importFrom bcdata bcdc_query_geodata filter collect select
#' @export
read_tsa <- function(tsa_name, Skeena_boundary = TRUE){

  aoi_record <- "8daa29da-d7f4-401c-83ae-d962e3a28980"
  skeena_record <- "dfc492c0-69c5-4c20-a6de-2c9bc999301f"

  rlang::with_options(
    {
      aoi <- bcdata::bcdc_query_geodata(aoi_record) |>
        bcdata::filter(TSA_NUMBER_DESCRIPTION %in% tsa_name) |>
        bcdata::select(TSA_NUMBER_DESCRIPTION) |>
        bcdata::collect() |>
        sf::st_union() |> #get rid of boundaries between blocks
        sf::st_as_sf() |>
        sf::st_make_valid() |> #make sure shape is valid
        rename_geometry("Shape") |>
        sf::st_transform(3005)
    },
    bcdata.chunk_limit = 9999
  )

  #make sure aoi within Skeena boundary (if needed)
  if (isTRUE(Skeena_boundary)) {

    rlang::with_options(
      {
        Skeena_aoi <- bcdata::bcdc_query_geodata(skeena_record) |>
          bcdata::filter(ORG_UNIT_NAME == "Skeena Natural Resource Region") |>
          bcdata::select(ORG_UNIT_NAME) |>
          bcdata::collect() |>
          sf::st_union() |> #get rid of boundaries between blocks
          sf::st_as_sf() |>
          sf::st_make_valid() |> #make sure shape is valid
          rename_geometry("Shape") |>
          sf::st_transform(3005)
      },
      bcdata.chunk_limit = 9999
    )

    aoi <- sf::st_intersection(aoi, Skeena_aoi) |>
      sf::st_make_valid()

  }

  return(aoi)

}


#' Cache directory
#' @rdname cache
#' @export
cache_dir <- function() {
  d <- tools::R_user_dir("SSGBM.VRI.BEM", "cache")
  if (!dir.exists(d)) {
    message("Creating directory to cache package assets at ", d)
    dir.create(d, showWarnings = FALSE, recursive = TRUE)
  }
  return(d)
}

#' Remove cache
#' @rdname cache
#' @export
cache_remove <- function() {
  unlink(cache_dir(), recursive = TRUE)
}

number_of_records <- function(x) {
  x$query_list$CQL_FILTER <- bcdata:::finalize_cql(x$query_list$CQL_FILTER)
  query_list <- x$query_list
  cli <- x$cli
  bcdata:::bcdc_number_wfs_records(query_list, cli)
}

resolve_resource <- function(x) {
  x <- x[x$format == "fgdb",c("url", "id")][1,]
  path <- file.path(cache_dir(), x$id)
  if (!dir.exists(path)) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    file <- file.path(path, basename(x$url))
    rlang::with_options(
      download.file(x$url, file, mode = "wb"),
      timeout = 3600
    )
    if (tools::file_ext(file) |> tolower() == "zip") {
      unzip(file, exdir = path)
      unlink(file)
      file <- tools::file_path_sans_ext(file)
    }
  } else {
    file <- file.path(path, tools::file_path_sans_ext(basename(x$url)))
    message("Reusing resource from cache [%s]" |> sprintf(file))
  }
  if (tools::file_ext(file) %in% "") file <- sprintf("%s.gdb", file)
  return(file)
}
