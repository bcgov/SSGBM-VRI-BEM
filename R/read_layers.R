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
#' @importFrom bcdata bcdc_query_geodata filter collect INTERSECTS select  `%>%`
#' @export
read_vri <- function(dsn = NULL, layer = "VEG_R1_PLY_polygon", wkt_filter = NULL) {


  if (is.null(dsn)){
    vri_query <- bcdc_query_geodata(record =  "2ebb35d8-c82f-4a17-9c96-612ac3532d55")  %>%
      select(BCLCS_LEVEL_1, BCLCS_LEVEL_2, BCLCS_LEVEL_3, BCLCS_LEVEL_4, BCLCS_LEVEL_5,
             SPECIES_CD_1, SPECIES_CD_2, SPECIES_CD_3, SPECIES_CD_4, SPECIES_CD_5, SPECIES_CD_6,
             SPECIES_PCT_1, SPECIES_PCT_2, SPECIES_PCT_3, SPECIES_PCT_4, SPECIES_PCT_5, SPECIES_PCT_6,
             CROWN_CLOSURE, LAND_COVER_CLASS_CD_1, EST_COVERAGE_PCT_1, LINE_5_VEGETATION_COVER,
             HARVEST_DATE, PROJ_AGE_1, SOIL_MOISTURE_REGIME_1, SOIL_NUTRIENT_REGIME,INVENTORY_STANDARD_CD,
             BEC_ZONE_CODE,BEC_SUBZONE, BEC_VARIANT,BEC_PHASE)

    if(length(wkt_filter) > 0 ){
      vri_query <- vri_query %>% filter(INTERSECTS(sf::st_as_sfc(wkt_filter)))
    }

    vri <- collect(vri_query)

  } else {

    vri <- st_read(dsn = dsn, layer = layer, quiet = TRUE, wkt_filter = if(is.null(wkt_filter)){character(0)} else{wkt_filter})
  }

  setnames(vri,

           old = c("BCLCS_LEVEL_1", "BCLCS_LEVEL_2", "BCLCS_LEVEL_3", "BCLCS_LEVEL_4", "BCLCS_LEVEL_5",
                   "SPECIES_CD_1", "SPECIES_CD_2", "SPECIES_CD_3", "SPECIES_CD_4", "SPECIES_CD_5", "SPECIES_CD_6",
                   "SPECIES_PCT_1", "SPECIES_PCT_2", "SPECIES_PCT_3", "SPECIES_PCT_4", "SPECIES_PCT_5"
                   ,"SPECIES_PCT_6","CROWN_CLOSURE", "LAND_COVER_CLASS_CD_1", "EST_COVERAGE_PCT_1",
                   "LINE_5_VEGETATION_COVER","HARVEST_DATE","BEC_ZONE_CODE","BEC_SUBZONE",
                   "BEC_VARIANT","BEC_PHASE"),

           new = c("BCLCS_LV_1", "BCLCS_LV_2", "BCLCS_LV_3", "BCLCS_LV_4", "BCLCS_LV_5",
                   "SPEC_CD_1", "SPEC_CD_2", "SPEC_CD_3", "SPEC_CD_4", "SPEC_CD_5", "SPEC_CD_6",
                   "SPEC_PCT_1", "SPEC_PCT_2", "SPEC_PCT_3", "SPEC_PCT_4", "SPEC_PCT_5", "SPEC_PCT_6",
                   "CR_CLOSURE", "LAND_CD_1", "COV_PCT_1", "LBL_VEGCOV", "HRVSTDT",
                   "VRI_BEC_ZONE","VRI_BEC_SUBZON","VRI_BEC_VRT","VRI_BEC_PHASE"),

           skip_absent = TRUE
           )
  #Restructure bem while waiting for real info
  vri <- rename_geometry(vri, "Shape")
  #make shape valid because ARCGIS draw polygon differently than sf
  vri$Shape <- st_cast(st_make_valid(vri$Shape),"MULTIPOLYGON")

  # if we have a filter cut all the shapes that are outside of the aoi area
  if (!is.null(wkt_filter)) {
    st_agr(vri) <- "constant"
    vri <-  st_intersection(vri, st_as_sfc(wkt_filter, crs = st_crs(vri)))
    vri$Shape <- st_cast(vri$Shape,"MULTIPOLYGON")
  }

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
read_bem <- function(dsn, layer = "BEM", wkt_filter = NULL) {
  bem <- st_read(dsn = dsn, layer = layer, quiet = TRUE, wkt_filter = if(is.null(wkt_filter)){character(0)} else{st_as_text(st_as_sfc(wkt_filter))})
  if(length(wkt_filter) > 0 ){
    bem <- st_intersection(bem,wkt_filter)
  }
  #Restructure bem while waiting for real info
  bem <- rename_geometry(bem, "Shape")
  #make sure all missing BEM values = NA
  bem <- bem |> mutate_if(is.character,function(x) ifelse(x %in% c(""," "),NA,x))
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
#' @importFrom bcdata bcdc_query_geodata filter collect INTERSECTS select  `%>%`
#' @export
read_wetlands <- function(dsn = NULL, layer = "FWA_WETLANDS_POLY",  wkt_filter = character(0)) {

  #If dsn is null read information from bcdata
  if (is.null(dsn)){
    wl_query <- bcdc_query_geodata(record =  "93b413d8-1840-4770-9629-641d74bd1cc6") %>%
      select(GEOMETRY)

    if(length(wkt_filter) > 0 ){
      wl_query <- wl_query %>% filter(INTERSECTS(sf::st_as_sfc(wkt_filter)))
    }
    wetlands <- collect(wl_query)

  } else {
    wetlands <- st_read(dsn = dsn, layer = layer, quiet = TRUE,  wkt_filter = wkt_filter)
  }


  #Restructure bem while waiting for real info
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
#' @importFrom bcdata bcdc_query_geodata filter collect INTERSECTS select `%>%`
#' @export
read_rivers <- function(dsn = NULL, layer = "FWA_RIVERS_POLY",  wkt_filter = character(0)) {

  if (is.null(dsn)){
    rivers_query <- bcdc_query_geodata(record =  "f7dac054-efbf-402f-ab62-6fc4b32a619e") %>%
      select(GEOMETRY)

    if(length(wkt_filter) > 0 ){
      rivers_query <- rivers_query %>% filter(INTERSECTS(sf::st_as_sfc(wkt_filter)))
    }
    rivers <- collect(rivers_query)

    rivers <- rename_geometry(rivers, "GEOMETRY")

  } else {

    rivers <- st_read(dsn = dsn, layer = layer, quiet = TRUE, wkt_filter = wkt_filter)
  }
  #make shape valid because ARCGIS draw polygon differently than sf
  rivers$GEOMETRY <- sf::st_make_valid(rivers$GEOMETRY)
  return(rivers)
}



#' Read lakes polygons
#'
#' @inheritParams read_vri
#' @return sf object
#' @import sf
#' @importFrom bcdata bcdc_query_geodata filter collect INTERSECTS select  `%>%`
#' @export
read_lakes <- function(dsn = NULL, layer = "FWA_LAKES_POLY",  wkt_filter = character(0)) {

  #If dsn is null read information from bcdata
  if (is.null(dsn)){
    lakes_query <- bcdc_query_geodata(record =  "cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6") %>%
      select(GEOMETRY)

    if(length(wkt_filter) > 0 ){
      lakes_query <- lakes_query %>% filter(INTERSECTS(sf::st_as_sfc(wkt_filter)))
    }
    lakes <- collect(lakes_query)

  } else {
    lakes <- st_read(dsn = dsn, layer = layer, quiet = TRUE,  wkt_filter = wkt_filter)
  }


  #Restructure bem while waiting for real info
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
#' @importFrom bcdata bcdc_query_geodata filter collect INTERSECTS select  `%>%`
#' @export
#' from WHSE_BASEMAPPING.BTM_PRESENT_LAND_USE_V1_SVW, which CEF Human Disturbance 2021 BTM glaciers and snow is based on
#'
read_glaciers <- function(dsn = NULL, layer = "BTM_PLU_V1",  wkt_filter = character(0)) {

  #If dsn is null read information from bcdata
  if (is.null(dsn)){
    glaciers_query <- bcdc_query_geodata(record =  "134fdc69-7b0c-4c50-b77c-e8f2553a1d40") %>%
      filter(PRESENT_LAND_USE_LABEL == "Glaciers and Snow") %>%
      select(GEOMETRY)

    if(length(wkt_filter) > 0 ){
      glaciers_query <- glaciers_query %>% filter(INTERSECTS(sf::st_as_sfc(wkt_filter)))
    }
    glaciers <- collect(glaciers_query)

  } else {
    glaciers <- st_read(dsn = dsn, layer = layer, quiet = TRUE,  wkt_filter = wkt_filter)
  }


  #Restructure bem while waiting for real info
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
#' @importFrom bcdata bcdc_query_geodata filter collect INTERSECTS select  `%>%`
#' @export
read_ccb <- function(dsn = NULL, layer = "CNS_CUT_BL_polygon",  wkt_filter = character(0)) {

  # If DSN is null fetch the information from the bcdata
  if (is.null(dsn)){
    ccb_query <- bcdc_query_geodata(record = "b1b647a6-f271-42e0-9cd0-89ec24bce9f7") %>%
      select(HARVEST_YEAR)

    if(length(wkt_filter) > 0 ){
      ccb_query <- ccb_query %>% filter(INTERSECTS(sf::st_as_sfc(wkt_filter)))
    }
    ccb <- collect(ccb_query)

    ccb <- rename_geometry(ccb, "Shape")
  } else {

    ccb <- st_read(dsn = dsn, layer = layer, quiet = TRUE, wkt_filter = wkt_filter)
  }

  #make shape valid because ARCGIS draw polygon differently than sf
  ccb$Shape <- sf::st_make_valid(ccb$Shape)
  return(ccb)
}

#' Read fire polygons
#'
#' Read the forest fire severity (fire) layer
#'
#' @inheritParams read_vri
#' @return sf object
#' @import sf
#' @importFrom bcdata bcdc_query_geodata filter collect INTERSECTS select  `%>%`
#' @export
read_fire <- function(dsn = NULL, layer = "WHSE_FOREST_VEGETATION_VEG_BURN_SEVERITY_SP",  wkt_filter = character(0)) {

  #If dsn is null read information from bcdata
  if (is.null(dsn)){
    wl_query <- bcdc_query_geodata(record =  "22c7cb44-1463-48f7-8e47-88857f207702") %>%
      select(SHAPE)

    if(length(wkt_filter) > 0 ){
      wl_query <- wl_query %>% filter(INTERSECTS(sf::st_as_sfc(wkt_filter)))
    }
    fire <- collect(wl_query)

  } else {
    fire <- st_read(dsn = dsn, layer = layer, quiet = TRUE,  wkt_filter = wkt_filter)
  }


  #Restructure bem while waiting for real info
  fire <- rename_geometry(fire, "Shape")
  #make shape valid because ARCGIS draw polygon differently than sf
  fire$Shape <- sf::st_make_valid(fire$Shape)
  return(fire)
}
