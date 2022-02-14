#' Read VRI spatial dataset
#'
#' Read the vegetation resources inventory  (VRI) layer
#'
#' @param dsn data source name (interpretation varies by driver - for some drivers, dsn is a file name, but may also be a folder, or contain the name and access credentials of a database); in case of GeoJSON, dsn may be the character string holding the geojson data. It can also be an open database connection.
#' @param layer layer name (varies by driver, may be a file name without extension); in case layer is missing, st_read will read the first layer of dsn, give a warning and (unless quiet = TRUE) print a message when there are multiple layers, or give an error if there are no layers in dsn. If dsn is a database connection, then layer can be a table name or a database identifier (see Id). It is also possible to omit layer and rather use the query argument.
#' @param wkt_filter character; WKT representation of a spatial filter (may be used as bounding box, selecting overlapping geometries)
#' @return sf object
#' @import sf
#' @export
read_vri <- function(dsn, layer = "VEG_R1_PLY_polygon", wkt_filter = NULL) {

  vri <- st_read(dsn = dsn, layer = layer, quiet = TRUE, wkt_filter = if(is.null(wkt_filter)){character(0)} else{wkt_filter})

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
read_bem <- function(dsn, layer = "BEM", wkt_filter = character(0)) {
  bem <- st_read(dsn = dsn, layer = layer, quiet = TRUE, wkt_filter = wkt_filter)
  #Restructure bem while waiting for real info
  bem <- rename_geometry(bem, "Shape")
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
#' @export
read_wetlands <- function(dsn, layer = "FWA_WETLANDS_POLY",  wkt_filter = character(0)) {
  wetlands <- st_read(dsn = dsn, layer = layer, quiet = TRUE,  wkt_filter = wkt_filter)
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
#' @export
read_rivers <- function(dsn, layer = "FWA_RIVERS_POLY",  wkt_filter = character(0)) {
  rivers <- st_read(dsn = dsn, layer = layer, quiet = TRUE, wkt_filter = wkt_filter)
  #make shape valid because ARCGIS draw polygon differently than sf
  rivers$GEOMETRY <- sf::st_make_valid(rivers$GEOMETRY)
  return(rivers)
}

#' Read CCB polygons
#'
#' Read the consolidated cutblocks (CCB) layer
#'
#' @inheritParams read_vri
#' @return sf object
#' @import sf
#' @export
read_ccb <- function(dsn, layer = "CNS_CUT_BL_polygon",  wkt_filter = character(0)) {
  ccb <- st_read(dsn = dsn, layer = layer, quiet = TRUE, wkt_filter = wkt_filter)
  #make shape valid because ARCGIS draw polygon differently than sf
  ccb$Shape <- sf::st_make_valid(ccb$Shape)
  return(ccb)
}

