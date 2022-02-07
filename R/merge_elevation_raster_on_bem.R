#' Compute slope and aspect from elevation and compute mean by ifc polygon
#'
#' @param elev_raster SpatRaster object that represent the elevation
#' @param ifc sf object
#' @param elevation_threshold numeric elevation threshold use to create above elevation indicator
#' @return data.table
#' @import data.table
#' @importFrom terra extract terrain `add<-` vect
#' @export
#'
merge_elevation_raster_on_sf <- function(elev_raster, ifc, elevation_threshold = 1400) {

  # compute slope and aspect
  terrain_raster <- terrain(elev_raster, v = c("slope", "aspect"), unit = "radians")

  # combine elevation slope and aspect into one layered raster
  add(terrain_raster) <- elev_raster

  # extract raster values for each of ifc polygons
  # adjustment because ARCGIS default aspect for flat slope is -1 (not exactly north) degree (we use radians for sin and cos so 6.265732 radiants)  whereas in terra it's 90 degree (east)
  # compute mean slope in pct (0 degree is 0% and 90 degree is 100%) by ifc
  # compute mean aspect using circular mean by ifc and converting to positive degrees ( to 360)
  mean_raster_by_ifc <- setDT(extract(terrain_raster, vect(ifc)))[slope == 0, aspect := 6.265732][, .(ELEV = mean(dem),
                                                                      MEAN_SLOPE = mean(slope, na.rm = T) * 57.29578/90,
                                                                      MEAN_ASP =  ((atan2(mean(sin(aspect), na.rm = T), mean(cos(aspect), na.rm = T)) * 57.29578) + 360) %% 360),
                                                                  by = .(ifc_index = as.integer(ID))]



  setDT(ifc)

  for (variable in c("ELEV", "MEAN_SLOPE", "MEAN_ASP")) {
    # remove variables if they already exist in ifc
    if (!is.null(ifc[[variable]])){
      set(ifc, j = variable, value = NULL)
    }

    # Add variables to ifc
    set(ifc, i = mean_raster_by_ifc[["ifc_index"]], j = variable, value = mean_raster_by_ifc[[variable]])
  }

  which_no_elev <- which(is.na(ifc[["ELEV"]]))
  if (length(which_no_elev) > 0) {
    warning("could not calculate elevation for the following lines : ", paste(which_no_elev, collapse = ", "))
  }

  # create elevation above threshold indicator
  if (!is.null(ifc[["ABOVE_ELEV_THOLD"]])){
    set(ifc, j = "ABOVE_ELEV_THOLD", value = NULL)
  }
  value_map <- c("N", "Y")
  set(ifc, j = "ABOVE_ELEV_THOLD", value = value_map[(ifc[["ELEV"]] > elevation_threshold) + 1])

  return(st_as_sf(ifc))

}
