#' Compute slope and aspect from elevation and compute mean by bem polygon
#'
#' @param elev_raster SpatRaster object that represent the elevation
#' @param bem sf object that represent BEM (broad ecosystem mapping) features
#' @return data.table
#' @import data.table
#' @import terra
#' @export
#'
merge_elevation_raster_on_bem <- function(elev_raster, bem) {

  # compute slope and aspect
  terrain_raster <- terrain(elev_raster, v = c("slope", "aspect"), unit = "radians")

  # combine elevation slope and aspect into one layered raster
  add(terrain_raster) <- elev_raster

  mean_raster_by_bem <- setDT(extract(terrain_raster, vect(bem)))[, .(mean_elev = mean(dem),
                                                                      mean_slope = atan2(mean(sin(slope), na.rm = T), mean(cos(slope), na.rm = T)),
                                                                      mean_aspect =  atan2(mean(sin(aspect), na.rm = T), mean(cos(aspect), na.rm = T))), by = .(bem_index = as.integer(ID))]


  classes_bem <- attr(bem, "class")
  setDT(bem)

  for (variable in c("mean_elev", "mean_slope", "mean_aspect")) {
    set(bem, i = mean_raster_by_bem[["bem_index"]], j = variable, value = mean_raster_by_bem[[variable]])
  }

  # TO DO does not seem to work still return a data.table object
  attr(bem, "class") <- classes_bem

  return(st_as_sf(bem))

}
