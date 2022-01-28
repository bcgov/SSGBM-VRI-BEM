#' Compute slope and aspect from elevation and compute mean by bem polygon
#'
#' @param elev_raster SpatRaster object that represent the elevation
#' @param bem sf object that represent BEM (broad ecosystem mapping) features
#' @return data.table
#' @import data.table
#' @import terra
#'
merge_elevation_raster_on_bem <- function(elev_raster, bem) {

  classes_bem <- attr(bem, "class")
  setDT(bem)

  terrain_raster <- terrain(elev_raster, v = c("slope", "aspect"), unit = "degrees")

  add(terrain_raster) <- elev

  mean_raster_by_bem <- setDT(extract(terrain_raster, vect(bem)))[, .(mean_elev = mean(dem),
                                                                      mean_slope = mean(slope/360),
                                                                      mean_aspect = 360 * mean(aspect/360)), by = .(bem_index = ID)]


  for (variable in c("mean_elev", "mean_slope", "mean_aspect")) {
    set(bem, i = mean_raster_by_bem[["bem_index"]], j = variable, value = mean_raster_by_bem[[variable]])
  }


  attr(bem, "class") <- classes_bem

  return(bem)

}
