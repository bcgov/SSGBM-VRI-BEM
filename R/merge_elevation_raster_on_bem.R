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

  mean_raster_by_bem <- setDT(extract(terrain_raster, vect(bem)))[, .(ELEV = mean(dem),
                                                                      MEAN_SLOPE = atan2(mean(sin(slope), na.rm = T), mean(cos(slope), na.rm = T)) * 57.29578,
                                                                      MEAN_ASP =  atan2(mean(sin(aspect), na.rm = T), mean(cos(aspect), na.rm = T)) * 57.29578), by = .(bem_index = as.integer(ID))]


  setDT(bem)

  for (variable in c("ELEV", "MEAN_SLOPE", "MEAN_ASP")) {
    set(bem, i = mean_raster_by_bem[["bem_index"]], j = variable, value = mean_raster_by_bem[[variable]])
  }

  return(st_as_sf(bem))

}
