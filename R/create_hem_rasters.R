#' This is data to be included in my package
#'
#' @name hem_fields
#' @docType data
#' @keywords data
NULL

#' This is data to be included in my package
#'
#' @name albers
#' @docType data
#' @keywords data
NULL

#' Create raster with layers for each hem fields
#'
#'
#' @param vri_bem sf object that represent VRI (vegetation ressource inventory) features
#' @param path character string that represent the path the output raster will be save to
#' @param xmin numeric that represent the xmin of the extent of the raster
#' @param xmax numeric that represent the xmax of the extent of the raster
#' @param ymin numeric that represent the ymin of the extent of the raster
#' @param ymax numeric that represent the ymax of the extent of the raster
#' @param resolution numeric that represent the resolution of the raster
#' @param crs character string of the WKT to be used for the projection of the raster
#'
#' @return spatRaster object
#' @importFrom terra vect rast rasterize `add<-` set.names writeRaster
#' @export
create_hem_rasters <- function(vri_bem, path,
                               xmin = 1020387.5, xmax = 1030387.5,
                               ymin = 960987.5, ymax = 970987.5,
                               resolution = 100, crs = SSGBM.VRI.BEM::albers) {

  if (FALSE) {
    hem
  }

  v_vri <- terra::vect(vri_bem)
  r_vri <- terra::rast(v_vri, xmin = xmin , xmax = xmax , ymin = ymin, ymax = ymax, resolution = resolution, crs = crs)
  raster_vri <- terra::rasterize(v_vri, r_vri, SSGBM.VRI.BEM::hem_fields$names[1])
  for (field in SSGBM.VRI.BEM::hem_fields$names[-1]) {
      terra::add(raster_vri) <- terra::rasterize(v_vri, r_vri, field)
    }

  layer_names <- SSGBM.VRI.BEM::hem_fields[ , fcase(!(names %in% c("Dynamic_All", "Static_All")), names,
                      names == "Dynamic_All", "Dynamic_all_meets_site_con",
                      names == "Static_All", "Static_all_meets_site_con")]

  terra::set.names(raster_vri, layer_names)

  terra::writeRaster(raster_vri, path)

  return(raster_vri)
}
