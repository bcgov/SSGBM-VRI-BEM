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
#' @import terra
#' @export
create_hem_rasters <- function(vri_bem, path , xmin = 1020387.5, xmax = 1030387.5, ymin = 960987.5, ymax = 970987.5, resolution = 100, crs = 'PROJCS["NAD_1983_BC_Environment_Albers",GEOGCS["GCS_North_American_1983",DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137.0,298.257222101]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Albers"],PARAMETER["False_Easting",1000000.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",-126.0],PARAMETER["Standard_Parallel_1",50.0],PARAMETER["Standard_Parallel_2",58.5],PARAMETER["Latitude_Of_Origin",45.0],UNIT["Meter",1.0]]') {

  v_vri <- vect(vri_bem)
  r_vri <- rast(v_vri, xmin = xmin , xmax = xmax , ymin = ymin, ymax = ymax, resolution = resolution, crs = crs)
  raster_vri <- rasterize(v_vri, r_vri, hem_fields$names[1])
  for (field in hem_fields$names[-1]) {
      add(raster_vri) <- rasterize(v_vri, r_vri, field)
    }

  layer_names <- hem_fields[ , fcase(!(names %in% c("Dynamic_All", "Static_All")), names,
                      names == "Dynamic_All", "Dynamic_all_meets_site_con",
                      names == "Static_All", "Static_all_meets_site_con")]

  set.names(raster_vri, layer_names)

  writeRaster(raster_vri, path)

  return(raster_vri)
}
