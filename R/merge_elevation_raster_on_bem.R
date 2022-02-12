#' Compute slope and aspect from elevation and their mean by polygon
#'
#' This function overlays the user-specified polygon feature class with slope and aspect raster to
#' add mean aspect, mean slope and slope modifiers to the polygon attributes
#'
#' @param elev_raster SpatRaster object that represent the elevation
#' @param vri_bem sf object containing VRI-BEM
#' @param elevation_threshold numeric elevation threshold use to create above elevation indicator
#' @return data.table
#' @import data.table
#' @importFrom terra extract terrain `add<-` vect
#' @export
#'
merge_elevation_raster_on_sf <- function(elev_raster, vri_bem, elevation_threshold = 1400) {

  # compute slope and aspect
  # TODO check if terra is able to compute this even when the raster is to big to me loaded in RAM at once
  terrain_raster <- terrain(elev_raster, v = c("slope", "aspect"), unit = "radians")

  # combine elevation slope and aspect into one layered raster
  add(terrain_raster) <- elev_raster

  # extract raster values for each of vri_bem polygons
  # adjustment because ARCGIS default aspect for flat slope is -1 (not exactly north) degree (we use radians for sin and cos so 6.265732 radiants)  whereas in terra it's 90 degree (east)
  # compute mean slope in pct (0 degree is 0% and 90 degree is 100%) by vri_bem
  # compute mean aspect using circular mean by vri_bem and converting to positive degrees ( to 360)
  mean_raster_by_vri_bem <- setDT(extract(terrain_raster,
                                          vect(vri_bem)))[slope == 0,
                                                          aspect := 6.265732][, .(ELEV = mean(dem),
                                                                                  MEAN_SLOPE = mean(slope, na.rm = T) * 57.29578/90,
                                                                                  MEAN_ASP = ((atan2(mean(sin(aspect), na.rm = T), mean(cos(aspect), na.rm = T)) * 57.29578) + 360) %% 360),
                                                                              by = .(vri_bem_index = as.integer(ID))]



  setDT(vri_bem)

  for (variable in c("ELEV", "MEAN_SLOPE", "MEAN_ASP")) {
    # remove variables if they already exist in vri_bem
    if (!is.null(vri_bem[[variable]])){
      set(vri_bem, j = variable, value = NULL)
    }

    # Add variables to vri_bem
    set(vri_bem, i = mean_raster_by_vri_bem[["vri_bem_index"]], j = variable, value = mean_raster_by_vri_bem[[variable]])
  }

  which_no_elev <- which(is.na(vri_bem[["ELEV"]]))
  if (length(which_no_elev) > 0) {
    warning("could not calculate elevation for the following lines : ", paste(which_no_elev, collapse = ", "))
  }

  # create elevation above threshold indicator
  if (!is.null(vri_bem[["ABOVE_ELEV_THOLD"]])){
    set(vri_bem, j = "ABOVE_ELEV_THOLD", value = NULL)
  }
  value_map <- c("N", "Y")
  set(vri_bem, j = "ABOVE_ELEV_THOLD", value = value_map[(vri_bem[["ELEV"]] > elevation_threshold) + 1])

  return(st_as_sf(vri_bem))

}
