#' Compute slope and aspect from elevation and their mean by polygon
#'
#' This function overlays the user-specified polygon feature class with slope and aspect raster to
#' add mean aspect, mean slope and slope modifiers to the polygon attributes
#'
#' @param elev_raster SpatRaster object that represent the elevation
#' @param vri_bem sf object containing VRI-BEM
#' @param elevation_threshold numeric elevation threshold used to create above elevation indicator (`ABOVE_ELEV_THOLD`)
#' @param terrain_raster SpatRaster that contains slope and aspect computed in radiants
#' @return VRI-BEM augmented of the following variables : MEAN_ASP, MEAN_SLOPE, SLOPE_MOD and ABOVE_ELEV_THOLD.
#' @details
#' Aspect and slope are calculated by extracting the information from the elevation raster.
#'
#' Based on whether the the elevation is above the selected threshold then ABOLVE_ELEV_THOLD is created with either "Y" or "N".
#'
#' The SLOPE_MOD is then defined using the following logic:
#'  * k - _cool aspect_:  if aspect is 285-359 or 0-134 AND slope is 25%-100% (except in CWH and MH zones, it's 35%-100%)
#'  * q - _very steep cool aspect_:  if aspect is 285-359 or 0-134 AND slope > 100%
#'  * w - _warm aspect_:  if aspect is 135-284 AND slope is 25%-100% (except in CWH and MH zones, it's 35%-100%)
#'  * z - _very steep warm aspect_: if aspect is 135-284 AND slope > 100%
#'
#' @import data.table
#' @importFrom terra extract terrain `add<-` vect
#' @export
#'
merge_elevation_raster_on_sf <- function(elev_raster, vri_bem, elevation_threshold = 1400, terrain_raster = NA) {

  # TODO check if terra is able to compute this even when the raster is to big to me loaded in RAM at once
  # Compute slope and aspect ----

  if (is.na(terrain_raster)) {
    terrain_raster <- terrain(elev_raster, v = c("slope", "aspect"), unit = "radians")
  }

  # Combine elevation slope and aspect into one layered raster
  add(terrain_raster) <- elev_raster

  # Extract raster values for each of vri_bem polygons ----
  # ARCGIS default aspect for flat slope is -1 (not exactly north) degree (we use radians for sin and cos so 6.265732 radiants)  whereas in terra it's 90 degree (east)
  # we excluded all points that had a slope of 0 when computing the mean aspect to avoid creating bias towards the default value when the slope is 0 (in rivers for example)
  # compute mean slope in pct (0 degree is 0% and 90 degree is 100%) by vri_bem
  # compute mean aspect using circular mean by vri_bem and converting to positive degrees ( to 360)
  mean_raster_by_vri_bem <- setDT(extract(terrain_raster,
                                          vect(vri_bem)))[, .(ELEV = mean(dem),
                                                              MEAN_SLOPE = mean(slope, na.rm = T) * 57.29578/90,
                                                              MEAN_ASP = ((atan2(sum(sin(aspect) * (slope > 0), na.rm = T)/sum(slope > 0, na.rm = T), sum(cos(aspect) * (slope > 0), na.rm = T)/sum(slope > 0 , na.rm = T)) * 57.29578) + 360) %% 360),
                                                              by = .(vri_bem_index = as.integer(ID))]



  setDT(vri_bem)

  # Merge info into VRI-BEM ----
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

  # Create elevation above threshold indicator ----
  if (!is.null(vri_bem[["ABOVE_ELEV_THOLD"]])){
    set(vri_bem, j = "ABOVE_ELEV_THOLD", value = NULL)
  }
  value_map <- c("N", "Y")
  set(vri_bem, j = "ABOVE_ELEV_THOLD", value = value_map[(vri_bem[["ELEV"]] > elevation_threshold) + 1])


  # Populate SLOPE_MOD field ----
  ## "k" (cool aspect) if aspect is 285-359 or 0-134 AND slope is 25%-100% (except in CWH and MH zones, it's 35%-100%)
  ## "q" (very steep cool aspect) if aspect is 285-359 or 0-134 AND slope > 100%
  ## "w" (warm aspect) if aspect is 135-284 AND slope is 25%-100% (except in CWH and MH zones, it's 35%-100%)
  ## "z" (very steep warm aspect) if aspect is 135-284 AND slope > 100%


  set(vri_bem, j = "SLOPE_MOD", value = NA_character_)

  no_slope_mod_MC <- c("LL","LS","LA","La","OW","Pd","PD","RE","RI","Ri","Wa","WE","Wm","Ww","Ws","WL")
  vri_bem[!is.na(MEAN_SLOPE) & !is.na(MEAN_ASP) & !BEUMC_S1 %in% no_slope_mod_MC & !BEUMC_S2 %in% no_slope_mod_MC & !BEUMC_S3 %in% no_slope_mod_MC,
          SLOPE_MOD:=fcase(BGC_ZONE %in% c("CWH", "MH") & (MEAN_ASP >= 285 | MEAN_ASP <= 134) & (MEAN_SLOPE >= 10 & MEAN_SLOPE < 35), "j",
                           BGC_ZONE %in% c("CWH", "MH") & (MEAN_ASP >= 285 | MEAN_ASP <= 134) & (MEAN_SLOPE >= 35 & MEAN_SLOPE <= 100), "k",
                           BGC_ZONE %in% c("CWH", "MH") & (MEAN_ASP >= 285 | MEAN_ASP <= 134) & (MEAN_SLOPE > 100), "q",

                           BGC_ZONE %in% c("CWH", "MH") & (MEAN_ASP >= 135 & MEAN_ASP <= 284) & (MEAN_SLOPE >= 10 & MEAN_SLOPE < 35), "j",
                           BGC_ZONE %in% c("CWH", "MH") & (MEAN_ASP >= 135 & MEAN_ASP <= 284) & (MEAN_SLOPE >= 35 & MEAN_SLOPE <= 100), "w",
                           BGC_ZONE %in% c("CWH", "MH") & (MEAN_ASP >= 135 & MEAN_ASP <= 284) & (MEAN_SLOPE > 100), "z",

                           (MEAN_ASP >= 285 | MEAN_ASP <= 134) & (MEAN_SLOPE >= 10 & MEAN_SLOPE < 25), "j",
                           (MEAN_ASP >= 285 | MEAN_ASP <= 134) & (MEAN_SLOPE >= 25 & MEAN_SLOPE <= 100), "k",
                           (MEAN_ASP >= 285 | MEAN_ASP <= 134) & (MEAN_SLOPE > 100), "q",

                           (MEAN_ASP >= 135 & MEAN_ASP <= 284) & (MEAN_SLOPE >= 10 & MEAN_SLOPE < 25), "j",
                           (MEAN_ASP >= 135 & MEAN_ASP <= 284) & (MEAN_SLOPE >= 25 & MEAN_SLOPE <= 100), "w",
                           (MEAN_ASP >= 135 & MEAN_ASP <= 284) & (MEAN_SLOPE > 100), "z",

                           default = NA_character_)]


  return(st_as_sf(vri_bem))

}
