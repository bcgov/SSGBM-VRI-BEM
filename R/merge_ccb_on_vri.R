#' Merge BEM (broad ecosystem mapping) attributes on VRI (vegetation ressource inventory) features
#'
#' @param vri sf object that represent VRI (vegetation ressource inventory) features
#' @param ccb sf object that represent CCB (consolidated cutblock) features
#' @return sf object that represent the intersections of ccb and vri
#' @import sf
#' @export
merge_ccb_on_vri <- function(vri, ccb) {

  # tell sf that attributes are constant throughout the geometries to avoid warning
  st_agr(ccb) <- "constant"
  st_agr(vri) <- "constant"

  return(st_intersection(vri, ccb))

}
