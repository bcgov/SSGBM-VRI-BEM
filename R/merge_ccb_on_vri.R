#' Union merge between CCB attributes and VRI features
#'
#' Performs a Union merge between CCB (Consolidated Cutblocks) attributes on VRI (vegetation ressource inventory) features
#'
#' @param vri_bem sf object that represent VRI (vegetation ressource inventory) features
#' @param ccb sf object that represent CCB (consolidated cutblock) features
#' @return sf object that represent the intersections of ccb and vri
#' @import sf
#' @export
merge_ccb_on_vri <- function(vri_bem, ccb) {

  # tell sf that attributes are constant throughout the geometries to avoid warning
  st_agr(ccb) <- "constant"
  st_agr(vri_bem) <- "constant"
  # TODO merge union
  return(st_as_sf(st_intersection(vri_bem, ccb)))

  #TODO the more I think about it , the more  simple intersection is probably best
  # why would we want the polygon were we have information for ccb but not vri
  # and also the inverse when we have vri but no ccb (I guess we want them if the ccb variable are not necessary)
}
