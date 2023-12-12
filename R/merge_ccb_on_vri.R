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

  vri_ccb_intersection <- st_intersection(vri_bem, ccb) |> st_make_valid() |> st_collection_extract() |> st_cast("POLYGON") |> st_make_valid()
  vri_ccb_diff <- st_difference(vri_bem, st_union(st_geometry(ccb))) |> st_make_valid() |> st_collection_extract() |> st_cast("POLYGON", warn = FALSE) |> st_make_valid()

  return(dplyr::bind_rows(vri_ccb_intersection, vri_ccb_diff) |> transform_bc_albers())
}
