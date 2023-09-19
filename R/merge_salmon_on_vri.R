#' Union merge between CCB attributes and VRI features
#'
#' Performs a Union merge between CCB (Consolidated Cutblocks) attributes on VRI (vegetation ressource inventory) features
#'
#' @param vri_bem sf object that represent VRI (vegetation ressource inventory) features
#' @param salmon sf object that represent salmon features
#' @return sf object that represent the intersections of salmon and vri
#' @import sf
#' @export
merge_salmon_on_vri <- function(vri_bem, salmon) {
  # tell sf that attributes are constant throughout the geometries to avoid warning
  st_agr(salmon) <- "constant"
  st_agr(vri_bem) <- "constant"

  vri_salmon_intersection <- st_intersection(vri_bem, salmon) |> st_make_valid() |> st_collection_extract() |> st_cast("POLYGON") |> st_make_valid()
  vri_salmon_diff <- st_difference(vri_bem, st_union(st_geometry(salmon))) |> st_make_valid() |> st_collection_extract() |> st_cast("POLYGON", warn = FALSE) |> st_make_valid()

  return(dplyr::bind_rows(vri_salmon_intersection, vri_salmon_diff))
}
