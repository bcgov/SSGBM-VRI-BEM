#' Union merge between CCB attributes and VRI features
#'
#' Performs a Union merge between CCB (Consolidated Cutblocks) attributes on VRI (vegetation ressource inventory) features
#'
#' @param vri_bem sf object that represent VRI (vegetation ressource inventory) features
#' @param ccb sf object that represent CCB (consolidated cutblock) features
#' @return sf object that represent the intersections of ccb and vri
#' @import sf
#' @importFrom bcmaps transform_bc_albers
#' @importFrom dplyr bind_rows
#' @export
merge_ccb_on_vri <- function(vri_bem, ccb) {

  # tell sf that attributes are constant throughout the geometries to avoid warning
  sf::st_agr(ccb) <- "constant"
  sf::st_agr(vri_bem) <- "constant"

  vri_ccb_intersection <- sf::st_intersection(vri_bem, ccb) |>
    sf::st_make_valid() |>
    sf::st_collection_extract() |>
    sf::st_cast("POLYGON") |>
    sf::st_make_valid()

  vri_ccb_diff <- sf::st_difference(vri_bem, sf::st_union(sf::st_geometry(ccb))) |>
    sf::st_make_valid() |>
    sf::st_collection_extract() |>
    sf::st_cast("POLYGON", warn = FALSE) |>
    sf::st_make_valid()

  dplyr::bind_rows(vri_ccb_intersection, vri_ccb_diff) |>
    bcmaps::transform_bc_albers()

}
