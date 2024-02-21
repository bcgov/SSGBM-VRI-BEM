#' Fix missing lake polygons
#'
#'Compare upland/non-lake areas with FWA Lakes and cut in lakes when necessary
#'
#'@param vri_bem VRI-BEM feature class
#'@param lakes sf
#'
#'@import sf
#'@importFrom dplyr mutate select filter case_when bind_rows
#'@importFrom units set_units
#'@importFrom bcmaps transform_bc_albers
#'@export

correct_small_lakes <- function (vri_bem, lakes){

  if (FALSE) {
    BEUMC_S1 <- SDEC_1 <- SDEC_2 <- SDEC_3 <- area <- lake <- NULL
  }

  lakes_presence <- lakes |>
    sf::st_cast("POLYGON") |>
    dplyr::mutate(lake = "Yes") |>
    dplyr::select(-(1:8))

  vri_known_lakes <- dplyr::filter(vri_bem, BEUMC_S1 %in% c("LS","LL","OW"))

  vri_lakes_intersect <- dplyr::filter(vri_bem, !BEUMC_S1 %in% c("LS","LL","OW")) |>
    sf::st_intersection(lakes_presence) |>
    {\(x) {dplyr::mutate(x, area = sf::st_area(x))}}() |>
    dplyr::mutate(BEUMC_S1 = dplyr::case_when(
      area < units::set_units(100000,"m^2") ~ "OW",
      area > units::set_units(100000,"m^2") ~ "LS",
      area > units::set_units(600000,"m^2") ~ "LL")
    ) |>
    dplyr::select(-c(area,lake))

  vri_lakes_diff <- dplyr::filter(vri_bem,!BEUMC_S1 %in% c("LS","LL","OW")) |>
    sf::st_difference(sf::st_union(sf::st_geometry(lakes_presence))) |>
    {\(x) {dplyr::filter(x, st_geometry_type(x) %in% c("POLYGON","MULTIPOLYGON"))}}() |>
    sf::st_make_valid() |>
    sf::st_cast("POLYGON", warn = FALSE) |>
    sf::st_make_valid()

  vri_bem <- dplyr::bind_rows(vri_known_lakes, vri_lakes_intersect,vri_lakes_diff) |>
    bcmaps::transform_bc_albers() |>
    {\(x) {dplyr::filter(x, st_geometry_type(x) %in% c("POLYGON","MULTIPOLYGON"))}}() |>
    sf::st_cast("POLYGON",warn=FALSE)

  #If OW, LS, LL, there should be no BEUMC_S2/S3 and SDEC_1 should be 10 for consistency with update_bem_from_vri
  vri_bem <- vri_bem |>
    dplyr::mutate(SDEC_1 = dplyr::case_when(
      BEUMC_S1 %in% c("LS","LL","OW") ~ 10,
      .default = SDEC_1),
      SDEC_2 = dplyr::case_when(
        BEUMC_S1 %in% c("LS","LL","OW") ~ 0,
        .default = SDEC_2),
      SDEC_3 = dplyr::case_when(
        BEUMC_S1 %in% c("LS","LL","OW") ~ 0,
        .default = SDEC_3),
      lbl_edit = dplyr::case_when(
        BEUMC_S1 %in% c("LS","LL","OW") ~ "Corrected with FWA Lakes polygons",
        .default = lbl_edit))

  return(vri_bem)
}
