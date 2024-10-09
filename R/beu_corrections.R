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
      area > units::set_units(600000,"m^2") ~ "LL"),
      lbl_edit = dplyr::case_when(
        BEUMC_S1 %in% c("LS","LL","OW") ~ "Corrected with FWA Lakes polygons",
        .default = lbl_edit)
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
  #remove floodplain label for lakes
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
      SITE_M3A = dplyr::case_when(
        BEUMC_S1 %in% c("LS","LL","OW") ~ NA_character_,
        .default = SITE_M3A)) |>
    st_collection_extract()

  return(vri_bem)
}

#'SDEC fix
#'@description
#'Adjust SDEC so all deciles total to 10.
#'@details
#'If SDEC_total > 10, subtract from primary ecosystem (BEUMC_S1) until they equal 10
#'If SDEC_total < 10, add to primary ecosystem until total is 10
#'
#'@param vri_bem VRI-BEM feature class
#'
#'@import sf
#'@import data.table
#'@importFrom dplyr case_when
#'@export

SDEC_adjustment <- function(vri_bem) {
setDT (vri_bem)

vri_bem[, SDEC_2:= case_when(is.na(SDEC_2) ~ 0,.default = SDEC_2)]
vri_bem[, SDEC_3:= case_when(is.na(SDEC_3) ~ 0,.default = SDEC_3)]

vri_bem[,SDEC_total := SDEC_1 + SDEC_2 + SDEC_3]

#If SDEC_total is under 10, add additional value to primary ecosystem
#If SDEC_total is over 10, remove additional value from primary ecosystem

vri_bem[,SDEC_1 := case_when(
  SDEC_1 != 10 & SDEC_2 == 0 & SDEC_3 == 0 ~ 10,
  SDEC_1 != 10 & SDEC_total < 10 ~ SDEC_1 + (10-SDEC_total),
  SDEC_total > 10 ~ 10 - SDEC_2 - SDEC_3,
  SDEC_1 > 10 ~ 10,
  .default = SDEC_1)]

return(st_as_sf(vri_bem))
}


#'BEU fix
#'
#'@description
#'Fix BEUs which are in wrong location
#'@details
#'If BEUMC_S1 is missing, fill with BEUMC_S2 or BEUMC_S3 if they exist
#'If BEUMC are adjusted, SDEC also adjusted
#'
#'@param vri_bem VRI-BEM feature class
#'
#'@import sf
#'@import data.table
#'@importFrom dplyr case_when
#'@export

BEUMC_adjustment <- function(vri_bem) {
  setDT (vri_bem)

  #If BEUMC_S1 missing, fill with BEUMC_S2 or BEUMC_S3 if they exist
  vri_bem[(is.na(BEUMC_S1)),':='
          (SDEC_1 = case_when(
            !is.na(BEUMC_S2) ~ SDEC_1 + SDEC_2,
            is.na(BEUMC_S2) & !is.na(BEUMC_S3) ~ 10,
            is.na(BEUMC_S2) & is.na(BEUMC_S3) ~ 10,
            .default = SDEC_1),
          SDEC_2 = case_when(
            !is.na(BEUMC_S2) & !is.na(BEUMC_S3) ~ SDEC_3,
            is.na(BEUMC_S3) ~ 0,
            is.na(BEUMC_S2) ~ 0,
            .default = SDEC_2),
          BEUMC_S1 = case_when(
            !is.na(BEUMC_S2) ~ BEUMC_S2,
            is.na(BEUMC_S2) & !is.na(BEUMC_S3) ~ BEUMC_S3,
            .default = NA_character_),
          BEUMC_S2 = case_when(
            BEUMC_S2 == BEUMC_S1 & !is.na(BEUMC_S3) ~ BEUMC_S3,
            BEUMC_S2 == BEUMC_S1 & is.na(BEUMC_S3) ~ NA_character_,
            .default = NA_character_),
          BEUMC_S3 = NA_character_,
          SDEC_3 = 0)]

  return(st_as_sf(vri_bem))
}
