#' Fix missing lake polygons
#'
#'Compare upland/non-lake areas with FWA Lakes and cut in lakes when necessary
#'
#'@param vri_bem VRI-BEM feature class
#'@param lakes sf
#'
#'@import sf
#'@export

correct_small_lakes <- function (vri_bem, lakes){

  lakes_presence <- lakes |>
    st_cast("POLYGON") |>
    mutate(lake = "Yes") |>
    dplyr::select(-(1:8))

  vri_known_lakes <- filter(vri_bem,BEUMC_S1 %in% c("LS","LL","OW"))

  vri_lakes_intersect <- filter(vri_bem,!BEUMC_S1 %in% c("LS","LL","OW")) %>%
    st_intersection(.,lakes_presence) %>%
    dplyr::mutate(area = st_area(.)) %>%
    mutate(BEUMC_S1 = case_when(
      area < units::set_units(100000,m^2) ~ "OW",
      area > units::set_units(100000,m^2) ~ "LS",
      area > units::set_units(600000,m^2) ~ "LL")) %>%
    select(-c(area,lake))

  vri_lakes_diff <- filter(vri_bem,!BEUMC_S1 %in% c("LS","LL","OW")) %>%
   st_difference(., st_union(st_geometry(lakes_presence))) %>% st_make_valid() %>% st_cast("POLYGON", warn = FALSE) |> st_make_valid()

  vri_bem <- dplyr::bind_rows(vri_known_lakes,vri_lakes_intersect,vri_lakes_diff) |> transform_bc_albers()

  #If OW, LS, LL, there should be no BEUMC_S2/S3 and SDEC_1 should be 10 for consistency with update_bem_from_vri
  vri_bem <- vri_bem |>
    mutate(SDEC_1 = case_when(
      BEUMC_S1 %in% c("LS","LL","OW") ~ 10,
      .default = SDEC_1),
      SDEC_2 = case_when(
        BEUMC_S1 %in% c("LS","LL","OW") ~ 0,
        .default = SDEC_2),
      SDEC_3 = case_when(
        BEUMC_S1 %in% c("LS","LL","OW") ~ 0,
        .default = SDEC_3))

  return(vri_bem)
}
