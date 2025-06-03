#' Add fire perimeters
#'
#' Joins historic fire perimeters to vri_bem by percent overlap
#'
#' @param vri_bem sf object
#' @param fire_perimeter sf object that represents historical BC Wildfire Fire Perimeters features
#' @return sf object that represent the intersections of vri_bem and fire_perimeter
#' @import sf
#' @import tibble
#' @importFrom dplyr mutate select left_join group_by summarise
#' @export
#'

merge_fire_perimeters <- function(vri_bem,fire_perimeter) {

  #add temporary unique ID to vri_bem
  vri_bem <- tibble::rowid_to_column(vri_bem,"temp_id")

  #get percent intersection area and year of most recent fire
  fire_intersect <- st_intersection(vri_bem,fire_perimeter) |>
    (\(x) mutate(x, intersect_area = st_area(x)))() |>
    dplyr::select(temp_id,FIRE_YEAR,intersect_area) |>
    st_drop_geometry() |>
    group_by(temp_id,FIRE_YEAR) |>
    summarise(intersect_area = sum(intersect_area)) |>
    ungroup() |>
    group_by(temp_id) |>
    filter(FIRE_YEAR == max(FIRE_YEAR)) |>
    ungroup()

  #calculate percent burn in polygon

  vri_bem <- mutate(vri_bem,Shape_Area = as.numeric(st_area(vri_bem))) |>
    dplyr::left_join(fire_intersect,by = c("temp_id")) |>
    dplyr::mutate(percent_burned = case_when(
      !is.na(intersect_area) ~ as.numeric(intersect_area/Shape_Area*100),
      .default = 0),
      most_recent_fire = FIRE_YEAR)|>
    dplyr::select(-c("temp_id","intersect_area","FIRE_YEAR")) |>
    #ensure water polygons do not have fire ratings
    dplyr::mutate(percent_burned = case_when(
      BEUMC_S1 %in% c("OW","LL","LS") ~ 0,
      .default = percent_burned),
      most_recent_fire = case_when(
        BEUMC_S1 %in% c("OW","LL","LS") ~ NA,
        .default = most_recent_fire))

  return(vri_bem)
}
