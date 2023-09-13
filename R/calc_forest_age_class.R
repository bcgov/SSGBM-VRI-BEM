#' Compute variables for structural stage look up and stand composition look up
#'
#' @param vri_bem sf object that represent VRI (vegetation resource inventory) features
#' @param most_recent_harvest_year integer that represent the most recent harvest year
#' @return sf object VRI-BEM with new attributes: PROJ_AGE_1, VRI_AGE_CL_STS and VRI_AGE_CL_STD
#' @details
#' PROJ_AGE_1 is calculated as the difference in years between the harvest date and the most recent harvest year
#' Based on the the age the VRI_AGE_CL_STS are defined as below:
#'
#'  | PROJ_AGE_1 | VRI_AGE_CL_STS |
#'  |:----------:|--------------:|
#'  | < 0     | -1  |
#'  | 0-3     |  2  |
#'  | 4-10    |  7  |
#'  | 11-30   | 20  |
#'  | 31-40   | 35  |
#'  | 41-60   | 50  |
#'  | 61-80   | 70  |
#'  | 81-140  | 125 |
#'  | 141-249 | 195 |
#'  | > 249   | 301 |
#'
#' Based on the the age the VRI_AGE_CL_STD are defined as below:
#'
#'  | PROJ_AGE_1 | VRI_AGE_CL_STD |
#'  |:----------:|--------------:|
#'  | < 0   |  -1  |
#'  | 0-15  |  15  |
#'  | 16-30 |  30  |
#'  | 31-50 |  50  |
#'  | 51-80 |  80  |
#'  | > 80  | 9999 |
#'
#' @import data.table
#' @export
#'
calc_forest_age_class <- function(vri_bem, most_recent_harvest_year) {

    # use data.table for fast data manipulation
  #classes_vri <- attr(vri_bem, "class")
  vriCRS <- st_crs(vri_bem)
  setDT(vri_bem)

  # if proj_age_1 is empty the year of harvest date and most_recent_harvest_year to compute the projected age
  # the VRI layer (PROJ_AGE_1) is more out-of-date than the CCB (HARVEST_YEAR), so in all cases where HARVEST_YEAR is not NA, it should be used to calculate polygon age. When HARVEST_YEAR is NA, PROJ_AGE_1 should retain its initial value

  #make sure PROJ_AGE is numeric
  vri_bem[, PROJ_AGE_1 := as.numeric(PROJ_AGE_1)]

  vri_bem[!is.na(HARVEST_YEAR), PROJ_AGE_1 := most_recent_harvest_year - HARVEST_YEAR]

  # create variable for structural stage look up
  vri_bem[ , VRI_AGE_CL_STS := fcase(PROJ_AGE_1 < 0, -1,
                                     PROJ_AGE_1 <= 3, 2,
                                     PROJ_AGE_1 <= 10, 7,
                                     PROJ_AGE_1 <= 30, 20,
                                     PROJ_AGE_1 <= 40, 35,
                                     PROJ_AGE_1 <= 60, 50,
                                     PROJ_AGE_1 <= 80, 70,
                                     PROJ_AGE_1 <= 140, 125,
                                     PROJ_AGE_1 <= 249, 195,
                                     PROJ_AGE_1 > 249, 301,
                                     default = -1)]

  vri_bem[, VRI_AGE_CL_STS := as.numeric(VRI_AGE_CL_STS)] #RW edit: make sure VRI_AGE_CL_STS is numeric

  # create variable for stand composition look up
  vri_bem[ , VRI_AGE_CL_STD := fcase(PROJ_AGE_1 < 0, -1,
                                     PROJ_AGE_1 <= 15, 15,
                                     PROJ_AGE_1 <= 30, 30,
                                     PROJ_AGE_1 <= 50, 50,
                                     PROJ_AGE_1 <= 80, 80,
                                     PROJ_AGE_1 > 80, 9999,
                                     default = -1)]

  vri_bem[, VRI_AGE_CL_STD := as.numeric(VRI_AGE_CL_STD)] #RW edit: make sure VRI_AGE_CL_STD is numeric


  # change object back to sf and return
  #attr(vri_bem, "class") <- classes_vri
  vri_bem <- vri_bem |> st_as_sf(sf_column_name="Shape",crs=vriCRS) |> st_make_valid()

  return(vri_bem)

}

