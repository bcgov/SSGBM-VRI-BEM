#' compute variable for structural stage look up and stand composition look up
#'
#' @param vri sf object that represent VRI (vegetation ressource inventory) features
#' @param most_recent_harvest_year integer that represent the most recent harvers year
#' @return sf object vri
#' @import data.table
#' @export
#'
calc_forest_age_class <- function(vri, most_recent_harvest_year) {

    # use data.table for fast data manipulation
  classes_vri <- attr(vri, "class")
  setDT(vri)

  # if proj_age_1 is empty the year of harvest date and most_recent_harvest_year to compute the projected age
  vri[is.na(PROJ_AGE_1), PROJ_AGE_1 := most_recent_harvest_year - year(as.Date(HRVSTDT, format = "%Y%m%d"))]

  # create variable for structural stage look up
  vri[ , VRI_AGE_CL_STS := fcase(PROJ_AGE_1 < 0, -1,
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

  # create variable for stand composition look up
  vri[ , VRI_AGE_CL_STD := fcase(PROJ_AGE_1 < 0, -1,
                                 PROJ_AGE_1 <= 15, 15,
                                 PROJ_AGE_1 <= 30, 30,
                                 PROJ_AGE_1 <= 50, 50,
                                 PROJ_AGE_1 <= 80, 80,
                                 PROJ_AGE_1 <= 50, 50,
                                 PROJ_AGE_1 > 80, 9999,
                                 default = -1)]


  # change object back to sf and return
  attr(vri, "class") <- classes_vri

  return(vri)
}

