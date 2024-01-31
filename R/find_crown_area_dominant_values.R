#' Find Area-Dominant CROWN_BEAR and CROWN_MOOSE
#'
#' This function determines the area-dominant CROWN_BEAR and CROWN_MOOSE values for each VRI-BEM polygon.
#'
#'updated to assign crown values directly from the VRI. It's not necessary to intersect with the BEM
#'
#' @param vri sf object that represent VRI (vegetation resources inventory) features
#' @return sf object containing VRI-BEM
#' @import data.table
#' @import sf
#' @export
find_crown_area_dominant_values <- function(vri) {

  # use data.table for fast data manipulation
  classes_vri <- attr(vri, "class")
  data.table::setDT(vri)

  # merge CROWN_ALL on vri
  set(vri, j = paste0("CROWN_ALL_",1:3), value = vri$CROWN_ALL)

  # blank Crown all if not ecounter specific condition

  for (i in 1:3) {
    set(vri, i = which(!(vri[[paste0("FORESTED_", i)]] == "Y" & substr(vri[[paste0("STRCT_S", i)]], start = 1, stop = 1) %in% c("4", "5", "6", "7","7a","7b"))), j = paste0("CROWN_ALL_", i) , value = NA)

  }

  attr(vri, "class") <- classes_vri

  return(vri)

}
