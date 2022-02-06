#' overlays a BEM and VRI feature classes and determines the area-dominant CROWN_BEAR and CROWN_MOOSE values for each BEM polygon.
#'
#' @param vri sf object that represent VRI (vegetation ressource inventory) features
#' @param bem sf object that represent BEM (broad ecosystem mapping) features
#' @intersection_dt data.table  area and index of vri and bem intersections
#' @return sf object
#' @import data.table
#' @import sf
#' @export
find_crown_area_dominant_values <- function(vri, bem, intersection_dt = NULL) {

  # if no intersections object previoulsy calculated is passed , recompute the intersections
  if (is.null(intersection_dt)) {
    intersections <- st_intersection(vri$Shape, bem$Shape)
    intersection_dt <- data.table(vri_index = attr(intersections, "idx")[, 1], bem_index = attr(intersections, "idx")[, 2], area = st_area(intersections))
  }

  # use data.table for fast data manipulation
  classes_vri <- attr(vri, "class")
  setDT(vri)

  #make sure variable index is integer for speed efficiency
  set(intersection_dt, j = "vri_index", value = as.integer(intersection_dt$vri_index))

  # merge CROWN BEAR and CROWN MOOSE info on intersection data
  intersection_dt[ , CROWN_BEAR := .subset2(vri, "CROWN_BEAR")[vri_index]]
  intersection_dt[ , CROWN_MOOSE := .subset2(vri, "CROWN_MOOSE")[vri_index]]

  # calculate the area covered by each type of bear for each bem
  area_by_bem_bear <- intersection_dt[ , .(area = sum(area)), by = .(bem_index, CROWN_BEAR)]
  # find the type of bear that covers the most area for each bem
  most_covered_bear_by_bem <- area_by_bem_bear[ , .SD$CROWN_BEAR[which.max(area)], by = bem_index]

  # calculate the area covered by each type of moose for each bem
  area_by_bem_moose <- intersection_dt[ , .(area = sum(area)), by = .(bem_index, CROWN_MOOSE)]
  # find the type of moose that covers the most area for each bem
  most_covered_moose_by_bem <- area_by_bem_moose[ , .SD$CROWN_MOOSE[which.max(area)], by = bem_index]

  # merge the crown bear on vri
  match_lines <- match(vri[["TEIS_ID"]][intersection_dt$vri_index],  bem$TEIS_ID[most_covered_bear_by_bem[["bem_index"]]])
  set(vri, i = intersection_dt$vri_index, j = paste0("CROWN_BEAR_", 1:3), value = most_covered_bear_by_bem[["V1"]][match_lines])

  # merge the crown moose on vri
  match_lines <- match(vri[["TEIS_ID"]][intersection_dt$vri_index],  bem$TEIS_ID[most_covered_moose_by_bem[["bem_index"]]])
  set(vri, i = intersection_dt$vri_index, j = paste0("CROWN_MOOSE_", 1:3), value = most_covered_moose_by_bem[["V1"]][match_lines])

  # blank Crown moose and crown moose if not ecounter specific condition

  for (i in 1:3) {
    set(vri, i = which(!(vri[[paste0("FORESTED_", i)]] == "Y" & substr(vri[[paste0("STRCT_S", i)]], start = 1, stop = 1) %in% c("4", "5", "6", "7"))), j = paste0("CROWN_BEAR_", i) , value = NA)
    set(vri, i = which(!(vri[[paste0("FORESTED_", i)]] == "Y" & substr(vri[[paste0("STRCT_S", i)]], start = 1, stop = 1) %in% c("4", "5", "6", "7"))), j = paste0("CROWN_MOOSE_", i), value = NA)

  }

  attr(vri, "class") <- classes_vri

  return(vri)

}
