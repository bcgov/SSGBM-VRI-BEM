#' Merge BEM (broad ecosystem mapping) attributes on VRI (vegetation ressource inventory) features
#'
#' @param vri sf object that represent BEM (broad ecosystem mapping) features
#' @param bem sf object that represent VRI (vegetation ressource inventory) features
#' @return sf object that represent the original VRI with merged BEM attributes based on largest overlay
#' @import sf
#' @import data.table
#' @import units
merge_vri_bem <- function(vri, bem) {

  #make sure that all geometries are valid, since they are from esri format
  bem$geometry <- st_make_valid(bem$geometry)
  vri$geometry <- st_make_valid(vri$geometry)

  # check if teis_id seems already merged on vri
  if ("TEIS_ID" %in% names(vri)) {
    # TODO message and exit
  }

  # check if bem contains duplicate teis_id
  if (length(unique(bem$TEIS_ID)) < nrow(bem)) {
    # TODO message and exit
  }

  # cast multipart polygon to singlepart
  vri <- st_cast(vri,"POLYGON", warn = F)

  # use data.table to optimise speed
  classes_vri <- attr(vri, "class")
  setDT(vri)

  # remove feature with area below 1000
  vri <- vri[which(st_area(vri$geometry) >= set_units(1000, "m^2"))]

  # new unique id
  set(vri, j = "VRI_OBJ_ID", value = seq.int(along.with = nrow(vri)))

  attr(vri, "class") <- classes_vri

  # merge bem attributes on larger intersecting area with vri
  intersections <- st_intersection(vri$geometry, bem$geometry)
  setDT(vri)
  setDT(bem)
  intersection_dt <- data.table(vri = attr(intersections, "idx")[, 1], bem = attr(intersections, "idx")[, 2], area = st_area(intersections))
  index_dt <- intersection_dt[, .SD$bem[which.max(area)], by = vri]
  vri <- cbind(vri[index_dt$vri], bem[index_dt$V1])

  # check for vri that have no bem match
  if (length(which(is.na(vri$TEIS_ID))) > 0) {
    # TODO message and do something
  }

  attr(vri, "class") <- classes_vri

  return(vri)
}
