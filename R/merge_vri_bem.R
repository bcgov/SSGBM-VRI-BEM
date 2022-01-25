#' Merge BEM (broad ecosystem mapping) attributes on VRI (vegetation ressource inventory) features
#'
#' @param vri sf object that represent VRI (vegetation ressource inventory) features
#' @param bem sf object that represent BEM (broad ecosystem mapping) features
#' @return sf object that represent the original VRI with merged BEM attributes based on largest overlay
#' @import sf
#' @import data.table
#' @import units
merge_vri_bem <- function(vri, bem) {

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
  classes_bem <- attr(bem, "class")
  setDT(vri)
  setDT(bem)


  # remove feature with area below 1000
  vri[, vri_area := st_area(vri$Shape)]
  vri <- vri[vri_area >= set_units(1000, "m^2")]

  # new unique id
  set(vri, j = "VRI_OBJ_ID", value = seq.int(along.with = nrow(vri)))

  # merge bem attributes on larger intersecting area with vri
  intersections <- st_intersection(vri$Shape, bem$Shape)
  intersection_dt <- data.table(vri_index = attr(intersections, "idx")[, 1], bem_index = attr(intersections, "idx")[, 2], area = st_area(intersections))
  index_dt <- intersection_dt[, .SD$bem_index[which.max(area)], by = vri_index]
  vri <- cbind(vri[index_dt$vri_index,-c("Shape")], bem[index_dt$V1, -c("Shape")], vri[index_dt$vri_index, "Shape"]) |> st_as_sf()

  # check for vri that have no bem match
  if (length(which(is.na(vri$TEIS_ID))) > 0) {
    # TODO message and do something
  }

  attr(bem, "class") <- classes_bem

  return(vri)
}
