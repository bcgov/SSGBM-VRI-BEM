#' Merge BEM (broad ecosystem mapping) attributes on VRI (vegetation ressource inventory)
#'
#' @param vri sf object that represent BEM (broad ecosystem mapping) features
#' @param bem sf object that represent VRI (vegetation ressource inventory) features
#' @return sf object that represent the original VRI with merged BEM attributes based on largest overlay
#' @import sf
#' @import data.table
merge_vri_bem <- function(vri, bem) {

  # use data.table on bem
  classes_bem <- attr(bem, "class")
  setDT(bem)

  # check if teis_id seems already merged on vri
  if ("TEIS_ID" %in% names(vri)) {
    # TODO message and exit
  }

  # check if bem contains duplicate teis_id
  if (length(unique(bem$TEIS_ID)) < nrow(bem)) {
    # TODO message and exit
  }

  # cast multipart polygon to singlepart
  vri <- st_cast(vri,"POLYGON")

  # use data.table to optimise speed
  classes_vri <- attr(vri, "class")
  vri <- setDT(vri)

  # remove feature with area below 1000
  vri <- vri[SHAPE_AREA >= 1000]

  attr(vri, "class") <- classes_vri
  vri <- st_join(x = vri, y = bem, join = st_intersects, largest = TRUE)

  return(vri)
}
