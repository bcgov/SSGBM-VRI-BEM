#' Merge BEM (broad ecosystem mapping) attributes on VRI (vegetation ressource inventory) features
#'
#' @param vri sf object that represent VRI (vegetation ressource inventory) features
#' @param bem sf object that represent BEM (broad ecosystem mapping) features
#' @param return_intersection_dt boolean, if TRUE will return a list that contains the sf object and the intesection data.table of VRI and BEM
#' @return sf object that represent the original VRI with merged BEM attributes based on largest overlay
#' @import sf
#' @import data.table
#' @import units
#' @export
merge_bem_on_vri <- function(vri, bem, return_intersection_dt = FALSE) {

  # check if teis_id seems already merged on vri
  if ("TEIS_ID" %in% names(vri)) {
    deleted_columns <- setdiff(names(bem), names(vri))
    vri[, deleted_columns] <- NULL
    warning(paste("BEM attributes were already merged on VRI, the following columns were deleted", deleted_columns))
  }

  # check if bem contains duplicate teis_id
  if (length(unique(bem$TEIS_ID)) < nrow(bem)) {
    stop("duplicate values in TEIS_ID")
  }

  # cast multipart polygon to singlepart
  vri <- st_cast(vri,"POLYGON", warn = F)

  # use data.table to optimise speed
  classes_vri <- attr(vri, "class")
  setDT(vri)


  # remove feature with area below 1000
  vri[, vri_area := st_area(vri$Shape)]
  vri <- vri[vri_area >= set_units(1000, "m^2")]

  # new unique id
  set(vri, j = "VRI_OBJ_ID", value = seq.int(length.out = nrow(vri)))

  # find bem larger intersecting area with vri
  intersections <- st_intersection(vri$Shape, bem$Shape)
  intersection_dt <- data.table(vri_index = as.integer(attr(intersections, "idx")[, 1]), bem_index = attr(intersections, "idx")[, 2], area = st_area(intersections))
  index_dt <- intersection_dt[, .SD$bem_index[which.max(area)], by = .(vri_index)]

  # find geometry class column in bem
  which_col_to_merge <- which(!sapply(bem , function(x) "sfc" %in% class(x)))
  bem_names <- names(bem)

  # merge bem column on vri
  for (i in seq.int(along.with = which_col_to_merge)) {
    if (which_col_to_merge[i]) {
      set(vri, i = index_dt$vri_index, j = bem_names[i], value = bem[[bem_names[i]]][index_dt$V1])
    }
  }

  # check for vri that have no bem match
  if (length(which(is.na(vri$TEIS_ID))) > 0) {
    warning("The following VRI_OBJ_ID had no overlaping bem : ", paste(vri[is.na(TEIS_ID), VRI_OBJ_ID], collapse = ", "))
  }

  # return final result
  if (return_intersection_dt) {
    return(list(vri = st_as_sf(vri), intersection_dt = intersection_dt))
  }
  else {
    return(st_as_sf(vri))
  }
}
