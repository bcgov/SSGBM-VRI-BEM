#' Merge BEM attributes on VRI features
#'
#'This function copies all of the attributes in the specified BEM (broad ecosystem mapping) to each polygon in the specified VRI (vegetation resource inventory).
#
#' @param vri sf object that represent VRI (vegetation resources inventory) features
#' @param bem sf object that represent BEM (broad ecosystem mapping) features
#' @param return_intersection_dt Boolean, if TRUE will return a list that contains the sf object and the intersection data.table of VRI and BEM
#' @return sf object that represent the original VRI with merged BEM attributes based on largest overlay.
#' @details
#' This function output will be a copy of the VRI polygons clipped to the BEM, then exploded to single part, and its slivers will be eliminated.
#' Its attribute table will have all of the BEM attribute fields added to the existing VRI attributes.
#' The BEM attribute fields will be populated by copying the BEM values of the majority area BEM polygon within each VRI polygon.
#' @import sf
#' @import data.table
#' @importFrom units set_units
#' @export
merge_bem_on_vri <- function(vri, bem, return_intersection_dt = FALSE) {

  if (FALSE) {
    .<-area<-cell<-merge_bem_on_vri.data.<-TEIS_ID<-vri_area<-VRI_BEC_PHASE<-VRI_BEC_SUBZON<-
      VRI_BEC_VRT<-VRI_BEC_ZONE<-vri_index<-VRI_OBJ_ID<-NULL
  }

  # check if teis_id seems already merged on vri
  if (!is.null(vri[["TEIS_ID"]])) {
    duplicated_names <- setdiff(names(bem), names(vri))
    vri[, duplicated_names] <- NULL
    warning("BEM attributes were previously added into VRI. The following BEM attributes were removed from VRI: ",
            paste(duplicated_names, collapse = ", "))
  }

  # check if BEM contains duplicate TEIS_ID
  if (length(unique(bem$TEIS_ID)) < nrow(bem)) {
    stop("Duplicate values of TEIS_ID found in `bem`.")
  }

  # cast multipart polygon to singlepart
  vri <- st_cast(vri, "POLYGON", warn = F)

  # use data.table to optimise speed
  classes_vri <- attr(vri, "class")
  setDT(vri)


  # remove feature with area below 1000
  vri[, vri_area := st_area(vri$Shape)]
  vri <- vri[vri_area >= set_units(1000, "m^2")]

  # new unique id
  # TODO add argument so that when looping the id we create each loop are diffrent (maybe add a starting vri_obj_id in the arguments)
  set(vri, j = "VRI_OBJ_ID", value = seq.int(length.out = nrow(vri)))

  # find bem largest intersecting area with vri
  intersections <- st_intersection(vri$Shape, bem$Shape)
  intersection_dt <- data.table(vri_index = as.integer(attr(intersections, "idx")[, 1]), bem_index = attr(intersections, "idx")[, 2], area = st_area(intersections))
  set(intersection_dt, j = "VRI_OBJ_ID", value = vri[["VRI_OBJ_ID"]][intersection_dt[["vri_index"]]])
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

  # check for VRI that have no BEM match
  if (length(which(is.na(vri$TEIS_ID))) > 0) {
    warning("The following VRI_OBJ_ID had no overlaping bem : ", paste(vri[is.na(TEIS_ID), VRI_OBJ_ID], collapse = ", "))
  }

  #replace BEM biogeoclimatic attributes with VRI
  vri <- vri |> dplyr::mutate(BGC_ZONE = VRI_BEC_ZONE, BGC_SUBZON = VRI_BEC_SUBZON, BGC_VRT = VRI_BEC_VRT, BGC_PHASE = VRI_BEC_PHASE) |> dplyr::select(-c(VRI_BEC_ZONE,VRI_BEC_SUBZON,VRI_BEC_VRT,VRI_BEC_PHASE))

  #ensure vri-bem attributes are consistently NA if value absent
  for(j in seq_along(vri)){
    set(vri,i=which(vri[[j]]==""),j=j,value=NA)

  # return final result
  if (return_intersection_dt) {
    return(list(vri = st_as_sf(vri),
                intersection_dt = intersection_dt))
  }
  else {
    return(st_as_sf(vri))
  }
}
}


#' Merge BEM attributes on VRI features
#'
#'This function copies all of the attributes in the specified BEM (broad ecosystem mapping) to each polygon in the specified VRI (vegetation resource inventory).
#
#' @param vri data.table object that represent VRI (vegetation resources inventory) features
#' @param bem data.table object that represent BEM (broad ecosystem mapping) features
#' @return data.table object that represent the original VRI with merged BEM attributes.
#' @details
#' Its attribute table will have all of the BEM attribute fields added to the existing VRI attributes.
#' The BEM attribute fields will be populated by copying the BEM values.
#' @import data.table
#' @export
#' @keywords internal
merge_bem_on_vri.data.table <- function(vri, bem) {

  if (FALSE) {
    TEIS_ID<-cell<-NULL
  }

  # check if teis_id seems already merged on vri
  if (!is.null(vri[["TEIS_ID"]])) {
    duplicated_names <- setdiff(names(bem), names(vri))
    vri[, duplicated_names] <- NULL
    warning("BEM attributes were previously added into VRI. The following BEM attributes were removed from VRI: ",
            paste(duplicated_names, collapse = ", "))
  }

  # merge bem column on vri
  col_to_merge <- names(bem)[which(!sapply(bem , function(x) x %in% c("cell", "x")))]

  set(vri, j = col_to_merge, value = bem[, col_to_merge, with = F])

  # check for VRI that have no BEM match
  if (length(which(is.na(vri$TEIS_ID))) > 0) {
    warning("The following cell had no overlaping bem : ", paste(vri[is.na(TEIS_ID), cell], collapse = ", "))
  }

  # return final result
  return(vri)
}
