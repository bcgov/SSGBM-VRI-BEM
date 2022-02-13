#' Creates the updated vri-bem object
#'
#'
#' @param vri sf object of vri
#' @param bem sf objects of bem
#' @param rivers sf object of rivers
#' @param wetlands sf object of wetlands
#' @param beu_bec_csv data.table of beu_bec_csv
#' @param beu_wetland_update_csv data.table of wetlands correction csv
#' @param clear_site_ma boolean, if TRUE variable SITE_M1A, SITE_M2A will be cleared
#' @param use_ifelse boolean, if TRUE correction done after the combine_duplicated_BEUMC will only be applied on rows that were not affected by the correction of duplicated BEUMC
#' @param verbose boolean , if TRUE function will return message to indicate progress throughout the function execution
#' @return sf object updated vri-bem
#' @export
#'
create_updated_vri_bem <- function(vri, bem, rivers, wetlands, beu_bec_csv, beu_wetland_update_csv,
                                   clear_site_ma = TRUE, use_ifelse = TRUE, return_intersection_dt = FALSE, verbose = TRUE) {

  # merge bem attributes on vri
  if (verbose) {
    message("Merging bem on vri")
  }
  vri_bem <- merge_bem_on_vri(vri = vri,
                              bem = bem,
                              return_intersection_dt = return_intersection_dt)

  if (return_intersection_dt) {
    vri_bem_intersection_dt <- vri_bem$intersection_dt
    vri_bem <- vri_bem$vri
  }

  # remove vri that had no matching bem
  vri_bem <- vri_bem[which(!is.na(vri_bem$TEIS_ID)),]

  # update the bem attributes based on the vri attributes
  if (verbose) {
    message("Updating bem attributes from vri attributes")
  }
  vri_bem <- update_bem_from_vri(vri_bem = vri_bem,
                                 rivers = rivers,
                                 beu_bec = beu_bec_csv,
                                 clear_site_ma = clear_site_ma,
                                 use_ifelse = use_ifelse)

  # update the bem wetlands based on csv of wetlands
  if (verbose) {
    message("Updating bem attributes from wetlands corrections")
  }
  vri_bem <- update_bem_from_wetlands(vri_bem = vri_bem,
                                      wetlands = wetlands,
                                      buc = beu_wetland_update_csv)

  if (return_intersection_dt) {
    return(list(vri_bem = vri_bem, intersection_dt = vri_bem_intersection_dt))
  }
  else {
    return(vri_bem)
  }
}
