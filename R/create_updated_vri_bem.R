create_updated_vri_bem <- function(vri, bem, rivers, wetlands, beu_bec_csv, beu_wetland_update_csv,
                                   clear_site_ma = TRUE, use_ifelse = TRUE, return_intersection_dt = FALSE) {

  # merge bem attributes on vri
  vri_bem <- merge_bem_on_vri(vri = vri,
                              bem = bem,
                              return_intersection_dt = FALSE)

  if (return_intersection_dt) {
    vri_bem_intersection_dt <- vri_bem$intersection_dt
    vri_bem <- vri_bem$vri
  }

  # remove vri that had no matching bem
  vri_bem <- vri_bem[which(!is.na(vri_bem$TEIS_ID)),]

  # update the bem attributes based on the vri attributes
  vri_bem <- update_bem_from_vri(vri_bem = vri_bem,
                                 rivers = rivers,
                                 beu_bec = beu_bec_csv,
                                 clear_site_ma = clear_site_ma,
                                 use_ifelse = use_ifelse)

  # update the bem wetlands based on csv of wetlands
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
