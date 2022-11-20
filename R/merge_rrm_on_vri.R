#' merge_rrm_on_vri
#'
#' merge suitability and capability rating from rrm onto vri-bem
#' and calculate highest value and weighted average for each scores
#'
#' @param vri_bem sf object that represent VRI (vegetation ressource inventory) features
#' @param rrm_dt data.table object that contains the rrm
#' @return sf  vri-bem object with new columns for rating
#' @import data.table
#' @export
merge_rrm_on_vri <- function(vri_bem, rrm_dt) {

  setDT(vri_bem)

  format_rrm_dt(rrm_dt = rrm_dt)

  #TODO ## do NOT assign a rating if FORESTED_# = "Y" and STRCT_S# = "7a" and VRI_AGE_CLS_STS = -1

  # merge on decile 1 ----
  vri_bem[rrm_dt, on = .(ECO_SEC = Eco_sec, BGC_ZONE = Bgc_zone, BGC_SUBZON = Bgc_subzon, BGC_VRT = Bgc_vrt,
                         BGC_PHASE = Bgc_phase, BEUMC_S1 = Beumc, SLOPE_MOD = Slope_mod,
                         SITE_M3A = Site_m3a, SNOW_CODE = Snow_code, ABOVE_ELEV = Above_Elev_Thold,
                         CROWN_BEAR_1 = Crown_Bear, STRCT_S1 = Strct_d, STAND_A1 = Stand_d),
          c("MURAR_PEFD_SU_1") := .(i.MURAR_PEFD_6C)]


  set(vri_bem, j = "MURAR_PEFD_CAP_1", value =  rrm_dt[vri_bem, on = .(Eco_sec = ECO_SEC, Bgc_zone = BGC_ZONE, Bgc_subzon = BGC_SUBZON , Bgc_vrt = BGC_VRT ,
                                                                        Bgc_phase = BGC_PHASE , Beumc = BEUMC_S1, Slope_mod = SLOPE_MOD,
                                                                        Site_m3a = SITE_M3A, Snow_code = SNOW_CODE, Above_Elev_Thold = ABOVE_ELEV ,
                                                                        Crown_Bear = CROWN_BEAR_1), .(MURAR_PEFD_6C), mult = "first"]$MURAR_PEFD_6C)


  # merge on decile 2 ----
  vri_bem[rrm_dt, on = .(ECO_SEC = Eco_sec, BGC_ZONE = Bgc_zone, BGC_SUBZON = Bgc_subzon, BGC_VRT = Bgc_vrt,
                         BGC_PHASE = Bgc_phase, BEUMC_S2 = Beumc, SLOPE_MOD = Slope_mod,
                         SITE_M3A = Site_m3a, SNOW_CODE = Snow_code, ABOVE_ELEV = Above_Elev_Thold,
                         CROWN_BEAR_2 = Crown_Bear, STRCT_S2 = Strct_d, STAND_A2 = Stand_d),
          c("MURAR_PEFD_SU_2") := .(i.MURAR_PEFD_6C)]

  vri_bem[rrm_dt, on = .(ECO_SEC = Eco_sec, BGC_ZONE = Bgc_zone, BGC_SUBZON = Bgc_subzon, BGC_VRT = Bgc_vrt,
                         BGC_PHASE = Bgc_phase, BEUMC_S1 = Beumc, SLOPE_MOD = Slope_mod,
                         SITE_M3A = Site_m3a, SNOW_CODE = Snow_code, ABOVE_ELEV = Above_Elev_Thold,
                         CROWN_BEAR_1 = Crown_Bear, STRCT_S1 = Strct_d, STAND_A1 = Stand_d),
          c("MURAR_PEFD_SU_1") := .(i.MURAR_PEFD_6C)]


  set(vri_bem, j = "MURAR_PEFD_CAP_2", value =  rrm_dt[vri_bem, on = .(Eco_sec = ECO_SEC, Bgc_zone = BGC_ZONE, Bgc_subzon = BGC_SUBZON , Bgc_vrt = BGC_VRT ,
                                                                       Bgc_phase = BGC_PHASE , Beumc = BEUMC_S2, Slope_mod = SLOPE_MOD,
                                                                       Site_m3a = SITE_M3A, Snow_code = SNOW_CODE, Above_Elev_Thold = ABOVE_ELEV ,
                                                                       Crown_Bear = CROWN_BEAR_2), .(MURAR_PEFD_6C), mult = "first"]$MURAR_PEFD_6C)



  # merge on decile 3 ----
  vri_bem[rrm_dt, on = .(ECO_SEC = Eco_sec, BGC_ZONE = Bgc_zone, BGC_SUBZON = Bgc_subzon, BGC_VRT = Bgc_vrt,
                         BGC_PHASE = Bgc_phase, BEUMC_S3 = Beumc, SLOPE_MOD = Slope_mod,
                         SITE_M3A = Site_m3a, SNOW_CODE = Snow_code, ABOVE_ELEV = Above_Elev_Thold,
                         CROWN_BEAR_3 = Crown_Bear, STRCT_S3 = Strct_d, STAND_A3 = Stand_d),
          c("MURAR_PEFD_SU_3") := .(i.MURAR_PEFD_6C)]

  set(vri_bem, j = "MURAR_PEFD_CAP_3", value =  rrm_dt[vri_bem, on = .(Eco_sec = ECO_SEC, Bgc_zone = BGC_ZONE, Bgc_subzon = BGC_SUBZON , Bgc_vrt = BGC_VRT ,
                                                                       Bgc_phase = BGC_PHASE , Beumc = BEUMC_S3, Slope_mod = SLOPE_MOD,
                                                                       Site_m3a = SITE_M3A, Snow_code = SNOW_CODE, Above_Elev_Thold = ABOVE_ELEV ,
                                                                       Crown_Bear = CROWN_BEAR_3), .(MURAR_PEFD_6C), mult = "first"]$MURAR_PEFD_6C)



  # calc highest suitability value ----

  # assign temporary worst rating  to rating NA to make calculation of best rating easier
  set(vri_bem, i = which(!(vri_bem[["FORESTED_1"]] == "Y" & vri_bem[["STRCT_S1"]] == "7a" & vri_bem[["VRI_AGE_CL_STS"]] == -1) & !(vri_bem[["FORESTED_1"]] == "N" & vri_bem[["STRCT_S1"]] == "" & vri_bem[["ABOVE_ELEV"]] == "N")), j = "MURAR_PEFD_SU_1", value = NA)
  set(vri_bem, i = which(!(vri_bem[["FORESTED_2"]] == "Y" & vri_bem[["STRCT_S2"]] == "7a" & vri_bem[["VRI_AGE_CL_STS"]] == -1) & !(vri_bem[["FORESTED_2"]] == "N" & vri_bem[["STRCT_S2"]] == "" & vri_bem[["ABOVE_ELEV"]] == "N")), j = "MURAR_PEFD_SU_2", value = NA)
  set(vri_bem, i = which(!(vri_bem[["FORESTED_3"]] == "Y" & vri_bem[["STRCT_S3"]] == "7a" & vri_bem[["VRI_AGE_CL_STS"]] == -1) & !(vri_bem[["FORESTED_3"]] == "N" & vri_bem[["STRCT_S3"]] == "" & vri_bem[["ABOVE_ELEV"]] == "N")), j = "MURAR_PEFD_SU_3", value = NA)

  set(vri_bem, i = which(vri_bem[["MURAR_PEFD_SU_1"]] > 6), j = "MURAR_PEFD_SU_1", value = NA)
  set(vri_bem, i = which(vri_bem[["MURAR_PEFD_SU_2"]] > 6), j = "MURAR_PEFD_SU_2", value = NA)
  set(vri_bem, i = which(vri_bem[["MURAR_PEFD_SU_3"]] > 6), j = "MURAR_PEFD_SU_3", value = NA)

  which_su_1_is_na <- which(is.na(vri_bem[["MURAR_PEFD_SU_1"]]))
  which_su_2_is_na <- which(is.na(vri_bem[["MURAR_PEFD_SU_2"]]))
  which_su_3_is_na <- which(is.na(vri_bem[["MURAR_PEFD_SU_3"]]))

  set(vri_bem, i = which_su_1_is_na, j = "MURAR_PEFD_SU_1", value = 9)
  set(vri_bem, i = which_su_2_is_na, j = "MURAR_PEFD_SU_2", value = 9)
  set(vri_bem, i = which_su_3_is_na, j = "MURAR_PEFD_SU_3", value = 9)


  vri_bem[ , MURAR_PEFD_SU_HV := fcase((MURAR_PEFD_SU_1 <= MURAR_PEFD_SU_2) & (MURAR_PEFD_SU_1 <= MURAR_PEFD_SU_3), MURAR_PEFD_SU_1,
                                       MURAR_PEFD_SU_2 <= MURAR_PEFD_SU_3, MURAR_PEFD_SU_2,
                                       MURAR_PEFD_SU_3 <= MURAR_PEFD_SU_2, MURAR_PEFD_SU_3,
                                       default = NA)]

   vri_bem[MURAR_PEFD_SU_HV > 8, MURAR_PEFD_SU_HV := NA]

   # calc weighted suitability rating ----

   #TODO  What should we consider a rating that had not match in the RRM output but has percentage > 0
  vri_bem[, MURAR_PEFD_SU_WA := round(((MURAR_PEFD_SU_1 * SDEC_1) + (MURAR_PEFD_SU_2 * SDEC_2) + (MURAR_PEFD_SU_3 * SDEC_3))/(SDEC_1 + SDEC_2 + SDEC_3))]
  vri_bem[MURAR_PEFD_SU_WA > 6, MURAR_PEFD_SU_WA := NA]

  # calc highest capability value ----

  # assign temporary worst rating  to rating NA to make calculation of best rating easier
  set(vri_bem, i = which(vri_bem[["MURAR_PEFD_CAP_1"]] > 6), j = "MURAR_PEFD_CAP_1", value = NA)
  set(vri_bem, i = which(vri_bem[["MURAR_PEFD_CAP_2"]] > 6), j = "MURAR_PEFD_CAP_2", value = NA)
  set(vri_bem, i = which(vri_bem[["MURAR_PEFD_CAP_3"]] > 6), j = "MURAR_PEFD_CAP_3", value = NA)

  which_cap_1_is_na <- which(is.na(vri_bem[["MURAR_PEFD_CAP_1"]]))
  which_cap_2_is_na <- which(is.na(vri_bem[["MURAR_PEFD_CAP_2"]]))
  which_cap_3_is_na <- which(is.na(vri_bem[["MURAR_PEFD_CAP_3"]]))

  set(vri_bem, i = which_cap_1_is_na, j = "MURAR_PEFD_CAP_1", value = 9)
  set(vri_bem, i = which_cap_2_is_na, j = "MURAR_PEFD_CAP_2", value = 9)
  set(vri_bem, i = which_cap_3_is_na, j = "MURAR_PEFD_CAP_3", value = 9)

  vri_bem[ , MURAR_PEFD_CAP_HV := fcase((MURAR_PEFD_CAP_1 <= MURAR_PEFD_CAP_2) & (MURAR_PEFD_CAP_1 <= MURAR_PEFD_CAP_3), MURAR_PEFD_CAP_1,
                                       MURAR_PEFD_CAP_2 <= MURAR_PEFD_CAP_3, MURAR_PEFD_CAP_2,
                                       MURAR_PEFD_CAP_3 <= MURAR_PEFD_CAP_2, MURAR_PEFD_CAP_3,
                                       default = NA)]
  vri_bem[MURAR_PEFD_CAP_HV > 8, MURAR_PEFD_CAP_HV := NA]

  # calc weighted capability rating ----

  #TODO  What should we consider a rating that had not match in the RRM output but has percentage > 0
  vri_bem[, MURAR_PEFD_CAP_WA := round(((MURAR_PEFD_CAP_1 * SDEC_1) + (MURAR_PEFD_CAP_2 * SDEC_2) + (MURAR_PEFD_CAP_3 * SDEC_3))/(SDEC_1 + SDEC_2 + SDEC_3))]
  vri_bem[MURAR_PEFD_CAP_WA > 6, MURAR_PEFD_CAP_WA := NA]

  # rating for decile that are 0 should be NA
  set(vri_bem, i = which(vri_bem$SDEC_1 == 0), j = c("MURAR_PEFD_SU_1","MURAR_PEFD_CAP_1"), value = NA)
  set(vri_bem, i = which(vri_bem$SDEC_2 == 0), j = c("MURAR_PEFD_SU_2","MURAR_PEFD_CAP_2"), value = NA)
  set(vri_bem, i = which(vri_bem$SDEC_3 == 0), j = c("MURAR_PEFD_SU_3","MURAR_PEFD_CAP_3"), value = NA)

  set(vri_bem, i = which_su_1_is_na, j = "MURAR_PEFD_SU_1", value = NA)
  set(vri_bem, i = which_su_2_is_na, j = "MURAR_PEFD_SU_2", value = NA)
  set(vri_bem, i = which_su_3_is_na, j = "MURAR_PEFD_SU_3", value = NA)

  set(vri_bem, i = which_cap_1_is_na, j = "MURAR_PEFD_CAP_1", value = NA)
  set(vri_bem, i = which_cap_2_is_na, j = "MURAR_PEFD_CAP_2", value = NA)
  set(vri_bem, i = which_cap_3_is_na, j = "MURAR_PEFD_CAP_3", value = NA)

  return(st_as_sf(vri_bem))

}
