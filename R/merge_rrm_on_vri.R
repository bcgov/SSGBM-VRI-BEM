#' merge_rrm_on_vri
#'
#' merge suitability and capability rating from rrm onto vri-bem
#' and calculate highest value and weighted average for each scores
#'
#' @param vri_bem sf object that represent VRI (vegetation ressource inventory) features
#' @param rrm_dt data.table object that contains the rrm
#' @param return_sf logical, if TRUE  return sf object , if FALSE return data.table object and update by reference
#' @return sf  vri-bem object with new columns for rating
#' @import data.table
#' @export
merge_rrm_on_vri <- function(vri_bem, rrm_dt, return_sf = TRUE) {

  setDT(vri_bem)

  # calc capability rating and format rrm table
  calc_capability_rating(rrm_dt = rrm_dt)
  format_rrm_dt(rrm_dt = rrm_dt)

  # Create temp Stand variable to match variable used in the creation of the rrm
  vri_bem[ , STAND_A1_temp := STAND_A1]
  vri_bem[STAND_A1 %in% c("B", "C", "M") & !STRCT_S1 %in% c("4", "5", "6", "7"), STAND_A1_temp := ""]

  vri_bem[ , STAND_A2_temp := STAND_A2]
  vri_bem[STAND_A2 %in% c("B", "C", "M") & !STRCT_S2 %in% c("4", "5", "6", "7"), STAND_A2_temp := ""]

  vri_bem[ , STAND_A3_temp := STAND_A3]
  vri_bem[STAND_A3 %in% c("B", "C", "M") & !STRCT_S3 %in% c("4", "5", "6", "7"), STAND_A3_temp := ""]


  # merge on decile 1 ----
  vri_bem[rrm_dt, on = .(ECO_SEC = Eco_sec, BGC_ZONE = Bgc_zone, BGC_SUBZON = Bgc_subzon, BGC_VRT = Bgc_vrt,
                         BGC_PHASE = Bgc_phase, BEUMC_S1 = Beumc, SLOPE_MOD = Slope_mod,
                         SITE_M3A = Site_m3a, SNOW_CODE = Snow_code, ABOVE_ELEV = Above_Elev_Thold,
                         CROWN_BEAR_1 = Crown_Bear, STRCT_S1 = Strct_d, STAND_A1_temp = Stand_d),
          c("MURAR_PEFD_SU_1", "MURAR_PEFD_CAP_1") := .(i.MURAR_PEFD_6C, i.MURAR_PEFD_CAP)]

  # merge on decile 2 ----
  vri_bem[rrm_dt, on = .(ECO_SEC = Eco_sec, BGC_ZONE = Bgc_zone, BGC_SUBZON = Bgc_subzon, BGC_VRT = Bgc_vrt,
                         BGC_PHASE = Bgc_phase, BEUMC_S2 = Beumc, SLOPE_MOD = Slope_mod,
                         SITE_M3A = Site_m3a, SNOW_CODE = Snow_code, ABOVE_ELEV = Above_Elev_Thold,
                         CROWN_BEAR_2 = Crown_Bear, STRCT_S2 = Strct_d, STAND_A2_temp = Stand_d),
          c("MURAR_PEFD_SU_2", "MURAR_PEFD_CAP_2") := .(i.MURAR_PEFD_6C, i.MURAR_PEFD_CAP)]

  # merge on decile 3 ----
  vri_bem[rrm_dt, on = .(ECO_SEC = Eco_sec, BGC_ZONE = Bgc_zone, BGC_SUBZON = Bgc_subzon, BGC_VRT = Bgc_vrt,
                         BGC_PHASE = Bgc_phase, BEUMC_S3 = Beumc, SLOPE_MOD = Slope_mod,
                         SITE_M3A = Site_m3a, SNOW_CODE = Snow_code, ABOVE_ELEV = Above_Elev_Thold,
                         CROWN_BEAR_3 = Crown_Bear, STRCT_S3 = Strct_d, STAND_A3_temp = Stand_d),
          c("MURAR_PEFD_SU_3", "MURAR_PEFD_CAP_3") := .(i.MURAR_PEFD_6C, i.MURAR_PEFD_CAP)]

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

   # don't consider a rating that had not match in the RRM output but has percentage > 0
   set(vri_bem, i = which_su_1_is_na, j = "MURAR_PEFD_SU_1", value = 0)
   set(vri_bem, i = which_su_2_is_na, j = "MURAR_PEFD_SU_2", value = 0)
   set(vri_bem, i = which_su_3_is_na, j = "MURAR_PEFD_SU_3", value = 0)

  vri_bem[, MURAR_PEFD_SU_WA := round(((MURAR_PEFD_SU_1 * SDEC_1) + (MURAR_PEFD_SU_2 * SDEC_2) + (MURAR_PEFD_SU_3 * SDEC_3))/(SDEC_1 * !is.na(MURAR_PEFD_SU_1) + SDEC_2 * !is.na(MURAR_PEFD_SU_2) + SDEC_3 * !is.na(MURAR_PEFD_SU_3)))]
  vri_bem[MURAR_PEFD_SU_WA > 6 | MURAR_PEFD_SU_WA == 0, MURAR_PEFD_SU_WA := NA]

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

  set(vri_bem , j = "STAND_A1_temp", value = NULL)
  set(vri_bem , j = "STAND_A2_temp", value = NULL)
  set(vri_bem , j = "STAND_A3_temp", value = NULL)

  if (return_sf) {
    return(st_as_sf(vri_bem))
  } else {
    return(vri_bem)
  }

}
