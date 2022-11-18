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
  calc_capability_rating(rrm_dt = rrm_dt)

  #TODO check if we expect to have all possible eco in the rrm output

  # merge on decile 1
  vri_bem[rrm_dt, on = .(ECO_SEC = Eco_sec, BGC_ZONE = Bgc_zone, BGC_SUBZON = Bgc_subzon, BGC_VRT = Bgc_vrt,
                         BGC_PHASE = Bgc_phase, BEUMC_S1 = Beumc, SLOPE_MOD = Slope_mod,
                         SITE_M3A = Site_m3a, SNOW_CODE = Snow_code, ABOVE_ELEV = Above_Elev_Thold,
                         CROWN_BEAR_1 = Crown_Bear, STRCT_S1 = Strct_d, STAND_A1 = Stand_d),
          c("MURAR_PEFD_SU_1", "MURAR_PEFD_CAP_1") := .(i.MURAR_PEFD_6C, i.MURAR_PEFD_CAP)]

  # merge on decile 2
  vri_bem[rrm_dt, on = .(ECO_SEC = Eco_sec, BGC_ZONE = Bgc_zone, BGC_SUBZON = Bgc_subzon, BGC_VRT = Bgc_vrt,
                         BGC_PHASE = Bgc_phase, BEUMC_S2 = Beumc, SLOPE_MOD = Slope_mod,
                         SITE_M3A = Site_m3a, SNOW_CODE = Snow_code, ABOVE_ELEV = Above_Elev_Thold,
                         CROWN_BEAR_2 = Crown_Bear, STRCT_S2 = Strct_d, STAND_A2 = Stand_d),
          c("MURAR_PEFD_SU_2", "MURAR_PEFD_CAP_2") := .(i.MURAR_PEFD_6C, i.MURAR_PEFD_CAP)]


  # merge on decile 3
  vri_bem[rrm_dt, on = .(ECO_SEC = Eco_sec, BGC_ZONE = Bgc_zone, BGC_SUBZON = Bgc_subzon, BGC_VRT = Bgc_vrt,
                         BGC_PHASE = Bgc_phase, BEUMC_S3 = Beumc, SLOPE_MOD = Slope_mod,
                         SITE_M3A = Site_m3a, SNOW_CODE = Snow_code, ABOVE_ELEV = Above_Elev_Thold,
                         CROWN_BEAR_3 = Crown_Bear, STRCT_S3 = Strct_d, STAND_A3 = Stand_d),
          c("MURAR_PEFD_SU_3", "MURAR_PEFD_CAP_3") := .(i.MURAR_PEFD_6C, i.MURAR_PEFD_CAP)]


  # calc highest suitability value
  vri_bem[ , MURAR_PEFD_SU_HV := fcase((MURAR_PEFD_SU_1 >= MURAR_PEFD_SU_2) & (MURAR_PEFD_SU_1 >= MURAR_PEFD_SU_3), MURAR_PEFD_SU_1,
                                       MURAR_PEFD_SU_2 >= MURAR_PEFD_SU_3, MURAR_PEFD_SU_2,
                                       MURAR_PEFD_SU_3 >= MURAR_PEFD_SU_2, MURAR_PEFD_SU_3,
                                       default = NA)]

   vri_bem[MURAR_PEFD_SU_HV > 8, MURAR_PEFD_SU_HV := NA]

   # calc weighted suitability rating
  vri_bem[, MURAR_PEFD_SU_WA := round(((MURAR_PEFD_SU_1 * SDEC_1) + (MURAR_PEFD_SU_2 * SDEC_2) + (MURAR_PEFD_SU_3 * SDEC_3))/(SDEC_1 + SDEC_2 + SDEC_3))]
  vri_bem[MURAR_PEFD_SU_WA > 6, MURAR_PEFD_SU_WA := NA]

  # calc highest capability value
  #TODO check how fcase handles NA
  vri_bem[ , MURAR_PEFD_CAP_HV := fcase((MURAR_PEFD_CAP_1 >= MURAR_PEFD_CAP_2) & (MURAR_PEFD_CAP_1 >= MURAR_PEFD_CAP_3), MURAR_PEFD_CAP_1,
                                       MURAR_PEFD_CAP_2 >= MURAR_PEFD_CAP_3, MURAR_PEFD_CAP_2,
                                       MURAR_PEFD_CAP_3 >= MURAR_PEFD_CAP_2, MURAR_PEFD_CAP_3,
                                       default = NA)]
  vri_bem[MURAR_PEFD_CAP_HV > 8, MURAR_PEFD_CAP_HV := NA]

  # calc weighted capability rating
  #TODO deal with NA when computing WA
  vri_bem[, MURAR_PEFD_CAP_WA := round(((MURAR_PEFD_CAP_1 * SDEC_1) + (MURAR_PEFD_CAP_2 * SDEC_2) + (MURAR_PEFD_CAP_3 * SDEC_3))/(SDEC_1 + SDEC_2 + SDEC_3))]
  vri_bem[MURAR_PEFD_CAP_WA > 6, MURAR_PEFD_CAP_WA := NA]


  return(st_as_sf(vri_bem))

}