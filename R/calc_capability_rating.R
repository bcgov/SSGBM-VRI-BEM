#' calc_capability_rating
#'
#' calculate capability rating which is the best suitability rating among ecosystem regardless of structural stage
#'
#' @param rrm_dt data.table object that contains the rrm
#' @return data.table rrm with new column capability_rating
#' @import data.table
#' @export
calc_capability_rating <- function(rrm_dt) {
  setDT(rrm_dt)
  rrm_dt[ , MURAR_PEFD_CAP := min(MURAR_PEFD_6C), by = .(Eco_sec, Bgc_zone, Bgc_subzon, Bgc_vrt, Bgc_phase, Beumc, Slope_mod, Site_m3a,
                                                         Snow_code, Above_Elev_Thold, Crown_Bear)]
  return(rrm_dt)
}
