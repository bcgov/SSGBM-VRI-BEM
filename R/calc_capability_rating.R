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
  rating_variables <- grep("_6C$", names(rrm_dt), value = T)
  cap_rating_variables <- paste0(substr(rating_variables, 1, nchar(rating_variables) - 3), "_CAP")
  rrm_dt[ , (cap_rating_variables) := lapply(.SD, min), by = .(Eco_sec, Bgc_zone, Bgc_subzon, Bgc_vrt, Bgc_phase, Beumc, Slope_mod, Site_m3a, Salmon,
                                                         Snow_code, Above_Elev_Thold, Crown_Bear), .SDcols = (rating_variables)]
  return(rrm_dt)
}
