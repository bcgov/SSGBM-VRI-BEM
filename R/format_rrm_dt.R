#' format_rrm_dt
#'
#' format the rrm data to match the type of columns of vri-bem
#'
#' @param rrm_dt data.table object that contains the rrm
#' @return data.table  formatted rrm_dt
#' @import data.table
#' @export
format_rrm_dt <- function(rrm_dt) {
  rrm_dt[, Bgc_vrt := as.character(Bgc_vrt)]
  rrm_dt[is.na(Bgc_vrt), Bgc_vrt := ""]

  rrm_dt[, Bgc_phase := as.character(Bgc_phase)]

  rrm_dt[Site_m3a == "", Site_m3a := NA_character_]

  return(rrm_dt)
}
