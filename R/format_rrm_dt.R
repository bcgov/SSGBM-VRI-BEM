#' format_rrm_dt
#'
#' format the rrm data to match the type of columns of vri-bem
#'
#' @param rrm_dt data.table object that contains the rrm
#' @param animal character, "bear" or "moose"
#' @return data.table  formatted rrm_dtf
#' @import data.table
#' @export
format_rrm_dt <- function(rrm_dt,animal) {

  if(animal %in% c("moose","bear")){

  if (FALSE) {
    Above_Elev_Thold<-Beumc<-Bgc_phase<-Bgc_subzon<-Bgc_vrt<-Bgc_zone<-Crown_all<-
      Eco_sec<-MURAR_PEFD_6C<-Site_m3a<-Slope_mod<-Snow_code<-NULL
  }

  rrm_dt[, Bgc_vrt := as.character(Bgc_vrt)]
  rrm_dt[, Bgc_phase := as.character(Bgc_phase)]
  rrm_dt[, Slope_mod := as.character(Slope_mod)]
  rrm_dt <- rrm_dt |> dplyr::mutate_if(is.character, function(x) ifelse(x == "",NA_character_,x))

  if(animal == "bear") {
  setorder(rrm_dt, Eco_sec, Bgc_zone, Bgc_subzon, Bgc_vrt, Bgc_phase, Beumc,
           Slope_mod, Site_m3a, Snow_code, Above_Elev_Thold, Crown_all, MURAR_PEFD_6C)

  }
  if(animal == "moose") {

    setorder(rrm_dt, Eco_sec, Bgc_zone, Bgc_subzon, Bgc_vrt, Bgc_phase, Beumc,
             Slope_mod, Site_m3a, Snow_code, Above_Elev_Thold, Crown_all)
  }

  return(rrm_dt)
}

if(animal == "huckleberry"){

  if (FALSE) {
    HUCK_ELEV_Thold<-Huck_asp<-Bgc_phase<-Bgc_subzon<-Bgc_vrt<-Bgc_zone<-Crown_All<-
      Eco_sec<-VACCMEM_6C<-NULL
  }

  rrm_dt[, Bgc_vrt := as.character(Bgc_vrt)]
  rrm_dt[, Bgc_phase := as.character(Bgc_phase)]
  rrm_dt <- rrm_dt |> dplyr::mutate_if(is.character, function(x) ifelse(x == "",NA_character_,x))

  return(rrm_dt)
}
}
