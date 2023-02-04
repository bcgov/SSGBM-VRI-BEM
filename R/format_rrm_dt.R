#' format_rrm_dt
#'
#' format the rrm data to match the type of columns of vri-bem
#'
#' @param rrm_dt data.table object that contains the rrm
#' @param animal character, "bear" or "moose"
#' @return data.table  formatted rrm_dt
#' @import data.table
#' @export
format_rrm_dt <- function(rrm_dt,animal) { #added animal because will change based on bear/moose
  if(animal == "bear") { #added bear to differentiate
  rrm_dt[, Bgc_vrt := as.character(Bgc_vrt)]
  rrm_dt[is.na(Bgc_vrt), Bgc_vrt := ""]

  rrm_dt[, Bgc_phase := as.character(Bgc_phase)]

  rrm_dt[Site_m3a == "", Site_m3a := NA_character_]

  setorder(rrm_dt, Eco_sec, Bgc_zone, Bgc_subzon, Bgc_vrt, Bgc_phase, Beumc,
           Slope_mod, Site_m3a, Snow_code, Above_Elev_Thold, Crown_Bear, MURAR_PEFD_6C)

  }
  if(animal == "moose") { #all code below is new, to distinguish moose from bear
    rrm_dt[, Bgc_vrt := as.character(Bgc_vrt)]
    rrm_dt[is.na(Bgc_vrt), Bgc_vrt := ""]

    rrm_dt[, Bgc_phase := as.character(Bgc_phase)]

    rrm_dt[Site_m3a == "", Site_m3a := NA_character_]

    rrm_dt[, Slope_mod := as.character(Slope_mod)]

    rrm_dt[Crown_Moose == "", Crown_Moose := NA_character_]

    setorder(rrm_dt, Eco_sec, Bgc_zone, Bgc_subzon, Bgc_vrt, Bgc_phase, Beumc,
             Slope_mod, Site_m3a, Snow_code, Above_Elev_Thold, Crown_Moose)
  }

  return(rrm_dt)
}
