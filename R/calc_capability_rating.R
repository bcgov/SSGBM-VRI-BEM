#' calc_capability_rating
#'
#' calculate capability rating which is the best suitability rating among ecosystem regardless of structural stage
#'
#' @param rrm_dt data.table object that contains the rrm
#' @param animal character, "bear" or "moose"
#' @return data.table rrm with new column capability_rating
#' @import data.table
#' @export
calc_capability_rating <- function(rrm_dt, animal) {

  if(animal %in% c("bear","moose")){
    if (FALSE) {
      .<-Eco_sec<-Bgc_zone<-Bgc_subzon<-Bgc_vrt<-Bgc_phase<- Beumc<-Slope_mod<-Site_m3a<-
      Salmon<-Snow_code<-Above_Elev_Thold<-Crown_all<-NULL
    }
  }

  if(animal == "huckleberry"){
    if (FALSE) {
      .<-Eco_sec<-Bgc_zone<-Bgc_subzon<-Bgc_vrt<-Bgc_phase<-Huck_asp<-HUCK_ELEV_Thold<-Crown_All<-NULL
    }
  }

  data.table::setDT(rrm_dt)
  rating_variables <- grep("_6C$", names(rrm_dt), value = TRUE)
  cap_rating_variables <- paste0(rating_variables, "_CAP")

   if (animal == "bear") {
   rrm_dt[ , (cap_rating_variables) := lapply(.SD, min, na.rm=TRUE), by = .(Eco_sec, Bgc_zone, Bgc_subzon, Bgc_vrt, Bgc_phase, Beumc, Slope_mod, Site_m3a, Salmon, Snow_code, Above_Elev_Thold, Crown_all), .SDcols = (rating_variables)]

  }

  if (animal == "moose") {
  rrm_dt[ , (cap_rating_variables) := lapply(.SD, min, na.rm=TRUE), by = .(Eco_sec, Bgc_zone, Bgc_subzon, Bgc_vrt, Bgc_phase, Beumc, Slope_mod, Site_m3a, Snow_code, Above_Elev_Thold, Crown_all), .SDcols = (rating_variables)]

  }

  if (animal == "huckleberry") {
    rrm_dt[ , (cap_rating_variables) := lapply(.SD, min, na.rm=TRUE), by = .(Eco_sec, Bgc_zone, Bgc_subzon, Bgc_vrt, Bgc_phase, Huck_asp, HUCK_ELEV_Thold, Crown_All), .SDcols = (rating_variables)]

  }

  return(rrm_dt)
}
