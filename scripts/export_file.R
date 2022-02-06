create_RRM_ecosystem <- function(bfc, lookup_csv, unique_ecosyst_dt){

  #TODO : should it be a parameter?
  fields <-  c("TEIS_ID","BAPID","ECO_SEC","BGC_ZONE","BGC_SUBZON","BGC_VRT","BGC_PHASE",
               "SDEC_1","SDEC_2","SDEC_3","BEUMC_S1","BEUMC_S2","BEUMC_S3",
               "SITEMC_S1","SITEMC_S2","SITEMC_S3","STRCT_S1","STRCT_S2","STRCT_S3",
               "STAND_A1","STAND_A2","STAND_A3","CROWN_MOOSE_1","CROWN_MOOSE_2","CROWN_MOOSE_3",
               "SLOPE_MOD","SITE_M3A","SNOW_CODE","ABOVE_ELEV_THOLD","Shape_Area")

  # TODO: validate it is a data.table?
  bem_dt <- bfc[, fields, with = FALSE]


  bem_1_dt <- bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1),
                     .(area = sum(Shape_Area*SDEC_1/10)),
                      by = .(id = create_key(eco_sec = ECO_SEC, bgc_zone = BGC_ZONE, bgc_subzone = BGC_SUBZON,
                                             bgc_vrt = BGC_VRT, bgc_phase = BGC_PHASE, beumc = BEUMC_S1,
                                             slope_mod = SLOPE_MOD, site_m3a = SITE_M3A, snow_code = SNOW_CODE,
                                             above_eleve_thold = ABOVE_ELEV_THOLD, crown_moose = CROWN_MOOSE_1,
                                             strct = STRCT_S1, stand = STAND_A1))]

  bem_2_dt <- bem_dt[SDEC_2 > 0 & ! is.na(BEUMC_S2),
                     .(area = sum(Shape_Area*SDEC_2/10)),
                     by = .(id = create_key(eco_sec = ECO_SEC, bgc_zone = BGC_ZONE, bgc_subzone = BGC_SUBZON,
                                            bgc_vrt = BGC_VRT, bgc_phase = BGC_PHASE, beumc = BEUMC_S2,
                                            slope_mod = SLOPE_MOD, site_m3a = SITE_M3A, snow_code = SNOW_CODE,
                                            above_eleve_thold = ABOVE_ELEV_THOLD, crown_moose = CROWN_MOOSE_2,
                                            strct = STRCT_S2, stand = STAND_A2))]

 bem_3_dt <-  bem_dt[SDEC_3 > 0 & ! is.na(BEUMC_S3),
                     .(area = sum(Shape_Area*SDEC_3/10)),
                     by = .(id = create_key(eco_sec = ECO_SEC, bgc_zone = BGC_ZONE, bgc_subzone = BGC_SUBZON,
                                            bgc_vrt = BGC_VRT, bgc_phase = BGC_PHASE, beumc = BEUMC_S3,
                                            slope_mod = SLOPE_MOD, site_m3a = SITE_M3A, snow_code = SNOW_CODE,
                                            above_eleve_thold = ABOVE_ELEV_THOLD, crown_moose = CROWN_MOOSE_3,
                                            strct = STRCT_S3, stand = STAND_A3))]

 rbind(bem_1_dt, bem_2_dt, bem_3_dt)[, sum(area), by = id]


}




create_key <- function(eco_sec, bgc_zone, bgc_subzone, bgc_vrt, bgc_phase, beumc, slope_mod,
                       site_m3a, snow_code, above_eleve_thold, crown_moose, strct, stand){

  paste0(replace_missing(eco_sec), "~",
         replace_missing(bgc_zone), "~",
         replace_missing(bgc_subzone), "~",
         replace_missing(as.character(bgc_vrt), regexp = "0"), "~",
         replace_missing(bgc_phase), "~",
         replace_missing(beumc), "~",
         replace_missing(slope_mod), "~",
         replace_missing(site_m3a), "~",
         replace_missing(snow_code), "~",
         replace_missing(above_eleve_thold), "~",
         replace_missing(crown_moose), "~",
         replace_missing(strct), "~",
         replace_missing(stand)
         )

}

replace_missing <- function(values, regexp = " "){
  values[is.na(values)] <- ""
  gsub(other_values_to_replace, "", values)
}


