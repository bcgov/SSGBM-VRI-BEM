calc_hem_fields <- function(vri_bem, fire, hem_fields, current_year = year(Sys.Date())) {
  # TODO find fire database to test
  vri_bem_fire_int <- st_intersection(vri_bem, fire)

  # TODO compute total percentage of overlap between vribem and fire

  vri_bem[, Static_Wetland_ST := fifelse(BCLCS_LV_1 == "V" & BCLCS_LV_2 == "N" & BCLCS_LV_3 == "W" & BCLCS_LV_4 == "ST" & BCLCS_LV_5 %in% c("OP", "ST", "DE"), 1, 0)]

  vri_bem[, Static_Wetland_SL := fifelse(BCLCS_LV_1 == "V" & BCLCS_LV_2 == "N" & BCLCS_LV_3 == "W" & BCLCS_LV_4 == "SL" & BCLCS_LV_5 %in% c("OP", "ST", "DE"), 1, 0)]

  vri_bem[, Static_Wetland_HE := fcase(BCLCS_LV_1 == "V" & BCLCS_LV_2 == "N" & BCLCS_LV_3 == "W" & BCLCS_LV_4 == "HE" & BCLCS_LV_5 %in% c("OP", "ST", "DE"), 1,
                                       BCLCS_LV_1 == "V" & BCLCS_LV_2 == "N" & BCLCS_LV_3 == "W" & BCLCS_LV_4 == "BY" & BCLCS_LV_5 == "CL", 1,
                                       default = 0)]

  vri_bem[, Static_Wetland_Shrub_Riparian := fifelse(Static_Wetland_ST == 1 | Static_Wetland_SL == 1 | Static_Wetland_HE == 1, 1, 0)]

  vri_bem[, Static_Upland := fcase(BCLCS_LV_1 == "V" & BCLCS_LV_2 == "N" & BCLCS_LV_3 == "U" & BCLCS_LV_4 %in% c("ST","HE") & BCLCS_LV_5 %in% c("OP", "DE"), 1,
                                   BCLCS_LV_1 == "V" & BCLCS_LV_2 == "N" & BCLCS_LV_3 == "U" & BCLCS_LV_4 == "SL" & BCLCS_LV_5 == "OP", 1,
                                   BCLCS_LV_1 == "V" & BCLCS_LV_2 == "N" & BCLCS_LV_3 == "U" & BCLCS_LV_4 == "BY" & BCLCS_LV_5 == "CL", 1,
                                   BCLCS_LV_1 == "V" & BCLCS_LV_2 == "T" & BCLCS_LV_3 == "U" & BCLCS_LV_4 == "TC" & BCLCS_LV_5 %in% c("SP", "OP"),
                                   default = 0)]

  vri_bem[, Static_Willow := fifelse(SPEC_CD_1 %in% c("W", "WS") | SPEC_CD_2 %in% c("W", "WS"), 1, 0)]

  vri_bem[, Static_Sb_Bog := fifelse(SPEC_CD_1 == "SB" & SPEC_PCT_1 > 89, 1, 0)]

  riparian_values <- c("sl,ri",
                       "sl,ri,by",
                       "sl,ri,he",
                       "sl,ri,he,by",
                       "sl,ri,hf",
                       "sl,ri,hg",
                       "st,ri",
                       "st,ri,by",
                       "st,ri,he",
                       "st,ri,he,by",
                       "st,ri,hf",
                       "st,ri,hg",
                       "st,ri,hg,by",
                       "ri,by",
                       "ri,by,sl,he",
                       "ri,he",
                       "ri,he,st",
                       "ri,hg",
                       "ri,hg,sl",
                       "ri,sl",
                       "ri,sl,by",
                       "ri,sl,by,he",
                       "ri,sl,he",
                       "ri,sl,he,by",
                       "ri,sl,hf",
                       "ri,sl,hf,by",
                       "ri,st",
                       "ri,st,by",
                       "ri,st,he",
                       "ri,st,he,by",
                       "ri,st,hf",
                       "ri,st,hg",
                       "ri,st,hg,by")

  vri_bem[, Static_Riparian := fifelse(LBL_VEGCOV %in% riparian_values, 1, 0)]

  vri_bem[, Waterbody := fifelse(BCLCS_LV_5 == "LA", 1, 0)]

  vri_bem[ , Elev_Threshold := fifelse(ELEV < 1501, 1, 0)]

  vri_bem[, Slope_Limit := fifelse(MEAN_SLOPE < 81, 1, 0)]

  vri_bem[, W_Site_Conditions_Met := fifelse(Waterbody == 1 & Elev_Threshold == 1 & Slope_Limit == 1)]

  vri_bem[, Age_Class8_9 := fifelse(PROJ_AGE_1 %in% c("8", "9"), 1, 0)]

  vri_bem[, Static_Persist_Decid := fcase(PROJ_AGE_1 %in% c("4", "5", "6", "7", "8", "9") & (SPEC_CD_1 %in% c("D", "DG", "DR", "A", "AC", "AT", "ACT", "ACB", "AX", "E", "EA", "EX", "EP") & SPEC_PCT_1 > 60), 1,
                                          PROJ_AGE_1 %in% c("4", "5", "6", "7", "8", "9") & (SPEC_CD_2 %in% c("D", "DG", "DR", "A", "AC", "AT", "ACT", "ACB", "AX", "E", "EA", "EX", "EP") & SPEC_PCT_2 > 60), 1,
                                          default = 0)]

  vri_bem[, W_Shelter_1 := fifelse(SPEC_CD_1 %in% c("BA", "BB", "BL", "C", "FDI", "H", "HM", "HW", "HX", "HXM", "P", "PA", "PL", "PLC", "PLI", "S",
                                                    "SB", "SE", "SS", "SW", "SX", "SXE", "SXL", "SXS", "SXW", "SXX") & SPEC_PCT_1 > 40 & PROJ_AGE_1 %in% c("7","8", "9") & Elev_Threshold == 1 & Slope_Limit == 1 & CROWN_MOOSE > 35 & (Static_Sb_Bog == 0 | Static_Persist_Decid == 0), 1, 0)]


  vri_bem[, W_Shelter_1 := fifelse(W_Shelter_1 == 1 & Static_Persist_Decid == 1, 0, W_Shelter_1)]

  vri_bem[, Dynamic_WFD_4to10 := fifelse(AGE_CL_STS == 7 & Elev_Threshold == 1 & Slope_Limit == 1, 1, 0)]

  vri_bem[, Dynamic_WFD_11to30 := fifelse(AGE_CL_STS == 20 & Elev_Threshold == 1 & Slope_Limit == 1, 1, 0)]

  vri_bem[, Security_1 := fifelse(PROJ_AGE_1 %in% c("3","4", "5", "6", "7", "8", "9") & Elev_Threshold == 1 & Slope_Limit == 1, 1, 0)]

  vri_bem[, Static_Brush := fifelse(LBL_VEGCOV %in% c("NC", "NCBR", "NP", "NPBR", "NPBU", "NSR"), 1, 0)]

  vri_bem[, Static_WFD_All := fifelse((Static_Brush == 1 | Static_Persist_Decid == 1 | Static_Sb_Bog == 1) & Elev_Threshold == 1 & Slope_Limit == 1, 1, 0)]

  vri_bem[, Dynamic_WFD_All := fifelse((Dynamic_WFD_4to10 == 1 | Dynamic_WFD_11to30 == 1) & Elev_Threshold == 1 & Slope_Limit == 1, 1, 0)]

  vri_bem[ , Dynamic_L := fifelse((current_year - HARVEST_YEAR) <= 31, 1, 0)]

  #TODO use fire percentage to calc Dynamic_F
  vri_bem[, Dynamic_F := ifelse(fire_pct > 50 & Dynamic_WFD_All == 1, 1, 0)]

  #TODO check if all hem fields are computed

  return(vri_bem)
}
