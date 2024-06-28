#' Calculate all hem fields for the vri_bem database
#'
#'
#' @param vri_bem sf object that represent VRI (vegetation ressource inventory) features
#' @param fire sf object that represent fires
#' @param hem_fields data.table containing a column called 'names' with the name of the hem fields to compute, default to the hem_field object in the package
#' @param current_year numeric of the year use to compute age, default to year of current date
#'
#' @return sf object
#' @import sf
#' @import data.table
#' @export
calc_hem_fields <- function(vri_bem, fire, hem_fields = SSGBM.VRI.BEM::hem_fields, current_year = year(Sys.Date())) {

  if (FALSE) {
    .<-AGE_CL_STS<-Age_Class8_9<-area<-BCLCS_LV_1<-BCLCS_LV_2<-BCLCS_LV_3<-BCLCS_LV_4<-BCLCS_LV_5<-
      CROWN_MOOSE<-Dynamic_F<-Dynamic_L<-Dynamic_WFD_11to30<-Dynamic_WFD_4to10<-Dynamic_WFD_All<-
      ELEV<-Elev_Threshold<-fire_area<-fire_pct<-HARVEST_YEAR<-LBL_VEGCOV<-MEAN_SLOPE<-PROJ_AGE_1<-
      Security_1<-Slope_Limit<-SPEC_CD_1<-SPEC_CD_2<-SPEC_PCT_1<-SPEC_PCT_2<-Static_Brush<-
      Static_Persist_Decid<-Static_Riparian<-Static_Sb_Bog<-Static_Upland<-Static_Wetland_HE<-
      Static_Wetland_Shrub_Riparian<-Static_Wetland_SL<-Static_Wetland_ST<-Static_WFD_All<-
      Static_Willow<-vri_area<-vri_index<-W_Shelter_1<-W_Site_Conditions_Met<-Waterbody<-NULL
  }

  # Compute percentage of vri with fire
  st_agr(fire) <- "constant"
  st_agr(vri_bem) <- "constant"
  intersections <- st_intersection(st_geometry(vri_bem), st_geometry(fire))
  intersection_dt <- data.table(vri_index = as.integer(attr(intersections, "idx")[, 1]), fire_index = attr(intersections, "idx")[, 2], area = st_area(intersections))
  index_dt <- intersection_dt[, as.numeric(sum(area)), keyby = .(vri_index)]
  setDT(vri_bem)
  set(vri_bem, i = index_dt$vri_index, j = "fire_area", value = index_dt$V1)
  vri_bem[ , fire_pct := fifelse(is.na(fire_area), fire_area/vri_area * 100, 0)]

  # calculate hem fields
  vri_bem[, Static_Wetland_ST := fifelse(BCLCS_LV_1 == "V" & BCLCS_LV_2 == "N" & BCLCS_LV_3 == "W" & BCLCS_LV_4 == "ST" & BCLCS_LV_5 %in% c("OP", "ST", "DE"), 1, 0)]

  vri_bem[, Static_Wetland_SL := fifelse(BCLCS_LV_1 == "V" & BCLCS_LV_2 == "N" & BCLCS_LV_3 == "W" & BCLCS_LV_4 == "SL" & BCLCS_LV_5 %in% c("OP", "ST", "DE"), 1, 0)]

  vri_bem[, Static_Wetland_HE := fcase(BCLCS_LV_1 == "V" & BCLCS_LV_2 == "N" & BCLCS_LV_3 == "W" & BCLCS_LV_4 == "HE" & BCLCS_LV_5 %in% c("OP", "ST", "DE"), 1,
                                       BCLCS_LV_1 == "V" & BCLCS_LV_2 == "N" & BCLCS_LV_3 == "W" & BCLCS_LV_4 == "BY" & BCLCS_LV_5 == "CL", 1,
                                       default = 0)]

  vri_bem[, Static_Wetland_Shrub_Riparian := fifelse(Static_Wetland_ST == 1 | Static_Wetland_SL == 1 | Static_Wetland_HE == 1, 1, 0)]

  vri_bem[, Static_Upland := fcase(BCLCS_LV_1 == "V" & BCLCS_LV_2 == "N" & BCLCS_LV_3 == "U" & BCLCS_LV_4 %in% c("ST","HE") & BCLCS_LV_5 %in% c("OP", "DE"), 1,
                                   BCLCS_LV_1 == "V" & BCLCS_LV_2 == "N" & BCLCS_LV_3 == "U" & BCLCS_LV_4 == "SL" & BCLCS_LV_5 == "OP", 1,
                                   BCLCS_LV_1 == "V" & BCLCS_LV_2 == "N" & BCLCS_LV_3 == "U" & BCLCS_LV_4 == "BY" & BCLCS_LV_5 == "CL", 1,
                                   BCLCS_LV_1 == "V" & BCLCS_LV_2 == "T" & BCLCS_LV_3 == "U" & BCLCS_LV_4 == "TC" & BCLCS_LV_5 %in% c("SP", "OP"), 1,
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

  vri_bem[, Waterbody := fifelse(BCLCS_LV_5 == "LA", 0, 1)]

  vri_bem[ , Elev_Threshold := fifelse(ELEV < 1501, 1, 0)]

  vri_bem[, Slope_Limit := fifelse(MEAN_SLOPE < 81, 1, 0)]

  vri_bem[, W_Site_Conditions_Met := fifelse(Waterbody == 1 & Elev_Threshold == 1 & Slope_Limit == 1, 1, 0)]

  vri_bem[, Age_Class8_9 := fifelse(PROJ_AGE_1 > 140, 1, 0)]

  vri_bem[, Static_Persist_Decid := fcase(PROJ_AGE_1 > 60 & (SPEC_CD_1 %in% c("D", "DG", "DR", "A", "AC", "AT", "ACT", "ACB", "AX", "E", "EA", "EX", "EP") & SPEC_PCT_1 > 60), 1,
                                          PROJ_AGE_1 > 60 & (SPEC_CD_2 %in% c("D", "DG", "DR", "A", "AC", "AT", "ACT", "ACB", "AX", "E", "EA", "EX", "EP") & SPEC_PCT_2 > 60), 1,
                                          default = 0)]

  vri_bem[, W_Shelter_1 := fifelse(SPEC_CD_1 %in% c("BA", "BB", "BL", "C", "FDI", "H", "HM", "HW", "HX", "HXM", "P", "PA", "PL", "PLC", "PLI", "S",
                                                    "SB", "SE", "SS", "SW", "SX", "SXE", "SXL", "SXS", "SXW", "SXX") & SPEC_PCT_1 > 40 & PROJ_AGE_1 > 120 & Elev_Threshold == 1 & Slope_Limit == 1 & CR_CLOSURE > 35 & (Static_Sb_Bog == 0 | Static_Persist_Decid == 0), 1, 0)]


  vri_bem[, W_Shelter_1 := fifelse(W_Shelter_1 == 1 & Static_Persist_Decid == 1, 0, W_Shelter_1)]

  vri_bem[, Dynamic_WFD_4to10 := fifelse(AGE_CL_STS == 7 & Elev_Threshold == 1 & Slope_Limit == 1, 1, 0)]

  vri_bem[, Dynamic_WFD_11to30 := fifelse(AGE_CL_STS == 20 & Elev_Threshold == 1 & Slope_Limit == 1, 1, 0)]

  vri_bem[, Security_1 := fifelse(PROJ_AGE_1 > 40 & Elev_Threshold == 1 & Slope_Limit == 1, 1, 0)]

  vri_bem[, Static_Brush := fifelse(LBL_VEGCOV %in% c("NC", "NCBR", "NP", "NPBR", "NPBU", "NSR"), 1, 0)]

  vri_bem[, Static_WFD_All := fifelse((Static_Brush == 1 | Static_Persist_Decid == 1 | Static_Sb_Bog == 1) & Elev_Threshold == 1 & Slope_Limit == 1, 1, 0)]

  vri_bem[, Dynamic_WFD_All := fifelse((Dynamic_WFD_4to10 == 1 | Dynamic_WFD_11to30 == 1) & Elev_Threshold == 1 & Slope_Limit == 1, 1, 0)]

  vri_bem[ , Dynamic_L := fifelse((current_year - HARVEST_YEAR) <= 31, 1, 0)]

  vri_bem[, Dynamic_F := ifelse(fire_pct > 50 & Dynamic_WFD_All == 1, 1, 0)]

  # check missing hem fields
  missing_hem <- setdiff(hem_fields$names, names(vri_bem))
  if (length(missing_hem) > 0) {
    warning(paste0(missing_hem, " was not computed \n"))
  }

  return(st_as_sf(vri_bem))
}
