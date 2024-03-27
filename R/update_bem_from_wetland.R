#' Update BEM  attributes based on Wetland polygons
#'
#' Updates BEM (broad ecosystem mapping) attributes based on intersections with wetland polygons
#'
#' @param vri_bem sf object that represents the VRI-BEM feature class
#' @param wetlands sf object that represents Wetlands polygon feature class
#' @param buc data.table object that represents BEU wetland update table
#' @details
#' This function intersects the VBRI-BEM and the wetlands, compute the percentage of wetland area for each BEM and then performs the corrections below.
#'
#'  * Remove all wetland from 3rd component. They were added during the BEM process and no longer apply at the VRI scale
#'  * Merge all allowed BEU codes from `buc`.
#'  * Reassign ecosystem based on new allowed codes:
#'     - Add new Wetland when there is none in VRI-BEM and one should exists
#'     - Remove wetland when there is one in VRI-BEM but none should exists
#'     - Shift ecosystems variables from component when there is one in VRI-BEM but in the wrong component
#'  * Perform riparian mapcode adjustment. Assign 10 WL when all the conditions below are met :
#'     - SITE_M3A = 'a',
#'     - MEAN_SLOP < 10,
#'     - BEUMC_S1 is vegetated,  and
#'     - BGC_ZONE is one of the following : "CDF", "BWBS", "SWB", "ESSF", "ICH", "CWH", "SBPS", "SBS"
#'
#' @return sf object with corrections from wetlands
#' @import sf
#' @import data.table
#' @importFrom dplyr case_when mutate
#' @export
update_bem_from_wetlands <- function(vri_bem, wetlands, buc) {

  if (FALSE) {
    .<-area<-BCLCS_LV_4<-BEUMC_S1<-BEUMC_S2<-BEUMC_S3<-BGC_ZONE<-Code_Orig<-Code_WL1<-Code_WL10<-
      Code_WL2<-Code_WL3<-Code_WL4<-Code_WL5<-Code_WL6<-Code_WL7<-Code_WL8<-curr_beu_code<-
      curr_wl_zone<-i.Code_WL0<-i.Code_WL1<-i.Code_WL10<-i.Code_WL2<-i.Code_WL3<-i.Code_WL4<-
      i.Code_WL5<-i.Code_WL6<-i.Code_WL7<-i.Code_WL8<-Lbl_edit_wl<-new_beu_code<-SDEC_1<-SDEC_2<-
      SDEC_3<-SITE_M3A<-vri_area<-wetland_area<-wl_pct<-NULL
  }

  # Ensure vri_area is up-to-date
  vri_bem <- dplyr::mutate(vri_bem,vri_area = as.numeric(sf::st_area(vri_bem)))

  setDT(vri_bem)

  # check if all required attributes are there
  validate_required_attributes(ifc = vri_bem,
                               required_attributes = c("TEIS_ID", "SDEC_1", "BEUMC_S1", "REALM_1", "GROUP_1", "CLASS_1", "KIND_1", "SITE_S1",
                                                       "SITEAM_S1A", "SITEAM_S1B", "SITEAM_S1C", "SITEAM_S1D", "SITEMC_S1", "SITE_M1A", "SITE_M1B",
                                                       "STRCT_S1", "STRCT_M1", "STAND_A1", "SERAL_1", "TREE_C1", "SHRUB_C1", "DISTCLS_1",
                                                       "DISTSCLS_1", "DISSSCLS_1", "SECL_1", "SESUBCL_1", "COND_1", "VIAB_1", "SDEC_2", "BEUMC_S2",
                                                       "REALM_2", "GROUP_2", "CLASS_2", "KIND_2", "SITE_S2", "SITEAM_S2A", "SITEAM_S2B", "SITEAM_S2C",
                                                       "SITEAM_S2D", "SITEMC_S2", "SITE_M2A", "SITE_M2B", "STRCT_S2", "STRCT_M2", "STAND_A2",
                                                       "SERAL_2", "TREE_C2", "SHRUB_C2", "DISTCLS_2", "DISTSCLS_2", "DISSSCLS_2", "SECL_2",
                                                       "SESUBCL_2", "COND_2", "VIAB_2", "SDEC_3", "BEUMC_S3", "REALM_3", "GROUP_3", "CLASS_3",
                                                       "KIND_3", "SITE_S3", "SITEAM_S3A", "SITEAM_S3B", "SITEAM_S3C", "SITEAM_S3D", "SITEMC_S3",
                                                       "SITE_M3A", "SITE_M3B", "STRCT_S3", "STRCT_M3", "STAND_A3", "SERAL_3", "TREE_C3", "SHRUB_C3",
                                                       "DISTCLS_3", "DISTSCLS_3", "DISSSCLS_3", "SECL_3", "SESUBCL_3", "COND_3", "VIAB_3", "BGC_ZONE",
                                                       "MEAN_SLOPE", "BCLCS_LV_4"))

  if (is.null(vri_bem[["Lbl_edit_wl"]])) {
    set(vri_bem , j = "Lbl_edit_wl", value = "")
  }

  # Add SMPL_TYPE field if it doesn't already exist

  if (is.null(vri_bem[["SMPL_TYPE"]])) {
    set(vri_bem , j = "SMPL_TYPE", value = "")
  }

  # Make note of which polygons have SITE_M3A == 'a' for later (before the 'a' is potentially moved to
  # SITE_M1A or SITE_M2A)
  site_m3a_eq_a <- vri_bem[["SITE_M3A"]] == "a"

  # compute % of wetland area for each BEM ----

  # Find intersections between BEM and Wetlands
  intersections <- st_intersection(vri_bem$Shape, wetlands$Shape)
  intersection_dt <- data.table(vri_bem = attr(intersections, "idx")[, 1], wetlands = attr(intersections, "idx")[, 2], area = st_area(intersections))
  index_dt <- intersection_dt[, .(wetland_area = sum(area, na.rm = TRUE)), by = vri_bem]

  vri_bem[index_dt[["vri_bem"]], wetland_area := index_dt[["wetland_area"]]]
  # as.numeric to remove class units
  vri_bem[, wl_pct:= as.numeric(wetland_area/vri_area*100)]
  vri_bem[is.na(wl_pct), `:=`(wl_pct = 0, wetland_area = set_units(0, "m^2"))]

  vri_bem[, Lbl_edit_wl := "No Wetland."]
  vri_bem[wl_pct > 0, Lbl_edit_wl := paste0(wl_pct, "% of polygon occupied by wetland.")]

  vri_bem[, Lbl_edit_wl := paste0(Lbl_edit_wl, " Current BEU: ", SDEC_1, " ", BEUMC_S1)]



  vri_bem[SDEC_2 > 0, Lbl_edit_wl := paste0(Lbl_edit_wl, ", ", SDEC_2, " ", BEUMC_S2)]
  vri_bem[SDEC_3 > 0, Lbl_edit_wl := paste0(Lbl_edit_wl, ", ", SDEC_3, " ", BEUMC_S3)]

  # Remove all 'WL' mapcodes from Dec3 - these were added during the BEM process and no longer apply at
  # the VRI scale
  vri_bem[(SDEC_3 > 0 & BEUMC_S3 == "WL"),
          `:=`(SDEC_1 = SDEC_1 + SDEC_3,
               SDEC_3 = 0,
               BEUMC_S3 = NA_character_)]

  set_shifted_eco_variables(vri_bem, i = which(vri_bem[["SDEC_3"]] > 0 & vri_bem[["BEUMC_S3"]] == "WL"), list(c(3, NA)))

  # If curr_beu_code is 4 digits: 1 digit for each decile, with the 4th digit:
  #     0 if none of the 3 components is WL
  #     1 if the 1st component is WL
  #     2 if the 2nd component is WL
  #     3 if the 3rd component is WL
  # If curr_beu_code is 5 digits: the first decile value must 10, the 2nd and 3rd are 0's,
  # and the 5th digit is:
  #     0 if the first and only component is not WL
  #     1 if the first and only component is WL

  vri_bem[, curr_wl_zone:=fcase(BEUMC_S1 == "WL", 1,
                                #cases where BEUMC_S3/S2 exist but SDEC_3 == 0
                                BEUMC_S2 == "WL" & SDEC_3 != 0, 2,
                                BEUMC_S3 == "WL" & SDEC_3 != 0, 3,
                                default = 0)]

  #ensure SDEC_2 and SDEC_3 have zeroes and not NA
  vri_bem[, SDEC_2:= case_when(is.na(SDEC_2) ~ 0,.default = SDEC_2)]
  vri_bem[, SDEC_3:= case_when(is.na(SDEC_3) ~ 0,.default = SDEC_3)]

  vri_bem[, curr_beu_code:= as.numeric(paste0(SDEC_1, SDEC_2, SDEC_3, curr_wl_zone))]


  vri_bem[ , Lbl_edit_wl := paste0(Lbl_edit_wl, " (", curr_beu_code, ")")]

  # Merge allowed BEU codes  -----
  # no WL 9 but it's normal
  vri_bem[buc, on = list(curr_beu_code = Code_Orig),
          `:=`(Code_WL0 = i.Code_WL0,
               Code_WL1 = i.Code_WL1,
               Code_WL2 = i.Code_WL2,
               Code_WL3 = i.Code_WL3,
               Code_WL4 = i.Code_WL4,
               Code_WL5 = i.Code_WL5,
               Code_WL6 = i.Code_WL6,
               Code_WL7 = i.Code_WL7,
               Code_WL8 = i.Code_WL8,
               Code_WL10 = i.Code_WL10)]

  # Allowed BEU codes adjustments (line 364) -----
  #Added "ES", "SC","SH","SW" to water features
  #Changed to ignore any water BEU (including WL) with a decile of 10
  #Removed expression ignoring BCLC tree levels because it leads to missing wetland and riparian corridors
  vri_bem[!(SDEC_1 == 10 & BEUMC_S1 %in% c("BB","CB","CR","ER","PB","PR","RR","RS","SK","SR","TF","WG","WR","YB","YS","BG", "ES", "FE", "ME", "MR","SC","SH","ST","SW","WL","FS", "IM", "IN", "LL", "LS", "RE","SP","OW")) &
            #!(SDEC_1 == 10 & BEUMC_S1 == "WL") &
            wl_pct >= 8 &
            #!BCLCS_LV_4 %in% c("TB", "TC", "TM") & #Removed condition: defer to FWA over VRI BCLC data
            curr_beu_code %in% buc$Code_Orig,

          new_beu_code:= fcase(wl_pct < 14, Code_WL1, # condition that wl_pct >= 8 is above
                               wl_pct < 25, Code_WL2,
                               wl_pct < 35, Code_WL3,
                               wl_pct < 45, Code_WL4,
                               wl_pct < 55, Code_WL5,
                               wl_pct < 65, Code_WL6,
                               wl_pct < 75, Code_WL7,
                               wl_pct < 80, Code_WL8,
                               wl_pct >= 80, Code_WL10)]

  # overwrite SDEC using new_beu_code
  vri_bem[curr_beu_code!=new_beu_code,
          `:=`(SDEC_1 = (new_beu_code - new_beu_code %% 1000) /1000,
               SDEC_2 = (new_beu_code %% 1000 - new_beu_code %% 100) /100,
               SDEC_3 = (new_beu_code %% 100 - new_beu_code %% 10) /10,
               new_wl_zone = new_beu_code %% 10)]

  # Reassign ecosystems ----
  #  - reassign eco components
  #  - make adjustment below when creating a WL component
  #  - create label specifying which rows where updated


  # When new code says that is 100% wetland in unit 1 , blank all variables for unit 1 , 2 and 3

  # values for unit 1 will be created later when there was no current wetland and the new wetland is in zone 1
  set_shifted_eco_variables(vri_bem, i = which(vri_bem[["SDEC_1"]] == 10 & vri_bem[["curr_wl_zone"]] == 0 & vri_bem[["new_wl_zone"]] == 1), list(c(1,NA), c(2,NA), c(3,NA)))


  # When the new code says that there is no wetland at all, blank any wetland units

  # Old 1 / New 0 :  Remove WL from component 1, (2 & 3 move up toward 1)
  set_shifted_eco_variables(vri_bem, i = which(vri_bem[["curr_wl_zone"]] == 1 & vri_bem[["new_wl_zone"]] == 0), list(c(1,2), c(2,3), c(3,NA)))
  # Old 2 / New 0 : Remove WL from component 2  (3 move up to 2)
  set_shifted_eco_variables(vri_bem, i = which(vri_bem[["curr_wl_zone"]] == 2 & vri_bem[["new_wl_zone"]] == 0), list(c(2,3), c(3, NA)))
  # Old 3 / New 0 : Remove WL from component 3
  set_shifted_eco_variables(vri_bem, i = which(vri_bem[["curr_wl_zone"]] == 3 & vri_bem[["new_wl_zone"]] == 0), list(c(3, NA)))


  #If more specific wetland category exists, defer to that
  #If adding new WL component, also make REALM_# = "W", GROUP_# = "W" and KIND_# = "U"
  wetland_features = c("BB","CB","CR","ER","PB","PR","RR","RS","SK","SR","TF","WG","WR","YB","YS","BG", "ES", "FE", "ME", "MR","SC","SH","ST","SW")

  #OLD 0/new 1 when BEUMC_S1 or BEUMC_S2 include wetland features
  vri_bem[(BEUMC_S1 %in% wetland_features | BEUMC_S2 %in% wetland_features) &
            (curr_wl_zone == 0 & new_wl_zone == 1),
          ':=' (BEUMC_S1 = case_when(
            BEUMC_S1 %in% wetland_features ~ BEUMC_S1,
            BEUMC_S2 %in% wetland_features & !BEUMC_S1 %in% wetland_features ~ BEUMC_S2,
            .default = "WL"),
            BEUMC_S2 = case_when(
              BEUMC_S1 %in% wetland_features & BEUMC_S1 %in% wetland_features ~ BEUMC_S2,
              BEUMC_S2 %in% wetland_features & !BEUMC_S1 %in% wetland_features ~ BEUMC_S1,
              BEUMC_S2 %in% wetland_features & !BEUMC_S1 %in% wetland_features & !is.na(BEUMC_S3) ~ BEUMC_S3,
              BEUMC_S2 %in% wetland_features & !BEUMC_S1 %in% wetland_features & is.na(BEUMC_S3) ~ NA,
              .default = BEUMC_S2),
            SDEC_1 = case_when(
              !is.na(BEUMC_S2) & !is.na(BEUMC_S3) ~ SDEC_1,
              is.na(BEUMC_S2) & !is.na(BEUMC_S3) ~ SDEC_1 + SDEC_2,
              is.na(BEUMC_S2) & is.na(BEUMC_S3) ~ 10,
              .default = SDEC_1),
            SDEC_2 = case_when(
              SDEC_1 == 10 ~ 0,
              is.na(BEUMC_S2) ~ 0,
              !is.na(BEUMC_S2) & is.na(BEUMC_S3) ~ SDEC_2 + SDEC_3,
              .default = SDEC_2),
            SDEC_3 = case_when(
              SDEC_1 == 10 ~ 0,
              (SDEC_1 + SDEC_2) == 10 ~ 0,
              is.na(BEUMC_S3) ~ 0,
              .default = SDEC_3),
            BEUMC_S3 = case_when(
              SDEC_3 == 0 ~ NA,
              .default = BEUMC_S3),
            REALM_1 = "W",
            GROUP_1 = "W",
            KIND_1 = "U")
  ]
  # Old 0 / New 1 when BEUMC_S1 or BEUMC_S2 do not include wetland features: add WL to component 1, (2 & 3 move down toward 3)
  which_0_to_1 <- which(vri_bem[["curr_wl_zone"]] == 0 & vri_bem[["new_wl_zone"]] == 1 & !vri_bem[["BEUMC_S1"]] %in% wetland_features & !vri_bem[["BEUMC_S2"]] %in% wetland_features)
  set_shifted_eco_variables(vri_bem, i = which_0_to_1, list(c(2,1), c(3,2)))
  vri_bem[which_0_to_1, `:=`(BEUMC_S1 = "WL", REALM_1 = "W", GROUP_1 = "W", KIND_1 = "U")]

  # Old 0 / New 2 : Add WL to component 2, (2 move down to 3)
  vri_bem[(curr_wl_zone == 0 & new_wl_zone == 2),
          ':=' (
            BEUMC_S3 = case_when(
              SDEC_3 == 0 ~ NA,
              !BEUMC_S2 %in% wetland_features & SDEC_3 != 0 ~ BEUMC_S2,
              .default = BEUMC_S3),
            BEUMC_S2 = case_when(
              !BEUMC_S2 %in% wetland_features ~ "WL",
              .default = BEUMC_S2),
            SDEC_2 = case_when(
              is.na(BEUMC_S3) ~ SDEC_2 + SDEC_3,
              .default = SDEC_2),
            SDEC_3 = case_when(
              (SDEC_1 + SDEC_2) == 10 ~ 0,
              is.na(BEUMC_S3) ~ 0,
              .default = SDEC_3),
            REALM_1 = "W",
            GROUP_1 = "W",
            KIND_1 = "U")
  ]

  # Old 0 / New 3 : add WL to component 3
  which_0_to_3 <- which(vri_bem[["curr_wl_zone"]] == 0 & vri_bem[["new_wl_zone"]] == 3)
  set_shifted_eco_variables(vri_bem, i = which_0_to_3, list(c(3,NA)))
  vri_bem[which_0_to_3, `:=`(BEUMC_S3 = "WL", REALM_3 = "W", GROUP_3 = "W", KIND_3 = "U")]

  # When there is already a wetland but in the wrong unit

  # invert eco_vars 1 & 2
    vri_bem[(curr_wl_zone == 2 & new_wl_zone == 1) | (curr_wl_zone == 1 & new_wl_zone == 2 & !is.na(BEUMC_S2)),c("BEUMC_S1","BEUMC_S2") := .(BEUMC_S2,BEUMC_S1)]

  # invert eco_vars 1 & 3
  # note: should not be wl in 3 because it was removed earlier in this script as remnant of BEM creation process
  which_switch_1_3 <- which((vri_bem[["curr_wl_zone"]] == 3 & vri_bem[["new_wl_zone"]] == 1) | (vri_bem[["curr_wl_zone"]] == 1 & vri_bem[["new_wl_zone"]] == 3))
  set_shifted_eco_variables(vri_bem, i = which_switch_1_3, list(c(1,3), c(3,1)))

  # invert eco_vars 2 & 3
  vri_bem[(curr_wl_zone == 2 & new_wl_zone == 3 & !is.na(BEUMC_S3)) | (curr_wl_zone == 3 & new_wl_zone == 2),c("BEUMC_S2","BEUMC_S3") := .(BEUMC_S3,BEUMC_S2)]

  # update Label
  vri_bem[curr_beu_code!=new_beu_code,
          Lbl_edit_wl:=paste0(Lbl_edit_wl, "; Updated BEU: ", SDEC_1, " ", BEUMC_S1, ", ", SDEC_2, " ", BEUMC_S2, ", ", SDEC_3, " ", "BEUMC_S3")]

  vri_bem[curr_beu_code!=new_beu_code,
          Lbl_edit_wl:=paste0(Lbl_edit_wl, "; Updated BEU: ", SDEC_1, " ", BEUMC_S1, ", ", SDEC_2, " ", BEUMC_S2, ", ", SDEC_3, " ", "BEUMC_S3",
                              " (", new_beu_code, ")")]

  #If more specific wetland category exists with WL, combine into one category
  #If BEUMC_S1 has generic WL and BEUMC_S2 has wetland feature, combine under specific wetland feature
  vri_bem[(BEUMC_S1 == "WL" & BEUMC_S2 %in% wetland_features), ':=' (
    BEUMC_S1 = BEUMC_S2,
    BEUMC_S2 = fifelse(is.na(BEUMC_S3),NA_character_,BEUMC_S3),
    BEUMC_S3 = NA,
    SDEC_1 = SDEC_1 + SDEC_2,
    SDEC_2 = fifelse(SDEC_3 == 0,0,SDEC_3),
    SDEC_3 = 0)]
  #If BEUMC_S2 has generic WL and BEUMC_S1 has specific wetland/riparian type, combine within BEUMC_S1
  vri_bem[(BEUMC_S2 == "WL" & BEUMC_S1 %in% wetland_features), ':=' (
    BEUMC_S2 = fifelse(is.na(BEUMC_S3),NA_character_,BEUMC_S3),
    BEUMC_S3 = NA,
    SDEC_1 = SDEC_1 + SDEC_2,
    SDEC_2 = fifelse(SDEC_3 == 0,0,SDEC_3),
    SDEC_3 = 0)]
  #If BEUMC_S3 is generic WL and BEUMC_S1 or BEUMC_S2 more specific wetland type, combine within those (deferring to BEUMC_S1)
  vri_bem[(BEUMC_S3 == "WL" & BEUMC_S1 %in% wetland_features)|(BEUMC_S3 == "WL" & BEUMC_S2 %in% wetland_features), ':=' (
    SDEC_3 = 0,
    SDEC_1 = fifelse(BEUMC_S1 %in% wetland_features,SDEC_1 + SDEC_3,SDEC_1),
    SDEC_2 = fifelse(BEUMC_S2 %in% wetland_features & !BEUMC_S1 %in% wetland_features,SDEC_2 + SDEC_3,SDEC_2),
    BEUMC_S3 = NA)]

  #correct cases where wetland has been attributed to both BEUMC_S1 and _S2 or _S3
  vri_bem[(BEUMC_S1 == BEUMC_S2) | (BEUMC_S1 == BEUMC_S3) | (BEUMC_S2 == BEUMC_S3),':='(
    BEUMC_S2 = case_when(
      BEUMC_S1 == BEUMC_S2 & is.na(BEUMC_S3) ~ NA_character_,
      BEUMC_S1 == BEUMC_S2 & !is.na(BEUMC_S3) ~ BEUMC_S3,
      .default = BEUMC_S2),
    BEUMC_S3 = case_when(
      BEUMC_S1 == BEUMC_S3 ~ NA_character_,
      BEUMC_S2 == BEUMC_S3 ~ NA_character_,
      SDEC_3 == 0 ~ NA_character_,
      .default = BEUMC_S3),
    SDEC_1 = case_when(
      BEUMC_S1 == BEUMC_S2 ~ SDEC_1 + SDEC_2,
      BEUMC_S1 == BEUMC_S3 ~ SDEC_1 + SDEC_3,
      .default = SDEC_1),
    SDEC_2 = case_when(
      BEUMC_S1 == BEUMC_S2 & is.na(BEUMC_S3) ~ 0,
      BEUMC_S1 == BEUMC_S2 & !is.na(BEUMC_S3) ~ SDEC_3,
      .default = SDEC_2),
    SDEC_3 = case_when(
      BEUMC_S1 == BEUMC_S2 | BEUMC_S1 == BEUMC_S3 |BEUMC_S2 == BEUMC_S3 ~ 0,
      is.na(BEUMC_S3) ~ 0,
      .default = SDEC_3)
    )]

  # Riparian Mapcode adjustments (line 681) -----

  non_veg  <- c("RI", "WL", "BB", "UR", "OW", "LS", "LL", "RE", "CL", "GB", "GL", "GP", "MI", "RO", "TA", "TC", "TR",
                "UV", "BG", "CB", "FE", "MR", "PB", "RS", "SH", "SK", "SW", "WG", "TF", "YB", "YS", "AU", "AV", "ES",
                "IM", "ME", "OV", "RM", "SC", "SM", "ST")

  riparian_mapcode_dt <- data.table(bgc_zone = c("CDF", "BWBS", "SWB", "ESSF", "ICH", "CWH", "SBPS", "SBS"),
                                    beumc_s1 = c("CR", "PR", "PR", "ER", "RR", "SR", "WR", "WR"))


  riparian_update_lines <- which(site_m3a_eq_a & vri_bem[["MEAN_SLOPE"]] < 10 & !vri_bem[["BEUMC_S1"]] %in% non_veg & vri_bem[["BGC_ZONE"]] %in% riparian_mapcode_dt[["bgc_zone"]])

  set(vri_bem, i = riparian_update_lines, j = "SDEC_1", value = 10)
  set(vri_bem, i = riparian_update_lines, j = "SDEC_2", value = 0)
  set(vri_bem, i = riparian_update_lines, j = "SDEC_3", value = 0)
  set(vri_bem, i = riparian_update_lines, j = "BEUMC_S1", value = riparian_mapcode_dt$beumc_s1[match(vri_bem$BGC_ZONE[riparian_update_lines],
                                                                                                     riparian_mapcode_dt$bgc_zone)])
  set_shifted_eco_variables(vri_bem, i = riparian_update_lines, list(c(2,NA), c(3,NA)))

  vri_bem[(riparian_update_lines),
          Lbl_edit_wl:= paste0("Updated to 10 ", BEUMC_S1, " because SITE_M3A = 'a', Slope < 10, and BGC_ZONE = '", BGC_ZONE, ".")]

  vri_bem[site_m3a_eq_a , SITE_M3A := "a"]

  vri_bem[,':='(
    BEUMC_S2 = case_when(
      SDEC_2 == 0 ~ NA_character_,
      .default = BEUMC_S2),
    BEUMC_S3 = case_when(
      SDEC_3 == 0 ~ NA_character_,
      .default = BEUMC_S3),
    SDEC_1 = case_when(
      is.na(BEUMC_S2) ~ SDEC_1 + SDEC_2,
      is.na(BEUMC_S3) ~ SDEC_1 + SDEC_3,
      is.na(BEUMC_S2) & is.na(BEUMC_S3) ~ 10,
      .default = SDEC_1),
    SDEC_2 = case_when(
      is.na(BEUMC_S2) ~ 0,
      .default = SDEC_2),
    SDEC_3 = case_when(
      is.na(BEUMC_S3) ~ 0,
      .default = SDEC_3))]

  # delete temp columns
  set(vri_bem, j = c("curr_wl_zone",
                     "curr_beu_code","Code_WL0", "Code_WL1", "Code_WL2", "Code_WL3",
                     "Code_WL4", "Code_WL5", "Code_WL6", "Code_WL7", "Code_WL8", "Code_WL10",
                     "new_beu_code", "new_wl_zone"), value = NULL)

  return(st_as_sf(vri_bem))
}

