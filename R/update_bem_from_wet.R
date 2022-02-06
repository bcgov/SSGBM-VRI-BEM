#' Upadate BEM (broad ecosystem mapping) attributes based on Wetland polygons attributes
#'
#' @param bfc sf object that represents the BEM feature class
#' @param wfc sf object that represents Wetlands polygon feature class
#' @param buc data.table object that represents BEU wetland update table
#' @return sf object
#' @import sf
#' @import data.table
#' @export
update_bem_from_wet <- function(bfc, wfc, buc) {

  setDT(bfc)

  # check if all required attributes are there
  validate_required_attributes(ifc = bfc,
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
  # check if bem contains duplicate teis_id
  if (length(unique(bfc$TEIS_ID)) < nrow(bfc)) {
    warning("Duplicate TEIS_ID values found. Repopulating TEIS_ID field with unique values.")
    set(bfc, j = "TEIS_ID", value = seq.int(length.out = nrow(bfc)))
  }

  if (is.null(bfc[["Lbl_edit_wl"]])) {
    set(bfc , j = "Lbl_edit_wl", value = "")
  }

  # Add SMPL_TYPE field if it doesn't already exist

  if (is.null(bfc[["SMPL_TYPE"]])) {
    set(bfc , j = "SMPL_TYPE", value = "")
  }

  # create eco variables vectors so its easier to blank them
  eco_variables_string_1 <- c("REALM_1", "GROUP_1", "CLASS_1", "KIND_1", "SITE_S1", "SITEAM_S1A",
                              "SITEAM_S1B", "SITEAM_S1C", "SITEAM_S1D", "SITEMC_S1", "SITE_M1A", "SITE_M1B", "STRCT_S1", "STRCT_M1", "STAND_A1", "SERAL_1",
                              "DISTCLS_1", "DISTSCLS_1", "DISSSCLS_1", "SECL_1",
                              "SESUBCL_1", "COND_1", "VIAB_1")
  eco_variables_string_2 <- sub("1", "2", eco_variables_string_1)
  eco_variables_string_3 <- sub("1", "3", eco_variables_string_1)

  eco_variables_integer_1 <- c("TREE_C1", "SHRUB_C1")
  eco_variables_integer_2 <- sub("1", "2", eco_variables_integer_1)
  eco_variables_integer_3 <- sub("1", "3", eco_variables_integer_1)


  # Make note of which polygons have SITE_M3A == 'a' for later (before the 'a' is potentially moved to
  # SITE_M1A or SITE_M2A)
  site_m3a_eq_a <- bfc[["SITE_M3A"]] == "a"


  # compute % of wetland area for each BEM ----

  # Find intersections between BEM and Wetlands
  intersections <- st_intersection(bfc$Shape, wfc$Shape)
  intersection_dt <- data.table(bfc = attr(intersections, "idx")[, 1], wfc = attr(intersections, "idx")[, 2], area = st_area(intersections))
  index_dt <- intersection_dt[, .(wetland_area = sum(area)), by = bfc]

  bfc[index_dt[["bfc"]], wetland_area := index_dt[["wetland_area"]]]
  bfc[, pct_area:= wetland_area/vri_area]

  bfc[, Lbl_edit_wl := "No Wetland."]
  bfc[!is.na(pct_area), Lbl_edit_wl := paste0(pct_area, "% of polygon occupied by wetland.")]

  bfc[, Lbl_edit_wl := paste0(Lbl_edit_wl, " Current BEU: ", SDEC_1, " ", BEUMC_S1)]

  # creation of condition variables that will be used multiple times to avoid having to compute them more than once


  bfc[SDEC_2 > 0, Lbl_edit_wl := paste0(Lbl_edit_wl, ", ", SDEC_2, " ", BEUMC_S2)]
  bfc[SDEC_3 > 0, Lbl_edit_wl := paste0(Lbl_edit_wl, ", ", SDEC_3, " ", BEUMC_S3)]


  # If curr_beu_code is 4 digits: 1 digit for each decile, with the 4th digit:
  #     0 if none of the 3 components is WL
  #     1 if the 1st component is WL
  #     2 if the 2nd component is WL
  #     3 if the 3rd component is WL
  # If curr_beu_code is 5 digits: the first decile value must 10, the 2nd and 3rd are 0's,
  # and the 5th digit is:
  #     0 if the first and only component is not WL
  #     1 if the first and only component is WL

  bfc[, SDEC_2_num := fifelse(is.na(SDEC_2), 0, SDEC_2)]
  bfc[, SDEC_3_num := fifelse(is.na(SDEC_3), 0, SDEC_3)]
  bfc[, curr_wl_zone:=fcase(BEUMC_S1 == "WL", 1,
                            BEUMC_S2 == "WL", 2,
                            BEUMC_S3 == "WL", 3,
                            default = 0)]

  bfc[, curr_beu_code:= paste0(SDEC_1, SDEC_2_num, SDEC_3_num, curr_wl_zone)]


  bfc[ , Lbl_edit_wl := paste0(Lbl_edit_wl, " (", curr_beu_code, ")")]


  # line 351
  bfc[(SDEC_3 > 0 & BEUMC_S3 == "WL"),
      `:=`(SDEC_1 = SDEC_3,
           SDEC_3 = NA,
           BEUMC_S3 = "")]

  # line 355
  bfc[(SDEC_3 > 0 & BEUMC_S3 == "WL"), c(eco_variables_string_1, eco_variables_string_2, eco_variables_string_3) := ""]
  bfc[(SDEC_3 > 0 & BEUMC_S3 == "WL"), c(eco_variables_integer_1, eco_variables_integer_2, eco_variables_integer_3) := NA_integer_]

  # Merge allowed BEU codes  -----
  bfc[buc, on = list(curr_beu_code = Code_Orig),
      `:=`(Code_WL0 = i.Code_WL0,
           Code_WL1 = i.Code_WL1,
           Code_WL2 = i.Code_WL2,
           Code_WL3 = i.Code_WL3,
           Code_WL4 = i.Code_WL4,
           Code_WL5 = i.Code_WL5,
           Code_WL6 = i.Code_WL6,
           Code_WL8 = i.Code_WL8,
           Code_WL10 = i.COde_WL10)]

  # Allowed BEU codes adjustments (line 364) -----
  bfc[!(SDEC_1 >= 5 & BEUMC_S1 %in% c("BB", "CB", "CR", "ER", "RI", "PB", "PR", "RR", "RS", "SK", "SR", "TF",
                                      "WG", "WR", "YB", "YS", "BG", "FE", "MR", "OW", "SH", "SW", "BA", "LS", "LL")) &
        !(SDEC_1 == 10 & BEUMC_S1 == "WL") & wl_pct >= 8 & !BCLCS_LV_4 %in% c("TB", "TC", "TM") &
        curr_beu_code %in% buc$Code_Orig,

      new_beu_code:= fcase(wl_pct < 14, Code_WL1, # condition that wl_pct is above
                           wl_pct < 25, Code_WL2,
                           wl_pct < 35, Code_WL3,
                           wl_pct < 45, Code_WL4,
                           wl_pct < 55, Code_WL5,
                           wl_pct < 65, Code_WL6,
                           wl_pct < 75, Code_WL7,
                           wl_pct < 80, Code_WL8,
                           wl_pct >= 80, Code_WL10)]
  #Create new vars
  bfc[, `:=`(new_sdec_1 = SDEC_1,
             new_beumc_s1 = BEUMC_S1,
             new_sdec_2 = SDEC_2,
             new_beumc_s2 = BEUMC_S2,
             new_sdec_3 = SDEC_3,
             new_beumc_s3 = BEUMC_S3)]

  bfc[curr_beu_code!=new_beu_code,
        `:=`(new_sdec_1 = (new_beu_code - new_beu_code %% 1000) /1000,
             new_sdec_2 = (new_beu_code %% 1000 - new_beu_code %% 100) /100,
             new_sdec_3 = (new_beu_code %% 100 - new_beu_code %% 10) /10,
             new_wl_zone = new_beu_code %% 10)]

  # Reassign ecosystems ----
  #  - reassign eco components
  #  - make adjustment bellow when creating a WL component
  #  - create label specifying which rows where updated (ref: phyton line #618)

  #bfc[new_sdec_1 == 10, ]#Emplty eco_vars for 2 & 3]
  set_shifted_eco_variables(bfc, bfc[["new_sdec_1"]] == 10, list(c(1,2), c(NA,NA)))

  #bfc[new_wl_zone == curr_wl_zone, ] # No change assign all eco_vars to same as before

  # Old 1 / New 0 :  Remove WL from component 1, (2 & 3 move up toward 1)
  set_shifted_eco_variables(bfc, bfc[["curr_wl_zone"]] == 1 & bfc[["new_wl_zone"]] == 0, list(c(1,2,3), c(2,3,NA)))
  # Old 2 / New 0 : Remove WL from component 2  (3 move up to 2)
  set_shifted_eco_variables(bfc, bfc[["curr_wl_zone"]] == 2 & bfc[["new_wl_zone"]] == 0, list(c(2,3), c(3, NA)))
  # Old 3 / New 0 : Remove WL from component 3
  set_shifted_eco_variables(bfc, bfc[["curr_wl_zone"]] == 3 & bfc[["new_wl_zone"]] == 0, list(c(3), c(NA)))

  # When adding a new WL component, also make REALM_# = "W", GROUP_# = "W" and KIND_# = "U"
  # Old 0 / New 1 : add WL to component 1, (2 & 3 move down toward 3)
  cond_0_to_1 <- bfc[["curr_wl_zone"]] == 0 & bfc[["new_wl_zone"]] == 1
  set_shifted_eco_variables(bfc, which_0_to_1, list(c(2,3), c(1,2)))
  bfc[(cond_0_to_1), `:=`(BEUMC_S1 = "WL", REALM_1 = "W", GROUP_1 = "W", KIND_1 = "U")]
  # Old 0 / New 2 : Add WL to component 2, (2 move down to 3)
  cond_0_to_2 <- bfc[["curr_wl_zone"]] == 0 & bfc[["new_wl_zone"]] == 2
  set_shifted_eco_variables(bfc, which_0_to_2, list(c(3), c(2)))
  bfc[(cond_0_to_2), `:=`(BEUMC_S2 = "WL", REALM_2 = "W", GROUP_2 = "W", KIND_2 = "U")]
  # Old 0 / New 3 : add WL to component 3
  cond_0_to_3 <- bfc[["curr_wl_zone"]] == 0 & bfc[["new_wl_zone"]] == 3
  bfc[(cond_0_to_3), `:=`(BEUMC_S3 = "WL", REALM_3 = "W", GROUP_3 = "W", KIND_3 = "U")]

  # Old 2 / New 1 : invert eco_vars 1 & 2
  set_shifted_eco_variables(bfc, bfc[["curr_wl_zone"]] == 2 & bfc[["new_wl_zone"]] == 1, list(c(1,2), c(2,1)))
  # Old 2 / New 3 : invert eco_vars 2 & 3
  set_shifted_eco_variables(bfc, bfc[["curr_wl_zone"]] == 2 & bfc[["new_wl_zone"]] == 3, list(c(2,3), c(3,2)))

  # Old 3 / New 1 : invert eco vars 1 & 3
  set_shifted_eco_variables(bfc, bfc[["curr_wl_zone"]] == 3 & bfc[["new_wl_zone"]] == 1, list(c(1,3), c(3,1)))
  # Old 3 / New 2 : invert eco_vars 2 & 3
  set_shifted_eco_variables(bfc, bfc[["curr_wl_zone"]] == 3 & bfc[["new_wl_zone"]] == 2, list(c(2,3), c(3,2)))


  #Old 1 / New 2 : invert eco vars 1 & 2
  set_shifted_eco_variables(bfc, bfc[["curr_wl_zone"]] == 1 & bfc[["new_wl_zone"]] == 2, list(c(1,2), c(2,1)))
  #Old 1 / New 3 : move 1 to 3 (2 & 3 shift up towards 1)
  set_shifted_eco_variables(bfc, bfc[["curr_wl_zone"]] == 1 & bfc[["new_wl_zone"]] == 3, list(c(1,2,3), c(3,1,2)))

  # update Label
  bfc[curr_beu_code!=new_beu_code,
      Lbl_edit_wl:=paste0(Lbl_edit_wl, "; Updated BEU: ", SDEC_1, " ", BEUMC_S1, ", ", SDEC_2, " ", BEUMC_S2, ", ", SDEC_3, " ", "BEUMC_S3")]

  bfc[curr_beu_code!=new_beu_code,
      Lbl_edit_wl:=paste0(Lbl_edit_wl, "; Updated BEU: ", SDEC_1, " ", BEUMC_S1, ", ", SDEC_2, " ", BEUMC_S2, ", ", SDEC_3, " ", "BEUMC_S3",
                          " (", new_beu_code, ")")]


  # Riparian Mapcode adjustments (line 681) -----

  non_veg  <- c("RI", "WL", "BB", "UR", "OW", "LS", "LL", "RE", "CL", "GB", "GL", "GP", "MI", "RO", "TA", "TC", "TR",
               "UV", "BG", "CB", "FE", "MR", "PB", "RS", "SH", "SK", "SW", "WG", "TF", "YB", "YS", "AU", "AV", "ES",
               "IM", "ME", "OV", "RM", "SC", "SM", "ST")

  riparian_mapcode_dt <- data.table(bgc_zone = c("CDF", "BWBS", "SWB", "ESSF", "ICH", "CWH", "SBPS", "SBS"),
                                    beumc_s1 = c("CR", "PR", "PR", "ER", "RR", "SR", "WR", "WR"))


  riparian_update_lines <- which(site_m3a_eq_a & MEAN_SLOPE < 10 & !BEUMC_S1 %in% non_veg & BGC_ZONE %in% riparian_mapcode_dt[["bgc_zone"]])

  set(bfc, i = riparian_update_lines, j = "SDEC_1", value = 10)
  set(bfc, i = riparian_update_lines, j = "SDEC_2", value = 0)
  set(bfc, i = riparian_update_lines, j = "SDEC_3", value = 0)
  set(bfc, i = riparian_update_lines, j = "BEUMC_S1", value = riparian_mapcode_dt$beumc_s1[match(bfc$BGC_ZONE[riparian_update_lines],
                                                                                                 riparian_mapcode_dt$bgc_zone)])
  set(bfc, i = riparian_update_lines, j = "BEUMC_S2", value = "")
  set(bfc, i = riparian_update_lines, j = "BEUMC_S3", value = "")
  set(bfc, i = riparian_update_lines, j = c(eco_variables_string_2, eco_variables_string_3), value = "")
  set(bfc, i = riparian_update_lines, j = c(eco_variables_integer_2, eco_variables_integer_3), value = NA_integer_)
  set(bfc, i = riparian_update_lines, j = "Lbl_edit_wl", value = paste0(updates))

  bfc[(riparian_update_lines),
      Lbl_edit_wl:= paste0("Updated to 10 ", BEUMC_S1, " because SITE_M3A = 'a', Slope < 10, and BGC_ZONE = '", BGC_ZONE, ".")]

  bfc[site_m3a_eq_a , SITE_M3A := "a"]


  return(st_as_sf(bfc))
}

