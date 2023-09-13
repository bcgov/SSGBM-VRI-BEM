#' Update BEM attributes based on VRI attributes
#'
#'This function updates BEM (broad ecosystem mapping) attributes based on VRI (vegetation resource inventory) attributes, according to the document "Corrections to the BEM map codes".
#'
#' @param vri_bem sf object that represent the combined vri & bem polygon feature class
#' @param rivers sf object that represent Rivers polygon feature class (FWA_Rivers)
#' @param beu_bec data.table object of allowed BEC and BEM Code Combos
#' @param clear_site_ma boolean, if TRUE variable SITE_M1A, SITE_M2A will be cleared
#' @param use_ifelse boolean, if TRUE correction done after the combine_duplicated_BEUMC will only be applied on rows that were not affected by the correction of duplicated BEUMC
#' @details
#' This function performs some adjustments to BEM attributes based on VRI attributes :
#'   * Combine together components with duplicated BEUMC
#'   * Perform corrections based on BC Land Cover Classification Scheme (BCLCS) and land : assign related SDEC_1 to 10
#'   * Remove inadequate wetlands
#'   * Update STAND_... based on species
#'
#' @return sf object which contains adjusted map codes
#' @import sf
#' @import data.table
#' @export

update_bem_from_vri <- function(vri_bem, rivers, beu_bec, clear_site_ma = TRUE, use_ifelse = TRUE) {

  # TODO verify that FORESTED_1 and BEUMC_S1 are blank when needed

  classes_vri_bem <- attr(vri_bem, "class")
  setDT(vri_bem)

  if (is.null(vri_bem[["vri_area"]])) {
    set(vri_bem, j = "vri_area", value = st_area(vri_bem$Shape))
  }

  # validate inputs ----
  validate_required_attributes(ifc = vri_bem,
                               required_attributes = c("SDEC_1", "BEUMC_S1", "REALM_1", "GROUP_1", "CLASS_1", "KIND_1", "SITE_S1", "SITEAM_S1A",
                                                       "SITEAM_S1B", "SITEAM_S1C", "SITEAM_S1D", "SITEMC_S1", "SITE_M1A", "SITE_M1B", "STRCT_S1",
                                                       "STRCT_M1", "STAND_A1", "SERAL_1", "TREE_C1", "SHRUB_C1", "DISTCLS_1", "DISTSCLS_1",
                                                       "DISSSCLS_1", "SECL_1", "SESUBCL_1", "COND_1", "VIAB_1", "SDEC_2", "BEUMC_S2", "REALM_2",
                                                       "GROUP_2", "CLASS_2", "KIND_2", "SITE_S2", "SITEAM_S2A", "SITEAM_S2B", "SITEAM_S2C",
                                                       "SITEAM_S2D", "SITEMC_S2", "SITE_M2A", "SITE_M2B", "STRCT_S2", "STRCT_M2", "STAND_A2",
                                                       "SERAL_2", "TREE_C2", "SHRUB_C2", "DISTCLS_2", "DISTSCLS_2", "DISSSCLS_2", "SECL_2",
                                                       "SESUBCL_2", "COND_2", "VIAB_2", "SDEC_3", "BEUMC_S3", "REALM_3", "GROUP_3", "CLASS_3",
                                                       "KIND_3", "SITE_S3", "SITEAM_S3A", "SITEAM_S3B", "SITEAM_S3C", "SITEAM_S3D", "SITEMC_S3",
                                                       "SITE_M3A", "SITE_M3B", "STRCT_S3", "STRCT_M3", "STAND_A3", "SERAL_3", "TREE_C3", "SHRUB_C3",
                                                       "DISTCLS_3", "DISTSCLS_3", "DISSSCLS_3", "SECL_3", "SESUBCL_3", "COND_3", "VIAB_3", "SLOPE_MOD",
                                                       "FORESTED_1", "FORESTED_2", "FORESTED_3", "BCLCS_LV_1", "BCLCS_LV_2", "BCLCS_LV_3",
                                                       "BCLCS_LV_4", "BCLCS_LV_5", "SPEC_CD_1", "AGE_CL_STS", "LAND_CD_1",
                                                       "COV_PCT_1", "LBL_VEGCOV", "Area_Ha", "BGC_ZONE", "BGC_SUBZON",
                                                       "SPEC_PCT_1"))

  if (is.null(vri_bem[["lbl_edit"]])) {
    set(vri_bem , j = "lbl_edit", value = "")
  }

  if (is.null(vri_bem[["DEC_Total"]])) {
    set(vri_bem , j = "DEC_Total", value = 0L)
  }

  if (is.null(vri_bem[["SMPL_TYPE"]])) {
    set(vri_bem, j = "SMPL_TYPE", value = NA_character_)
  }

 # perform corrections ----

  if (clear_site_ma) {
    set(vri_bem , j = c("SITE_M1A", "SITE_M2A"), value = NA_character_)
  }

  set(vri_bem , j = "SITE_M3A", value = NA_character_) #M3A is always cleared
  set(vri_bem , j = "Area_Ha", value = as.numeric(round(vri_bem[["vri_area"]]/10000, 2)))


  set(vri_bem, j = "row_updated", value = FALSE)
  set(vri_bem, j = "blank_eco_variables", value = FALSE)


  ## Remove duplicate labels (line 259) ----
  # In BEM may have had two of the same forested unit; one associated with one set
  # of site conditions and the other representing different conditions.
  # Site modifiers are NOT updated in this product. Therefore, duplicate labels were combined.

  vri_bem <- combine_duplicated_BEUMC(ifc = vri_bem, use_ifelse = use_ifelse)

  ## OW - Shallow Open Water (line 279) ----
  # -shallow open water typically associated with floating vegetation
  # -For LIW for moose, LS is rated 0.05, and OW is rated 0.25 because of its common association with a
  # shrub fringe
  which_OW <- which(is.na(vri_bem[["SMPL_TYPE"]]) & vri_bem[["BCLCS_LV_1"]] == "N" & vri_bem[["BCLCS_LV_5"]] == "LA" &
                      vri_bem[["Area_Ha"]] <= 2 & !vri_bem[["row_updated"]])

  vri_bem[(which_OW), `:=`(SDEC_1 = 10,
                           BEUMC_S1 = "OW",
                           lbl_edit = "Updated to 10 OW because BCLCS_LV_1 = 'N', BCLCS_LV_5 = 'LA', Area <= 10 ha",
                           row_updated = TRUE,
                           blank_eco_variables = TRUE)]


  ## LS - Small Lake (line 291) ----
  which_LS <- which(is.na(vri_bem[["SMPL_TYPE"]]) & vri_bem[["BCLCS_LV_1"]] == "N" & vri_bem[["BCLCS_LV_5"]] == "LA" &
                      vri_bem[["Area_Ha"]] > 2 & vri_bem[["Area_Ha"]] <=60 & !vri_bem[["row_updated"]])

  vri_bem[(which_LS), `:=`(SDEC_1 = 10,
                       BEUMC_S1 = "LS",
                       lbl_edit = "Updated to 10 LS because BCLCS_LV_1 = 'N', BCLCS_LV_5 = 'LA', Area <= 60 ha",
                       row_updated = TRUE,
                       blank_eco_variables = TRUE)]


  ## LL - Large Lake (line 303) ----
  which_LL <- which(is.na(vri_bem[["SMPL_TYPE"]]) & vri_bem[["BCLCS_LV_1"]] == "N" & vri_bem[["BCLCS_LV_5"]] == "LA" &
                      vri_bem[["Area_Ha"]] > 60 & !vri_bem[["row_updated"]])

  vri_bem[(which_LL), `:=`(SDEC_1 = 10,
                       BEUMC_S1 = "LL",
                       lbl_edit = "Updated to 10 LL because BCLCS_LV_1 = 'N', BCLCS_LV_5 = 'LA', Area > 60 ha",
                       row_updated = TRUE,
                       blank_eco_variables = TRUE)]



    ## RE - Reservoir (line 315) ----
  which_RE <- which(is.na(vri_bem[["SMPL_TYPE"]]) & vri_bem[["BCLCS_LV_1"]] == "N" & vri_bem[["BCLCS_LV_5"]] == "RE" & !vri_bem[["row_updated"]])

  vri_bem[(which_RE), `:=`(SDEC_1 = 10,
                       BEUMC_S1 = "RE",
                       lbl_edit = "Updated to 10 RE because BCLCS_LV_1 = 'N', BCLCS_LV_5 = 'RE'",
                       row_updated = TRUE,
                       blank_eco_variables = TRUE)]


  ## RI - Rivers (line 331) ----
  # There are two VRI codes (labels) that apply to rivers; river (RI) and river sediments (RS)
  # The default applied was to assign 'FP' (Fast Perennial Stream) to BEU_MC where rivers were identified by
  # this query.
  which_RI <- which(is.na(vri_bem[["SMPL_TYPE"]]) & vri_bem[["BCLCS_LV_1"]] == "N" & vri_bem[["BCLCS_LV_5"]] %in% c("RI", "RS") & !vri_bem[["row_updated"]])

  vri_bem[(which_RI), `:=`(SDEC_1 = 10,
                       BEUMC_S1 = "RI",
                       lbl_edit = "Updated to 10 RI because BCLCS_LV_1 = 'N', BCLCS_LV_5 = 'RI' or 'RS'",
                       row_updated = TRUE,
                       blank_eco_variables = TRUE)]



  ## WL - Wetland (line 367) ----
  which_WL <- which(is.na(vri_bem[["SMPL_TYPE"]]) & vri_bem[["BCLCS_LV_1"]] == "V" & vri_bem[["BCLCS_LV_2"]] == "N"
                    & vri_bem[["BCLCS_LV_3"]] == "W" & vri_bem[["AGE_CL_STS"]] == -1 & !vri_bem[["row_updated"]])

  vri_bem[(which_RI), `:=`(SDEC_1 = 10,
                       BEUMC_S1 = "WL",
                       lbl_edit = "Updated to 10 WL because BCLCS_LV_1/2/3 = 'V'/'N'/'W' and AGE_CL_STS = -1",
                       row_updated = TRUE,
                       blank_eco_variables = TRUE)]


  ## Remove Wetland - Forested (line 380) ----
  # should not contain a wetland (WL) label component.
  # Remove WL decile component and update value in Decile
  vri_bem <- remove_inadequate_wetlands(ifc = vri_bem)


  ## BB - Black Spruce Bog (line 448) ----

  which_BB <- which(is.na(vri_bem[["SMPL_TYPE"]]) & vri_bem[["SPEC_CD_1"]] == "SB" & vri_bem[["SPEC_PCT_1"]] >= 90 &
                      !vri_bem[["row_updated"]])

  vri_bem[(which_BB), `:=`(SDEC_1 = 10,
                       BEUMC_S1 = "BB",
                       lbl_edit = "Updated to 10 BB because SPEC_CD_1 = 'SB'",
                       row_updated = TRUE,
                       blank_eco_variables = TRUE)]


  ## AP - Anthropogenic and Non-vegetated (line 457) ----
  which_AP <- which(is.na(vri_bem[["SMPL_TYPE"]]) & vri_bem[["BCLCS_LV_5"]] == "AP" & !vri_bem[["row_updated"]])

  vri_bem[(which_AP), `:=`(SDEC_1 = 10,
                       BEUMC_S1 = "UR",
                       lbl_edit = "Updated to 10 UR because BCLCS_LV_5 = 'AP'",
                       row_updated = TRUE,
                       blank_eco_variables = TRUE)]

  ## BU - (line 464) -----
  which_BU <- which(is.na(vri_bem[["SMPL_TYPE"]]) & vri_bem[["BCLCS_LV_5"]] == "BU" & !vri_bem[["row_updated"]])

  vri_bem[(which_BU), `:=`(SDEC_1 = 10,
                       DISTCLS_1 = "F",
                       lbl_edit = "Updated to 10 UR because BCLCS_LV_5 = 'AP'",
                       row_updated = TRUE)]


  ## CL - Cliff (line 470) ----
  which_CL <- which(is.na(vri_bem[["SMPL_TYPE"]]) & vri_bem[["SLOPE_MOD"]]  %in% c("q", "z") & !vri_bem[["row_updated"]])

  vri_bem[(which_CL), `:=`(SDEC_1 = 10,
                       BEUMC_S1 = "CL",
                       lbl_edit = "Updated to 10 CL because Slope Mod is q or z",
                       row_updated = TRUE,
                       blank_eco_variables = TRUE)]

  #TODO see if comment below from python script still applies
  # TO BE ADDED
  # if  If Site Mod 3A = "a" and STS_AGE_CL >= 1 then assign 10 PR, ER OR WR as per which
  # BEC zone those units are allowed (ER can only go in the ESSF, etc.).  In common language
  # I'm saying that forested units adjacent to floodplains should be one of these riparian
  # forest types as the dominant forest ecosystem.

  ## GB - Gravel Bar (line 484) ----
  which_GB <- which(is.na(vri_bem[["SMPL_TYPE"]]) & vri_bem[["BCLCS_LV_5"]] == "GB" & !vri_bem[["row_updated"]])


  vri_bem[(which_GB), `:=`(SDEC_1 = 10,
                       BEUMC_S1 = "GB",
                       lbl_edit = "Updated to 10 GB because BCLCS_LV_5 = 'GB'",
                       row_updated = TRUE,
                       blank_eco_variables = TRUE)]


  ## GL - Glacier (line 491) ----
  which_GL <- which(is.na(vri_bem[["SMPL_TYPE"]]) & vri_bem[["BCLCS_LV_5"]] %in% c("GL", "PN") & !vri_bem[["row_updated"]])

  vri_bem[(which_GL), `:=`(SDEC_1 = 10,
                       BEUMC_S1 = "GL",
                       lbl_edit = "Updated to 10 GL because BCLCS_LV_5 = 'GL' or 'PN'",
                       row_updated = TRUE,
                       blank_eco_variables = TRUE)]



  ## GP - Gravel Pit (line 498) ----
  which_GP <- which(is.na(vri_bem[["SMPL_TYPE"]]) & vri_bem[["BCLCS_LV_5"]] == "GP" & !vri_bem[["row_updated"]])

  vri_bem[(which_GP), `:=`(SDEC_1 = 10,
                       BEUMC_S1 = "GP",
                       lbl_edit = "Updated to 10 GP because BCLCS_LV_5 = 'GB'",
                       row_updated = TRUE,
                       blank_eco_variables = TRUE)]

  ## LL - Landing (line 505) ---- #RW removed below code--BEUMC_S1 of 'LL' is already Large Lake. Moved Landing (BCLCS_LV_5 == "LL") to UV
  # which_LL <- which(is.na(vri_bem[["SMPL_TYPE"]]) & vri_bem[["BCLCS_LV_5"]] == "LL" & !vri_bem[["row_updated"]])

  #vri_bem[(which_LL), `:=`(SDEC_1 = 10,
  #                     BEUMC_S1 = "LL",
  #                     lbl_edit = "Updated to 10 LL because BCLCS_LV_5 = 'LL'",
  #                     row_updated = TRUE,
  #                     blank_eco_variables = TRUE)]

  ## MI - Mine (line 512) ----
  which_MI <- which(is.na(vri_bem[["SMPL_TYPE"]]) & vri_bem[["BCLCS_LV_5"]] %in% c("MI", "TZ", "MZ") & !vri_bem[["row_updated"]])

  vri_bem[(which_MI), `:=`(SDEC_1 = 10,
                       BEUMC_S1 = "MI",
                       lbl_edit = "Updated to 10 MI because BCLCS_LV_5 = 'MI', 'TZ' or 'MZ'",
                       row_updated = TRUE,
                       blank_eco_variables = TRUE)]

  ## RO - Rock (line 519) ----
  which_RO <- which(is.na(vri_bem[["SMPL_TYPE"]]) & vri_bem[["BCLCS_LV_5"]] %in% c("RO", "BR", "BI") & !vri_bem[["row_updated"]])

  vri_bem[(which_RO), `:=`(SDEC_1 = 10,
                       BEUMC_S1 = "RO",
                       lbl_edit = "Updated to 10 RO because BCLCS_LV_5 = 'RO', 'BR' or 'BI'",
                       row_updated = TRUE,
                       blank_eco_variables = TRUE)]


  ## TA - Talus (line 526) ----
  which_TA <- which(is.na(vri_bem[["SMPL_TYPE"]]) & vri_bem[["BCLCS_LV_5"]] == "TA" & !vri_bem[["row_updated"]])

  vri_bem[(which_TA), `:=`(SDEC_1 = 10,
                       BEUMC_S1 = "TA",
                       lbl_edit = "Updated to 10 TA because BCLCS_LV_5 = 'TA'",
                       row_updated = TRUE,
                       blank_eco_variables = TRUE)]

  ## TC - Transportation Corridor (line 533) ----
  which_TC <- which(is.na(vri_bem[["SMPL_TYPE"]]) & vri_bem[["BCLCS_LV_5"]] %in% c("TC", "RN", "RZ") & !vri_bem[["row_updated"]])

  vri_bem[(which_TC), `:=`(SDEC_1 = 10,
                       BEUMC_S1 = "TC",
                       lbl_edit = "Updated to 10 TC because BCLCS_LV_5 = 'TC', 'RN' or 'RZ'",
                       row_updated = TRUE,
                       blank_eco_variables = TRUE)]

  ##  TR - Transmission Corridor (line 540) ----
  which_TR <- which(is.na(vri_bem[["SMPL_TYPE"]]) & vri_bem[["BCLCS_LV_5"]] == "TR" & !vri_bem[["row_updated"]])

  vri_bem[(which_TR), `:=`(SDEC_1 = 10,
                       BEUMC_S1 = "TR",
                       lbl_edit = "Updated to 10 TR because BCLCS_LV_5 = 'TR'",
                       row_updated = TRUE,
                       blank_eco_variables = TRUE)]


  ## UV - Unvegetated (line 547) ----
  which_UV <- which(is.na(vri_bem[["SMPL_TYPE"]]) & vri_bem[["BCLCS_LV_5"]] %in% c("UV", "RS", "MU", "ES", "CB", "MN", "RM","LL") & !vri_bem[["row_updated"]]) #edited to add LL ("landing")

  vri_bem[(which_UV), `:=`(SDEC_1 = 10,
                           BEUMC_S1 = "UV",
                           lbl_edit = "Updated to 10 UV because BCLCS_LV_5 = 'UV', 'RS', 'MU', 'ES', 'CB', 'MN','LL', or 'RM'", #edited to add LL ("landing")
                           row_updated = TRUE,
                           blank_eco_variables = TRUE)]

  which_UV <- which(is.na(vri_bem[["SMPL_TYPE"]]) & vri_bem[["LAND_CD_1"]] %in% c("UV", "RS", "MU", "ES", "CB", "MN", "RM") &
                      vri_bem[["COV_PCT_1"]] >= 95 & !vri_bem[["row_updated"]])

  vri_bem[(which_UV), `:=`(SDEC_1 = 10,
                       BEUMC_S1 = "UV",
                       lbl_edit = "Updated to 10 UV because LAND_CD_1 = 'UV', 'RS', 'MU', 'ES', 'CB', 'MN' or 'RM' and COV_PCT_1 >= 95'",
                       row_updated = TRUE,
                       blank_eco_variables = TRUE)]


  ## UR - Urban (line 564) ----
  which_UR <- which(is.na(vri_bem[["SMPL_TYPE"]]) & vri_bem[["BCLCS_LV_5"]] == "UR" & !vri_bem[["row_updated"]])

  vri_bem[(which_UR), `:=`(SDEC_1 = 10,
                       BEUMC_S1 = "UR",
                       lbl_edit = "Updated to 10 UR because BCLCS_LV_5 = 'UR'",
                       row_updated = TRUE,
                       blank_eco_variables = TRUE)]


  ## TC - Transportation Corridor (Component 2) (line 564) ----

  which_TC2 <-  which(is.na(vri_bem[["SMPL_TYPE"]]) & vri_bem[["BCLCS_LV_2"]] == "T" & vri_bem[["SDEC_1"]] == 10 &
                        vri_bem[["LBL_VEGCOV"]] %in% c('rz', 'rz,by', 'rz,by,he', 'rz,by,he,sl', 'rz,by,sl', 'rz,by,sl,he', 'rz,by,st', 'rz,he',
                                                                'rz,by,sl,he', 'rz,by,st', 'rz,he', 'rz,he,by', 'rz,he,by,sl', 'rz,he,sl', 'rz,he,sl,by',
                                                                'rz,he,st', 'rz,he,st,by', 'rz,hf,by', 'rz,hf,sl,by', 'rz,hg', 'rz,hg,sl', 'rz,sl',
                                                                'rz,sl,by', 'rz,sl,by,he', 'rz,sl,he', 'rz,sl,he,by', 'rz,sl,hf', 'rz,sl,hf,by', 'rz,sl,hg',
                                                                'rz,st', 'rz,st,he', 'rz,st,he,by', 'rz,st,hf', 'rz,st,hg') &
                        !vri_bem[["row_updated"]])


  vri_bem[(which_TC2), `:=`(SDEC_1 = 8,
                        SDEC_2 = 2,
                        BEUMC_S2 = "TC",
                        lbl_edit = "Added 2nd component 2 TC because BCLCS_LV_2 = 'T' and LBL_VEGCOV begins with 'rz'",
                        row_updated = TRUE)]


  # Update STAND_A1 ----
  # line 608 (no `else if` be careful! it's a simple if)
  which_stand_B <- which(is.na(vri_bem[["SMPL_TYPE"]]) & vri_bem[["SPEC_CD_1"]] %in% c("AC", "ACB", "ACT", "AT", "EP") &
                           vri_bem[["SPEC_PCT_1"]] >= 75 & vri_bem[["STAND_A1"]] %in% c("C", "M"))

  vri_bem[(which_stand_B), `:=`(STAND_A1 = "B",
                            lbl_edit = paste0(lbl_edit, fifelse(lbl_edit == "", "", "; "),
                                              "Updated STAND_A1 to 'B' because SPEC_CD_1 = '", SPEC_CD_1, "' and SPEC_PCT_1 >= 75 and STAND_A1 was 'C' or 'M'"),
                            row_updated = TRUE)]


  # line 618
  which_stand_M <- which(is.na(vri_bem[["SMPL_TYPE"]]) & vri_bem[["SPEC_CD_1"]] %in% c("AC", "ACB", "ACT", "AT", "EP") &
                           vri_bem[["SPEC_PCT_1"]] >= 50 & vri_bem[["SPEC_PCT_1"]] < 75 & vri_bem[["STAND_A1"]] %in% c("C", "B"))

  vri_bem[(which_stand_M), `:=`(STAND_A1 = "M",
                            lbl_edit = paste0(lbl_edit, fifelse(lbl_edit == "", "", "; "),
                                              "Updated STAND_A1 to 'M' because SPEC_CD_1 = '", SPEC_CD_1, "' and SPEC_PCT_1 >= 50 and < 75 and STAND_A1 was 'C' or 'B'"),
                            row_updated = TRUE)]

  # line 627
  which_stand_C <- which(is.na(vri_bem[["SMPL_TYPE"]]) & vri_bem[["SPEC_CD_1"]] %in% c("B", "BB", "BL", "CW", "FD", "FDI", "HM", "HW", "PA", "PL", "PLI",
                                                                                  "S", "SB", "SE", "SS", "SW", "SX", "SXW") &
                           vri_bem[["SPEC_PCT_1"]] >= 75 & vri_bem[["STAND_A1"]] == "M")

  vri_bem[(which_stand_C), `:=`(STAND_A1 = "C",
                            lbl_edit = paste0(lbl_edit, fifelse(lbl_edit == "", "", "; "),
                                              "Updated STAND_A1 to 'C' because SPEC_CD_1 = '", SPEC_CD_1, "' and SPEC_PCT_1 >= 75 and STAND_A1 was 'M'"),
                            row_updated = TRUE)]


  #Blank Eco Fields (line 639) ----
  which_to_blank <- which(vri_bem[["blank_eco_variables"]])

  set_shifted_eco_variables(vri_bem, i = which_to_blank, list(c(1,NA), c(2,NA), c(3,NA)), character_variables_1 = c("REALM_1", "GROUP_1", "CLASS_1", "KIND_1", "SITE_S1", "SITEAM_S1A",
                                                                                                   "SITEAM_S1B", "SITEAM_S1C", "SITEAM_S1D", "SITEMC_S1", "SITE_M1A", "SITE_M1B", "STRCT_S1", "STRCT_M1", "STAND_A1", "SERAL_1",
                                                                                                   "DISTCLS_1", "DISTSCLS_1", "DISSSCLS_1", "SECL_1",
                                                                                                   "SESUBCL_1", "COND_1", "VIAB_1", "FORESTED_1"))
  set(vri_bem, i = which_to_blank, j = c("SDEC_2", "SDEC_3"), value = 0)

  # line 654
  vri_bem[is.na(SMPL_TYPE), DEC_Total:= SDEC_1 + SDEC_2 + SDEC_3]

  vri_bem[is.na(SMPL_TYPE) & DEC_Total != 10,
      `:=`(lbl_edit = paste0(lbl_edit, fifelse(lbl_edit == "", "", "; "),
                             "**** DECILE TOTAL ", SDEC_1, "+", SDEC_2, "+", SDEC_3, "=", DEC_Total),
           row_updated = TRUE)]

  # bgc subzone and beu mapcode

   set(vri_bem, j = "merge_key", value = paste0(vri_bem[["BGC_ZONE"]], vri_bem[["BGC_SUBZON"]]))

   # merge and change beu for decile 1

   vri_bem[beu_bec, on = .(merge_key = `BGC Subzone`, BEUMC_S1 = `BEU_#`), `:=`(script_rule = `i.Script rule`, change_to_beu = `i.Change to BEU =`)]

   vri_bem[script_rule == "Error" &  nchar(change_to_beu) == 2, `:=`(BEUMC_S1 = change_to_beu,
                                                                 lbl_edit = paste0(lbl_edit, fifelse(lbl_edit == "", "", "; "), merge_key, " ", BEUMC_S1, " corrected to ", change_to_beu, " in decile 1"),
                                                                 row_updated = TRUE)]

   vri_bem[script_rule == "Error" &  nchar(change_to_beu) != 2, `:=`(lbl_edit = paste0(lbl_edit, fifelse(lbl_edit == "", "", "; "), merge_key, " ", BEUMC_S1, " in decile 1 is invalid combination (mapper needs to assess)"),
                                                                 row_updated = TRUE)]

   vri_bem[script_rule == "Error" &  is.na(change_to_beu), `:=`(lbl_edit = paste0(lbl_edit, fifelse(lbl_edit == "", "", "; "), merge_key, " ", BEUMC_S1, " in decile 1 combination is not listed"),
                                                            row_updated = TRUE)]

   # remove merged variables
   set(vri_bem, j = c("script_rule", "change_to_beu"), value = NULL)


   # merge and change beu for decile 2

   vri_bem[beu_bec, on = .(merge_key = `BGC Subzone`, BEUMC_S2 = `BEU_#`), `:=`(script_rule = `i.Script rule`, change_to_beu = `i.Change to BEU =`)]

   vri_bem[script_rule == "Error" &  nchar(change_to_beu) == 2, `:=`(BEUMC_S2 = change_to_beu,
                                                                 lbl_edit = paste0(lbl_edit, fifelse(lbl_edit == "", "", "; "), merge_key, " ", BEUMC_S2, " corrected to ", change_to_beu, " in decile 2"),
                                                                 row_updated = TRUE)]

   vri_bem[script_rule == "Error" &  nchar(change_to_beu) != 2, `:=`(lbl_edit = paste0(lbl_edit, fifelse(lbl_edit == "", "", "; "), merge_key, " ", BEUMC_S2, " in decile 2 is invalid combination (mapper needs to assess)"),
                                                                 row_updated = TRUE)]

   vri_bem[script_rule == "Error" &  is.na(change_to_beu), `:=`(lbl_edit = paste0(lbl_edit, fifelse(lbl_edit == "", "", "; "), merge_key, " ", BEUMC_S2, " in decile 2 combination is not listed"),
                                                            row_updated = TRUE)]

   set(vri_bem, j = c("script_rule", "change_to_beu"), value = NULL)


   # merge and change beu for decile 3

   vri_bem[beu_bec, on = .(merge_key = `BGC Subzone`, BEUMC_S3 = `BEU_#`), `:=`(script_rule = `i.Script rule`, change_to_beu = `i.Change to BEU =`)]

   vri_bem[script_rule == "Error" &  nchar(change_to_beu) == 2, `:=`(BEUMC_S3 = change_to_beu,
                                                                 lbl_edit = paste0(lbl_edit, fifelse(lbl_edit == "", "", "; "), merge_key, " ", BEUMC_S3, " corrected to ", change_to_beu, " in decile 3"),
                                                                 row_updated = TRUE)]

   vri_bem[script_rule == "Error" &  nchar(change_to_beu) != 2, `:=`(lbl_edit = paste0(lbl_edit, fifelse(lbl_edit == "", "", "; "), merge_key, " ", BEUMC_S3, " in decile 3 is invalid combination (mapper needs to assess)"),
                                                                 row_updated = TRUE)]

   vri_bem[script_rule == "Error" &  is.na(change_to_beu), `:=`(lbl_edit = paste0(lbl_edit, fifelse(lbl_edit == "", "", "; "), merge_key, " ", BEUMC_S3, " in decile 3 combination is not listed"),
                                                            row_updated = TRUE)]

   set(vri_bem, j = c("script_rule", "change_to_beu"), value = NULL)

  # for all feature that intersect with rivers
  # SITE_M3A becomes "a"
  # and lbl is updated to say the old value became "a"

  #TODO
  # maybe reverse the geometry and the unique ( need to test)
  # just need to find the line that intersect with rivers

  which_lines <- unique(unlist(sf:::CPL_geos_binop(rivers$GEOMETRY,
                                                   vri_bem$Shape,
                                                   "intersects",
                                                   pattern = NA_character_,
                                                   prepared = TRUE)))

  vri_bem[(which_lines),
      `:=`(lbl_edit = paste0(lbl_edit, fifelse(lbl_edit == "", "", "; "),
                             "Updated SITE_M3A from '", SITE_M3A, "' to 'a' because polygon is adjacent to river"),
           SITE_M3A = "a")]


  # remove temp variables

  set(vri_bem, j = c("row_updated", "blank_eco_variables", "merge_key"), value = NULL)

  attr(vri_bem, "class") <- classes_vri_bem
  return(vri_bem)

}


combine_duplicated_BEUMC <- function(ifc, use_ifelse = TRUE){

  duplicated <- ifc[["BEUMC_S1"]] == ifc[["BEUMC_S2"]] & is.na(ifc[["SMPL_TYPE"]])

  if (any(duplicated, na.rm = TRUE)){

    ifc[(duplicated),  SDEC_1:= SDEC_1+SDEC_2]

    which_dup <- which(duplicated)

    set(ifc , i = which_dup, j = "SDEC_2", value = ifc[["SDEC_3"]][which_dup])
    set(ifc , i = which_dup, j = "SDEC_3", value = 0)

    set_shifted_eco_variables(ifc, i = which_dup, list(c(2,3), c(3,NA)))

    set(ifc, i = which_dup, j = "lbl_edit", value = "Combined components 1 and 2 with same BEUMC_S# code into single component 1")
    set(ifc, i = which_dup, j = "row_updated", value = use_ifelse)

  }

  ifc
}


remove_inadequate_wetlands <- function(ifc){

  #validate variables exists in ifc
  validate_required_attributes(ifc,
                               required_attributes = c("BEUMC_S1", "BEUMC_S2", "BEUMC_S3", "BCLCS_LV_4", "SDEC_1", "SDEC_2", "SDEC_3"))


  which_treed_WL_3 <-  which(is.na(ifc[["SMPL_TYPE"]]) & ifc[["BCLCS_LV_4"]] %in% c("TB", "TC", "TM") &
                               ifc[["BEUMC_S3"]] == "WL" & !ifc[["row_updated"]])

  #Replace wetland in 3rd component ----
  set(ifc, i = which_treed_WL_3, j = "SDEC_2", value = ifc[["SDEC_2"]][which_treed_WL_3] + ifc[["SDEC_3"]][which_treed_WL_3])
  set(ifc, i = which_treed_WL_3, j = "SDEC_3", value = 0)
  set_shifted_eco_variables(ifc, i = which_treed_WL_3, list(c(3, NA)))
  set(ifc, i = which_treed_WL_3, j = "lbl_edit", value = "Removed WL in component 3 because BCLCS_LV_4 = 'TB', 'TC' or 'TM'")
  set(ifc, i = which_treed_WL_3, j = "row_updated", value = TRUE)



  #Replace wetlands in 2nd component ----

  which_treed_WL_2_from_3 <-  which(is.na(ifc[["SMPL_TYPE"]]) & ifc[["BCLCS_LV_4"]] %in% c("TB", "TC", "TM") &
                                      ifc[["BEUMC_S2"]] == "WL" & ifc[["SDEC_3"]] > 0 & !ifc[["row_updated"]])

  which_treed_WL_2_to_1 <-  which(is.na(ifc[["SMPL_TYPE"]]) & ifc[["BCLCS_LV_4"]] %in% c("TB", "TC", "TM") &
                                      ifc[["BEUMC_S2"]] == "WL" & ifc[["SDEC_3"]] %in% c(0, NA_integer_) & !ifc[["row_updated"]])


  ## When there is a value in 3rd component update 2nd from 3rd ----
  set_shifted_eco_variables(ifc, i = which_treed_WL_2_from_3, list(c(2,3), c(3,NA)))
  set(ifc, i = which_treed_WL_2_from_3, j = "SDEC_2", value = ifc[["SDEC_2"]][which_treed_WL_2_from_3] + ifc[["SDEC_3"]][which_treed_WL_2_from_3])
  set(ifc, i = which_treed_WL_2_from_3, j = "SDEC_3", value = 0)
  set(ifc, i = which_treed_WL_2_from_3, j = "lbl_edit", value = "Removed WL in component 2 because BCLCS_LV_4 = 'TB', 'TC' or 'TM'")
  set(ifc, i = which_treed_WL_2_from_3, j = "row_updated", value = TRUE)

  ## When there is no value in 3rd component update 1st from 2nd -----
  set(ifc, i = which_treed_WL_2_to_1, j = "SDEC_1", value = ifc[["SDEC_1"]][which_treed_WL_2_to_1] + ifc[["SDEC_2"]][which_treed_WL_2_to_1])
  set_shifted_eco_variables(ifc, i = which_treed_WL_2_to_1, list(c(2, NA)))
  set(ifc, i = which_treed_WL_2_to_1, j = "SDEC_2", value = 0)
  set(ifc, i = which_treed_WL_2_to_1, j = "lbl_edit", value = "Removed WL in component 2 because BCLCS_LV_4 = 'TB', 'TC' or 'TM'")
  set(ifc, i = which_treed_WL_2_to_1, j = "row_updated", value = TRUE)


  #Replace wetlands from 1st component -----
  which_treed_WL_1_from_2 <-  which(is.na(ifc[["SMPL_TYPE"]]) & ifc[["BCLCS_LV_4"]] %in% c("TB", "TC", "TM") &
                                      ifc[["BEUMC_S1"]] == "WL" & ifc[["SDEC_2"]] > 0 & !ifc[["row_updated"]])

  set_shifted_eco_variables(ifc, which_treed_WL_1_from_2, list(c(1,2), c(2,3), c(3,NA)))
  set(ifc, i = which_treed_WL_1_from_2, j = "SDEC_3", value = 0)
  set(ifc, i = which_treed_WL_1_from_2, j = "lbl_edit", value = "Removed WL in component 1 because BCLCS_LV_4 = 'TB', 'TC' or 'TM'")
  set(ifc, i = which_treed_WL_1_from_2, j = "row_updated", value = TRUE)

  #Warning if polyfgon is pule WL ----
  which_treed_pure_WL <- which(is.na(ifc[["SMPL_TYPE"]]) & ifc[["BCLCS_LV_4"]] %in% c("TB", "TC", "TM") &
                                 ifc[["BEUMC_S1"]] %in% c(0, NA_integer_))

  set(ifc, i = which_treed_pure_WL, j = "lbl_edit", value = "**** Warning: Polygon is pure WL, but BCLCS_LV_4 = 'TB', 'TC' or 'TM'")
  set(ifc, i = which_treed_pure_WL, j = "row_updated", value = TRUE)

  # Remove BEU for lakes ----
  # In cases for small lakes (LS), large lakes (LL), and open water (OW) where BCLCS_LV_5 AND LAND_CD_1 DO NOT equal LA,
  # remove BEU label for lakes -- will need to be manually assigned. Otherwise, if BCLCS_LV_5 OR LAND_CD_1 = LA, leave BEU as-is.
  # include "OT" with "LA". Sometimes lakes are assigned BCLCS = "OT"
  which_lakes_w_BEU <- which(ifc[["BEUMC_S1"]] %in% c("LS", "LL", "OW") & !ifc[["BCLCS_LV_5"]] %in% c("LA","OT") & !ifc[["LAND_CD_1"]] %in% c("LA","OT"))
  set(ifc, i = which_lakes_w_BEU, j = "BEUMC_S1", value = NA_character_)

  #If the BEU was correctly assigned to an ecosystem which should not have a structural stage (as identified in the lookup table)
  # remove associated structure/stand information so it correctly populates suitability

  which_BEU_1_in_list <- which(ifc[["BEUMC_S1"]] %in% c("LS", "LL", "OW","MI","GL","TC","UR","RE","RI","ES","ST","UR"))
  which_BEU_2_in_list <- which(ifc[["BEUMC_S2"]] %in% c("LS", "LL", "OW","MI","GL","TC","UR","RE","RI","ES","ST","UR"))
  which_BEU_3_in_list <- which(ifc[["BEUMC_S3"]] %in% c("LS", "LL", "OW","MI","GL","TC","UR","RE","RI","ES","ST","UR"))

  set(ifc, i = which_BEU_1_in_list, j = c("STRCT_S1", "STAND_A1"), value = NA_character_)
  set(ifc, i = which_BEU_2_in_list, j = c("STRCT_S2", "STAND_A2"), value = NA_character_)
  set(ifc, i = which_BEU_3_in_list, j = c("STRCT_S3", "STAND_A3"), value = NA_character_)

  #make non-forested features are not indicated as forested
  set(ifc, i = which_BEU_1_in_list, j = "FORESTED_1", value = "N")
  set(ifc, i = which_BEU_2_in_list, j = "FORESTED_2", value = "N")
  set(ifc, i = which_BEU_3_in_list, j = "FORESTED_3", value = "N")

  ifc

}


