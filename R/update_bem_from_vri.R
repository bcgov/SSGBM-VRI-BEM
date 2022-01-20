#' Upadate BEM (broad ecosystem mapping) attributes based on VRI (vegetation resource inventory) attributes
#'
#' @param ifc sf object that represent the input polygon feature class
#' @param rfc sf object that represent Rivers polygon feature class (FWA_Rivers)
#' @param bec_bea data.table object of allowed BEC and BEM Code Combos
#' @param clear_site_ma boolean, if TRUE variable SITE_M1A, SITE_M2A will be cleared
#' @return sf object
#' @import sf
#' @import data.table
update_bem_from_vri <- function(ifc, rfc, bec_beu, clear_site_ma) {

  classes_ifc <- attr(ifc, "class")
  setDT(ifc)

  # in python they refer a couple time to SHAPE@AREA, I don't know if this variable is already computed
  # when the gdb is created and ARCgis just acces it , or it's recomputed when refered to (the first option is more probable)
  # We already computed feature areas in the script where we merged the bem on the vri , can we just assumed that will have an attributes already
  # computed like "SHAPE_AREA" that we can use instead of recompute the area in this script?
  if (is.null(.subset2(ifc, "SHAPE_AREA"))) {
    set(ifc , j = "SHAPE_AREA", value = st_area(ifc$geometry))
  }


  required_attributes <-  c("SDEC_1", "BEUMC_S1", "REALM_1", "GROUP_1", "CLASS_1", "KIND_1", "SITE_S1", "SITEAM_S1A",
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
                                             "FORESTED_1", "FORESTED_2", "FORESTED_3", "BCLCS_LEVEL_1", "BCLCS_LEVEL_2", "BCLCS_LEVEL_3",
                                             "BCLCS_LEVEL_4", "BCLCS_LEVEL_5", "SPECIES_CD_1", "AGE_CL_STS", "LAND_COVER_CLASS_CD_1",
                                             "EST_COVERAGE_PCT_1", "LINE_5_VEGETATION_COVER", "Area_Ha", "BGC_ZONE", "BGC_SUBZON",
                                             "SPECIES_PCT_1")


  existing_attributes <- names(ifc)

  missing_attributes <- setdiff(required_attributes, existing_attributes)

  if (length(missing_attributes) > 0) {
    # TODO message and exit
  }


  if (!("Lbl_edit" %in% existing_attributes)) {
    # TODO logger inform
    set(ifc , j = "Lbl_edit", value = "")
    required_attributes <- c(required_attributes, "Lbl_edit")
  }

  if (!("Dec_Total" %in% existing_attributes)) {
    # TODO logger inform
    set(ifc , j = "Dec_Total", value = numeric(nrow(ifc)))
    required_attributes <- c(required_attributes, "Dec_Total")
  }


  # bec beu csv part
  # are those legit variable names in R?

  required_variables <- c("BGC Subzone", "BEU_#", "Script rule", "Change to BEU =")
  missing_variables <- setdiff(required_variables, names(bec_beu))
  if (length(missing_variables) > 0) {
    # TODO message and exit
  }

 # in python they read the csv line by line and
 # they create two dictionnaries, one for the allowed combinations and another for the combinations not allowed
 # the dictionnaries are essentially a list that has element or each BGC subzones and for each of the subzones the BEU #
 # I'll see what these are use for later , but I think in R, it ill be simpler to just work with the csv in a data.table
 # we could probably do something pretty simple like (we can of course optimise)
 # bec_beu[`Script rule` == "Error", .(`BGC subzone`, `BEU_#`)]
 # bec_beu[`Script rule` != "Error", .(`BGC subzone`, `BEU_#`)]

 # perform correction


  set(ifc , j = "Site_M3A", value = "")
  set(ifc , j = "Area_HA", value = round(ifc[["SHAPE_AREA"]]/10000, 2))

  if (clear_site_ma) {
    set(ifc , j = "Site_M1A", value = "")
    set(ifc , j = "Site_M2A", value = "")
  }


  if (!("SMPL_TYPE" %in% existing_attributes)) {
    # TODO logger inform
    set(ifc , j = "Dec_Total", value = character(nrow(ifc)))
    required_attributes <- c(required_attributes, "SMPL_TYPE")
  }

  # Updating attribute table with corrections

  # we create condition variables and vectors of variables that will help us optimise the corrections

  set(ifc, j = "row_updated", value = 0)
  set(ifc, j = "blank_eco_variables", value = 0)
  set(ifc, j = "lbl_join", value = "")

  eco_variables_1 <- c("BEUMC_S1", "REALM_1", "GROUP_1", "CLASS_1", "KIND_1", "SITE_S1", "SITEAM_S1A",
                       "SITEAM_S1B", "SITEAM_S1C", "SITEAM_S1D", "SITEMC_S1", "SITE_M1A", "SITEAM_S1D",
                       "SITEMC_S1", "SITE_M1A", "SITE_M1B", "STRCT_S1", "STRCT_M1", "STAND_A1", "SERAL_1",
                       "TREE_C1", "SHRUB_C1", "DISTCLS_1", "DISTSCLS_1", "DISSSCLS_1", "SECL_1",
                       "SESUBCL_1", "COND_1", "VIAB_1", "FORESTED_1")
  eco_variables_2 <- sub("1", "2", eco_variables_1)
  eco_variables_3 <- sub("1", "3", eco_variables_1)

  seq_eco_variables <- seq.int(along.with = eco_variables_1)

  fill_empty_ind <- substr(eco_variables_1, 1, 4) %in% c("TREE", "SHRU")

  smpl_type_is_empy <- ifc[["SMPL_TYPE"]] %in% c("", "None")

  beumc_s1_eq_beumc_s2 <- ifc[["BEUMC_S1"]] == ifc[["BEUMC_S2"]]

  sdec_1_gt_0 <- ifc[["SDEC_1"]] > 0
  sdec_2_gt_0 <- ifc[["SDEC_2"]] > 0
  sdec_3_gt_0 <- ifc[["SDEC_3"]] > 0

  blcs_level_1_eq_N <- ifc[["BCLCS_LEVEL_1"]] == "N"
  blcs_level_1_eq_N <- ifc[["BCLCS_LEVEL_1"]] == "V"

  blcs_level_2_eq_N <- ifc[["BCLCS_LEVEL_2"]] == "N"

  blcs_level_3_eq_N <- ifc[["BCLCS_LEVEL_3"]] == "W"

  blcs_level_4_in_TB_TC_TM <- ifc[["BCLCS_LEVEL_4"]] %in% c("TB", "TC", "TM")

  blcs_level_5_eq_LA <- ifc[["BCLCS_LEVEL_5"]] == "LA"
  blcs_level_5_eq_RE <- ifc[["BCLCS_LEVEL_5"]] == "RE"
  blcs_level_5_in_RI_RS <- ifc[["BCLCS_LEVEL_5"]] %in% c("RI", "RS")
  blcs_level_5_in_RI_RS <- ifc[["BCLCS_LEVEL_5"]] %in% c("RI", "RS")

  area_ha_le_2 <- ifc[["Area_Ha"]] <= 2
  area_ha_le_60 <- ifc[["Area_Ha"]] <= 60

  age_cl_sts_eq_minus_1 <- ifc[["AGE_CL_STS"]] == -1

  beumc_s1_eq_WL <- ifc[["BEUMC_S1"]] == "WL"
  beumc_s2_eq_WL <- ifc[["BEUMC_S2"]] == "WL"
  beumc_s3_eq_WL <- ifc[["BEUMC_S3"]] == "WL"

  species_pct_1_ge_75 <- ifc[["SPECIES_PCT_1"]] >= 75

  # we get in all the condition that needs corrections

  # line 259
  condition <- smpl_type_is_empy & beumc_s1_eq_beumc_s2
  any_previous_condition <- condition

  if (any(condition)) {
    if (any(condition & sdec_1_gt_0 & sdec_2_gt_0)) {
      which_lines <- which(condition_1 & sdec_1_gt_0 & sdec_2_gt_0)
      set(ifc , i = which_lines, j = "SDEC_1", value = ifc[["SDEC_1"]][which_lines] + ifc[["SDEC_2"]][which_lines])
    }

    # line 262
    which_lines <- which(condition)
    set(ifc , i = which_lines, j = "SDEC_2", value = ifc[["SDEC_3"]][which_lines])
    # verify what type of NA and if None is equivalent to NA
    set(ifc , i = which_lines, j = "SDEC_3", value = NA)

    # line 264
    for (i in seq_eco_variables) {

      # var 2 are feed with value from var 3
      set(ifc, i = which_lines, j = eco_variables_2[i], value = ifc[[eco_variables_3[i]]][which_lines])

      # var 3 are fill with empty values
      # again we need to make sure we understand the equivalent of none vs empty

      if (fill_empty_ind[i]) {
        set(ifc, i = which_lines, j = eco_variables_3[i], value = NA)
      }
      else {
        set(ifc, i = which_lines, j = eco_variables_3[i], value = "")
      }
    }

    set(ifc, i = which_lines, j = "Lbl_edit", value = "Combined components 1 and 2 with same BEUMC_S# code into single component 1")
    set(ifc, i = which_lines, j = "lbl_join", value = "; ")
    set(ifc, i = which_lines, j = "row_updated", value = 1)
  }

  # line 279
  condition <- smpl_type_is_empy & !beumc_s1_eq_beumc_s2 & blcs_level_1_eq_N & blcs_level_5_eq_LA & area_ha_le_2
  any_previous_condition <- any_previous_condition | condition

  if (any(condition)) {
    which_lines <- which(condition)
    set(ifc, i = which_lines , j = "SDEC_1", value = 10)
    set(ifc, i = which_lines , j = "BEUMC_S1", value = "OW")
    set(ifc, i = which_lines, j = "Lbl_edit", value = "Updated to 10 OW because BCLCS_LEVEL_1 = 'N', BCLCS_LEVEL_5 = 'LA', Area <= 10 ha")
    set(ifc, i = which_lines, j = "lbl_join", value = "; ")
    set(ifc, i = which_lines, j = "row_updated", value = 1)
    set(ifc, i = which_lines, j = "blank_eco_variables", value = 1)

  }

  # line 291
  condition <- smpl_type_is_empy & !beumc_s1_eq_beumc_s2 & blcs_level_1_eq_N & blcs_level_5_eq_LA & !area_ha_le_2 & area_ha_le_60
  any_previous_condition <- any_previous_condition | condition

  if (any(condition)) {
    which_lines <- which(condition)
    set(ifc, i = which_lines, j = "SDEC_1", value = 10)
    set(ifc, i = which_lines, j = "BEUMC_S1", value = "LS")
    set(ifc, i = which_lines, j = "Lbl_edit", value = "Updated to 10 LS because BCLCS_LEVEL_1 = 'N', BCLCS_LEVEL_5 = 'LA', Area <= 60 ha")
    set(ifc, i = which_lines, j = "lbl_join", value = "; ")
    set(ifc, i = which_lines, j = "row_updated", value = 1)
    set(ifc, i = which_lines, j = "blank_eco_variables", value = 1)
  }

  # lines 303
  condition <- smpl_type_is_empy & !beumc_s1_eq_beumc_s2 & blcs_level_1_eq_N & blcs_level_5_eq_LA & !area_ha_le_60
  any_previous_condition <- any_previous_condition | condition

  if (any(condition)) {
    which_lines <- which(condition)
    set(ifc, i = which_lines, j = "SDEC_1", value = 10)
    set(ifc, i = which_lines, j = "BEUMC_S1", value = "LL")
    set(ifc, i = which_lines, j = "Lbl_edit", value = "Updated to 10 LL because BCLCS_LEVEL_1 = 'N', BCLCS_LEVEL_5 = 'LA', Area > 60 ha")
    set(ifc, i = which_lines, j = "lbl_join", value = "; ")
    set(ifc, i = which_lines, j = "row_updated", value = 1)
    set(ifc, i = which_lines, j = "blank_eco_variables", value = 1)
  }

  # line 315
  condition <- smpl_type_is_empy & !beumc_s1_eq_beumc_s2 & blcs_level_1_eq_N & blcs_level_5_eq_RE
  any_previous_condition <- any_previous_condition | condition

  if (any(condition)) {
    which_lines <- which(condition)
    set(ifc, i = which_lines, j = "SDEC_1", value = 10)
    set(ifc, i = which_lines, j = "BEUMC_S1", value = "RE")
    set(ifc, i = which_lines, j = "Lbl_edit", value = "Updated to 10 RE because BCLCS_LEVEL_1 = 'N', BCLCS_LEVEL_5 = 'RE'")
    set(ifc, i = which_lines, j = "lbl_join", value = "; ")
    set(ifc, i = which_lines, j = "row_updated", value = 1)
    set(ifc, i = which_lines, j = "blank_eco_variables", value = 1)
  }

  # line 331
  condition <- smpl_type_is_empy & !beumc_s1_eq_beumc_s2 & blcs_level_1_eq_N & blcs_level_5_in_RI_RS
  any_previous_condition <- any_previous_condition | condition

  if (any(condition)) {
    which_lines <- which(condition)
    set(ifc, i = which_lines, j = "SDEC_1", value = 10)
    set(ifc, i = which_lines, j = "BEUMC_S1", value = "RI")
    set(ifc, i = which_lines, j = "Lbl_edit", value = "Updated to 10 RI because BCLCS_LEVEL_1 = 'N', BCLCS_LEVEL_5 = 'RI' or 'RS'")
    set(ifc, i = which_lines, j = "lbl_join", value = "; ")
    set(ifc, i = which_lines, j = "row_updated", value = 1)
    set(ifc, i = which_lines, j = "blank_eco_variables", value = 1)
  }

  # line 367
  condition <- smpl_type_is_empy & !beumc_s1_eq_beumc_s2 & blcs_level_1_eq_V & blcs_level_2_eq_N & blcs_level_3_eq_W & age_cl_sts_eq_minus_1
  any_previous_condition <- any_previous_condition | condition

  if (any(condition)) {
    which_lines <- which(condition)
    set(ifc, i = which_lines, j = "SDEC_1", value = 10)
    set(ifc, i = which_lines, j = "BEUMC_S1", value = "WL")
    set(ifc, i = which_lines, j = "Lbl_edit", value = "Updated to 10 WL because BCLCS_LEVEL_1/2/3 = 'V'/'N'/'W' and AGE_CL_STS = -1")
    set(ifc, i = which_lines, j = "lbl_join", value = "; ")
    set(ifc, i = which_lines, j = "row_updated", value = 1)
    set(ifc, i = which_lines, j = "blank_eco_variables", value = 1)
  }

  # line 380
  condition <- !any_previous_condition & smpl_type_is_empy & blcs_level_4_in_TB_TC_TM
  any_previous_condition <- any_previous_condition | condition

  if (any(condition)) {

    sub_condition <- condition & beumc_s1_eq_WL

    if (any(sub_condition)) {
      if (any(sub_condition & sdec_2_gt_0)) {
        which_lines <- which(sub_condition & sdec_2_gt_0)
        set(ifc, i = which_lines, j = "SDEC_1", value = ifc[["SDEC_1"]][which_lines] + ifc[["SDEC_2"]][which_lines])
        set(ifc, i = which_lines, j = "SDEC_2", value = ifc[["SDEC_3"]][which_lines])
        set(ifc, i = which_lines, j = "SDEC_3", value = NA)
        # line 390
        for (i in seq_eco_variables) {

          # var 1 are feed with value from var 2
          set(ifc, i = which_lines, j = eco_variables_1[i], value = ifc[[eco_variables_2[i]]][which_lines])

          # var 2 are feed with value from var 3
          set(ifc, i = which_lines, j = eco_variables_2[i], value = ifc[[eco_variables_3[i]]][which_lines])

          # var 3 are fill with empty values
          # again we need to make sure we understand the equivalent of none vs empty

          if (fill_empty_ind[i]) {
            set(ifc, i = which_lines, j = eco_variables_3[i], value = NA)
          }
          else {
            set(ifc, i = which_lines, j = eco_variables_3[i], value = "")
          }
        }
        set(ifc, i = which_lines, j = "Lbl_edit", value = "Removed WL in component 1 because BCLCS_LEVEL_4 = 'TB', 'TC' or 'TM'")
        set(ifc, i = which_lines, j = "lbl_join", value = "; ")
        set(ifc, i = which_lines, j = "row_updated", value = 1)
      }
      if (any(sub_condition & !sdec_2_gt_0)) {
        which_lines <- which(condition_9 & !sdec_2_gt_0)
        set(ifc, i = which_lines, j = "Lbl_edit", value = "**** Warning: Polygon is pure WL, but BCLCS_LEVEL_4 = 'TB', 'TC' or 'TM'")
        set(ifc, i = which_lines, j = "lbl_join", value = "; ")
        set(ifc, i = which_lines, j = "row_updated", value = 1)
      }
    }

    # line 404
    sub_condition <- condition & !beumc_s1_eq_WL & beumc_s2_eq_WL

    if (any(sub_condition)) {
      # line 405
      if (any(sub_condition & sdec_3_gt_0)) {
        which_lines <- which(sub_condition & sdec_3_gt_0)
        set(ifc, i = which_lines, j = "SDEC_2", value = ifc[["SDEC_2"]][which_lines] + ifc[["SDEC_3"]][which_lines])
        set(ifc, i = which_lines, j = "SDEC_3", value = NA)
        # line 409
        for (i in seq_eco_variables) {

          # var 2 are feed with value from var 3
          set(ifc, i = which_lines, j = eco_variables_2[i], value = ifc[[eco_variables_3[i]]][which_lines])

          # var 3 are fill with empty values
          # again we need to make sure we understand the equivalent of none vs empty

          if (fill_empty_ind[i]) {
            set(ifc, i = which_lines, j = eco_variables_3[i], value = NA)
          }
          else {
            set(ifc, i = which_lines, j = eco_variables_3[i], value = "")
          }
        }
        set(ifc, i = which_lines, j = "Lbl_edit", value = "Removed WL in component 2 because BCLCS_LEVEL_4 = 'TB', 'TC' or 'TM'")
        set(ifc, i = which_lines, j = "lbl_join", value = "; ")
        set(ifc, i = which_lines, j = "row_updated", value = 1)
      }

      # line 417
      if (any(sub_condition & !sdec_3_gt_0)) {
        which_lines <- which(sub_condition & !sdec_3_gt_0)
        set(ifc, i = which_lines, j = "SDEC_1", value = ifc[["SDEC_1"]][which_lines] + ifc[["SDEC_2"]][which_lines])
        set(ifc, i = which_lines, j = "SDEC_2", value = NA)
        for (i in seq_eco_variables) {

          # var 2 are fill with empty values
          # again we need to make sure we understand the equivalent of none vs empty

          if (fill_empty_ind[i]) {
            set(ifc, i = which_lines, j = eco_variables_2[i], value = NA)
          }
          else {
            set(ifc, i = which_lines, j = eco_variables_2[i], value = "")
          }
        }
        set(ifc, i = which_lines, j = "Lbl_edit", value = "Removed WL in component 2 because BCLCS_LEVEL_4 = 'TB', 'TC' or 'TM'")
        set(ifc, i = which_lines, j = "lbl_join", value = "; ")
        set(ifc, i = which_lines, j = "row_updated", value = 1)
      }
    }

    # line 429
    sub_condition <- condition & !beumc_s1_eq_WL & !beumc_s2_eq_WL & beumc_s3_eq_WL

    if (any(sub_condition)) {
      which_lines <- which(sub_condition)
      set(ifc, i = which_lines, j = "SDEC_2", value = ifc[["SDEC_2"]][which_lines] + ifc[["SDEC_3"]][which_lines])
      set(ifc, i = which_lines, j = "SDEC_3", value = NA)
      # line 432
      for (i in seq_eco_variables) {

        # var 3 are fill with empty values
        # again we need to make sure we understand the equivalent of none vs empty

        if (fill_empty_ind[i]) {
          set(ifc, i = which_lines, j = eco_variables_3[i], value = NA)
        }
        else {
          set(ifc, i = which_lines, j = eco_variables_3[i], value = "")
        }
      }
      set(ifc, i = which_lines, j = "Lbl_edit", value = "Removed WL in component 3 because BCLCS_LEVEL_4 = 'TB', 'TC' or 'TM'")
      set(ifc, i = which_lines, j = "lbl_join", value = "; ")
      set(ifc, i = which_lines, j = "row_updated", value = 1)
    }
  }

  # line 448
  condition <- !any_previous_condition & smpl_type_is_empy & ifc[["SPECIES_CD_1"]] == "SB" & ifc[["SPECIES_PCT_1"]] >= 90
  any_previous_condition <- any_previous_condition | condition

  if (any(condition)) {
   which_lines <- which(condition)
   set(ifc, i = which_lines, j = "SDEC_1", value = 10)
   set(ifc, i = which_lines, j = "BEUMC_S1", value = "BB")
   set(ifc, i = which_lines, j = "Lbl_edit", value = "Updated to 10 BB because SPECIES_CD_1 = 'SB'")
   set(ifc, i = which_lines, j = "lbl_join", value = "; ")
   set(ifc, i = which_lines, j = "row_updated", value = 1)
   set(ifc, i = which_lines, j = "blank_eco_variables", value = 1)
  }

  # line 457
  condition <- !any_previous_condition & smpl_type_is_empy & ifc[["BCLCS_LEVEL_5"]]  == "AP"
  any_previous_condition <- any_previous_condition | condition


  if (any(condition)) {
   which_lines <- which(condition)
   set(ifc, i = which_lines, j = "SDEC_1", value = 10)
   set(ifc, i = which_lines, j = "BEUMC_S1", value = "UR")
   set(ifc, i = which_lines, j = "Lbl_edit", value = "Updated to 10 UR because BCLCS_LEVEL_5 = 'AP'")
   set(ifc, i = which_lines, j = "lbl_join", value = "; ")
   set(ifc, i = which_lines, j = "row_updated", value = 1)
   set(ifc, i = which_lines, j = "blank_eco_variables", value = 1)
  }

  # line 464
  condition <- !any_previous_condition  & smpl_type_is_empy & ifc[["BCLCS_LEVEL_5"]]  == "BU"
  any_previous_condition <- any_previous_condition | condition

  if (any(condition)) {
   which_lines <- which(condition)
   set(ifc, i = which_lines, j = "DISTCLS_1", value = "F")
   set(ifc, i = which_lines, j = "Lbl_edit", value = "Updated to DISTCLS_1 'F' because BCLCS_LEVEL_5 = 'BU'")
   set(ifc, i = which_lines, j = "lbl_join", value = "; ")
   set(ifc, i = which_lines, j = "row_updated", value = 1)
  }

  # line 470
  condition <- !any_previous_condition  & smpl_type_is_empy & ifc[["SLOPE_MOD"]]  %in% c("q", "z")
  any_previous_condition <- any_previous_condition | condition

  if (any(condition)) {
   which_lines <- which(condition)
   set(ifc, i = which_lines, j = "SDEC_1", value = 10)
   set(ifc, i = which_lines, j = "BEUMC_S1", value = "CL")
   set(ifc, i = which_lines, j = "Lbl_edit", value = "Updated to 10 CL because Slope Mod is q or z")
   set(ifc, i = which_lines, j = "lbl_join", value = "; ")
   set(ifc, i = which_lines, j = "row_updated", value = 1)
   set(ifc, i = which_lines, j = "blank_eco_variables", value = 1)
  }

  # line 484
  condition <- !any_previous_condition & smpl_type_is_empy & ifc[["BCLCS_LEVEL_5"]] == "GB"
  any_previous_condition <- any_previous_condition | condition

  if (any(condition)) {
   which_lines <- which(condition)
   set(ifc, i = which_lines, j = "SDEC_1", value = 10)
   set(ifc, i = which_lines, j = "BEUMC_S1", value = "GB")
   set(ifc, i = which_lines, j = "Lbl_edit", value = "Updated to 10 GB because BCLCS_LEVEL_5 = 'GB'")
   set(ifc, i = which_lines, j = "lbl_join", value = "; ")
   set(ifc, i = which_lines, j = "row_updated", value = 1)
   set(ifc, i = which_lines, j = "blank_eco_variables", value = 1)
  }

  # line 491
  condition <- !any_previous_condition & smpl_type_is_empy & ifc[["BCLCS_LEVEL_5"]] %in% c("GL", "PN")
  any_previous_condition <- any_previous_condition | condition

  if (any(condition)) {
   which_lines <- which(condition)
   set(ifc, i = which_lines, j = "SDEC_1", value = 10)
   set(ifc, i = which_lines, j = "BEUMC_S1", value = "GL")
   set(ifc, i = which_lines, j = "Lbl_edit", value = "Updated to 10 GL because BCLCS_LEVEL_5 = 'GL' or 'PN'")
   set(ifc, i = which_lines, j = "lbl_join", value = "; ")
   set(ifc, i = which_lines, j = "row_updated", value = 1)
   set(ifc, i = which_lines, j = "blank_eco_variables", value = 1)
  }

  # line 498
  condition <- !any_previous_condition & smpl_type_is_empy & ifc[["BCLCS_LEVEL_5"]] == "GP"
  any_previous_condition <- any_previous_condition | condition

  if (any(condition)) {
   which_lines <- which(condition)
   set(ifc, i = which_lines, j = "SDEC_1", value = 10)
   set(ifc, i = which_lines, j = "BEUMC_S1", value = "GP")
   set(ifc, i = which_lines, j = "Lbl_edit", value = "Updated to 10 GP because BCLCS_LEVEL_5 = 'GP'")
   set(ifc, i = which_lines, j = "lbl_join", value = "; ")
   set(ifc, i = which_lines, j = "row_updated", value = 1)
   set(ifc, i = which_lines, j = "blank_eco_variables", value = 1)
  }

  # line 505
  condition <- !any_previous_condition & smpl_type_is_empy & ifc[["BCLCS_LEVEL_5"]] == "LL"
  any_previous_condition <- any_previous_condition | condition

  if (any(condition)) {
   which_lines <- which(condition)
   set(ifc, i = which_lines, j = "SDEC_1", value = 10)
   set(ifc, i = which_lines, j = "BEUMC_S1", value = "LL")
   set(ifc, i = which_lines, j = "Lbl_edit", value = "Updated to 10 LL because BCLCS_LEVEL_5 = 'LL'")
   set(ifc, i = which_lines, j = "lbl_join", value = "; ")
   set(ifc, i = which_lines, j = "row_updated", value = 1)
   set(ifc, i = which_lines, j = "blank_eco_variables", value = 1)
  }

  # line 512
  condition <- !any_previous_condition & smpl_type_is_empy & ifc[["BCLCS_LEVEL_5"]] %in% c("MI", "TZ", "MZ")
  any_previous_condition <- any_previous_condition | condition

  if (any(condition)) {
   which_lines <- which(condition)
   set(ifc, i = which_lines, j = "SDEC_1", value = 10)
   set(ifc, i = which_lines, j = "BEUMC_S1", value = "MI")
   set(ifc, i = which_lines, j = "Lbl_edit", value = "Updated to 10 MI because BCLCS_LEVEL_5 = 'MI', 'TZ' or 'MZ'")
   set(ifc, i = which_lines, j = "lbl_join", value = "; ")
   set(ifc, i = which_lines, j = "row_updated", value = 1)
   set(ifc, i = which_lines, j = "blank_eco_variables", value = 1)
  }

  # line 519
  condition <- !any_previous_condition  & smpl_type_is_empy & ifc[["BCLCS_LEVEL_5"]] %in% c("RO", "BR", "BI")
  any_previous_condition <- any_previous_condition | condition

  if (any(condition)) {
   which_lines <- which(condition)
   set(ifc, i = which_lines, j = "SDEC_1", value = 10)
   set(ifc, i = which_lines, j = "BEUMC_S1", value = "RO")
   set(ifc, i = which_lines, j = "Lbl_edit", value = "Updated to 10 RO because BCLCS_LEVEL_5 = 'RO', 'BR' or 'BI'")
   set(ifc, i = which_lines, j = "lbl_join", value = "; ")
   set(ifc, i = which_lines, j = "row_updated", value = 1)
   set(ifc, i = which_lines, j = "blank_eco_variables", value = 1)
  }

  # line 526
  condition <- !any_previous_condition  & smpl_type_is_empy & ifc[["BCLCS_LEVEL_5"]] == "TA"
  any_previous_condition <- any_previous_condition | condition

  if (any(condition)) {
   which_lines <- which(condition)
   set(ifc, i = which_lines, j = "SDEC_1", value = 10)
   set(ifc, i = which_lines, j = "BEUMC_S1", value = "TA")
   set(ifc, i = which_lines, j = "Lbl_edit", value = "Updated to 10 TA because BCLCS_LEVEL_5 = 'TA'")
   set(ifc, i = which_lines, j = "lbl_join", value = "; ")
   set(ifc, i = which_lines, j = "row_updated", value = 1)
   set(ifc, i = which_lines, j = "blank_eco_variables", value = 1)
  }

  # line 533
  condition <- !any_previous_condition & smpl_type_is_empy & ifc[["BCLCS_LEVEL_5"]] %in% c("TC", "RN", "RZ")
  any_previous_condition <- any_previous_condition | condition

  if (any(condition)) {
   which_lines <- which(condition)
   set(ifc, i = which_lines, j = "SDEC_1", value = 10)
   set(ifc, i = which_lines, j = "BEUMC_S1", value = "TC")
   set(ifc, i = which_lines, j = "Lbl_edit", value = "Updated to 10 TC because BCLCS_LEVEL_5 = 'TC', 'RN' or 'RZ'")
   set(ifc, i = which_lines, j = "lbl_join", value = "; ")
   set(ifc, i = which_lines, j = "row_updated", value = 1)
   set(ifc, i = which_lines, j = "blank_eco_variables", value = 1)

  }

  # line 540
  condition <- !any_previous_condition  & smpl_type_is_empy & ifc[["BCLCS_LEVEL_5"]] == "TR"
  any_previous_condition <- any_previous_condition | condition

  if (any(condition)) {
   which_lines <- which(condition)
   set(ifc, i = which_lines, j = "SDEC_1", value = 10)
   set(ifc, i = which_lines, j = "BEUMC_S1", value = "TR")
   set(ifc, i = which_lines, j = "Lbl_edit", value = "Updated to 10 TR because BCLCS_LEVEL_5 = 'TR'")
   set(ifc, i = which_lines, j = "lbl_join", value = "; ")
   set(ifc, i = which_lines, j = "row_updated", value = 1)
   set(ifc, i = which_lines, j = "blank_eco_variables", value = 1)
  }

  # line 547
  condition <- !any_previous_condition  & smpl_type_is_empy & ifc[["BCLCS_LEVEL_5"]] %in% c("UV", "RS", "MU", "ES", "CB", "MN", "RM")
  any_previous_condition <- any_previous_condition | condition

  if (any(condition)) {
   which_lines <- which(condition)
   set(ifc, i = which_lines, j = "SDEC_1", value = 10)
   set(ifc, i = which_lines, j = "BEUMC_S1", value = "UV")
   set(ifc, i = which_lines, j = "Lbl_edit", value = "Updated to 10 UV because BCLCS_LEVEL_5 = 'UV', 'RS', 'MU', 'ES', 'CB', 'MN' or 'RM'")
   set(ifc, i = which_lines, j = "lbl_join", value = "; ")
   set(ifc, i = which_lines, j = "row_updated", value = 1)
   set(ifc, i = which_lines, j = "blank_eco_variables", value = 1)
  }

  # line 555
  condition <- !any_previous_condition  & smpl_type_is_empy & ifc[["LAND_COVER_CLASS_CD_1"]] %in% c("UV", "RS", "MU", "ES", "CB", "MN", "RM") & ifc[["EST_COVERAGE_PCT_1"]] >= 95
  any_previous_condition <- any_previous_condition | condition

  if (any(condition)) {
    which_lines <- which(condition)
    set(ifc, i = which_lines, j = "SDEC_1", value = 10)
    set(ifc, i = which_lines, j = "BEUMC_S1", value = "UV")
    set(ifc, i = which_lines, j = "Lbl_edit", value = "Updated to 10 UV because LAND_COVER_CLASS_CD_1 = 'UV', 'RS', 'MU', 'ES', 'CB', 'MN' or 'RM' and EST_COVERAGE_PCT_1 >= 95'")
    set(ifc, i = which_lines, j = "lbl_join", value = "; ")
    set(ifc, i = which_lines, j = "row_updated", value = 1)
    set(ifc, i = which_lines, j = "blank_eco_variables", value = 1)
  }

  # line 564
  condition <- !any_previous_condition  & smpl_type_is_empy & ifc[["BCLCS_LEVEL_5"]] == "UR"
  any_previous_condition <- any_previous_condition | condition

  if (any(condition)) {
   which_lines <- which(condition)
   set(ifc, i = which_lines, j = "SDEC_1", value = 10)
   set(ifc, i = which_lines, j = "BEUMC_S1", value = "UR")
   set(ifc, i = which_lines, j = "Lbl_edit", value = "Updated to 10 UR because BCLCS_LEVEL_5 = 'UR'")
   set(ifc, i = which_lines, j = "lbl_join", value = "; ")
   set(ifc, i = which_lines, j = "row_updated", value = 1)
   set(ifc, i = which_lines, j = "blank_eco_variables", value = 1)
  }

  # line 564
  condition <- !any_previous_condition  & smpl_type_is_empy & ifc[["BCLCS_LEVEL_2"]] == "T" & ifc[["SDEC_1"]] == 10 & ifc[["LINE_5_VEGETATION_COVER"]] %in% c('rz', 'rz,by', 'rz,by,he', 'rz,by,he,sl', 'rz,by,sl', 'rz,by,sl,he', 'rz,by,st', 'rz,he',
                                                                                                                   'rz,by,sl,he', 'rz,by,st', 'rz,he', 'rz,he,by', 'rz,he,by,sl', 'rz,he,sl', 'rz,he,sl,by',
                                                                                                                   'rz,he,st', 'rz,he,st,by', 'rz,hf,by', 'rz,hf,sl,by', 'rz,hg', 'rz,hg,sl', 'rz,sl',
                                                                                                                   'rz,sl,by', 'rz,sl,by,he', 'rz,sl,he', 'rz,sl,he,by', 'rz,sl,hf', 'rz,sl,hf,by', 'rz,sl,hg',
                                                                                                                   'rz,st', 'rz,st,he', 'rz,st,he,by', 'rz,st,hf', 'rz,st,hg')


  any_previous_condition <- any_previous_condition | condition

  if (any(condition)) {
   which_lines <- which(condition)
   set(ifc, i = which_lines, j = "SDEC_1", value = 8)
   set(ifc, i = which_lines, j = "SDEC_2", value = 2)
   set(ifc, i = which_lines, j = "BEUMC_S1", value = "TC")
   set(ifc, i = which_lines, j = "Lbl_edit", value = "Added 2nd component 2 TC because BCLCS_LEVEL_2 = 'T' and LINE_5_VEGETATION_COVER begins with 'rz'")
   set(ifc, i = which_lines, j = "lbl_join", value = "; ")
   set(ifc, i = which_lines, j = "row_updated", value = 1)
  }

  # line 608 (no else if be careful it's a simple if)
  condition <- smpl_type_is_empy  & ifc[["SPECIES_CD_1"]] %in% c("AC", "ACB", "ACT", "AT", "EP")

  if (any(condition)) {
    # line 609
    sub_condition <-  condition & species_pct_1_ge_75 & ifc[["STAND_A1"]] %in% c("C", "M")
    if (any(sub_condition)) {
      which_lines <- which(sub_condition)
      set(ifc, i = which_lines, j = "STAND_A1", value = "B")
      set(ifc, i = which_lines, j = "Lbl_edit", value = paste0(ifc[["Lbl_edit"]][which_lines], ifc[["lbl_join"]][which_lines], "Updated STAND_A1 to 'B' because SPECIES_CD_1 = '", ifc[["SPECIES_CD_1"]][which_lines], "' and SPECIES_PCT_1 >= 75 and STAND_A1 was 'C' or 'M'"))
      set(ifc, i = which_lines, j = "lbl_join", value = "; ")
      set(ifc, i = which_lines, j = "row_updated", value = 1)
    }
    # line 618
    sub_condition <- condition & !species_pct_1_ge_75  & ifc[["STAND_A1"]] %in% c("C", "B")
    if (any(sub_condition)) {
      which_lines <- which(sub_condition)
      set(ifc, i = which_lines, j = "STAND_A1", value = "M")
      set(ifc, i = which_lines, j = "Lbl_edit", value = paste0(ifc[["Lbl_edit"]][which_lines], ifc[["lbl_join"]][which_lines] , "Updated STAND_A1 to 'M' because SPECIES_CD_1 = '", ifc[["SPECIES_CD_1"]][which_lines], "' and SPECIES_PCT_1 >= 50 and < 75 and STAND_A1 was 'C' or 'B'"))
      set(ifc, i = which_lines, j = "lbl_join", value = "; ")
      set(ifc, i = which_lines, j = "row_updated", value = 1)
    }
  }

  # line 627
  condition <-  smpl_type_is_empy & ifc[["SPECIES_CD_1"]] %in% c("B", "BB", "BL", "CW", "FD", "FDI", "HM", "HW", "PA", "PL", "PLI",
                                                                    "S", "SB", "SE", "SS", "SW", "SX", "SXW") & species_pct_1_ge_75 & ifc[["STAND_A1"]] == "M"

  if (any(condition)) {
    which_lines <- which(condition)
    set(ifc, i = which_lines, j ="STAND_A1", value =  "C")
    set(ifc, i = which_lines, j = "Lbl_edit", value = paste0(ifc[["Lbl_edit"]][which_lines], ifc[["lbl_join"]][which_lines], "Updated STAND_A1 to 'C' because SPECIES_CD_1 = '", ifc[["SPECIES_CD_1"]][which_lines], "' and SPECIES_PCT_1 >= 75 and STAND_A1 was 'M'"))
    set(ifc, i = which_lines, j = "lbl_join", value = "; ")
    set(ifc, i = which_lines, j = "row_updated", value = 1)
  }

  # line 639

  # blank remaining eco fields
  which_lines <- which(ifc[["blank_eco_variables"]] == 1)
  for (i in seq_eco_variables) {

    # var 3 are fill with empty values
    # again we need to make sure we understand the equivalent of none vs empty

    if (fill_empty_ind[i]) {
      set(ifc, i = which_lines, j = eco_variables_1[i], value = NA)
      set(ifc, i = which_lines, j = eco_variables_2[i], value = NA)
      set(ifc, i = which_lines, j = eco_variables_3[i], value = NA)
    }
    else {
      if (!(eco_variables_1[i] %in% c("BEUMC_S1", "STRCT_S1"))) {
        set(ifc, i = which_lines, j = eco_variables_1[i], value = "")
        set(ifc, i = which_lines, j = eco_variables_2[i], value = "")
        set(ifc, i = which_lines, j = eco_variables_3[i], value = "")
      }
    }
  }
  set(ifc, i = which_lines, j = "SDEC_2", value = NA)
  set(ifc, i = which_lines, j = "SDEC_3", value = NA)

  # line 654
  # can SDEC be negatives?

  set(ifc, j = "SEDC_1_txt", value = as.character(ifc[["SDEC_1"]]))
  set(ifc, j = "SEDC_2_txt", value = as.character(ifc[["SDEC_2"]]))
  set(ifc, j = "SEDC_3_txt", value = as.character(ifc[["SDEC_3"]]))
  set(ifc, i = which(is.na(ifc[["SDEC_1_txt"]])), j = "SEDC_1_txt", value = "0")
  set(ifc, i = which(is.na(ifc[["SDEC_2_txt"]])), j = "SEDC_2_txt", value = "0")
  set(ifc, i = which(is.na(ifc[["SDEC_3_txt"]])), j = "SEDC_3_txt", value = "0")

  set(ifc, i = which(smpl_type_is_empy), j = "DEC_Total", value = (ifc[["SDEC_1"]] * ifc[["SDEC_1"]] > 0) + (ifc[["SDEC_2"]] * ifc[["SDEC_2"]] > 0) + (ifc[["SDEC_3"]] * ifc[["SDEC_3"]] > 0))

  which_lines <- which(smpl_type_is_empy & decile_total != 10)
  set(ifc, i = which_lines, j = "Lbl_edit", value = paste0(ifc[["Lbl_edit"]][which_lines], ifc[["lbl_join"]][which_lines], "**** DECILE TOTAL ", ifc[["SDEC_1_txt"]][which_lines], "+", ifc[["SDEC_2_txt"]][which_lines], "+", ifc[["SDEC_3_txt"]][which_lines], "=", ifc[["DEC_Total"]][which_lines]))


  # bgc subzone and beu mapcode
  set(ifc, j = "bgc_zone_txt", value = as.character(ifc[["BGC_ZONE"]]))
  set(ifc, i = which(is.na(ifc[["bgc_zone_txt"]])) , j = "bgc_zone_txt", value = "")

  set(ifc, j = "bgc_subzone_txt", value = as.character(ifc[["BGC_SUBZON"]]))
  set(ifc, i = which(is.na(ifc[["bgc_subzone_txt"]])) , j = "bgc_subzone_txt", value = "")

  set(ifc, j = "subzone_txt", value = paste0(ifc[["bgc_zone_txt"]], ifc[["bgc_subzone_txt"]]))

  set(beu_bec, j = "merge_key", value = paste0(beu_bec[["BGC Subzone"]], "_", beu_bec[["BEU_#"]]))

  #for each decile merge bec beu on ifc on subzone and beu_# (BEUMC_Si), when script rule = 'Error' if len of "Change to BEU =" is 2 do x else do y , if no merge do z

  # I can probably optimise this even more  need to run some tests

  # I have trouble doing this in my head , i will be easier once I have the dataset

  for (i in seq.int(from = 1, to = 3)) {
    which_lines <- which(smpl_type_is_empy & ifc[[paste0("SDEC", i)]] > 0)
    match_lines <- match(paste0(ifc[["subzone_txt"]][which_lines], "_", paste0(ifc[["BEUMC_S"]][which_lines], i)), beu_bec[["merge_key"]])
    set(ifc, i = which_lines, j = "script_rule", value = beu_bec[["Script rule"]][match_lines])
    set(ifc, i = which_lines, j = "change_to_beu", value = beu_bec[["Change to BEU ="]][match_lines])

    which_lines <- which(ifc[["script_rule"]] == "Error")
  }

  # TODO  write csv


  # for all feature that intersect with rivers
  # SITE_M3A becomes "a"
  # and lbl is updated to say the old value became "a"

  # maybe reverse the geometry and the unique ( need to test)
  # just need to find the line that intersect with rivers
  which_lines <- sf:::CPL_geos_binop(
                   ifc$geometry,
                   rfc$geometry,
                  "intersects",
                   pattern = NA_character_,
                   prepared = TRUE
                  )

  set(ifc, i = which_lines, j = "Lbl_edit", value = paste0(ifc[["Lbl_edit"]][which_lines], ifc[["lbl_join"]][which_lines], "Updated SITE_M3A from '", ifc[["SITE_M3A"]][which_lines], "' to 'a' because polygon is adjacent to river"))
  set(ifc, i = which_lines, j = "SITE_M3A", value = "a")

  return(ifc)

}
