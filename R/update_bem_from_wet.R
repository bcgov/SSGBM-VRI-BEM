#' Upadate BEM (broad ecosystem mapping) attributes based on Wetland polygons attributes
#'
#' @param bfc sf object that represents the BEM feature class
#' @param wfc sf object that represents Wetlands polygon feature class
#' @param buc data.table object that represents BEU wetland update table
#' @return sf object
#' @import sf
#' @import data.table
update_bem_from_wet <- function(bfc, wfc, buc) {

  classes_ifc <- attr(bfc, "class")
  setDT(bfc)

  # check if all required attributes are there

  required_attributes <- c("TEIS_ID", "SDEC_1", "BEUMC_S1", "REALM_1", "GROUP_1", "CLASS_1", "KIND_1", "SITE_S1",
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
                            "MEAN_SLOPE", "BCLCS_LEVEL_4")

  existing_attributes <- names(bfc)

  if (length(setdiff(required_attributes, existing_attributes)) > 0) {
    # TODO message and action
  }

  # check if bem contains duplicate teis_id
  if (length(unique(bfc$TEIS_ID)) < nrow(bfc)) {
    # TODO message
    set(bfc, j = "TEIS_ID", value = seq.int(along.with = nrow(bfc)))
  }

  if (!("Lbl_edit_wl" %in% existing_attributes)) {
    # TODO logger inform
    set(bfc , j = "Lbl_edit_wl", value = "")
    required_attributes <- c(required_attributes, "Lbl_edit_wl")
  }

  # Add SMPL_TYPE field if it doesn't already exist

  if (!("SMPL_TYPE" %in% existing_attributes)) {
    # TODO logger inform
    set(bfc , j = "SMPL_TYPE", value = "")
    required_attributes <- c(required_attributes, "SMPL_TYPE")
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


  # TODO
  # Read the table of BEU decile and mapcode updates according to wetland overlap percentage

  # TODO merge the table on the bfc to have Code_WL_x

  # create column to have range of decile when merging


  # compute percentile of wetland area for each BEM

  # find intersections between BEM and Wetlands
  intersections <- st_intersection(bfc$Shape, wfc$Shape)
  intersection_dt <- data.table(bfc = attr(intersections, "idx")[, 1], wfc = attr(intersections, "idx")[, 2], area = st_area(intersections))
  index_dt <- intersection_dt[, .(pct_area = sum(area)/vri_area), by = bfc]

  bfc[index_dt[["bfc"]], pct_area := index_dt[["pct_area"]]]

  bfc[, Lbl_edit_wl := paste0(pct_area, "No Wetland.")]
  bfc[!is.na(pct_area), Lbl_edit_wl := paste0(pct_area, "% of polygon occupied by wetland.")]

  bfc[, Lbl_edit_wl := paste0(Lbl_edit_wl, " Current BEU: ", SDEC_1, " ", BEUMC_S1)]

  # creation of condition variables that will be used multiple times to avoid having to compute them more than once
  sdec_2_gt_0 <- bfc[["SDEC_1"]] > 5
  sdec_2_gt_0 <- bfc[["SDEC_2"]] > 0
  sdec_3_gt_0 <- bfc[["SDEC_3"]] > 0

  beumc_s3_eq_WL <- bfc[["BEUMC_S3"]] == "WL"







  bfc[(sdec_2_gt_0), Lbl_edit_wl := paste0(Lbl_edit_wl, ", ", SDEC_2, " ", BEUMC_S2)]
  bfc[(sdec_2_gt_0) & (sdec_3_gt_0), Lbl_edit_wl := paste0(Lbl_edit_wl, ", ", SDEC_3, " ", BEUMC_S3)]


  # If curr_beu_code is 4 digits: 1 digit for each decile, with the 4th digit:
  #     0 if none of the 3 components is WL
  #     1 if the 1st component is WL
  #     2 if the 2nd component is WL
  #     3 if the 3rd component is WL
  # If curr_beu_code is 5 digits: the first decile value must 10, the 2nd and 3rd are 0's,
  # and the 5th digit is:
  #     0 if the first and only component is not WL
  #     1 if the first and only component is WL


  bfc[ , Lbl_edit_wl := paste0(Lbl_edit_wl, " (", (((SDEC_1 * 10) + (SDEC_2 * !is.na(SDEC_2))) * 10 + (SDEC_3 * !is.na(SDEC_3))) * 10 + ((1 * BEUMC_S1 == "WL") + (2 * BEUMC_S1 != "WL" & BEUMC_S2 == "WL") + (3 * BEUMC_S1 != "WL" & BEUMC_S2 != "WL" & (beumc_s3_eq_WL))), ")")]


  # line 351
  bfc[(sdec_3_gt_0 & beumc_s3_eq_WL), `:=`(
    SDEC_1 = SDEC_3,
    SDEC_3 = NA,
    BEUMC_S3 = ""
  )]

  # line 355
  bfc[(sdec_3_gt_0 & beumc_s3_eq_WL), c(eco_variables_string_1, eco_variables_string_2, eco_variables_string_3) := ""]
  bfc[(sdec_3_gt_0 & beumc_s3_eq_WL), c(eco_variables_integer_1, eco_variables_integer_2, eco_variables_integer_3) := NA_integer_]

  # line 364
  condition <- ifc[["SDEC_1"]] < 5 &
               ifc[["pct_area"]] >= 8 &
               !(bfc[["BCLCS_LEVEL_4"]] %in% c("TB", "TC", "TM"))  &
               !(bfc[["BEUMC_S1"]] %in%  c("WL","BB", "CB", "CR", "ER", "RI", "PB", "PR", "RR", "RS", "SK", "SR", "TF","WG", "WR", "YB", "YS", "BG", "FE", "MR", "OW", "SH", "SW", "BA", "LS", "LL")) &
               !is.na(bfc[["Code_Orig"]])

  # we probably dont have to create the key the whole thing is juste a merge on the csv table , we could modify the csv table and merge






  # line 681

  non_veg  <- c("RI", "WL", "BB", "UR", "OW", "LS", "LL", "RE", "CL", "GB", "GL", "GP", "MI", "RO", "TA", "TC", "TR",
               "UV", "BG", "CB", "FE", "MR", "PB", "RS", "SH", "SK", "SW", "WG", "TF", "YB", "YS", "AU", "AV", "ES",
               "IM", "ME", "OV", "RM", "SC", "SM", "ST")

  riparian_mapcode_dt <- data.table(bgc_zone = c("CDF", "BWBS", "SWB", "ESSF", "ICH", "CWH", "SBPS", "SBS"), beumc_s1 = c("CR", "PR", "PR", "ER", "RR", "SR", "WR", "WR"))


  which_lines <- which(site_m3a_eq_a & ifc[["MEAN_SLOPE"]] < 10 & !(ifc[["BEUMC_S1"]] %in% non_veg) & ifc[["BGC_ZONE"]] %in% riparian_mapcode_dt[["bgc_zone"]])

  ifc[which_lines, c("SDEC_1", "SDEC_2", "SDEC_3", "BEUMC_S1", "BEUMC_S2", "BEUMC_S3",
                     eco_variables_string_2, eco_variables_string_3,
                     eco_variables_integer_2, eco_variables_integer_3) := list(10, 0, 0, BEUMC_S1[match(BGC_ZONE, riparian_mapcode_dt[["BGC_ZONE"]])], "", "", "", "", NA_integer_, NA_integer_)]


  ifc[site_m3a_eq_a , SITE_M3A := "a"]











}
