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

  # Make note of which polygons have SITE_M3A == 'a' for later (before the 'a' is potentially moved to
  # SITE_M1A or SITE_M2A)

  site_m3a_eq_a <- bfc[["SITE_M3A"]] == "a"

  # Read the table of BEU decile and mapcode updates according to wetland overlap percentage













}
