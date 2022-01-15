#' Merge BEM (broad ecosystem mapping) attributes on VRI (vegetation ressource inventory)
#'
#' @param ifc sf object that represent the input polygon feature class
#' @param rfc sf object that represent Rivers polygon feature class (FWA_Rivers)
#' @param bec_bea data.table object of allowed BEC and BEM Code Combos
#' @param clear_site_ma boolean, if TRUE variable SITE_M1A, SITE_M2A will be cleared
#' @return sf object
#' @import sf
#' @import data.table
update_bem_attributes <- function(ifc, rfc, bec_beu, clear_site_ma) {

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

  # Is this part really necessary in R
  # Maybe just to message that we created the columm Lbl_edit, but other than that the column
  # will be created in the perform correction regardless of it exists of not
  if (!("Lbl_edit" %in% existing_attributes)) {
    # TODO logger inform
    set(ifc , j = "Lbl_edit", value = character(nrow(ifc)))
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
  set(ifc , j = "Lbl_edit", value = "")
  set(ifc , j = "Site_M3A", value = "")
  set(ifc , j = "Area_HA", value = round(.subset2(ifc, "SHAPE_AREA")/10000, 2))

  if (clear_site_ma) {
    set(ifc , j = "Site_M1A", value = "")
    set(ifc , j = "Site_M2A", value = "")

  }

  # Again this might not be necessary since we'll just create the columns later when we feed value into it
  # in data.table you don't have to initialize column before adding value in it , you can do both at the same time
  if (!("SMPL_TYPE" %in% existing_attributes)) {
    # TODO logger inform
    set(ifc , j = "Dec_Total", value = character(nrow(ifc)))
    required_attributes <- c(required_attributes, "SMPL_TYPE")
  }







}
