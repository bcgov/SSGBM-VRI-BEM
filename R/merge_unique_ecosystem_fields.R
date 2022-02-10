#' Assign values from unique ecosystems table
#'
#' Populates the TEIS fields, REALM, GROUP, CLASS, and KIND as well as SNOW_CODE from the data provided by the unique ecosystem data table
#'
#' @param ifc sf object
#' @return sf object
#' @import data.table
#' @export

merge_unique_ecosystem_fields <- function(ifc, unique_ecosystem_dt) {

  # use data table for fast data manipulation and fast merges
  classes_ifc <- attr(ifc, "class")
  setDT(ifc)

  # merge REALM GROUP CLASS AND KIND for broad ecosystem unit major class 1

  # We also merge the SNOW, it doesn't depend on the BEUMC and we assume that all the lines in the ecosystem table for a given BEUMC have the same snow code


  # TODO check if we can rename the non valid name , since we create the csv ourselve it should not be a problem

  ifc[unique_ecosystem_dt, on = .(BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC_S1 = BEU_MC) , `:=`(REALM_1 = i.REALM,
                                                                                                    GROUP_1 = i.GROUP,
                                                                                                    CLASS_1 = i.CLASS,
                                                                                                    KIND_1 = i.KIND,
                                                                                                    SNOW_CODE = i.Snow_Code,
                                                                                                    FORESTED_1 = `i.Forested (Y/N)`,
                                                                                                    STS_CLIMAX_1 = i.Strct_Climax,
                                                                                                    STAND_CLIMAX_1 = i.Stand_Climax,
                                                                                                    STAND_1_Age_0_15 = `i.Stand_Age_0-15`,
                                                                                                    STAND_1_Age_16_30 = `i.Stand_Age_16-30`,
                                                                                                    STAND_1_Age_31_50 = `i.Stand_Age_31-50`,
                                                                                                    STAND_1_Age_51_80 = `i.Stand_Age_51-80`,
                                                                                                    STAND_1_Age_gt_80 = `i.Stand_Age_80+`,
                                                                                                    STS_1_Age_0_3 = `i.Struct_Age_0-3`,
                                                                                                    STS_1_Age_4_10 = `i.Struct_Age_4-10`,
                                                                                                    STS_1_Age_11_30 = `i.Struct_Age_11-30`,
                                                                                                    STS_1_Age_31_40 = `i.Struct_Age_31-40`,
                                                                                                    STS_1_Age_41_60 = `i.Struct_Age_41-60`,
                                                                                                    STS_1_Age_61_80 = `i.Struct_Age_61-80`,
                                                                                                    STS_1_Age_81_139 = `i.Struct_Age_81-139`,
                                                                                                    STS_1_Age_140_249 = `i.Struct_Age_140-249`,
                                                                                                    STS_1_Age_gt_249 = `i.Struct_Age_250+`)]



  # merge REALM GROUP CLASS AND KIND for broad ecosystem unit major class 2

  ifc[unique_ecosystem_dt, on = .(BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC_S2 = BEU_MC) , `:=`(REALM_2 = i.REALM,
                                                                                                    GROUP_2 = i.GROUP,
                                                                                                    CLASS_2 = i.CLASS,
                                                                                                    KIND_2 = i.KIND,
                                                                                                    FORESTED_2 = `i.Forested (Y/N)`,
                                                                                                    STS_CLIMAX_2 = i.Strct_Climax,
                                                                                                    STAND_CLIMAX_2 = i.Stand_Climax,
                                                                                                    STAND_2_Age_0_15 = `i.Stand_Age_0-15`,
                                                                                                    STAND_2_Age_16_30 = `i.Stand_Age_16-30`,
                                                                                                    STAND_2_Age_31_50 = `i.Stand_Age_31-50`,
                                                                                                    STAND_2_Age_51_80 = `i.Stand_Age_51-80`,
                                                                                                    STAND_2_Age_gt_80 = `i.Stand_Age_80+`,
                                                                                                    STS_2_Age_0_3 = `i.Struct_Age_0-3`,
                                                                                                    STS_2_Age_4_10 = `i.Struct_Age_4-10`,
                                                                                                    STS_2_Age_11_30 = `i.Struct_Age_11-30`,
                                                                                                    STS_2_Age_31_40 = `i.Struct_Age_31-40`,
                                                                                                    STS_2_Age_41_60 = `i.Struct_Age_41-60`,
                                                                                                    STS_2_Age_61_80 = `i.Struct_Age_61-80`,
                                                                                                    STS_2_Age_81_139 = `i.Struct_Age_81-139`,
                                                                                                    STS_2_Age_140_249 = `i.Struct_Age_140-249`,
                                                                                                    STS_2_Age_gt_249 = `i.Struct_Age_250+`)]


  # merge REALM GROUP CLASS AND KIND for broad ecosystem unit major class 3

  ifc[unique_ecosystem_dt, on = .(BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC_S3 = BEU_MC) , `:=`(REALM_3 = i.REALM,
                                                                                                    GROUP_3 = i.GROUP,
                                                                                                    CLASS_3 = i.CLASS,
                                                                                                    KIND_3 = i.KIND,
                                                                                                    FORESTED_3 = `i.Forested (Y/N)`,
                                                                                                    STS_CLIMAX_3 = i.Strct_Climax,
                                                                                                    STAND_CLIMAX_3 = i.Stand_Climax,
                                                                                                    STAND_3_Age_0_15 = `i.Stand_Age_0-15`,
                                                                                                    STAND_3_Age_16_30 = `i.Stand_Age_16-30`,
                                                                                                    STAND_3_Age_31_50 = `i.Stand_Age_31-50`,
                                                                                                    STAND_3_Age_51_80 = `i.Stand_Age_51-80`,
                                                                                                    STAND_3_Age_gt_80 = `i.Stand_Age_80+`,
                                                                                                    STS_3_Age_0_3 = `i.Struct_Age_0-3`,
                                                                                                    STS_3_Age_4_10 = `i.Struct_Age_4-10`,
                                                                                                    STS_3_Age_11_30 = `i.Struct_Age_11-30`,
                                                                                                    STS_3_Age_31_40 = `i.Struct_Age_31-40`,
                                                                                                    STS_3_Age_41_60 = `i.Struct_Age_41-60`,
                                                                                                    STS_3_Age_61_80 = `i.Struct_Age_61-80`,
                                                                                                    STS_3_Age_81_139 = `i.Struct_Age_81-139`,
                                                                                                    STS_3_Age_140_249 = `i.Struct_Age_140-249`,
                                                                                                    STS_3_Age_gt_249 = `i.Struct_Age_250+`)]



  # add std_crown fields
  ifc <- add_std_crown_fields(ifc)

  ifc[ , parkland_ind := substr(BGC_SUBZON, start = length(BGC_SUBZON), stop = length(BGC_SUBZON)) == "p"]

  # calculate for 1

  ifc[, STAND_AGE_1 := fcase(VRI_AGE_CL_STD <= 15, STAND_1_Age_0_15,
                             VRI_AGE_CL_STD <= 30, STAND_1_Age_16_30,
                             VRI_AGE_CL_STD <= 50, STAND_1_Age_31_50,
                             VRI_AGE_CL_STD <= 80, STAND_1_Age_51_80,
                             VRI_AGE_CL_STD > 80, STAND_1_Age_gt_80,
                             default = NA_character_)]

  ifc[, STS_AGE_1 := fcase(VRI_AGE_CL_STS <= 3, STS_1_Age_0_3,
                           VRI_AGE_CL_STS <= 10, STS_1_Age_4_10,
                           VRI_AGE_CL_STS <= 30, STS_1_Age_11_30,
                           VRI_AGE_CL_STS <= 40, STS_1_Age_31_40,
                           VRI_AGE_CL_STS <= 60, STS_1_Age_41_60,
                           VRI_AGE_CL_STS <= 80, STS_1_Age_61_80,
                           VRI_AGE_CL_STS <= 139, STS_1_Age_81_139,
                           VRI_AGE_CL_STS <= 249, STS_1_Age_140_249,
                           VRI_AGE_CL_STS > 249, STS_1_Age_gt_249,
                           default = NA_character_)]


  ifc[, STRCT_S1 := fcase(VRI_AGE_CL_STS > 0, STS_AGE_1,
                          FORESTED_1 == "N" | parkland_ind , STS_CLIMAX_1,
                          default = NA_character_)]

  # TODO validate what format is are the struct_age_ and stand_Age variables in the csv (number or text)
  ifc[substr(STRCT_S1, start = 1, stop = 1) %in% c("4", "5", "6", "7") ,
      STAND_A1 := fcase(STD_VRI != "", STD_VRI,
                        VRI_AGE_CL_STD > 0, STAND_AGE_1,
                        FORESTED_1 == "N" | parkland_ind , STAND_CLIMAX_1,
                        default = "")]

  #TODO temporarily change class
  ifc[is.na(STRCT_S1), STRCT_S1 := ""]
  ifc[is.na(STAND_A1), STAND_A1 := ""]


  # calculate for 2

  ifc[, STAND_AGE_2 := fcase(VRI_AGE_CL_STD <= 15, STAND_2_Age_0_15,
                             VRI_AGE_CL_STD <= 30, STAND_2_Age_16_30,
                             VRI_AGE_CL_STD <= 50, STAND_2_Age_31_50,
                             VRI_AGE_CL_STD <= 80, STAND_2_Age_51_80,
                             VRI_AGE_CL_STD > 80, STAND_2_Age_gt_80,
                             default = NA_character_)]

  ifc[, STS_AGE_2 := fcase(VRI_AGE_CL_STS <= 3, STS_2_Age_0_3,
                           VRI_AGE_CL_STS <= 10, STS_2_Age_4_10,
                           VRI_AGE_CL_STS <= 30, STS_2_Age_11_30,
                           VRI_AGE_CL_STS <= 40, STS_2_Age_31_40,
                           VRI_AGE_CL_STS <= 60, STS_2_Age_41_60,
                           VRI_AGE_CL_STS <= 80, STS_2_Age_61_80,
                           VRI_AGE_CL_STS <= 139, STS_2_Age_81_139,
                           VRI_AGE_CL_STS <= 249, STS_2_Age_140_249,
                           VRI_AGE_CL_STS > 249, STS_2_Age_gt_249,
                           default = NA_character_)]


  ifc[, STRCT_S2 := fcase(VRI_AGE_CL_STS > 0, STS_AGE_2,
                          FORESTED_2 == "N" | parkland_ind , STS_CLIMAX_2,
                          default = NA_character_)]

  # TODO validate what format is are the struct_age_ and stand_Age variables in the csv (number or text)
  ifc[substr(STRCT_S2, start = 1, stop = 1) %in% c("4", "5", "6", "7") ,
      STAND_A2 := fcase(STD_VRI != "", STD_VRI,
                        VRI_AGE_CL_STD > 0, STAND_AGE_2,
                        FORESTED_2 == "N" | parkland_ind , STAND_CLIMAX_2,
                        default = "")]

  #TODO temporarily change class
  ifc[is.na(STRCT_S2), STRCT_S2 := ""]
  ifc[is.na(STAND_A2), STAND_A2 := ""]


  # calculate for 3

  ifc[, STAND_AGE_3 := fcase(VRI_AGE_CL_STD <= 15, STAND_3_Age_0_15,
                             VRI_AGE_CL_STD <= 30, STAND_3_Age_16_30,
                             VRI_AGE_CL_STD <= 50, STAND_3_Age_31_50,
                             VRI_AGE_CL_STD <= 80, STAND_3_Age_51_80,
                             VRI_AGE_CL_STD > 80, STAND_3_Age_gt_80,
                             default = NA_character_)]

  ifc[, STS_AGE_3 := fcase(VRI_AGE_CL_STS <= 3, STS_3_Age_0_3,
                           VRI_AGE_CL_STS <= 10, STS_3_Age_4_10,
                           VRI_AGE_CL_STS <= 30, STS_3_Age_11_30,
                           VRI_AGE_CL_STS <= 40, STS_3_Age_31_40,
                           VRI_AGE_CL_STS <= 60, STS_3_Age_41_60,
                           VRI_AGE_CL_STS <= 80, STS_3_Age_61_80,
                           VRI_AGE_CL_STS <= 139, STS_3_Age_81_139,
                           VRI_AGE_CL_STS <= 249, STS_3_Age_140_249,
                           VRI_AGE_CL_STS > 249, STS_3_Age_gt_249,
                           default = NA_character_)]


  ifc[, STRCT_S2 := fcase(VRI_AGE_CL_STS > 0, STS_AGE_3,
                          FORESTED_3 == "N" | parkland_ind , STS_CLIMAX_3,
                          default = NA_character_)]

  # TODO validate what format is are the struct_age_ and stand_Age variables in the csv (number or text)
  ifc[substr(STRCT_S3, start = 1, stop = 1) %in% c("4", "5", "6", "7") ,
      STAND_A3 := fcase(STD_VRI != "", STD_VRI,
                        VRI_AGE_CL_STD > 0, as.character(STAND_AGE_3),
                        FORESTED_3 == "N" | parkland_ind , as.character(STAND_CLIMAX_3), #TODO remove as.char ?
                        default = "")]

  #TODO temporarily change class
  ifc[is.na(STRCT_S3), STRCT_S3 := ""]
  ifc[is.na(STAND_A3), STAND_A3 := ""]




  attr(ifc, "class") <- classes_ifc

  return(ifc)

}
