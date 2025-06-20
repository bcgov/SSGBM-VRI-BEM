#' Assign values from unique ecosystems table
#'
#' Populates the TEIS fields, REALM, GROUP, CLASS, and KIND as well as SNOW_CODE from the data provided by the unique ecosystem data table.
#' This is performed for each of the 3 components ("Deciles") and will create a variable for each of those component with the exception of the snow code.
#'
#' @param vri_bem sf object representing VRI-BEM
#' @param unique_ecosystem_dt data.table of the containing information for each unique ecosystems
#' @return VRI-BEM with the following information from unique ecosystem data:
#'
#'  * REALM
#'  * GROUP
#'  * KIND
#'  * SNOW_CODE
#'  * FORESTED
#'  * STS_CLIMAX
#'  * STAND
#'
#' @import data.table
#' @export

merge_unique_ecosystem_fields <- function(vri_bem, unique_ecosystem_dt) {

  if (FALSE) {
    .<-BEU_MC<-BGC_PHASE<-BGC_SUBZON<-BGC_VRT<-BGC_ZONE<-FORESTED_1<-FORESTED_2<-FORESTED_3<-
      i.CLASS<-`i.Forested (Y/N)`<-i.GROUP<-i.KIND<-i.REALM<-i.Snow_Code<-`i.Stand_Age_0-15`<-
      `i.Stand_Age_16-30`<-`i.Stand_Age_31-50`<-`i.Stand_Age_51-80`<-`i.Stand_Age_80+`<-
      i.Stand_Climax<-i.Strct_Climax<-`i.Struct_Age_0-3`<-`i.Struct_Age_11-30`<-
      `i.Struct_Age_140-249`<-i.Struct_Age_25<-`i.Struct_Age_250+`<-`i.Struct_Age_31-40`<-
      `i.Struct_Age_4-10`<-`i.Struct_Age_41-60`<-`i.Struct_Age_61-80`<-`i.Struct_Age_81-139`<-
      parkland_ind<-STAND_1_Age_0_15<-STAND_1_Age_16_30<-STAND_1_Age_31_50<-STAND_1_Age_51_80<-
      STAND_1_Age_gt_80<-STAND_2_Age_0_15<-STAND_2_Age_16_30<-STAND_2_Age_31_50<-STAND_2_Age_51_80<-
      STAND_2_Age_gt_80<-STAND_3_Age_0_15<-STAND_3_Age_16_30<-STAND_3_Age_31_50<-STAND_3_Age_51_80<-
      STAND_3_Age_gt_80<-STAND_A1<-STAND_A2<-STAND_A3<-STAND_AGE_1<-STAND_AGE_2<-STAND_AGE_3<-
      STAND_CLIMAX_1<-STAND_CLIMAX_2<-STAND_CLIMAX_3<-STD_VRI<-STRCT_S1<-STRCT_S2<-STRCT_S3<-
      STS_1_Age_0_3<-STS_1_Age_11_30<-STS_1_Age_140_249<-STS_1_Age_31_40<-STS_1_Age_4_10<-
      STS_1_Age_41_60<-STS_1_Age_61_80<-STS_1_Age_81_139<-STS_1_Age_gt_249<-STS_2_Age_0_3<-
      STS_2_Age_11_30<-STS_2_Age_140_249<-STS_2_Age_31_40<-STS_2_Age_4_10<-STS_2_Age_41_60<-
      STS_2_Age_61_80<-STS_2_Age_81_139<-STS_2_Age_gt_249<-STS_3_Age_0_3<-STS_3_Age_11_30<-
      STS_3_Age_140_249<-STS_3_Age_31_40<-STS_3_Age_4_10<-STS_3_Age_41_60<-STS_3_Age_61_80<-
      STS_3_Age_81_139<-STS_3_Age_gt_249<-STS_AGE_1<-STS_AGE_2<-STS_AGE_3<-STS_CLIMAX_1<-
      STS_CLIMAX_2<-STS_CLIMAX_3<-VRI_AGE_CL_STD<-VRI_AGE_CL_STS<-NULL
  }

  # use data table for fast data manipulation and fast merges
  classes_vri_bem <- attr(vri_bem, "class")
  setDT(vri_bem)

  # merge REALM GROUP CLASS AND KIND for broad ecosystem unit major class 1

  # We also merge the SNOW, it doesn't depend on the BEUMC and we assume that all the lines in the ecosystem table for a given BEUMC have the same snow code


  # TODO check if we can rename the non valid name , since we create the csv ourselves it should not be a problem

  vri_bem[unique_ecosystem_dt,
          on = .(BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC_S1 = BEU_MC) ,
          `:=`(REALM_1 = i.REALM,
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

  vri_bem[unique_ecosystem_dt,
          on = .(BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC_S2 = BEU_MC) ,
          `:=`(REALM_2 = i.REALM,
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

  vri_bem[unique_ecosystem_dt,
          on = .(BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC_S3 = BEU_MC) ,
          `:=`(REALM_3 = i.REALM,
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



  # add std_crown fields ----
  vri_bem <- add_std_crown_fields(vri_bem)

  vri_bem[ , parkland_ind := substr(BGC_SUBZON, start = length(BGC_SUBZON), stop = length(BGC_SUBZON)) == "p"]

  # calculate for 1 ----

  vri_bem[, STAND_AGE_1 := fcase(VRI_AGE_CL_STD <= 15, STAND_1_Age_0_15,
                                 VRI_AGE_CL_STD <= 30, STAND_1_Age_16_30,
                                 VRI_AGE_CL_STD <= 50, STAND_1_Age_31_50,
                                 VRI_AGE_CL_STD <= 80, STAND_1_Age_51_80,
                                 VRI_AGE_CL_STD > 80, STAND_1_Age_gt_80,
                                 default = NA_character_)]

  vri_bem[, STS_AGE_1 := fcase(VRI_AGE_CL_STS <= 3, STS_1_Age_0_3,
                               VRI_AGE_CL_STS <= 10, STS_1_Age_4_10,
                               VRI_AGE_CL_STS <= 30, STS_1_Age_11_30,
                               VRI_AGE_CL_STS <= 40, STS_1_Age_31_40,
                               VRI_AGE_CL_STS <= 60, STS_1_Age_41_60,
                               VRI_AGE_CL_STS <= 80, STS_1_Age_61_80,
                               VRI_AGE_CL_STS <= 139, STS_1_Age_81_139,
                               VRI_AGE_CL_STS <= 249, STS_1_Age_140_249,
                               VRI_AGE_CL_STS > 249, STS_1_Age_gt_249,
                               default = NA_character_)]


  vri_bem[, STRCT_S1 := fcase(VRI_AGE_CL_STS > 0, STS_AGE_1,
                              FORESTED_1 == "N" | parkland_ind, STS_CLIMAX_1,
                              default = NA_character_)]

    #Correct STRCT for shrub wetlands
  vri_bem[, STRCT_S1 := fcase(BEUMC_S1 == "WL" & BCLCS_LV_2 != "W" & BCLCS_LV_3 == "W" & BCLCS_LV_4 %in% c("HE","HF","HG"),"2",
                            default = STRCT_S1)]


  # TODO validate what format is are the struct_age_ and stand_Age variables in the csv (number or text)
  vri_bem[substr(STRCT_S1, start = 1, stop = 1) %in% c("4", "5", "6", "7","7a","7b") , #added 7a, b
          STAND_A1 := fcase(!is.na(STD_VRI), STD_VRI,
                            VRI_AGE_CL_STD > 0, STAND_AGE_1,
                            FORESTED_1 == "N" | parkland_ind , STAND_CLIMAX_1,
                            default = NA)]
# Correct stand vars in STRCT_S1 < 4
  vri_bem[substr(STRCT_S1, start = 1, stop = 1) %in% c(NA,"1", "1a", "1b", "1c", "2", "2a", "2b", "2c", "2d", "3", "3a", "3b", "3c"),
          STAND_A1 := fcase(VRI_AGE_CL_STD > 0, STAND_AGE_1,
                            FORESTED_1 == "N" | parkland_ind , STAND_CLIMAX_1,
                            default = NA)]

  #TODO temporarily change class
  vri_bem[is.na(STRCT_S1), STRCT_S1 := NA_character_]
  vri_bem[is.na(STAND_A1), STAND_A1 := NA_character_]


  # calculate for 2 ----

  vri_bem[, STAND_AGE_2 := fcase(VRI_AGE_CL_STD <= 15, STAND_2_Age_0_15,
                                 VRI_AGE_CL_STD <= 30, STAND_2_Age_16_30,
                                 VRI_AGE_CL_STD <= 50, STAND_2_Age_31_50,
                                 VRI_AGE_CL_STD <= 80, STAND_2_Age_51_80,
                                 VRI_AGE_CL_STD > 80, STAND_2_Age_gt_80,
                                 default = NA_character_)]

  vri_bem[, STS_AGE_2 := fcase(VRI_AGE_CL_STS <= 3, STS_2_Age_0_3,
                               VRI_AGE_CL_STS <= 10, STS_2_Age_4_10,
                               VRI_AGE_CL_STS <= 30, STS_2_Age_11_30,
                               VRI_AGE_CL_STS <= 40, STS_2_Age_31_40,
                               VRI_AGE_CL_STS <= 60, STS_2_Age_41_60,
                               VRI_AGE_CL_STS <= 80, STS_2_Age_61_80,
                               VRI_AGE_CL_STS <= 139, STS_2_Age_81_139,
                               VRI_AGE_CL_STS <= 249, STS_2_Age_140_249,
                               VRI_AGE_CL_STS > 249, STS_2_Age_gt_249,
                               default = NA_character_)]


  vri_bem[, STRCT_S2 := fcase(VRI_AGE_CL_STS > 0, STS_AGE_2,
                              FORESTED_2 == "N" | parkland_ind , STS_CLIMAX_2,
                              default = NA_character_)]

  # TODO validate what format is are the struct_age_ and stand_Age variables in the csv (number or text)
  vri_bem[substr(STRCT_S2, start = 1, stop = 1) %in% c("4", "5", "6", "7","7a","7b") , #added 7a, b
          STAND_A2 := fcase(!is.na(STD_VRI), STD_VRI,
                            VRI_AGE_CL_STD > 0, STAND_AGE_2,
                            FORESTED_2 == "N" | parkland_ind , STAND_CLIMAX_2,
                            default = NA_character_)]
  # Correct stand vars in STRCT_S2 < 4
  vri_bem[substr(STRCT_S2, start = 1, stop = 1) %in% c(NA,"1", "1a", "1b", "1c", "2", "2a", "2b", "2c", "2d", "3", "3a", "3b", "3c"),
          STAND_A2 := fcase(VRI_AGE_CL_STD > 0, STAND_AGE_2,
                            FORESTED_2 == "N" | parkland_ind , STAND_CLIMAX_2,
                            default = NA_character_)]

  #TODO temporarily change class
  vri_bem[is.na(STRCT_S2), STRCT_S2 := NA_character_]
  vri_bem[is.na(STAND_A2), STAND_A2 := NA_character_]


  # calculate for 3 ----

  vri_bem[, STAND_AGE_3 := fcase(VRI_AGE_CL_STD <= 15, STAND_3_Age_0_15,
                                 VRI_AGE_CL_STD <= 30, STAND_3_Age_16_30,
                                 VRI_AGE_CL_STD <= 50, STAND_3_Age_31_50,
                                 VRI_AGE_CL_STD <= 80, STAND_3_Age_51_80,
                                 VRI_AGE_CL_STD > 80, STAND_3_Age_gt_80,
                                 default = NA_character_)]

  vri_bem[, STS_AGE_3 := fcase(VRI_AGE_CL_STS <= 3, STS_3_Age_0_3,
                               VRI_AGE_CL_STS <= 10, STS_3_Age_4_10,
                               VRI_AGE_CL_STS <= 30, STS_3_Age_11_30,
                               VRI_AGE_CL_STS <= 40, STS_3_Age_31_40,
                               VRI_AGE_CL_STS <= 60, STS_3_Age_41_60,
                               VRI_AGE_CL_STS <= 80, STS_3_Age_61_80,
                               VRI_AGE_CL_STS <= 139, STS_3_Age_81_139,
                               VRI_AGE_CL_STS <= 249, STS_3_Age_140_249,
                               VRI_AGE_CL_STS > 249, STS_3_Age_gt_249,
                               default = NA_character_)]


  vri_bem[, STRCT_S3 := fcase(VRI_AGE_CL_STS > 0, STS_AGE_3, #should be STRCT_S3, not STRCT_S2
                              FORESTED_3 == "N" | parkland_ind , STS_CLIMAX_3,
                              default = NA_character_)]

  # TODO validate what format is are the struct_age_ and stand_Age variables in the csv (number or text)
  vri_bem[substr(STRCT_S3, start = 1, stop = 1) %in% c("4", "5", "6", "7","7a","7b") , #added 7a, b
          STAND_A3 := fcase(!is.na(STD_VRI), STD_VRI,
                            VRI_AGE_CL_STD > 0, STAND_AGE_3,
                            FORESTED_3 == "N" | parkland_ind, STAND_CLIMAX_3, #TODO remove as.char ?
                            default = NA_character_)]

  #Correct stand vars in strct_s3 < 4
  vri_bem[substr(STRCT_S3, start = 1, stop = 1) %in% c(NA,"1", "1a", "1b", "1c", "2", "2a", "2b", "2c", "2d", "3", "3a", "3b", "3c"),
          STAND_A3 := fcase(VRI_AGE_CL_STD > 0, STAND_AGE_3,
                            FORESTED_3 == "N" | parkland_ind, STAND_CLIMAX_3,
                            default = NA_character_)]

  vri_bem[is.na(STRCT_S3), STRCT_S3 := NA_character_]
  vri_bem[is.na(STAND_A3), STAND_A3 := NA_character_]

  attr(vri_bem, "class") <- classes_vri_bem

  return(vri_bem)

}
