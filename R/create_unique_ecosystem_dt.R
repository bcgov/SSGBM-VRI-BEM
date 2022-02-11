#' Create summary data of unique ecosystem
#'
#' Generates a data of unique BGC label and habitat combinations for the Look up Table starting point
#'
#' @param vri_bem sf object that represent BEM (broad ecosystem mapping) features
#' @return data.table that contains the frequency of each unique ecosystem and generates the following empty column to be feed later on:
#'
#'   * Forested (Y/N),
#'   * Strict_Climax,
#'   * Stand_Climax,
#'   * Stand_Age_0-15, Stand_Age_16-30, Stand_Age_31-50, Stand_Age_51-80, Stand_Age_80+,
#'   * Struct_Age_0-3, Struct_Age_4-10, Struct_Age_11-30, Struct_Age_31-40, Struct_Age_41-60, Struct_Age_61-80, Struct_Age_81-139, Struct_Age_140_249, Struct_Age_250+,
#'   * Snow_code
#'
#'
#' @import data.table
#' @export
create_unique_ecosystem_dt <- function(vri_bem) {

  vri_bem <- as.data.table(vri_bem)

  unique_ecosystem_dt <-rbind(vri_bem[SDEC_1 > 0, .N , by = .(BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEU_MC = BEUMC_S1)],
                              vri_bem[SDEC_2 > 0, .N , by = .(BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEU_MC = BEUMC_S2)],
                              vri_bem[SDEC_3 > 0, .N , by = .(BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEU_MC = BEUMC_S3)])[ , .(FREQ = sum(N)) , by = .(BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEU_MC)]

  unique_ecosystem_dt[ , c("REALM", "GROUP", "CLASS", "KIND", "Forested (Y/N)","Strct_Climax","Stand_Climax",
                           "Stand_Age_0-15","Stand_Age_16-30","Stand_Age_31-50","Stand_Age_51-80","Stand_Age_80+",
                           "Struct_Age_0-3","Struct_Age_4-10","Struct_Age_11-30","Struct_Age_31-40","Struct_Age_41-60",
                           "Struct_Age_61-80","Struct_Age_81-139","Struct_Age_140-249","Struct_Age_250+","Snow_Code") := NA_character_]

  return(unique_ecosystem_dt)
}

