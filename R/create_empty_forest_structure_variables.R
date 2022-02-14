#' Create empty variables for structure of forest
#'
#' Generates a data of unique BGC label and habitat combinations for the Look up Table starting point
#'
#' @param unique_ecosystem_dt data.table of the containing information for each unique ecosystems
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
create_empty_forest_structure_variables <- function(unique_ecosystem_dt) {
  unique_ecosystem_dt[ , c("REALM", "GROUP", "CLASS", "KIND", "Forested (Y/N)","Strct_Climax","Stand_Climax",
                           "Stand_Age_0-15","Stand_Age_16-30","Stand_Age_31-50","Stand_Age_51-80","Stand_Age_80+",
                           "Struct_Age_0-3","Struct_Age_4-10","Struct_Age_11-30","Struct_Age_31-40","Struct_Age_41-60",
                           "Struct_Age_61-80","Struct_Age_81-139","Struct_Age_140-249","Struct_Age_250+","Snow_Code") := NA_character_]
}
