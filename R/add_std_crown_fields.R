#' Assign STD_VRI, CROWN_BEAR and CROWN_MOOSE
#'
#' This script takes an input VRI feature class and adds two new field, STD_VRI,
#' CROWN_BEAR and CROWN_MOOSE. It populates the STD_VRI field "C", "B" or "M" based
#' on the existing values in the SPECIES_CD_# and SPECIES_PCT_# fields, and populates
#' the CROWN_BEAR field with codes 1 to 4, and CROWN_MOOSE with codes H, M, L, VL and N
#' based on the existing values in the CROWN_CLOSURE field.
#'
#' @param vri data.table object that represent VRI (vegetation resources inventory) features
#' @return sf object
#' @import data.table
#' @export
add_std_crown_fields <- function(vri) {



  # compute total percentage of species in list

  b_species <- c("D", "DR", "U", "UP", "A", "AC", "ACB", "ACT", "AX", "AT", "R", "RA", "E", "EA", "EXP", "EP", "EW",
                 "GP", "MB", "MB", "MV", "Q", "QG", "XH", "V", "VB", "VP", "W", "WS", "WA", "WB", "WD", "WP", "WT")

  # maybe find a better variable name if we can figure out what b_species are
  #TODO : make sure that SPEC_PCT_ ... are numeric when loading the VRIs
  vri[ , pct_in_species_1 := (SPEC_CD_1 %in% b_species) * as.numeric(SPEC_PCT_1)]
  vri[ , pct_in_species_2 := (SPEC_CD_2 %in% b_species) * as.numeric(SPEC_PCT_2)]
  vri[ , pct_in_species_3 := (SPEC_CD_3 %in% b_species) * as.numeric(SPEC_PCT_3)]
  vri[ , pct_in_species_4 := (SPEC_CD_4 %in% b_species) * as.numeric(SPEC_PCT_4)]
  vri[ , pct_in_species_5 := (SPEC_CD_5 %in% b_species) * as.numeric(SPEC_PCT_5)]
  vri[ , pct_in_species_6 := (SPEC_CD_6 %in% b_species) * as.numeric(SPEC_PCT_6)]

  vri[ , pct_in_species_tot := rowSums(.SD, na.rm = T), .SDcols = paste0("pct_in_species_", 1:6)]

  # create STD_VRI based on percentage

  vri[ , STD_VRI := fcase(pct_in_species_tot == 0, NA_character_,
                          pct_in_species_tot < 25, "C",
                          pct_in_species_tot < 75, "M",
                          default = "B")]

  #RW edit: Make sure that CR_CLOSURE is numeric
  vri[, CR_CLOSURE := as.numeric(CR_CLOSURE)]

  #Consolidated CROWN_BEAR and CROWN_MOOSE into one category, CROWN_ALL
  vri[ , CROWN_ALL := fcase(CR_CLOSURE <= 25, "VL-L",
                             CR_CLOSURE <= 40, "M",
                             CR_CLOSURE <= 60, "H",
                             CR_CLOSURE > 60, "VH",
                             default = NA)]

  return(vri)
}



