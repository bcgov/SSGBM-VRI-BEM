#' This script takes an input VRI feature class and adds two new field, STD_VRI,
#' CROWN_BEAR and CROWN_MOOSE. It populates the STD_VRI field "C", "B" or "M" based
#' on the existing values in the SPECIES_CD_# and SPECIES_PCT_# fields, and populates
#' the CROWN_BEAR field with codes 1 to 4, and CROWN_MOOSE with codes H, M, L, VL and N
#' based on the existing values in the CROWN_CLOSURE field.
#'
#' @param vri sf object that represent VRI (vegetation ressource inventory) features
#' @return sf object
#' @import data.table
#' @export
add_std_crown_fields <- function(vri) {

  # use data.table for fast data manipulation
  classes_vri <- attr(vri, "class")
  setDT(vri)

  # compute total percentage of species in list

  b_species <- c("D", "DR", "U", "UP", "A", "AC", "ACB", "ACT", "AX", "AT", "R", "RA", "E", "EA", "EXP", "EP", "EW",
                 "GP", "MB", "MB", "MV", "Q", "QG", "XH", "V", "VB", "VP", "W", "WS", "WA", "WB", "WD", "WP", "WT")

  # maybe find a better variable name if we can figure out what b_species are

  vri[ , pct_in_species_1 := (SPECIES_CD_1 %in% b_species) * SPECIES_PCT_1]
  vri[ , pct_in_species_2 := (SPECIES_CD_2 %in% b_species) * SPECIES_PCT_2]
  vri[ , pct_in_species_3 := (SPECIES_CD_3 %in% b_species) * SPECIES_PCT_3]
  vri[ , pct_in_species_4 := (SPECIES_CD_4 %in% b_species) * SPECIES_PCT_4]
  vri[ , pct_in_species_5 := (SPECIES_CD_5 %in% b_species) * SPECIES_PCT_5]
  vri[ , pct_in_species_6 := (SPECIES_CD_6 %in% b_species) * SPECIES_PCT_6]

  vri[ , pct_in_species_tot := rowSums(.SD, na.rm = T), .SDcols = paste0("pct_in_species_", 1:6)]

  # create STD_VRI based on percentage

  vri[ , STD_VRI := fcase(pct_in_species_tot == 0, "",
                          pct_in_species_tot < 25, "C",
                          pct_in_species_tot < 75, "M",
                          default = "B")]

  # create CROWN_BEAR based on CROWN_CLOSURE

  vri[ , CROWN_BEAR := fcase(CROWN_CLOSURE <= 20, 1,
                             CROWN_CLOSURE <= 40, 2,
                             CROWN_CLOSURE <= 60, 3,
                             CROWN_CLOSURE > 60, 4,
                             default = NA)]

  # create CROWN_MOOSE based on CROWN_CLOSURE

  vri[ , CROWN_MOOSE := fcase(CROWN_CLOSURE == 0, "N",
                              CROWN_CLOSURE <= 9, "VL",
                              CROWN_CLOSURE <= 25, "L",
                              CROWN_CLOSURE <= 60, "M",
                              CROWN_CLOSURE > 60, "H",
                              default = "")]


  attr(vri, "class") <- classes_vri

  return(vri)
}


