#' Create summary data of unique ecosystem
#'
#' Generates a data of unique BGC label and habitat combinations for the Look up Table starting point
#'
#' @param vri_bem sf object that represent BEM (broad ecosystem mapping) features
#' @param current_unique_ecosystem_csv string, file path of current unique ecosystem csv. Defaults to NULL
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
create_unique_ecosystem_dt <- function(vri_bem, current_unique_ecosyteme_csv = NULL) {

  vri_bem <- as.data.table(vri_bem)

  unique_ecosystem_dt <- summarize_unique_ecosystem(vri_bem)

  if (!is.null(current_unique_ecosyteme_csv)) {
    current_unique_ecosyteme_dt <- fread(current_unique_ecosyteme_csv)
    format_unique_ecosystem_dt(current_unique_ecosyteme_dt)
    unique_ecosystem_dt[, merge_ind := 1]
    current_unique_ecosyteme_dt[, merge_ind := 1]
    new_unique_ecosystem_dt <- merge(current_unique_ecosyteme_dt, unique_ecosystem_dt, by = c("BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE", "BEU_MC"), all = TRUE)
    nbr_missing <- sum(is.na(new_unique_ecosystem_dt$merge_ind.x) & new_unique_ecosystem_dt$merge_ind.y == 1)
    if (nbr_missing > 0) {
      warning(paste0("Some ecoystem are not present in the current unique ecosystem csv. \n Use fwrite to export the result of this function to csv and fill in the missing values \n line 1 to ", nbr_missing, " need to be filled in"))
      new_unique_ecosystem_dt[, merge_ind.x := NULL]
      new_unique_ecosystem_dt[, FREQ := fifelse(!is.na(FREQ.y), FREQ.y, FREQ.x)]
      new_unique_ecosystem_dt[, FREQ.x := NULL]
      new_unique_ecosystem_dt[, FREQ.y := NULL]
      return(new_unique_ecosystem_dt[order(merge_ind.y)][ , merge_ind.y := NULL])
    } else {
     message("current unique ecosystem csv covers all case")
      new_unique_ecosystem_dt[, merge_ind.y := NULL]
      new_unique_ecosystem_dt[, merge_ind.x := NULL]
      new_unique_ecosystem_dt[, FREQ.x := NULL]
      new_unique_ecosystem_dt[, FREQ.y := NULL]
      return(new_unique_ecosystem_dt)
    }
  } else {
    return(create_empty_forest_structure_variables(unique_ecosystem_dt))
  }
}

