#' Format unique ecosystem data
#'
#' Format the unique ecosystem data, to make sure that any conversion that happened when reading the csv did not affect the expected class of each fields
#'
#' @param unique_ecosystem_dt data.table object that represent the unique ecosystems
#' @return data.table
#' @import data.table
#' @export
format_unique_ecosystem_dt <- function(unique_ecosystem_dt) {

  unique_ecosystem_dt[is.na(BGC_VRT), BGC_VRT := 0]
  unique_ecosystem_dt[BGC_PHASE == "", BGC_PHASE := NA_character_]

}
