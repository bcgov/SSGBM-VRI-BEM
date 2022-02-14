#' Create summary data of unique ecosystem
#'
#' Generates a data of unique BGC label and habitat combinations for the Look up Table starting point
#'
#' @param vri_bem_dt data.table object that represent the vri bem
#' @return data.table that contains the frequency of each unique ecosystem
#' @import data.table
#' @export
summarize_unique_ecosystem <- function(vri_bem_dt) {
  rbind(vri_bem_dt[SDEC_1 > 0, .N , by = .(BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEU_MC = BEUMC_S1)],
        vri_bem_dt[SDEC_2 > 0, .N , by = .(BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEU_MC = BEUMC_S2)],
        vri_bem_dt[SDEC_3 > 0, .N , by = .(BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEU_MC = BEUMC_S3)])[ , .(FREQ = sum(N)) , by = .(BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEU_MC)]
}
