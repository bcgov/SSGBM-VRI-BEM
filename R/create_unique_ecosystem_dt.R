#' generates a data of unique BGC label and habitat combinations for the Look up Table starting point
#'
#' @param bem sf object that represent BEM (broad ecosystem mapping) features
#' @return data.table
#' @import data.table
create_unique_ecosytem_dt <- function(bem) {

  bem <- as.data.table(bem)

  unique_ecosystem_dt <-rbind(bem[SDEC_1 > 0, .N , by = .(BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1)],
                              bem[SDEC_2 > 0, .N , by = .(BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2)],
                              bem[SDEC_3 > 0, .N , by = .(BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3)])[ , .(sum(N)) , by = .(BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC)]

  return(unique_ecosystem_dt)
}

