summarize_unique_ecosystem <- function(vri_bem_dt) {
  rbind(vri_bem_dt[SDEC_1 > 0, .N , by = .(BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEU_MC = BEUMC_S1)],
        vri_bem_dt[SDEC_2 > 0, .N , by = .(BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEU_MC = BEUMC_S2)],
        vri_bem_dt[SDEC_3 > 0, .N , by = .(BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEU_MC = BEUMC_S3)])[ , .(FREQ = sum(N)) , by = .(BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEU_MC)]
}
