create_RRM_ecosystem <- function(bfc){

  # TODO: switch to a data.table?
  bem_dt <- as.data.table(bfc)



  summ_area <- rbind(
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = sum(Shape_Area*SDEC_1/10)),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_1, STRCT = STRCT_S1, STAND = STAND_A1, FORESTED = FORESTED_1)],

    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = sum(Shape_Area*SDEC_2/10)),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_2, STRCT = STRCT_S2, STAND = STAND_A2, FORESTED = FORESTED_2)],

    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = sum(Shape_Area*SDEC_3/10)),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_3, STRCT = STRCT_S3, STAND = STAND_A3, FORESTED = FORESTED_3)],


    #3yrs
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = 0),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_1, STRCT = STS_1_Age_0_3, STAND = STAND_1_Age_0_15, FORESTED = FORESTED_1)],
    #10yrs
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = 0),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_1, STRCT = STS_1_Age_4_10, STAND = STAND_1_Age_0_15, FORESTED = FORESTED_1)],
    #15 yrs
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = 0),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_1, STRCT = STS_1_Age_11_30, STAND = STAND_1_Age_0_15, FORESTED = FORESTED_1)],
    #30 yrs
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = 0),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_1, STRCT = STS_1_Age_11_30, STAND = STAND_1_Age_16_30, FORESTED = FORESTED_1)],
    #40 yrs
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = 0),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_1, STRCT = STS_1_Age_31_40, STAND = STAND_1_Age_31_50, FORESTED = FORESTED_1)],
    #50 yrs
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = 0),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_1, STRCT = STS_1_Age_41_60, STAND = STAND_1_Age_31_50, FORESTED = FORESTED_1)],
    #60 yrs
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = 0),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_1, STRCT = STS_1_Age_41_60, STAND = STAND_1_Age_51_80, FORESTED = FORESTED_1)],
    #80 yrs
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = 0),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_1, STRCT = STS_1_Age_61_80, STAND = STAND_1_Age_51_80, FORESTED = FORESTED_1)],
    #140 yrs (looks like 139)
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = 0),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_1, STRCT = STS_1_Age_81_139, STAND = STAND_1_Age_gt_80, FORESTED = FORESTED_1)],
    #249 yrs
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = 0),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_1, STRCT = STS_1_Age_140_249, STAND = STAND_1_Age_gt_80, FORESTED = FORESTED_1)],
    #9999 yrs
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = 0),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_1, STRCT = STS_1_Age_gt_249, STAND = STAND_1_Age_gt_80, FORESTED = FORESTED_1)],





    #3yrs
    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = 0),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_2, STRCT = STS_2_Age_0_3, STAND = STAND_2_Age_0_15, FORESTED = FORESTED_2)],
    #10yrs
    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = 0),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_2, STRCT = STS_2_Age_4_10, STAND = STAND_2_Age_0_15, FORESTED = FORESTED_2)],
    #15 yrs
    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = 0),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_2, STRCT = STS_2_Age_11_30, STAND = STAND_2_Age_0_15, FORESTED = FORESTED_2)],
    #30 yrs
    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = 0),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_2, STRCT = STS_2_Age_11_30, STAND = STAND_2_Age_16_30, FORESTED = FORESTED_2)],
    #40 yrs
    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = 0),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_2, STRCT = STS_2_Age_31_40, STAND = STAND_2_Age_31_50, FORESTED = FORESTED_2)],
    #50 yrs
    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = 0),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_2, STRCT = STS_2_Age_41_60, STAND = STAND_2_Age_31_50, FORESTED = FORESTED_2)],
    #60 yrs
    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = 0),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_2, STRCT = STS_2_Age_41_60, STAND = STAND_2_Age_51_80, FORESTED = FORESTED_2)],
    #80 yrs
    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = 0),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_2, STRCT = STS_2_Age_61_80, STAND = STAND_2_Age_51_80, FORESTED = FORESTED_2)],
    #140 yrs (looks like 139)
    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = 0),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_2, STRCT = STS_2_Age_81_139, STAND = STAND_2_Age_gt_80, FORESTED = FORESTED_2)],
    #249 yrs
    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = 0),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_2, STRCT = STS_2_Age_140_249, STAND = STAND_2_Age_gt_80, FORESTED = FORESTED_2)],
    #9999 yrs
    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = 0),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_2, STRCT = STS_2_Age_gt_249, STAND = STAND_2_Age_gt_80, FORESTED = FORESTED_2)],



    #3yrs
    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = 0),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_3, STRCT = STS_3_Age_0_3, STAND = STAND_3_Age_0_15, FORESTED = FORESTED_3)],
    #10yrs
    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = 0),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_3, STRCT = STS_3_Age_4_10, STAND = STAND_3_Age_0_15, FORESTED = FORESTED_3)],
    #15 yrs
    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = 0),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_3, STRCT = STS_3_Age_11_30, STAND = STAND_3_Age_0_15, FORESTED = FORESTED_3)],
    #30 yrs
    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = 0),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_3, STRCT = STS_3_Age_11_30, STAND = STAND_3_Age_16_30, FORESTED = FORESTED_3)],
    #40 yrs
    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = 0),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_3, STRCT = STS_3_Age_31_40, STAND = STAND_3_Age_31_50, FORESTED = FORESTED_3)],
    #50 yrs
    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = 0),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_3, STRCT = STS_3_Age_41_60, STAND = STAND_3_Age_31_50, FORESTED = FORESTED_3)],
    #60 yrs
    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = 0),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_3, STRCT = STS_3_Age_41_60, STAND = STAND_3_Age_51_80, FORESTED = FORESTED_3)],
    #80 yrs
    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = 0),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_3, STRCT = STS_3_Age_61_80, STAND = STAND_3_Age_51_80, FORESTED = FORESTED_3)],
    #140 yrs (looks like 139)
    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = 0),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_3, STRCT = STS_3_Age_81_139, STAND = STAND_3_Age_gt_80, FORESTED = FORESTED_3)],
    #249 yrs
    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = 0),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_3, STRCT = STS_3_Age_140_249, STAND = STAND_3_Age_gt_80, FORESTED = FORESTED_3)],
    #9999 yrs
    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = 0),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE = CROWN_MOOSE_3, STRCT = STS_3_Age_gt_249, STAND = STAND_3_Age_gt_80, FORESTED = FORESTED_3)]
    )

  summ_area[STAND %in% c("B", "C", "M") & !STRCT %in% c("4", "5", "6", "7"), STAND := ""]

  summ_area[, .(Hectares = sum(area_sum)),
            by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC, SLOPE_MOD, SITE_M3A,
                      SNOW_CODE, ABOVE_ELEV, CROWN_MOOSE, STRCT, STAND, FORESTED)]



}
