#' Create RRM ecosystem information for moose
#'
#' This function generates the unique list of ecosystems and determines all possible combinations of STS and STAND codes
#' that could apply to the ecosystem at any age value, and expand to include all existing and "potential" unique ecosystems
#'
#' @param vri_bem VRI-BEM feature class
#' @details
#' For each of the following combination compute the sum of the area
#'   * ECO_SEC
#'   * BGC_ZONE
#'   * BGC_SUBZON
#'   * BGC_VRT
#'   * BGC_PHASE
#'   * BEUMC  (BEUMC_S1, BEUMC_S2, BEUMC_S3)
#'   * SLOPE_MOD
#'   * SITE_M3A
#'   * SNOW_CODE
#'   * ABOVE_ELEV_THOLD
#'   * CROWN_ALL (CROWN_ALL_1, CROWN_ALL_2, CROWN_ALL_3) #formerly CROWN_MOOSE
#'   * STRCT (STRCT_S1, STRCT_S2, STRCT_S3)
#'   * STAND (STAND_A1, STAND_A2, STAND_A3)
#'   * FORESTED (FORESTED_1, FORESTED_2, FORESTED_3)
#'
#' For STRCT and STAND every combination is also made with the projected age values.
#'
#' @return Summary of Area by unique ecosystem
#' @import data.table
#' @export

create_RRM_ecosystem <- function(vri_bem){

  if (FALSE) {
    .<-ABOVE_ELEV_THOLD<-area_sum<-BEUMC<-BEUMC_S1<-BEUMC_S2<-BEUMC_S3<-BGC_PHASE<-BGC_SUBZON<-
      BGC_VRT<-BGC_ZONE<-CROWN_ALL<-CROWN_ALL_1<-CROWN_ALL_2<-CROWN_ALL_3<-ECO_SEC<-
      FORESTED<-FORESTED_1<-FORESTED_2<-FORESTED_3<-projection<-SDEC_1<-SDEC_2<-SDEC_3<-Shape_Area<-
      SITE_M3A<-SLOPE_MOD<-SNOW_CODE<-STAND<-STAND_1_Age_0_15<-STAND_1_Age_16_30<-
      STAND_1_Age_31_50<-STAND_1_Age_51_80<-STAND_1_Age_gt_80<-STAND_2_Age_0_15<-STAND_2_Age_16_30<-
      STAND_2_Age_31_50<-STAND_2_Age_51_80<-STAND_2_Age_gt_80<-STAND_3_Age_0_15<-STAND_3_Age_16_30<-
      STAND_3_Age_31_50<-STAND_3_Age_51_80<-STAND_3_Age_gt_80<-STAND_A1<-STAND_A2<-STAND_A3<-STRCT<-
      STRCT_S1<-STRCT_S2<-STRCT_S3<-STS_1_Age_0_3<-STS_1_Age_11_30<-STS_1_Age_140_249<-
      STS_1_Age_31_40<-STS_1_Age_4_10<-STS_1_Age_41_60<-STS_1_Age_61_80<-STS_1_Age_81_139<-
      STS_1_Age_gt_249<-STS_2_Age_0_3<-STS_2_Age_11_30<-STS_2_Age_140_249<-STS_2_Age_31_40<-
      STS_2_Age_4_10<-STS_2_Age_41_60<-STS_2_Age_61_80<-STS_2_Age_81_139<-STS_2_Age_gt_249<-
      STS_3_Age_0_3<-STS_3_Age_11_30<-STS_3_Age_140_249<-STS_3_Age_31_40<-STS_3_Age_4_10<-
      STS_3_Age_41_60<-STS_3_Age_61_80<-STS_3_Age_81_139<-STS_3_Age_gt_249<-NULL
  }

  #Update Shape_Area for accurate measurement of polygon area
  vri_bem <- dplyr::mutate(vri_bem, Shape_Area = as.numeric(sf::st_area(vri_bem)))

  bem_dt <- as.data.table(vri_bem)

  summ_area <- rbind(
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = sum(Shape_Area*SDEC_1/10), projection = FALSE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_1, STRCT = STRCT_S1, STAND = STAND_A1, FORESTED = FORESTED_1)],

    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = sum(Shape_Area*SDEC_2/10), projection = FALSE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_2, STRCT = STRCT_S2, STAND = STAND_A2, FORESTED = FORESTED_2)],

    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = sum(Shape_Area*SDEC_3/10), projection = FALSE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_3, STRCT = STRCT_S3, STAND = STAND_A3, FORESTED = FORESTED_3)],


    #3yrs
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_1, STRCT = STS_1_Age_0_3, STAND = STAND_1_Age_0_15, FORESTED = FORESTED_1)],
    #10yrs
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_1, STRCT = STS_1_Age_4_10, STAND = STAND_1_Age_0_15, FORESTED = FORESTED_1)],
    #15 yrs
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_1, STRCT = STS_1_Age_11_30, STAND = STAND_1_Age_0_15, FORESTED = FORESTED_1)],
    #30 yrs
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_1, STRCT = STS_1_Age_11_30, STAND = STAND_1_Age_16_30, FORESTED = FORESTED_1)],
    #40 yrs
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_1, STRCT = STS_1_Age_31_40, STAND = STAND_1_Age_31_50, FORESTED = FORESTED_1)],
    #50 yrs
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_1, STRCT = STS_1_Age_41_60, STAND = STAND_1_Age_31_50, FORESTED = FORESTED_1)],
    #60 yrs
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_1, STRCT = STS_1_Age_41_60, STAND = STAND_1_Age_51_80, FORESTED = FORESTED_1)],
    #80 yrs
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_1, STRCT = STS_1_Age_61_80, STAND = STAND_1_Age_51_80, FORESTED = FORESTED_1)],
    #140 yrs (looks like 139)
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_1, STRCT = STS_1_Age_81_139, STAND = STAND_1_Age_gt_80, FORESTED = FORESTED_1)],
    #249 yrs
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_1, STRCT = STS_1_Age_140_249, STAND = STAND_1_Age_gt_80, FORESTED = FORESTED_1)],
    #9999 yrs
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_1, STRCT = STS_1_Age_gt_249, STAND = STAND_1_Age_gt_80, FORESTED = FORESTED_1)],





    #3yrs
    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_2, STRCT = STS_2_Age_0_3, STAND = STAND_2_Age_0_15, FORESTED = FORESTED_2)],
    #10yrs
    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_2, STRCT = STS_2_Age_4_10, STAND = STAND_2_Age_0_15, FORESTED = FORESTED_2)],
    #15 yrs
    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_2, STRCT = STS_2_Age_11_30, STAND = STAND_2_Age_0_15, FORESTED = FORESTED_2)],
    #30 yrs
    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_2, STRCT = STS_2_Age_11_30, STAND = STAND_2_Age_16_30, FORESTED = FORESTED_2)],
    #40 yrs
    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_2, STRCT = STS_2_Age_31_40, STAND = STAND_2_Age_31_50, FORESTED = FORESTED_2)],
    #50 yrs
    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_2, STRCT = STS_2_Age_41_60, STAND = STAND_2_Age_31_50, FORESTED = FORESTED_2)],
    #60 yrs
    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_2, STRCT = STS_2_Age_41_60, STAND = STAND_2_Age_51_80, FORESTED = FORESTED_2)],
    #80 yrs
    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_2, STRCT = STS_2_Age_61_80, STAND = STAND_2_Age_51_80, FORESTED = FORESTED_2)],
    #140 yrs (looks like 139)
    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_2, STRCT = STS_2_Age_81_139, STAND = STAND_2_Age_gt_80, FORESTED = FORESTED_2)],
    #249 yrs
    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_2, STRCT = STS_2_Age_140_249, STAND = STAND_2_Age_gt_80, FORESTED = FORESTED_2)],
    #9999 yrs
    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_2, STRCT = STS_2_Age_gt_249, STAND = STAND_2_Age_gt_80, FORESTED = FORESTED_2)],



    #3yrs
    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_3, STRCT = STS_3_Age_0_3, STAND = STAND_3_Age_0_15, FORESTED = FORESTED_3)],
    #10yrs
    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_3, STRCT = STS_3_Age_4_10, STAND = STAND_3_Age_0_15, FORESTED = FORESTED_3)],
    #15 yrs
    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_3, STRCT = STS_3_Age_11_30, STAND = STAND_3_Age_0_15, FORESTED = FORESTED_3)],
    #30 yrs
    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_3, STRCT = STS_3_Age_11_30, STAND = STAND_3_Age_16_30, FORESTED = FORESTED_3)],
    #40 yrs
    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_3, STRCT = STS_3_Age_31_40, STAND = STAND_3_Age_31_50, FORESTED = FORESTED_3)],
    #50 yrs
    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_3, STRCT = STS_3_Age_41_60, STAND = STAND_3_Age_31_50, FORESTED = FORESTED_3)],
    #60 yrs
    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_3, STRCT = STS_3_Age_41_60, STAND = STAND_3_Age_51_80, FORESTED = FORESTED_3)],
    #80 yrs
    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_3, STRCT = STS_3_Age_61_80, STAND = STAND_3_Age_51_80, FORESTED = FORESTED_3)],
    #140 yrs (looks like 139)
    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_3, STRCT = STS_3_Age_81_139, STAND = STAND_3_Age_gt_80, FORESTED = FORESTED_3)],
    #249 yrs
    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_3, STRCT = STS_3_Age_140_249, STAND = STAND_3_Age_gt_80, FORESTED = FORESTED_3)],
    #9999 yrs
    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_3, STRCT = STS_3_Age_gt_249, STAND = STAND_3_Age_gt_80, FORESTED = FORESTED_3)]
  )

  summ_area[(projection) & STAND %in% c("B", "C", "M") & !STRCT %in% c("4", "5", "6", "7"), STAND := NA_character_]

  summ_area[, .(Hectares = (sum(area_sum)/10000)),
            by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC, SLOPE_MOD, SITE_M3A,
                      SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL, STRCT, STAND, FORESTED)]

}

#' Create RRM ecosystem information for moose
#'
#' This function generates the unique list of ecosystems and determines all possible combinations of STS and STAND codes
#' that could apply to the ecosystem at any age value, and expand to include all existing and "potential" unique ecosystems
#'
#' @param vri_bem VRI-BEM feature class
#' @details
#' For each of the following combination compute the sum of the area
#'   * ECO_SEC
#'   * BGC_ZONE
#'   * BGC_SUBZON
#'   * BGC_VRT
#'   * BGC_PHASE
#'   * BEUMC  (BEUMC_S1, BEUMC_S2, BEUMC_S3)
#'   * SLOPE_MOD
#'   * SITE_M3A
#'   * SNOW_CODE
#'   * ABOVE_ELEV
#'   * CROWN_ALL (CROWN_ALL_1, CROWN_ALL_2, CROWN_ALL_3)
#'   * STRCT (STRCT_S1, STRCT_S2, STRCT_S3)
#'   * STAND (STAND_A1, STAND_A2, STAND_A3)
#'   * FORESTED (FORESTED_1, FORESTED_2, FORESTED_3)
#'
#' For STRCT and STAND every combination is also made with the projected age values.
#'
#' @return Summary of Area by unique ecosystem
#' @import data.table
#' @export

create_RRM_ecosystem_moose <- function(vri_bem){
  create_RRM_ecosystem(vri_bem = vri_bem)
}


#' Create RRM ecosystem information for bear
#'
#' This function generates the unique list of ecosystems and determines all possible combinations of STS and STAND codes
#' that could apply to the ecosystem at any age value, and expand to include all existing and "potential" unique ecosystems
#'
#' @param vri_bem VRI-BEM feature class
#' @details
#' For each of the following combination compute the sum of the area
#'   * ECO_SEC
#'   * BGC_ZONE
#'   * BGC_SUBZON
#'   * BGC_VRT
#'   * BGC_PHASE
#'   * BEUMC  (BEUMC_S1, BEUMC_S2, BEUMC_S3)
#'   * SLOPE_MOD
#'   * SITE_M3A
#'   * SALMON (added RW 23Nov2022)
#'   * SNOW_CODE
#'   * ABOVE_ELEV
#'   * CROWN_ALL (CROWN_ALL_1, CROWN_ALL_2, CROWN_ALL_3) #formerly CROWN_BEAR
#'   * STRCT (STRCT_S1, STRCT_S2, STRCT_S3)
#'   * STAND (STAND_A1, STAND_A2, STAND_A3)
#'   * FORESTED (FORESTED_1, FORESTED_2, FORESTED_3)
#'
#' For STRCT and STAND every combination is also made with the projected age values.
#'
#' @return Summary of Area by unique ecosystem
#' @import data.table
#' @export

create_RRM_ecosystem_bear <- function(vri_bem){

  if (FALSE) {
    .<-ABOVE_ELEV_THOLD<-area_sum<-BEUMC<-BEUMC_S1<-BEUMC_S2<-BEUMC_S3<-BGC_PHASE<-BGC_SUBZON<-
      BGC_VRT<-BGC_ZONE<-CROWN_ALL<-CROWN_ALL_1<-CROWN_ALL_2<-CROWN_ALL_3<-ECO_SEC<-FORESTED<-
      FORESTED_1<-FORESTED_2<-FORESTED_3<-projection<-Salmon<-SDEC_1<-SDEC_2<-SDEC_3<-Shape_Area<-
      SITE_M3A<-SLOPE_MOD<-SNOW_CODE<-STAND<-STAND_1_Age_0_15<-STAND_1_Age_16_30<-
      STAND_1_Age_31_50<-STAND_1_Age_51_80<-STAND_1_Age_gt_80<-STAND_2_Age_0_15<-STAND_2_Age_16_30<-
      STAND_2_Age_31_50<-STAND_2_Age_51_80<-STAND_2_Age_gt_80<-STAND_3_Age_0_15<-STAND_3_Age_16_30<-
      STAND_3_Age_31_50<-STAND_3_Age_51_80<-STAND_3_Age_gt_80<-STAND_A1<-STAND_A2<-STAND_A3<-STRCT<-
      STRCT_S1<-STRCT_S2<-STRCT_S3<-STS_1_Age_0_3<-STS_1_Age_11_30<-STS_1_Age_140_249<-
      STS_1_Age_31_40<-STS_1_Age_4_10<-STS_1_Age_41_60<-STS_1_Age_61_80<-STS_1_Age_81_139<-
      STS_1_Age_gt_249<-STS_2_Age_0_3<-STS_2_Age_11_30<-STS_2_Age_140_249<-STS_2_Age_31_40<-
      STS_2_Age_4_10<-STS_2_Age_41_60<-STS_2_Age_61_80<-STS_2_Age_81_139<-STS_2_Age_gt_249<-
      STS_3_Age_0_3<-STS_3_Age_11_30<-STS_3_Age_140_249<-STS_3_Age_31_40<-STS_3_Age_4_10<-
      STS_3_Age_41_60<-STS_3_Age_61_80<-STS_3_Age_81_139<-STS_3_Age_gt_249<-NULL
  }

  #Update Shape_Area for accurate measurement of polygon area
  vri_bem <- dplyr::mutate(vri_bem, Shape_Area = as.numeric(sf::st_area(vri_bem)))

  bem_dt <- as.data.table(vri_bem)

  summ_area <- rbind(
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = sum(Shape_Area*SDEC_1/10), projection = FALSE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_1, STRCT = STRCT_S1, STAND = STAND_A1, FORESTED = FORESTED_1)],

    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = sum(Shape_Area*SDEC_2/10), projection = FALSE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_2, STRCT = STRCT_S2, STAND = STAND_A2, FORESTED = FORESTED_2)],

    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = sum(Shape_Area*SDEC_3/10), projection = FALSE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_3, STRCT = STRCT_S3, STAND = STAND_A3, FORESTED = FORESTED_3)],


    #3yrs
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_1, STRCT = STS_1_Age_0_3, STAND = STAND_1_Age_0_15, FORESTED = FORESTED_1)],
    #10yrs
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_1, STRCT = STS_1_Age_4_10, STAND = STAND_1_Age_0_15, FORESTED = FORESTED_1)],
    #15 yrs
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_1, STRCT = STS_1_Age_11_30, STAND = STAND_1_Age_0_15, FORESTED = FORESTED_1)],
    #30 yrs
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_1, STRCT = STS_1_Age_11_30, STAND = STAND_1_Age_16_30, FORESTED = FORESTED_1)],
    #40 yrs
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_1, STRCT = STS_1_Age_31_40, STAND = STAND_1_Age_31_50, FORESTED = FORESTED_1)],
    #50 yrs
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_1, STRCT = STS_1_Age_41_60, STAND = STAND_1_Age_31_50, FORESTED = FORESTED_1)],
    #60 yrs
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_1, STRCT = STS_1_Age_41_60, STAND = STAND_1_Age_51_80, FORESTED = FORESTED_1)],
    #80 yrs
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_1, STRCT = STS_1_Age_61_80, STAND = STAND_1_Age_51_80, FORESTED = FORESTED_1)],
    #140 yrs (looks like 139)
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_1, STRCT = STS_1_Age_81_139, STAND = STAND_1_Age_gt_80, FORESTED = FORESTED_1)],
    #249 yrs
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_1, STRCT = STS_1_Age_140_249, STAND = STAND_1_Age_gt_80, FORESTED = FORESTED_1)],
    #9999 yrs
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_1, STRCT = STS_1_Age_gt_249, STAND = STAND_1_Age_gt_80, FORESTED = FORESTED_1)],





    #3yrs
    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_2, STRCT = STS_2_Age_0_3, STAND = STAND_2_Age_0_15, FORESTED = FORESTED_2)],
    #10yrs
    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_2, STRCT = STS_2_Age_4_10, STAND = STAND_2_Age_0_15, FORESTED = FORESTED_2)],
    #15 yrs
    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_2, STRCT = STS_2_Age_11_30, STAND = STAND_2_Age_0_15, FORESTED = FORESTED_2)],
    #30 yrs
    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_2, STRCT = STS_2_Age_11_30, STAND = STAND_2_Age_16_30, FORESTED = FORESTED_2)],
    #40 yrs
    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_2, STRCT = STS_2_Age_31_40, STAND = STAND_2_Age_31_50, FORESTED = FORESTED_2)],
    #50 yrs
    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_2, STRCT = STS_2_Age_41_60, STAND = STAND_2_Age_31_50, FORESTED = FORESTED_2)],
    #60 yrs
    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_2, STRCT = STS_2_Age_41_60, STAND = STAND_2_Age_51_80, FORESTED = FORESTED_2)],
    #80 yrs
    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_2, STRCT = STS_2_Age_61_80, STAND = STAND_2_Age_51_80, FORESTED = FORESTED_2)],
    #140 yrs (looks like 139)
    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_2, STRCT = STS_2_Age_81_139, STAND = STAND_2_Age_gt_80, FORESTED = FORESTED_2)],
    #249 yrs
    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_2, STRCT = STS_2_Age_140_249, STAND = STAND_2_Age_gt_80, FORESTED = FORESTED_2)],
    #9999 yrs
    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_2, STRCT = STS_2_Age_gt_249, STAND = STAND_2_Age_gt_80, FORESTED = FORESTED_2)],



    #3yrs
    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_3, STRCT = STS_3_Age_0_3, STAND = STAND_3_Age_0_15, FORESTED = FORESTED_3)],
    #10yrs
    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_3, STRCT = STS_3_Age_4_10, STAND = STAND_3_Age_0_15, FORESTED = FORESTED_3)],
    #15 yrs
    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_3, STRCT = STS_3_Age_11_30, STAND = STAND_3_Age_0_15, FORESTED = FORESTED_3)],
    #30 yrs
    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_3, STRCT = STS_3_Age_11_30, STAND = STAND_3_Age_16_30, FORESTED = FORESTED_3)],
    #40 yrs
    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_3, STRCT = STS_3_Age_31_40, STAND = STAND_3_Age_31_50, FORESTED = FORESTED_3)],
    #50 yrs
    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_3, STRCT = STS_3_Age_41_60, STAND = STAND_3_Age_31_50, FORESTED = FORESTED_3)],
    #60 yrs
    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_3, STRCT = STS_3_Age_41_60, STAND = STAND_3_Age_51_80, FORESTED = FORESTED_3)],
    #80 yrs
    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_3, STRCT = STS_3_Age_61_80, STAND = STAND_3_Age_51_80, FORESTED = FORESTED_3)],
    #140 yrs (looks like 139)
    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_3, STRCT = STS_3_Age_81_139, STAND = STAND_3_Age_gt_80, FORESTED = FORESTED_3)],
    #249 yrs
    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_3, STRCT = STS_3_Age_140_249, STAND = STAND_3_Age_gt_80, FORESTED = FORESTED_3)],
    #9999 yrs
    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = 0, projection = TRUE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3, SLOPE_MOD, SITE_M3A,Salmon,
                     SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL = CROWN_ALL_3, STRCT = STS_3_Age_gt_249, STAND = STAND_3_Age_gt_80, FORESTED = FORESTED_3)]
  )

  summ_area[(projection) & STAND %in% c("B", "C", "M") & !STRCT %in% c("4", "5", "6", "7"), STAND := NA_character_]

  summ_area[, .(Hectares = (sum(area_sum)/10000)),
            by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC, SLOPE_MOD, SITE_M3A,Salmon,
                      SNOW_CODE, ABOVE_ELEV_THOLD, CROWN_ALL, STRCT, STAND, FORESTED)]

}

#' Create RRM ecosystem information for huckleberry
#'
#' This function generates a unique list of ecosystems based on select huckleberry habitat characteristics.
#' It only includes existing (not potential) ecosystems
#'
#' @param vri_bem VRI-BEM feature class
#' @details
#' For each of the following combination compute the sum of the area
#'   * ECO_SEC
#'   * BGC_ZONE
#'   * BGC_SUBZON
#'   * BGC_VRT
#'   * BGC_PHASE
#'   * HUCK_ASP
#'   * HUCK_ELEV_Thold
#'   * CROWN_ALL (CROWN_ALL_1, CROWN_ALL_2, CROWN_ALL_3)
#'   * STRCT (STRCT_S1, STRCT_S2, STRCT_S3)
#'   * STAND (STAND_A1, STAND_A2, STAND_A3)
#'   * FORESTED (FORESTED_1, FORESTED_2, FORESTED_3)
#'
#'
#' @return Summary of Area by unique ecosystem
#' @import data.table
#' @export

create_RRM_ecosystem_huckleberry <- function(vri_bem){

  if (FALSE) {
    .<-ABOVE_ELEV_THOLD<-area_sum<-BEUMC<-BEUMC_S1<-BEUMC_S2<-BEUMC_S3<-BGC_PHASE<-BGC_SUBZON<-
      BGC_VRT<-BGC_ZONE<-CROWN_ALL<-CROWN_ALL_1<-CROWN_ALL_2<-CROWN_ALL_3<-ECO_SEC<-FORESTED<-
      FORESTED_1<-FORESTED_2<-FORESTED_3<-projection<-Salmon<-SDEC_1<-SDEC_2<-SDEC_3<-Shape_Area<-
      SITE_M3A<-SLOPE_MOD<-SNOW_CODE<-STAND<-HUCK_ASP<-HUCK_ELEV_Thold<-NULL
  }

  #Update Shape_Area for accurate measurement of polygon area
  vri_bem <- dplyr::mutate(vri_bem, Shape_Area = as.numeric(sf::st_area(vri_bem)))

  bem_dt <- as.data.table(vri_bem)

  summ_area <- rbind(
    bem_dt[SDEC_1 > 0 & !is.na(BEUMC_S1) & !is.na(FORESTED_1), .(area_sum = sum(Shape_Area*SDEC_1/10), projection = FALSE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, HUCK_ASP, HUCK_ELEV_Thold,
                     CROWN_ALL = CROWN_ALL_1, STRCT = STRCT_S1, STAND = STAND_A1, FORESTED = FORESTED_1)],

    bem_dt[SDEC_2 > 0 & !is.na(BEUMC_S2) & !is.na(FORESTED_2), .(area_sum = sum(Shape_Area*SDEC_2/10), projection = FALSE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, HUCK_ASP, HUCK_ELEV_Thold,
                     CROWN_ALL = CROWN_ALL_2, STRCT = STRCT_S2, STAND = STAND_A2, FORESTED = FORESTED_2)],

    bem_dt[SDEC_3 > 0 & !is.na(BEUMC_S3) & !is.na(FORESTED_3), .(area_sum = sum(Shape_Area*SDEC_3/10), projection = FALSE),
           by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, HUCK_ASP, HUCK_ELEV_Thold,
                     CROWN_ALL = CROWN_ALL_3, STRCT = STRCT_S3, STAND = STAND_A3, FORESTED = FORESTED_3)])

  summ_area[(projection) & STAND %in% c("B", "C", "M") & !STRCT %in% c("4", "5", "6", "7"), STAND := NA_character_]

  summ_area[, .(Hectares = (sum(area_sum)/10000)),
            by = list(ECO_SEC, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, HUCK_ASP, HUCK_ELEV_Thold,
                      CROWN_ALL, STRCT, STAND, FORESTED)]

}
