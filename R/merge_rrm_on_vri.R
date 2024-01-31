#' merge_rrm_on_vri
#'
#' merge suitability and capability rating from rrm onto vri-bem
#' and calculate highest value and weighted average for each scores
#'
#' @param vri_bem sf object that represent VRI (vegetation ressource inventory) features
#' @param rrm_dt data.table object that contains the rrm
#' @param animal character, "bear" or "moose"
#' @param return_sf logical, if TRUE  return sf object , if FALSE return data.table object and update by reference
#' @return vri-bem object with new columns for rating
#' @import data.table
#' @export
merge_rrm_on_vri <- function(vri_bem, rrm_dt, animal, return_sf = TRUE) {

  if (FALSE) {
    Hectares_1<-Hectares_2<-Hectares_3<-rrm_merge_ind<-SDEC_1<-SDEC_2<-SDEC_3<-NULL
  }

  vri_bem <- vri_bem |> mutate(SDEC_1 = ifelse(is.na(SDEC_1), 0, SDEC_1),
                               SDEC_2 = ifelse(is.na(SDEC_2), 0, SDEC_2),
                               SDEC_3 = ifelse(is.na(SDEC_3), 0, SDEC_3)) #replace all NAs in SDEC_1, 2, 3 with 0

  setDT(vri_bem)

  # calc capability rating and format rrm table
  rrm_dt <- format_rrm_dt(rrm_dt = rrm_dt, animal = animal) #updated to format in a species-specific way
  rrm_dt <- calc_capability_rating(rrm_dt = rrm_dt, animal = animal)


  rating_variables <- grep("_6C$", names(rrm_dt), value = T)
  cap_rating_variables <- paste0(rating_variables, "_CAP")

  variables_merge_expr <- paste0("list(", paste(paste0("", c(rating_variables, cap_rating_variables , "Hectares")), collapse = ","), ")")

  first_decile_variables <- c(paste0(rating_variables, "_SU_1"), paste0(cap_rating_variables, "_1"), "Hectares_1")
  second_decile_variables <- c(paste0(rating_variables, "_SU_2"), paste0(cap_rating_variables, "_2"), "Hectares_2")
  third_decile_variables <- c(paste0(rating_variables, "_SU_3"), paste0(cap_rating_variables, "_3"), "Hectares_3")

  if (animal == "bear") {
    merge_rating_bear(vri_bem = vri_bem,
                      rrm_dt = rrm_dt,
                      rating_variables = list(first_decile_variables,
                                              second_decile_variables,
                                              third_decile_variables),
                      rating_variables_expr = variables_merge_expr)
  }

  if (animal == "moose") {
    merge_rating_moose(vri_bem = vri_bem,
                      rrm_dt = rrm_dt,
                      rating_variables = list(first_decile_variables,
                                              second_decile_variables,
                                              third_decile_variables),
                      rating_variables_expr = variables_merge_expr)
  }

  vri_bem[ , rrm_merge_ind := !is.na(fcoalesce(Hectares_1, Hectares_2, Hectares_3))]

  # calc highest suitability value ----

  which_na_list <- list()
  for (suitability_variable in rating_variables) {

    first_suit_var <- paste0(suitability_variable, "_SU_1")
    second_suit_var <- paste0(suitability_variable, "_SU_2")
    third_suit_var <- paste0(suitability_variable, "_SU_3")
    high_value_suit_var <- paste0(suitability_variable, "_SU_HV")
    weighted_average_suit_var <- paste0(suitability_variable, "_SU_WA")

    # assign temporary worst rating  to rating NA to make calculation of best rating easier
    set(vri_bem, i = which((vri_bem[["FORESTED_1"]] == "Y" & vri_bem[["STRCT_S1"]] == "7a" & vri_bem[["VRI_AGE_CL_STS"]] == -1) | (vri_bem[["FORESTED_1"]] == "N" & is.na(vri_bem[["STRCT_S1"]]) & vri_bem[["ABOVE_ELEV_THOLD"]] == "N") & !is.na(vri_bem[["STS_CLIMAX_1"]])), j = first_suit_var, value = NA)
    set(vri_bem, i = which((vri_bem[["FORESTED_2"]] == "Y" & vri_bem[["STRCT_S2"]] == "7a" & vri_bem[["VRI_AGE_CL_STS"]] == -1) | (vri_bem[["FORESTED_2"]] == "N" & is.na(vri_bem[["STRCT_S2"]]) & vri_bem[["ABOVE_ELEV_THOLD"]] == "N") & !is.na(vri_bem[["STS_CLIMAX_2"]])), j = second_suit_var, value = NA)
    set(vri_bem, i = which((vri_bem[["FORESTED_3"]] == "Y" & vri_bem[["STRCT_S3"]] == "7a" & vri_bem[["VRI_AGE_CL_STS"]] == -1) | (vri_bem[["FORESTED_3"]] == "N" & is.na(vri_bem[["STRCT_S3"]]) & vri_bem[["ABOVE_ELEV_THOLD"]] == "N") & !is.na(vri_bem[["STS_CLIMAX_3"]])), j = third_suit_var, value = NA)

    set(vri_bem, i = which(vri_bem[[first_suit_var]] > 6), j = first_suit_var, value = NA)
    set(vri_bem, i = which(vri_bem[[second_suit_var]] > 6), j = second_suit_var, value = NA)
    set(vri_bem, i = which(vri_bem[[third_suit_var]] > 6), j = third_suit_var, value = NA)

    which_na_list[[first_suit_var]] <- which(is.na(vri_bem[[first_suit_var]]))
    which_na_list[[second_suit_var]] <- which(is.na(vri_bem[[second_suit_var]]))
    which_na_list[[third_suit_var]] <- which(is.na(vri_bem[[third_suit_var]]))

    set(vri_bem, i = which_na_list[[first_suit_var]], j = first_suit_var, value = 9)
    set(vri_bem, i = which_na_list[[second_suit_var]], j = second_suit_var, value = 9)
    set(vri_bem, i = which_na_list[[third_suit_var]], j = third_suit_var, value = 9)

    # calc best rating
    fcase_expr <- parse_expr(paste0("fcase((",first_suit_var," <= ", second_suit_var, ") & (", first_suit_var, " <= ", third_suit_var, "), ", first_suit_var, ",
                                            ", second_suit_var, " <= ", third_suit_var, ", ", second_suit_var, ",
                                            ", third_suit_var, " <= ", second_suit_var, ", ", third_suit_var, ",
                                            default = NA)"))
    vri_bem[, (high_value_suit_var) := eval(fcase_expr)]

    set(vri_bem, i = which(vri_bem[[high_value_suit_var]] > 8), j = high_value_suit_var, value = NA)

    # calc weighted suitability rating ----

    # don't consider a rating that had not match in the RRM output but has percentage > 0
    set(vri_bem, i = which_na_list[[first_suit_var]], j = first_suit_var, value = 0)
    set(vri_bem, i = which_na_list[[second_suit_var]], j = second_suit_var, value = 0)
    set(vri_bem, i = which_na_list[[third_suit_var]], j = third_suit_var, value = 0)

    wa_expr <- parse_expr(paste0("round(((", first_suit_var, " * SDEC_1) + (", second_suit_var, " * SDEC_2) + (", third_suit_var, "* SDEC_3))/(SDEC_1 * (", first_suit_var, " != 0) + SDEC_2 * (", second_suit_var, " != 0) + SDEC_3 * (", third_suit_var, " != 0) ))"))
    vri_bem[, (weighted_average_suit_var) := eval(wa_expr)]
    set(vri_bem, i = which(vri_bem[[weighted_average_suit_var]] > 6 | vri_bem[[weighted_average_suit_var]] == 0 | is.nan(vri_bem[[weighted_average_suit_var]])), j = weighted_average_suit_var, value = NA)

  }

  # calc highest capability value ----

  for (cap_variable in cap_rating_variables) {
    first_cap_var <- paste0(cap_variable, "_1")
    second_cap_var <- paste0(cap_variable, "_2")
    third_cap_var <- paste0(cap_variable, "_3")
    high_value_cap_var <- paste0(cap_variable, "_HV")
    weighted_average_cap_var <- paste0(cap_variable, "_WA")

    # assign temporary worst rating  to rating NA to make calculation of best rating easier
    set(vri_bem, i = which(vri_bem[[first_cap_var]] > 6), j = first_cap_var, value = NA)
    set(vri_bem, i = which(vri_bem[[second_cap_var]] > 6), j = second_cap_var, value = NA)
    set(vri_bem, i = which(vri_bem[[third_cap_var]] > 6), j = third_cap_var, value = NA)

    which_na_list[[first_cap_var]] <- which(is.na(vri_bem[[first_cap_var]]))
    which_na_list[[second_cap_var]] <- which(is.na(vri_bem[[second_cap_var]]))
    which_na_list[[third_cap_var]] <- which(is.na(vri_bem[[third_cap_var]]))

    set(vri_bem, i = which_na_list[[first_cap_var]], j = first_cap_var, value = 9)
    set(vri_bem, i = which_na_list[[second_cap_var]], j = second_cap_var, value = 9)
    set(vri_bem, i = which_na_list[[third_cap_var]], j = third_cap_var, value = 9)

    # calc best rating
    fcase_expr <- parse_expr(paste0("fcase((",first_cap_var," <= ", second_cap_var, ") & (", first_cap_var, " <= ", third_cap_var, "), ", first_cap_var, ",
                                            ", second_cap_var, " <= ", third_cap_var, ", ", second_cap_var, ",
                                            ", third_cap_var, " <= ", second_cap_var, ", ", third_cap_var, ",
                                            default = NA)"))
    vri_bem[, (high_value_cap_var) := eval(fcase_expr)]

    set(vri_bem, i = which(vri_bem[[high_value_cap_var]] > 8), j = high_value_cap_var, value = NA)

    # calc weighted capability rating ----

    # don't consider a rating that had not match in the RRM output but has percentage > 0
    set(vri_bem, i = which_na_list[[first_cap_var]], j = first_cap_var, value = 0)
    set(vri_bem, i = which_na_list[[second_cap_var]], j = second_cap_var, value = 0)
    set(vri_bem, i = which_na_list[[third_cap_var]], j = third_cap_var, value = 0)

    wa_expr <- parse_expr(paste0("round(((", first_cap_var, " * SDEC_1) + (", second_cap_var, " * SDEC_2) + (", third_cap_var, "* SDEC_3))/(SDEC_1 * (", first_cap_var, " != 0) + SDEC_2 * (", second_cap_var, " != 0) + SDEC_3 * (", third_cap_var, " != 0) ))")) #otherwise not calculating CAP properly (moose)
    vri_bem[, (weighted_average_cap_var) := eval(wa_expr)]
    set(vri_bem, i = which(vri_bem[[weighted_average_cap_var]] > 6), j = weighted_average_cap_var, value = NA)

  }

  # rating for decile that are 0 should be NA
  set(vri_bem, i = which(vri_bem$SDEC_1 == 0), j = first_decile_variables, value = NA)
  set(vri_bem, i = which(vri_bem$SDEC_2 == 0), j = second_decile_variables, value = NA)
  set(vri_bem, i = which(vri_bem$SDEC_3 == 0), j = third_decile_variables, value = NA)

  for (rating_variable in names(which_na_list)) {
    set(vri_bem, i = which_na_list[[rating_variable]], j = rating_variable, value = NA)
  }

  set(vri_bem, j = c("Hectares_1", "Hectares_2", "Hectares_3"), value = NULL)

  if (return_sf) {
    return(st_as_sf(vri_bem))
  } else {
    return(vri_bem)
  }

}



#' merge_rating_bear
#'
#' merge suitability and capability rating from rrm onto vri-bem
#' and calculate highest value and weighted average for each scores
#'
#' @param vri_bem sf object that represent VRI (vegetation ressource inventory) features
#' @param rrm_dt data.table object that contains the rrm
#' @param rating_variables list, vector for each rating variables to create
#' @param rating_variables_expr parse_expr , expression for variable to merge in data.table
#' @return sf  vri-bem object with new columns for rating
#' @import data.table
#' @export
merge_rating_bear <- function(vri_bem, rrm_dt, rating_variables, rating_variables_expr) {
  # merge on decile 1 ----
  eval(parse_expr(paste0("vri_bem[rrm_dt, on = .(ECO_SEC = Eco_sec, BGC_ZONE = Bgc_zone, BGC_SUBZON = Bgc_subzon, BGC_VRT = Bgc_vrt,
                         BGC_PHASE = Bgc_phase, BEUMC_S1 = Beumc, SLOPE_MOD = Slope_mod,
                         SITE_M3A = Site_m3a, Salmon = Salmon, SNOW_CODE = Snow_code, ABOVE_ELEV_THOLD = Above_Elev_Thold,
                         CROWN_ALL_1 = Crown_All, STRCT_S1 = Strct_d, STAND_A1 = Stand_d),
          c(", paste0("'", paste(rating_variables[[1]], collapse = "','"), "'"), ") := ", rating_variables_expr,"]")))

  # merge on decile 2 ----
  eval(parse_expr(paste0("vri_bem[rrm_dt, on = .(ECO_SEC = Eco_sec, BGC_ZONE = Bgc_zone, BGC_SUBZON = Bgc_subzon, BGC_VRT = Bgc_vrt,
                         BGC_PHASE = Bgc_phase, BEUMC_S2 = Beumc, SLOPE_MOD = Slope_mod,
                         SITE_M3A = Site_m3a, Salmon = Salmon, SNOW_CODE = Snow_code, ABOVE_ELEV_THOLD = Above_Elev_Thold,
                         CROWN_ALL_2 = Crown_All, STRCT_S2 = Strct_d, STAND_A2 = Stand_d),
          c(", paste0("'", paste(rating_variables[[2]], collapse = "','"), "'"), ") := ", rating_variables_expr,"]")))



  # merge on decile 3 ----
  eval(parse_expr(paste0("vri_bem[rrm_dt, on = .(ECO_SEC = Eco_sec, BGC_ZONE = Bgc_zone, BGC_SUBZON = Bgc_subzon, BGC_VRT = Bgc_vrt,
                         BGC_PHASE = Bgc_phase, BEUMC_S3 = Beumc, SLOPE_MOD = Slope_mod,
                         SITE_M3A = Site_m3a, Salmon = Salmon, SNOW_CODE = Snow_code, ABOVE_ELEV_THOLD = Above_Elev_Thold,
                         CROWN_ALL_3 = Crown_All, STRCT_S3 = Strct_d, STAND_A3 = Stand_d),
          c(", paste0("'", paste(rating_variables[[3]], collapse = "','"), "'"), ") := ", rating_variables_expr,"]")))


  return(vri_bem)
}


#' merge_rating_moose
#'
#' merge suitability and capability rating from rrm onto vri-bem
#' and calculate highest value and weighted average for each scores
#'
#' @param vri_bem sf object that represent VRI (vegetation ressource inventory) features
#' @param rrm_dt data.table object that contains the rrm
#' @param rating_variables list, vector for each rating variables to create
#' @param rating_variables_expr parse_expr , expression for variable to merge in data.table
#' @return sf  vri-bem object with new columns for rating
#' @import data.table
#' @export
merge_rating_moose <- function(vri_bem, rrm_dt, rating_variables, rating_variables_expr) {
  # merge on decile 1 ----
  eval(parse_expr(paste0("vri_bem[rrm_dt, on = .(ECO_SEC = Eco_sec, BGC_ZONE = Bgc_zone, BGC_SUBZON = Bgc_subzon, BGC_VRT = Bgc_vrt,
                         BGC_PHASE = Bgc_phase, BEUMC_S1 = Beumc, SLOPE_MOD = Slope_mod,
                         SITE_M3A = Site_m3a, SNOW_CODE = Snow_code, ABOVE_ELEV_THOLD = Above_Elev_Thold,
                         CROWN_ALL_1 = Crown_All, STRCT_S1 = Strct_d, STAND_A1 = Stand_d),
          c(", paste0("'", paste(rating_variables[[1]], collapse = "','"), "'"), ") := ", rating_variables_expr,"]")))

  # merge on decile 2 ----
  eval(parse_expr(paste0("vri_bem[rrm_dt, on = .(ECO_SEC = Eco_sec, BGC_ZONE = Bgc_zone, BGC_SUBZON = Bgc_subzon, BGC_VRT = Bgc_vrt,
                         BGC_PHASE = Bgc_phase, BEUMC_S2 = Beumc, SLOPE_MOD = Slope_mod,
                         SITE_M3A = Site_m3a, SNOW_CODE = Snow_code, ABOVE_ELEV_THOLD = Above_Elev_Thold,
                         CROWN_ALL_2 = Crown_All, STRCT_S2 = Strct_d, STAND_A2 = Stand_d),
          c(", paste0("'", paste(rating_variables[[2]], collapse = "','"), "'"), ") := ", rating_variables_expr,"]")))



  # merge on decile 3 ----
  eval(parse_expr(paste0("vri_bem[rrm_dt, on = .(ECO_SEC = Eco_sec, BGC_ZONE = Bgc_zone, BGC_SUBZON = Bgc_subzon, BGC_VRT = Bgc_vrt,
                         BGC_PHASE = Bgc_phase, BEUMC_S3 = Beumc, SLOPE_MOD = Slope_mod,
                         SITE_M3A = Site_m3a, SNOW_CODE = Snow_code, ABOVE_ELEV_THOLD = Above_Elev_Thold,
                         CROWN_ALL_3 = Crown_All, STRCT_S3 = Strct_d, STAND_A3 = Stand_d),
          c(", paste0("'", paste(rating_variables[[3]], collapse = "','"), "'"), ") := ", rating_variables_expr,"]")))

  return(vri_bem)
}
