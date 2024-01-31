#' Extract large polygons and assign additional layers
#'
#'Extract large polygons and adjacent polygons,
#'Cut in CEF glaciers and FWA wetlands and lakes
#'Merge BEM and correct assigned BEU/SDEC
#'
#'@param vri_bem VRI-BEM feature class
#'@param glaciers sf
#'@param lakes sf
#'@param wetlands sf
#'@param bem sf
#'@importFrom dplyr mutate bind_rows mutate filter select group_by ungroup summarize case_when mutate_if
#'@importFrom tibble add_column
#'@import sf
#'@export

#run the xl polygons functions and merge with the regular vri-bem

amend_large_polygons <- function (vri_bem, glaciers, lakes, wetlands, bem){

  if (FALSE) {
    TEIS_ID<-NULL
  }

  vri_bem <- dplyr::mutate(vri_bem, area = sf::st_area(vri_bem))

  if(max(vri_bem$area) > units::set_units(35000000,"m^2")){

    #Extract large polygons
    vrixl <- extract_large_polygons(vri_bem)

    #Merge additional layers
    vrixl_merge <- merge_extra_layers(vrixl, glaciers, lakes, wetlands)

    #Merge BEM on XL polygons
    vri_bem_xl <- merge_bem_on_vri_xl(vrixl_merge, bem)

    #Remove XL polygons from initial VRI-BEM and replace with updated polygons
    vri_bem_xl_diff <- sf::st_difference(vri_bem, sf::st_union(sf::st_geometry(vri_bem_xl))) |> sf::st_make_valid() |>
      sf::st_collection_extract() |> sf::st_cast("POLYGON", warn = FALSE) |> sf::st_make_valid()

    #Make sure TEIS_ID is character
    vri_bem_xl_diff <- dplyr::mutate(vri_bem_xl_diff, TEIS_ID = as.character(TEIS_ID))
    vri_bem_xl <- dplyr::mutate(vri_bem_xl,TEIS_ID = as.character(TEIS_ID))

    #Bind vri-bem and xl polygons
    vri_bem <- dplyr::bind_rows(vri_bem_xl_diff, vri_bem_xl)

  }

  return(vri_bem)

}



#extract large polygons
extract_large_polygons <- function (vri_bem) {

  if (FALSE) {
    .<-area<-BCLCS_LV_1<-BCLCS_LV_2<-BCLCS_LV_3<-BCLCS_LV_4<-BCLCS_LV_5<-BGC_PHASE<-BGC_SUBZON<-
      BGC_VRT<-BGC_ZONE<-INVENTORY_STANDARD_CD<-m<-SPEC_CD_1<-NULL
  }

  #find polygons ovre 3500 ha
  large_polygons <- dplyr::mutate(vri_bem, area = sf::st_area(vri_bem)) |>
    dplyr::filter(area > units::set_units(35000000, "m^2"))

  #Find adjacent polygons
  adj_polygons <- sf::st_touches(large_polygons, vri_bem, sparse = TRUE)

  adj_polygons_ul <- unique(unlist(adj_polygons))

  #combine

  combine_polygons <- dplyr::bind_rows(large_polygons,vri_bem[adj_polygons_ul, ])

  #dissolve boundaries based on INVENTORY_STANDARD_CD, BCLCS_LEVEL_1, BCLCS_LEVEL_2, BCLCS_LEVEL_3, BCLCS_LEVEL_4, BCLCS_LEVEL_5, AND SPECIES_CD_1 (make sure names are updated eventually)
  #added BGC information because that needs to come from vri
  polygon_dissolve <- combine_polygons |> dplyr::group_by(INVENTORY_STANDARD_CD, BCLCS_LV_1, BCLCS_LV_2, BCLCS_LV_3, BCLCS_LV_4, BCLCS_LV_5, SPEC_CD_1, BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE) |> dplyr::summarize()

  #find polygons over 3500 ha of the dissolved polygons
  vrixl <- polygon_dissolve |> dplyr::ungroup() |> {\(x) {dplyr::mutate(x, area = sf::st_area(x))}}() |>
    dplyr::filter(area > units::set_units(35000000, "m^2"))

  return(vrixl)

}

#merge glaciers, wetlands, and lakes onto large polygons
###ADD EXTRA VRI LAYERS
merge_extra_layers <- function(vrixl, glaciers, lakes, wetlands) {

  if (FALSE) {
    BCLCS_LV_1<-BCLCS_LV_2<-BCLCS_LV_3<-BCLCS_LV_4<-BCLCS_LV_5<-BEUMC_S1<-BGC_PHASE<-BGC_SUBZON<-
      BGC_VRT<-BGC_ZONE<-DEC_1<-INVENTORY_STANDARD_CD<-SPEC_CD_1<-NULL
  }

  # tell sf that attributes are constant throughout the geometries to avoid warning
  sf::st_agr(vrixl) <- "constant"
  sf::st_agr(glaciers) <- "constant"
  sf::st_agr(lakes) <- "constant"
  sf::st_agr(wetlands) <- "constant"

  #merge CEF glaciers and snow on large VRI polygons
  merge_glaciers_intersection <- sf::st_intersection(vrixl, glaciers) |>
    sf::st_make_valid() |>
    sf::st_collection_extract() |>
    sf::st_cast("POLYGON",warn = FALSE) |>
    sf::st_make_valid() |>
    dplyr::mutate(BEUMC_S1 = "GL")
  merge_glaciers_diff <- sf::st_difference(vrixl, sf::st_union(sf::st_geometry(glaciers))) |>
    sf::st_make_valid() |>
    sf::st_collection_extract() |>
    sf::st_cast("POLYGON", warn = FALSE) |>
    sf::st_make_valid()
  vrixl <- dplyr::bind_rows(merge_glaciers_intersection,merge_glaciers_diff)

  #merge lakes on large VRI polygons
  merge_lakes_intersection <- sf::st_intersection(vrixl, lakes) |>
    sf::st_make_valid() |>
    sf::st_collection_extract() |>
    sf::st_cast("POLYGON",warn = FALSE) |>
    sf::st_make_valid()|>
    dplyr::mutate(BEUMC_S1 = "LL")
  merge_lakes_diff <- sf::st_difference(vrixl, sf::st_union(sf::st_geometry(lakes))) |>
    sf::st_make_valid() |>
    sf::st_collection_extract() |>
    sf::st_cast("POLYGON", warn = FALSE) |>
    sf::st_make_valid()
  vrixl <- dplyr::bind_rows(merge_lakes_intersection,merge_lakes_diff)

  #merge wetlands on large VRI polygons
  merge_wetlands_intersection <- sf::st_intersection(vrixl, wetlands) |>
    sf::st_make_valid() |>
    sf::st_collection_extract() |>
    sf::st_cast("POLYGON",warn=FALSE) |>
    sf::st_make_valid() |>
    dplyr::mutate(BEUMC_S1 = "WL")
  merge_wetlands_diff <- sf::st_difference(vrixl, sf::st_union(sf::st_geometry(wetlands))) |>
    sf::st_make_valid() |>
    sf::st_collection_extract() |>
    sf::st_cast("POLYGON", warn = FALSE) |>
    sf::st_make_valid()
  vrixl <- dplyr::bind_rows(merge_wetlands_intersection,merge_wetlands_diff)

  #bind VRI categories
  vrixl_merge <- vrixl |>
    dplyr::mutate(BCLCS_LV_1 = dplyr::case_when(
      BEUMC_S1 == "WL" ~ "V",
      BEUMC_S1 == "GL" ~ "N",
      BEUMC_S1 == "LL" ~ "N",
      !BEUMC_S1 %in% c("WL","GL","LL") ~ BCLCS_LV_1),
      BCLCS_LV_2 = dplyr::case_when(
        BEUMC_S1 == "WL" ~ "N",
        BEUMC_S1 == "GL" ~ "L",
        BEUMC_S1 == "LL" ~ "W",
        !BEUMC_S1 %in% c("WL","GL","LL") ~ BCLCS_LV_2),
      BCLCS_LV_3 = dplyr::case_when(
        BEUMC_S1 == "WL" ~ "W",
        BEUMC_S1 == "GL" ~ "A",
        BEUMC_S1 == "LL" ~ "W",
        !BEUMC_S1 %in% c("WL","GL","LL") ~ BCLCS_LV_3),
      BCLCS_LV_4 = dplyr::case_when(
        BEUMC_S1 == "WL" ~ NA,
        BEUMC_S1 == "GL" ~ "SI",
        BEUMC_S1 == "LL" ~ NA,
        !BEUMC_S1 %in% c("WL","GL","LL") ~ BCLCS_LV_4),
      BCLCS_LV_5 = dplyr::case_when(
        BEUMC_S1 == "WL" ~ NA,
        BEUMC_S1 == "GL" ~ "OT",
        BEUMC_S1 == "LL" ~ "LA",
        !BEUMC_S1 %in% c("WL","GL","LL") ~ BCLCS_LV_5),
      DEC_1 = dplyr::case_when( #use DEC_1 instead of SDEC_1 so it isn't overwritten with BEM merge
        BEUMC_S1 %in% c("WL","GL","LL") ~ 10))

  #keep only necessary fields from above and add empty VRI table schema
  vrixl_merge <- dplyr::select(vrixl_merge,INVENTORY_STANDARD_CD, BCLCS_LV_1, BCLCS_LV_2, BCLCS_LV_3, BCLCS_LV_4, BCLCS_LV_5, BEUMC_1 = BEUMC_S1, SPEC_CD_1, DEC_1,VRI_BEC_ZONE = BGC_ZONE, VRI_BEC_SUBZON = BGC_SUBZON, VRI_BEC_VRT = BGC_VRT, VRI_BEC_PHASE = BGC_PHASE) |>
    tibble::add_column(SPEC_CD_2 = NA, SPEC_CD_3 = NA, SPEC_CD_4 = NA, SPEC_CD_5 = NA, SPEC_CD_6 = NA, SPEC_PCT_1 = NA, SPEC_PCT_2 = NA, SPEC_PCT_3 = NA, SPEC_PCT_4 = NA, SPEC_PCT_5 = NA, SPEC_PCT_6 = NA, CR_CLOSURE = NA, LAND_CD_1 = NA, COV_PCT_1 = NA, LBL_VEGCOV = NA, HRVSTDT = NA, PROJ_AGE_1 = NA, SOIL_MOISTURE_REGIME_1 = NA, SOIL_NUTRIENT_REGIME = NA)

  return(vrixl_merge)
}

#merge xl vri with BEM
merge_bem_on_vri_xl <- function(vrixl_merge, bem){

  if (FALSE) {
    BEUMC_1<-DEC_1<-NULL
  }

  vri_bem_xl <- merge_bem_on_vri(vrixl_merge,bem)

  vri_bem_xl <- vri_bem_xl |>
    dplyr::mutate(BEUMC_S1 = dplyr::case_when(
      !is.na(BEUMC_1) ~ BEUMC_1,
      is.na(BEUMC_1) ~ BEUMC_S1),
      BEUMC_S2 = dplyr::case_when(
        !is.na(BEUMC_1) ~ NA,
        is.na(BEUMC_1) ~ BEUMC_S2),
      BEUMC_S3 = dplyr::case_when(
        !is.na(BEUMC_1) ~ NA,
        is.na(BEUMC_1) ~ BEUMC_S3),
      SDEC_1 = dplyr::case_when(
        !is.na(BEUMC_1) ~ DEC_1,
        is.na(BEUMC_1) ~ SDEC_1),
      SDEC_2 = dplyr::case_when(
        !is.na(BEUMC_1) ~ 0,
        is.na(BEUMC_1) ~ SDEC_2),
      SDEC_3 = dplyr::case_when(
        !is.na(BEUMC_1) ~ 0,
        is.na(BEUMC_1) ~ SDEC_3),
      POLY_COMM = dplyr::case_when(
        !is.na(BEUMC_1) ~ "XL edit",
        is.na(BEUMC_1) ~ NA))

  vri_bem_xl <- dplyr::select(vri_bem_xl,-c(BEUMC_1, DEC_1))

  return(vri_bem_xl)
}
