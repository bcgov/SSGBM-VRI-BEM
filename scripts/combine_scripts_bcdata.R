devtools::load_all()

aoi_wkt <- "MULTIPOLYGON (((1065018 932215.1, 941827.7 932215.1, 941827.7 1016988, 1065018 1016988, 1065018 932215.1)))"

p <- wk::as_wkt(aoi_wkt) |> sf::st_as_sf() |> sf::`st_crs<-`("EPSG:3005") |> terra::vect() |> terra::plet()

# read vri and bem layers
vri <- read_vri(wkt_filter = aoi_wkt)
bem <- read_bem("../SSGBM-VRI-BEM-data/BEM_VRI")

# 1a ----
vri_bem <- merge_bem_on_vri(vri = vri,
                            bem = bem,
                            return_intersection_dt = TRUE)

vri_bem_intersection_dt <- vri_bem$intersection_dt
vri_bem <- vri_bem$vri

# filter out vri that have no overlapping bem (usually you should make sure the BEM covers all VRI)
vri_bem <- vri_bem[which(!is.na(vri_bem$TEIS_ID)),]

# 1b ----
beu_bec_csv <- fread(system.file("csv/Allowed_BEC_BEUs_NE_ALL.csv", package = "SSGBM.VRI.BEM")) # fread("inst/csv/Allowed_BEC_BEUs_NE_ALL.csv")
rivers <- read_rivers(wkt_filter = aoi_wkt)

vri_bem <- update_bem_from_vri(vri_bem = vri_bem,
                               rivers = rivers,
                               beu_bec = beu_bec_csv,
                               clear_site_ma = TRUE,
                               use_ifelse = TRUE)

#1c ----
beu_wetland_update_csv <- fread(system.file("csv/beu_wetland_updates.csv", package = "SSGBM.VRI.BEM")) # fread("inst/csv/beu_wetland_updates.csv")
wetlands <- read_wetlands(wkt_filter = aoi_wkt)

vri_bem <- update_bem_from_wetlands(vri_bem = vri_bem,
                                    wetlands = wetlands,
                                    buc = beu_wetland_update_csv)
#1d ----
vri_bem <- update_beu_from_rules_dt(vri_bem = vri_bem,
                                    rules_dt = "../SSGBM-VRI-BEM-data/Rules_for_scripting_improved_forested_BEUs_Skeena_07Mar2022.xlsx")

#2 ----
unique_eco <- create_unique_ecosystem_dt(vri_bem = vri_bem)

fwrite(unique_eco, file = "../unique_ecosystem.csv")


#3abc ----
elev_rast <- terra::rast("../SSGBM-VRI-BEM-data/DEM_tif/dem.tif")

vri_bem <- merge_elevation_raster_on_sf(elev_raster = elev_rast,
                                        vri_bem = vri_bem,
                                        elevation_threshold = 1400)

# merge cutblock
ccb <- read_ccb(wkt_filter = aoi_wkt)

vri_bem <- merge_geometry(vri_bem, ccb, tolerance = units::as_units(10, "m2"))

#merge burn - What do you want to do with Burn?
# burn <- read_burn(wkt_filter = aoi_wkt)
# vri_bem <- merge_geometry(vri_bem, burn, tolerance = units::as_units(10, "m2"))

#4 ----
vri_bem <- calc_forest_age_class(vri_bem = vri_bem,
                                 most_recent_harvest_year = max(ccb$HARVEST_YEAR))


#4b /4d2 ----
unique_eco_example <- read_unique_ecosystem_dt("inst/csv/Skeena_VRIBEM_LUT.csv")
  vri_bem <- merge_unique_ecosystem_fields(vri_bem = vri_bem,
                                         unique_ecosystem_dt = unique_eco_example)

#4d3 ----
vri_bem <- find_crown_area_dominant_values(vri = vri_bem)

# calc hem fields
fire <- read_fire(wkt_filter = aoi_wkt)
vri_bem <- calc_hem_fields(vri_bem = vri_bem, fire = fire)

#5 ----
export_dt <- create_RRM_ecosystem(vri_bem = vri_bem)
fwrite(export_dt, file = "../RRM_input_table.csv")
saveRDS(export_dt, file = "./data-raw/RRM_input_table.rds")

#6 ---- In progress RRM to R - To validate before packaging in whole run function
export_dt <- readRDS("./data-raw/RRM_input_table.rds") |> data.table::setDT()
RSI_BGC_BEU <- unique(export_dt[,list(BGC_ZONE,BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC)])[order(BGC_ZONE,BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC)]
data.table::setkey(RSI_BGC_BEU, BGC_ZONE,BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC)

# Moose
template_dir <- "../SSGBM-VRI-BEM-data/Q12024/RRM_inputs/"
rsi_source <- "../SSGBM-VRI-BEM-data/Q12024/RRM_inputs/RSI_SOURCE of Ratings for Initial Attributes Tab in Models_MALAN_Skeena_15May2023.xlsx"
templates <- list.files(template_dir, "template.xlsx", full.names = TRUE)

rsi_rating <- readxl::read_xlsx(rsi_source, grep("rating", readxl::excel_sheets(path = rsi_source), value = TRUE, ignore.case = TRUE)[1])
data.table::setDT(rsi_rating)
data.table::setkey(rsi_rating, BGC_ZONE,BGC_SUBZON, BGC_VRT, BGC_PHASE, BEU_Mapcode)

moose_no_match <- RSI_BGC_BEU[!rsi_rating][,data.table::key(RSI_BGC_BEU), with = FALSE]
if (nrow(moose_no_match)) {
  logger::log_warn("No matching RSI ratings found for the following RRM ecosystem values.")
  print(moose_no_match)
}

moose_init <- mapply(
  function(x,y) {
    res <- rsi_rating[RSI_BGC_BEU]
    res <- res[, list(BGC_ZONE,BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEU_Mapcode, Iav_strct_d = res[[y]], RSI = res[[x]])]
    return(res)
  },
  names(rsi_rating) |> grep("_RSI$", x = _, value = TRUE),
  names(rsi_rating) |> grep("_Optimal Structural Stage$", x = _, value = TRUE),
  SIMPLIFY = FALSE
)
names(moose_init) <- names(rsi_rating) |> grep("_RSI$", x = _, value = TRUE) |> tolower() |> gsub("_rsi", "", x = _) |> trimws()  |> gsub("\\s+", "_", x = _) |> paste("moose", ... = _, sep = "_")

logger::log_threshold("WARN")
MALAN_WFD_6C <- rrm_calc_ratings(export_dt, templates[1], moose_init$moose_winter_forage)
MALAN_WST_6C <- rrm_calc_ratings(export_dt, templates[2], moose_init$moose_winter_shelter)
rrm_missing_lines(MALAN_WFD_6C)

# Grizzly
template_dir <- "../SSGBM-VRI-BEM-data/Q12024/Grizzly_RRM_inputs"
rsi_source <- "../SSGBM-VRI-BEM-data/Q12024/Grizzly_RRM_inputs/GM_31-March-2023_GB_BEUMC_RRM_Ratings_Skeena_Region.xlsx"
templates <- list.files(template_dir, "template.xlsx", full.names = TRUE)

rsi_rating <- readxl::read_xlsx(rsi_source, grep("rating", readxl::excel_sheets(path = rsi_source), value = TRUE, ignore.case = TRUE)[1])
data.table::setDT(rsi_rating)
data.table::setkey(rsi_rating, BGC_ZONE,BGC_SUBZON, BGC_VRT, BGC_PHASE, BEU_Mapcode)

grizzly_no_match <- RSI_BGC_BEU[!rsi_rating][,data.table::key(RSI_BGC_BEU), with = FALSE]
if (nrow(grizzly_no_match)) {
  logger::log_warn("No matching RSI ratings found for the following RRM ecosystem values.")
  print(grizzly_no_match)
}

grizzly_init <- mapply(
  function(x,y,z) {
    res <- rsi_rating[RSI_BGC_BEU]
    res <- res[, list(BGC_ZONE,BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEU_Mapcode, Iav_strct_d = res[[y]], Iav_site_m3a = res[[z]], RSI = res[[x]])]
    return(res)
  },
  names(rsi_rating) |> grep("^Grizzly_RSI", x = _, value = TRUE),
  names(rsi_rating) |> grep("^Grizzly_IAV_Structural", x = _, value = TRUE),
  names(rsi_rating) |> grep("^Grizzly_IAV_Site", x = _, value = TRUE),
  SIMPLIFY = FALSE
)
names(grizzly_init) <- names(rsi_rating) |> grep("^Grizzly_RSI_", x = _, value = TRUE) |> tolower() |> gsub("^grizzly_rsi_", "", x = _) |> trimws() |> gsub("\\s+", "_", x = _) |> paste("grizzly", ... = _, sep = "_")

logger::log_threshold("INFO")
MURAR_FFD_6C  <- rrm_calc_ratings(export_dt, templates[1], grizzly_init$grizzly_fall_forage)
MURAR_HI_6C   <- rrm_calc_ratings(export_dt, templates[2], grizzly_init$grizzly_hibernation)
MURAR_PEFD_6C <- rrm_calc_ratings(export_dt, templates[3], grizzly_init$grizzly_early_spring_forage)
MURAR_PLFD_6C <- rrm_calc_ratings(export_dt, templates[4], grizzly_init$grizzly_late_spring_forage)
MURAR_SFD_6C  <- rrm_calc_ratings(export_dt, templates[5], grizzly_init$grizzly_summer_forage)


# Append ratings
data.table::set(export_dt, j = "MALAN_WFD_6C", value = MALAN_WFD_6C[[1]]$RATING)
data.table::set(export_dt, j = "MALAN_WST_6C", value = MALAN_WST_6C[[1]]$RATING)
data.table::set(export_dt, j = "MURAR_FFD_6C", value = MURAR_FFD_6C[[1]]$RATING)
data.table::set(export_dt, j = "MURAR_HI_6C", value = MURAR_HI_6C[[1]]$RATING)
data.table::set(export_dt, j = "MURAR_PEFD_6C", value = MURAR_PEFD_6C[[1]]$RATING)
data.table::set(export_dt, j = "MURAR_PLFD_6C", value = MURAR_PLFD_6C[[1]]$RATING)
data.table::set(export_dt, j = "MURAR_SFD_6C", value = MURAR_SFD_6C[[1]]$RATING)

# PA scripts-convert
vri_bem <- readRDS("../SSGBM-VRI-BEM-data/vri_bem_step5.rds")
disturbance <- "../SSGBM-VRI-BEM-data/Q12024/Disturbance" |> sf::st_read() |> sf::st_transform(3005)
d <- depth_calculations(vri_bem, disturbance)
