devtools::load_all()

aoi_wkt <- "MULTIPOLYGON (((1065018 932215.1, 941827.7 932215.1, 941827.7 1016988, 1065018 1016988, 1065018 932215.1)))"

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
beu_bec_csv <- fread("csv/Allowed_BEC_BEUs_NE_ALL.csv")
rivers <- read_rivers(wkt_filter = aoi_wkt)

vri_bem <- update_bem_from_vri(vri_bem = vri_bem,
                               rivers = rivers,
                               beu_bec = beu_bec_csv,
                               clear_site_ma = TRUE,
                               use_ifelse = TRUE)

#1c ----
beu_wetland_update_csv <- fread("csv/beu_wetland_updates.csv")
wetlands <- read_wetlands(wkt_filter = aoi_wkt)

vri_bem <- update_bem_from_wetlands(vri_bem = vri_bem,
                                    wetlands = wetlands,
                                    buc = beu_wetland_update_csv)
#1d ----
rules_dt <- setDT(read_excel("/Users/nicolas/Documents/boostao/ssgbm/Improve_forested_BEU/Rules_for_scripting_improved_forested_BEUs_Skeena_07Mar2022.xlsx", sheet = "Combined_Rules_for_Script"))
vri_bem <- update_beu_from_rule_dt(vri_bem = vri_bem,
                                    rules_dt = rules_dt)

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

vri_bem <- merge_ccb_on_vri(vri_bem = vri_bem,
                            ccb = ccb)

#4 ----
vri_bem <- calc_forest_age_class(vri_bem = vri_bem,
                                 most_recent_harvest_year = 2020)


#4b /4d2 ----
unique_eco_example <- read_unique_ecosystem_dt("csv/Skeena_VRIBEM_LUT.csv")
vri_bem <- merge_unique_ecosystem_fields(vri_bem = vri_bem,
                                         unique_ecosystem_dt = unique_eco_example)

#4d3 ----
vri_bem <- find_crown_area_dominant_values(vri = vri_bem,
                                           bem = bem,
                                           intersection_dt = vri_bem_intersection_dt)

#5 ----
export_dt <- create_RRM_ecosystem(vri_bem = vri_bem)
fwrite(export_dt, file = "../RRM_input_table.csv")
