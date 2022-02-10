devtools::load_all()

# read vri and bem layers
vri <- read_vri("../SSGBM-VRI-BEM-data/VEG_COMP_LYR_R1_POLY")
bem <- read_bem("../SSGBM-VRI-BEM-data/BEM_VRI")

# 1a ----
vri_bem <- merge_bem_on_vri(vri, bem, return_intersection_dt = TRUE)
vri_bem_intersection_dt <- vri_bem$intersection_dt
vri_bem <- vri_bem$vri

# filter out vri that have no overlapping bem (usually you should make sure the BEM covers all VRI)
vri_bem <- vri_bem[which(!is.na(vri_bem$TEIS_ID)),]

# 1b ----
beu_bec_csv <- fread("csv/Allowed_BEC_BEUs_NE_ALL.csv")
rivers <- read_rivers("../SSGBM-VRI-BEM-data/CodeWithUs.gdb")

vri_bem <- update_bem_from_vri(ifc = vri_bem,
                               rfc = rivers,
                               beu_bec = beu_bec_csv,
                               clear_site_ma = TRUE)

#1c ----
beu_wetland_update_csv <- fread("csv/beu_wetland_updates.csv")
wetlands <- read_wetlands("../SSGBM-VRI-BEM-data/CodeWithUs.gdb")

vri_bem <- update_bem_from_wet(bfc = vri_bem,
                               wfc = wetlands,
                               buc = beu_wetland_update_csv)

#2 ----
unique_eco <- create_unique_ecosytem_dt(ifc = vri_bem)

fwrite(unique_eco, file = "../unique_ecosystem.csv")


#3abc ----
elev_rast <- terra::rast("../SSGBM-VRI-BEM-data/DEM_tif/dem.tif")

vri_bem <- merge_elevation_raster_on_sf(elev_raster = elev_rast,
                                        ifc = vri_bem)

# merge cutblock
ccb <- read_ccb("../SSGBM-VRI-BEM-data/CodeWithUs.gdb")

vri_bem <- merge_ccb_on_vri(vri = vri_bem, ccb = ccb)

#4 ----
vri_bem <- calc_forest_age_class(vri = vri_bem,
                                 most_recent_harvest_year = 2020)


#4b /4d2 ----
unique_eco_example <- read_unique_ecosystem_dt("csv/Skeena_VRIBEM_LUT.csv")
vri_bem <- merge_unique_ecosystem_fields(ifc = vri_bem,
                                         unique_ecosystem_dt = unique_eco_example)

#4d3 ----
vri_bem <- find_crown_area_dominant_values(vri = vri_bem,
                                                bem = bem,
                                                intersection_dt = vri_bem_intersection_dt)

#5 ----
export_dt <- create_RRM_ecosystem(bfc = vri_bem)
fwrite(export_dt, file = "../RRM_input_table.csv")
