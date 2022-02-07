devtools::load_all()

vri <- sf::st_read(dsn = "../SSGBM-VRI-BEM-data/VEG_COMP_LYR_R1_POLY", layer = "VEG_R1_PLY_polygon", quiet = TRUE)
bem <- sf::st_read(dsn = "../SSGBM-VRI-BEM-data/BEM_VRI", layer = "BEM", quiet = TRUE)


#Restructure bem while waiting for real info
bem <- rename_geometry(bem, "Shape")

#Restructure bem while waiting for real info
vri <- rename_geometry(vri, "Shape")

#make shape valid
#TODO switch to use geometry

bem$Shape <- sf::st_make_valid(bem$Shape)
vri$Shape <- st_make_valid(vri$Shape) |> st_cast("MULTIPOLYGON")

#3abc ----
elev_rast <- terra::rast("../SSGBM-VRI-BEM-data/DEM_tif/dem.tif")

bem <- merge_elevation_raster_on_bem(elev_raster = elev_rast,
                                     bem = bem)

# 1a ----
vri_bem <- merge_vri_on_bem(vri, bem)

# 1b ----
beu_bec_csv <- fread("csv/Allowed_BEC_BEUs_NE_ALL.csv")
rivers <- sf::st_read(dsn = "../SSGBM-VRI-BEM-data/CodeWithUs.gdb", layer = "FWA_RIVERS_POLY", quiet = TRUE)
vri_bem_updated <- update_bem_from_vri(ifc = vri_bem,
                                       rfc = rivers,
                                       clear_site_ma = TRUE,
                                       beu_bec = beu_bec_csv)


#1c ----
beu_wetland_update_csv <- fread("csv/beu_wetland_updates.csv")
wetlands <- sf::st_read(dsn = "../SSGBM-VRI-BEM-data/CodeWithUs.gdb", layer = "FWA_WETLANDS_POLY", quiet = TRUE)
wetlands <- rename_geometry(wetlands, "Shape")

updated_bem_from_wetland <- update_bem_from_wet(bfc = vri_bem_updated,
                                                wfc = wetlands,
                                                buc = beu_wetland_update_csv)


#2 ----
unique_eco <- create_unique_ecosytem_dt(bem = updated_bem_from_wetland)

fwrite(unique_eco, file = "../unique_ecosystem.csv")




#4 ----
vri_forest_age <- calc_forest_age_class(vri = updated_bem_from_wetland,
                                        most_recent_harvest_year = 2020)


#4b ----



#4c ----
vri_std_crown <- add_std_crown_fields(vri = vri_forest_age)



#4b /4d2 ----
merge_unique_ecosystem_fields(ifc = vri_std_crown,
                              unique_ecosystem_dt = unique_eco)


#4d3 ----
find_crown_area_dominant_values(vri = vri_std_crown,
                                bem = bem)


#5 ----
unique_eco_info <- fread("../unique_ecosystem.csv")
