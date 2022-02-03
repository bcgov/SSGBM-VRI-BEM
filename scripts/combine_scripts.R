library(sf)
library(data.table)

vri <- sf::st_read(dsn = "../SSGBM-VRI-BEM-data/VEG_COMP_LYR_R1_POLY", layer = "VEG_R1_PLY_polygon", quiet = TRUE)
bem <- sf::st_read(dsn = "../SSGBM-VRI-BEM-data/BEM_VRI", layer = "BEM", quiet = TRUE)

#Restructure bem while waiting for real info

bem$Shape <- bem$geometry
#bem$geometry <- NULL

#Restructure bem while waiting for real info
vri$Shape <- vri$geometry
#vri$geometry <- NULL

#make shape valid
#TODO switch to use geometry
bem$Shape <- sf::st_make_valid(bem$geometry)
vri$Shape <- st_make_valid(vri$geometry) |> st_cast("MULTIPOLYGON")
# 1a ----
vri_bem <- merge_vri_on_bem(vri, bem)

vri_bem


# 1b ----
beu_bec_csv <- fread("csv/Allowed_BEC_BEUs_NE_ALL.csv")
rivers <- sf::st_read(dsn = "../SSGBM-VRI-BEM-data/CodeWithUs.gdb", layer = "FWA_RIVERS_POLY", quiet = TRUE)
vri_bem_updated <- update_bem_from_vri(ifc = vri_bem,
                                       rfc = rivers,
                                       clear_site_ma = TRUE,
                                       beu_bec = beu_bec_csv)

vri_bem_updated

#1c ----
beu_wetland_update_csv <- fread("csv/beu_wetland_updates.csv")
wetlands <- sf::st_read(dsn = "../SSGBM-VRI-BEM-data/CodeWithUs.gdb", layer = "FWA_WETLANDS_POLY", quiet = TRUE)
wetlands$Shape <- wetlands$GEOMETRY

updated_bem_from_wetland <- update_bem_from_wet(bfc = vri_bem_updated,
                                                wfc = wetlands,
                                                buc = beu_wetland_update_csv)
debugonce(update_bem_from_wet)
