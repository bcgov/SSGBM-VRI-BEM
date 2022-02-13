devtools::load_all()
path <- "C:/Users/nicol/OneDrive/Documents/boostao/Python to R spatial script conversion/CodeWithUs.gdb/"
layers <- st_layers(dsn =  paste0(path, "CodeWithUs.gdb"))

vri <- st_read(dsn = paste0(path, "CodeWithUs.gdb"), layer = layers$name[layers$name == "code_with_us_aoi"])
#bem <- st_read(dsn = "paste0(path, "CodeWithUs.gdb"), layer = layers$name[21])
bem <- st_read(dsn = "C:/Users/nicol/OneDrive/Documents/boostao/Python to R spatial script conversion/tei_long_tbl_aoi")
rivers <- st_read(dsn = paste0(path, "CodeWithUs.gdb"), layer = layers$name[layers$name == "FWA_RIVERS_POLY"])

bem$Shape <- bem$geometry
bem$geometry <- NULL

bem$Shape <- st_make_valid(bem$Shape)
vri$Shape <- st_make_valid(vri$Shape) |> st_cast("MULTIPOLYGON")
rivers$GEOMETRY <- st_make_valid(rivers$GEOMETRY) |> st_cast("MULTIPOLYGON")

vri_bem <- merge_vri_bem(vri, bem)
vri_bem_updated  <- update_bem_from_vri(vri_bem, rivers)



rbind(fc[SDEC_1 > 0, .N , by = .(BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1)],
      fc[SDEC_2 > 0, .N , by = .(BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S2)],
      fc[SDEC_3 > 0, .N , by = .(BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S3)])[ , sum(N) , by = .(BGC_ZONE, BGC_SUBZON, BGC_VRT, BGC_PHASE, BEUMC = BEUMC_S1)]




aoi <- st_read(dsn = "../SSGBM-VRI-BEM-data/CodeWithUs.gdb", layer = "code_with_us_aoi", quiet = TRUE)
plot(aoi$SHAPE)
plot(vri_bem[which(is.na(vri_bem$TEIS_ID)), "Shape"], add = T)
