devtools::load_all()

unique_eco <-  create_unique_ecosystem_from_scratch(dsn = "../SSGBM-VRI-BEM-data/CodeWithUs.gdb",
                                     vri_dsn = "../SSGBM-VRI-BEM-data/VEG_COMP_LYR_R1_POLY",
                                     bem_dsn = "../SSGBM-VRI-BEM-data/BEM_VRI",
                                     wkt_filter = "POLYGON ((941827.7 932215.1, 1065018 932215.1, 1065018 1016988, 941827.7 1016988, 941827.7 932215.1))",
                                     n_iterations = 9)

unique_eco_2 <- create_unique_ecosystem_from_scratch(dsn = "../SSGBM-VRI-BEM-data/CodeWithUs.gdb",
                                                                    vri_dsn = "../SSGBM-VRI-BEM-data/VEG_COMP_LYR_R1_POLY",
                                                                    bem_dsn = "../SSGBM-VRI-BEM-data/BEM_VRI",
                                                                    wkt_filter = "POLYGON ((941827.7 932215.1, 1065018 932215.1, 1065018 1016988, 941827.7 1016988, 941827.7 932215.1))",
                                                                    n_iterations = 1)


rrm_output <-  create_RRM_ecosystem_from_scratch(dsn = "../SSGBM-VRI-BEM-data/CodeWithUs.gdb",
                                                 vri_dsn = "../SSGBM-VRI-BEM-data/VEG_COMP_LYR_R1_POLY",
                                                 bem_dsn = "../SSGBM-VRI-BEM-data/BEM_VRI",
                                                 elevation_dsn = "../SSGBM-VRI-BEM-data/DEM_tif/dem.tif",
                                                 most_recent_harvest_year = 2020,
                                                 wkt_filter = "POLYGON ((941827.7 932215.1, 1065018 932215.1, 1065018 1016988, 941827.7 1016988, 941827.7 932215.1))",
                                                 n_iterations = 9)


vri <- read_vri("../SSGBM-VRI-BEM-data/VEG_COMP_LYR_R1_POLY")
elevation <- terra::rast( "../SSGBM-VRI-BEM-data/DEM_tif/dem.tif")
bem <- read_bem("../SSGBM-VRI-BEM-data/BEM_VRI")

vri_rast <- rasterize(vect(vri))

extract(vect(vri), 3)

raster

attributes(elevation)
terra::
help(rast)


f <- system.file("ex/lux.shp", package="terra")
v <- vect(f)
vect_vri <- vect(vri)
r <- rast(vect_vri, ncols=ncol(elevation), nrows= nrow(elevation), nlyrs = ncol(vri))
value
x <- rasterize(vect_vri, r, c("FEATURE_ID"))

x <- rasterize(v, r, "NAME_2")

bem <- read_bem("../SSGBM-VRI-BEM-data/BEM_VRI")
v_bem <- vect(bem)
r_bem <- rast(v_bem, ncols = ncol(elevation), nrows = nrow(elevation))
raster_bem <- rasterize(v_bem, r_bem)
which_col_to_merge <- which(!sapply(bem , function(x) "sfc" %in% class(x)))
for (variable in names(bem)[which_col_to_merge]) {
  add(raster_bem) <- rasterize(v_bem, r_bem, variable)
}

writeRaster(raster_bem, "../bem.tif")


rast_bem <- rast("../bem.tif")
vri <- read_vri("../SSGBM-VRI-BEM-data/VEG_COMP_LYR_R1_POLY")
vect_vri <- vect(vri$Shape)
x_bem_vri <- setDT(extract(rast_bem[[1]], vect_vri))

