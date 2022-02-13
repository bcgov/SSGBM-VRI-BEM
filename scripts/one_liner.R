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
