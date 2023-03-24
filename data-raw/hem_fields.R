## code to prepare hem_fields dataset goes here
library(sf)
library(data.table)

hem_fields <- st_read("/Users/nicolas/Documents/boostao/ssgbm/HEM scripts[41]/HEM_fields.gdb", layer = "HEM_fields_update_elev_threshold_slope_limit") |> names()
hem_fields <- data.table(names = hem_fields)

usethis::use_data(hem_fields, overwrite = TRUE)
