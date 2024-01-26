## code to prepare hem_fields dataset goes here
library(sf)
library(data.table)

hem_fields <- sf::st_read(
  dsn = "./data-raw/HEM/HEM_fields.gdb",
  layer = "HEM_fields_update_elev_threshold_slope_limit"
) |>
  names() |>
  data.table::data.table(names = _)
