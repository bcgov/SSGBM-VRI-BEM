library(bcdata)
library(sf)

# BCDATA Dir for development
bcdata_dir <- "./data-raw/BCDATA"
dir.create(bcdata_dir, showWarnings = FALSE)

# Look for the right resource
find_url <- function(pattern) {
  search_results <- bcdata::bcdc_search(pattern)
  record_id <- resource_id <- NULL
  for (result in search_results) {
    for (i in seq_len(nrow(result[["resource_df"]]))) {
      if (grepl(pattern, result[["resource_df"]][["name"]][i], ignore.case = TRUE)) {
        return(result[["resource_df"]][["url"]][i])
      }
    }
  }
}

# Skeena extent
skeena_extent <- bcdata::bcdc_query_geodata("dfc492c0-69c5-4c20-a6de-2c9bc999301f") |>
  bcdata::filter(REGION_NAME == "Skeena Natural Resource Region") |>
  bcdata::collect() |>
  terra::ext()

# Testing Area of Interest
testing_aoi <- terra::vect("POLYGON ((941827.7 932215.1, 1065018 932215.1, 1065018 1016988, 941827.7 1016988, 941827.7 932215.1))")

# Vegetation Resource Inventory (VRI) - Forest Vegetation Composite Rank 1 Layer
pattern <- "VEG_COMP_LYR_R1_POLY"
vri_url <- find_url(pattern)
vri_out <- file.path(bcdata_dir, basename(vri_url))
if (!file.exists(vri_out)) {
  cli <- bcdata:::bcdc_http_client(vri_url)
  cli$get(disk = vri_out)
}
if (grepl("zip$", vri_out)) {
  if (!dir.exists(tools::file_path_sans_ext(vri_out))) {
    unzip(vri_out, exdir = bcdata_dir)
  }
  vri_out <- tools::file_path_sans_ext(vri_out)
}
vri <- terra::vect(vri_out, filter = testing_aoi)

# Broad Ecosystem Inventory

wetlands_id <- bcdata::bcdc_search("Freshwater Atlas Wetlands", n = 1)[[1]][["id"]]

# Wetlands ("93b413d8-1840-4770-9629-641d74bd1cc6")
ifelse(
  wetlands_id == "93b413d8-1840-4770-9629-641d74bd1cc6",
  "Wetlands id found in search match the one in the package",
  "Wetlands id found in search does not match the one in the package"
)
wetlands <- bcdata::bcdc_query_geodata(record = wetlands_id) |>
  bcdata::filter(bcdata::INTERSECTS(sf::st_as_sf(testing_aoi)))

#
