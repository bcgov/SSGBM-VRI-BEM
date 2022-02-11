## code to prepare `DATASET` dataset goes here
library(bcdata)
library(bcmaps)
library(sf)
library(rvest)
library(RPostgres)

# Vegetation Resource Inventory (VRI) 2020
url_vri <- bcdc_get_record("6ba30649-14cd-44ad-a11f-794feed39f40")$resource_df$url
curl::curl_download(url_vri, file.path("./data-raw/cache", basename(url_vri)))
# Load in postgis with psql;
# CREATE DATABASE vribem;
# \c vribem;
# CREATE USER vribem PASSWORD 'vribem';
# CREATE EXTENSION postgis;
# CREATE EXTENSION postgis_raster;
# ogr2ogr -overwrite -progress -f "PostgreSQL" PG:"host=localhost port=5432 dbname=vribem user=vribem password=vribem" --config OGR_ORGANIZE_POLYGONS CCW_INNER_JUST_AFTER_CW_OUTER -gt unlimited VEG_COMP_POLY_AND_LAYER_2020.gdb.zip

# Consolidated Cutblocks (CCL) 2020
url_ccl <- bcdc_get_record("b1b647a6-f271-42e0-9cd0-89ec24bce9f7")$resource_df$url[2]
curl::curl_download(url_ccl, file.path("./data-raw/cache", basename(url_ccl)))

# Digital Elevation Model (1:125000, so 62.5M instead of 25M per pixel but Public)
bc <- st_cast(st_as_sfc( bc_bbox("sf")), "POLYGON")
vrt <- cded(aoi = bc, ask = FALSE)
cache <-  show_cached_files()
cache <- cache[cache$is_dir == TRUE & grepl("cded", cache$file),]$file
tifs <- dir(cache, pattern = "tif$", full.names = TRUE, recursive = TRUE)
dir.create("./data-raw/cache/cded", recursive = TRUE, showWarnings = FALSE)
file.rename(tifs, file.path("./data-raw/cache/cded", basename(tifs)))
unlink(cache, recursive = TRUE)
## Run from cache dir (raster2pgsql is insalled as part of postgis geo bundle and
## should be made available on path by adding PostgreSQL\XX\bin to path)
##
## $ raster2pgsql -d -C -r -F -I -M -Y -e -s 4269 *.tif public.bc_elevation > out.sql
## $ psql --username=vribem --host=localhost -W -f out.sql vribem

# Broad Ecosystem Mapping (BEM) (TEI)
url_tei <- bcdc_get_record("0a83163b-a62f-4ce6-a9a1-21c228b0c0a3")$resource_df$url[3]
url_tei <- file.path(url_tei, fsep = "", html_attr(html_elements(read_html(url_tei), "a"), "href")[6:19])
lapply(url_tei, function(url_tei) {curl::curl_download(url_tei, file.path("./data-raw/cache", basename(url_tei)))})
# ogr2ogr -progress -f "PostgreSQL" PG:"host=localhost port=5432 dbname=vribem user=vribem password=vribem" --config OGR_ORGANIZE_POLYGONS CCW_INNER_JUST_AFTER_CW_OUTER -gt unlimited Dist_Pkg_PEM_Skeena_gdb_201609.zip
# ogr2ogr -progress -f "PostgreSQL" PG:"host=localhost port=5432 dbname=vribem user=vribem password=vribem" --config OGR_ORGANIZE_POLYGONS CCW_INNER_JUST_AFTER_CW_OUTER -gt unlimited TEIS_DataDictionary_20160106_gdb.zip

res <- bcdata::bcdc_search("Broad Ecosystem Mapping")
grep("ecosystem", names(res), value = TRUE)
res[[1]]
res[[2]]

# Now use db to create AOI
conn <- dbConnect(
  drv = Postgres(),
  dbname = "vribem",
  host = "localhost",
  port = 5432,
  user = "vribem",
  password = "vribem"
)
