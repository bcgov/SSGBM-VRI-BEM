library(sf)
library(data.table)

path <- "C:/Users/nicol/OneDrive/Documents/boostao/Python to R spatial script conversion/"

# read shape files
vri <- st_read(paste0(path, "veg_comp_poly_aoi/veg_comp_poly_aoi.shp"))
bem <- st_read(paste0(path, "tei_long_tbl_aoi/tei_long_tbl_aoi.shp"))


# check if teis_id seems already merged on vri
if ("TEIS_ID" %in% names(vri)) {
  # TODO message and exit
}

# check if bem contains duplicate teis_id
if (length(unique(bem$TEIS_ID)) < nrow(bem)) {
  # TODO message and exit
}

# cast multipart polygon to singlepart
vri <- st_cast(vri,"POLYGON")

# use data.table to optimise speed
classes_vri <- attr(vri, "class")
setDT(vri)

# remove feature with area below 1000
vri <- vri[SHAPE_AREA >= 1000]

# not sure what made the new ID unique in the python script
# might be useless because we dont use arcgis
set(vri, j = "temp_id", value = 1:nrow(vri))

# potentiel other way to do it
attr(vri, "class") <- classes_vri
vri_brem <- st_join(x = vri, y = bem, join = st_intersects, largest = TRUE)


# make sure that each vri has at least one bem that overlaps
if (length(is.na(bem_best_match)) > 0) {
  # TODO logger to message
}

# merge bem attributes on vri
for (bem_attribute in names(bem)[-c("geometry")]) {
  set(vri, j = bem_attributes, value = bem[[bem_attribute]][bem_best_match])
}


# it says in the python script that polygon count should match the input vri
# but isn't it possible that a polygon with an area below 1000 was remove
