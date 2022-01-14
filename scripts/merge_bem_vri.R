devtools::load_all()

path <- "C:/Users/nicol/OneDrive/Documents/boostao/Python to R spatial script conversion/"

# read shape files
vri <- st_read(paste0(path, "veg_comp_poly_aoi/veg_comp_poly_aoi.shp"))
bem <- st_read(paste0(path, "tei_long_tbl_aoi/tei_long_tbl_aoi.shp"))

print(Sys.time())
vri <- merge_vri_bem(vri, bem)
print(Sys.time())

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
vri <- vri[which(st_area(vri$geometry) >= set_units(1000, "m^2"))]

# this is what I beleive they do in the python script but I dont understand
#how it would work since the new area of the new singlepart polygon was no computed
#vri <- vri[SHAPE_AREA >= 1000]

# not sure what made the new ID unique in the python script
# might be useless because we dont use arcgis
set(vri, j = "temp_id", value = 1:nrow(vri))

# potentiel other way to do it
attr(vri, "class") <- classes_vri
bem$geometry <- st_make_valid(bem$geometry)
gc()

attr(bem, "class") <- classes_bem


classes_bem <- attr(bem, "class")
setDT(bem)

inter <- st_intersection(vri$geometry, bem$geometry)
areas <- st_area(inter)
inter <- attr(inter, "idx")
intersection_dt <- data.table(bem = inter[, 1], vri = inter[, 2], area = areas)
rm(areas)
rm(inter)
gc()


inter_1219 <- st_intersection(vri$geometry[1219], bem$geometry)

plot(vri$geometry[1219])


intersection_dt[ , .N]

intersection_dt[, bem_temp := bem]
intersection_dt[ ,bem := vri]
intersection_dt[ , vri := bem_temp]

index_dt <- intersection_dt[, .SD$bem[which.max(area)], by = vri]
match_lines <- intersection_dt[, .SD$bem[which.max(area)], by = vri]$V1

cbind(vri[index_dt$vri], bem[index_dt$V1])


for (variable in names(bem)) {
  set(vri, j = variable, value = .subset2(bem, variable)[match_lines])
}


matches <- sf:::CPL_geos_binop(
  vri$geometry,
  bem$geometry,
  "intersects",
  pattern = NA_character_,
  prepared = TRUE)

matches <- mapply(function(x ,i) x[which.max(st_area(vri$geometry[i], bem$geometry[x]))], matches, seq_len(nrow(vri)))

vri_brem <- st_join(x = vri, y = bem, join = st_intersects, largest = TRUE)


# make sure that each vri has at least one bem that overlaps
if (length(is.na(bem_best_match)) > 0) {
  # TODO logger to message
}

# merge bem attributes on vri
for (bem_attribute in names(bem)[-c("geometry")]) {
  set(vri, j = bem_attributes, value = bem[[bem_attribute]][bem_best_match])
}

overl <- st_contains(vri$geometry, st_point(c(896762.5, 967887.5)))
which(sapply(overl, function(x) length(x) >0))

plot(vri$geometry[145319], add = T, color = 'red')
plot(bem$geometry[79742])

plot(vri$geometry[145319])

st_intersection(vri$geometry[145319], bem$geometry[candidate[5]])
plot(bem$geometry[candidate[5]])


for (candidate in matches[[1]])  {
  print(candidate)
  st_intersection(vri$geometry[145319], bem$geometry[candidate])

}



vri <- st_join(x = vri, y = bem, join = st_intersects, largest = TRUE)



matches <- sf:::CPL_geos_binop(
  vri$geometry[145319],
  bem$geometry,
  "intersects",
  pattern = NA_character_,
  prepared = TRUE)

# it says in the python script that polygon count should match the input vri
# but isn't it possible that a polygon with an area below 1000 was remove
(p1 = st_point(c(1,2)))
class(p1)
st_bbox(p1)
(p2 = st_point(c(1,2,3)))
class(p2)
(p3 = st_point(c(1,2,3), "XYM"))
pts = matrix(1:10, , 2)
(mp1 = st_multipoint(pts))
pts = matrix(1:15, , 3)
(mp2 = st_multipoint(pts))
(mp3 = st_multipoint(pts, "XYM"))
pts = matrix(1:20, , 4)
(mp4 = st_multipoint(pts))
pts = matrix(1:10, , 2)
(ls1 = st_linestring(pts))
pts = matrix(1:15, , 3)
(ls2 = st_linestring(pts))
(ls3 = st_linestring(pts, "XYM"))
pts = matrix(1:20, , 4)
(ls4 = st_linestring(pts))
outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
pts = list(outer, hole1, hole2)
(ml1 = st_multilinestring(pts))
pts3 = lapply(pts, function(x) cbind(x, 0))
(ml2 = st_multilinestring(pts3))
(ml3 = st_multilinestring(pts3, "XYM"))
pts4 = lapply(pts3, function(x) cbind(x, 0))
(ml4 = st_multilinestring(pts4))
outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
pts = list(outer, hole1, hole2)
(pl1 = st_polygon(pts))
pts3 = lapply(pts, function(x) cbind(x, 0))
(pl2 = st_polygon(pts3))
(pl3 = st_polygon(pts3, "XYM"))
pts4 = lapply(pts3, function(x) cbind(x, 0))
(pl4 = st_polygon(pts4))
pol1 = list(outer, hole1, hole2)
pol2 = list(outer + 12, hole1 + 12)
pol3 = list(outer + 24)
mp = list(pol1,pol2,pol3)
(mp1 = st_multipolygon(mp))
pts3 = lapply(mp, function(x) lapply(x, function(y) cbind(y, 0)))
(mp2 = st_multipolygon(pts3))
(mp3 = st_multipolygon(pts3, "XYM"))
pts4 = lapply(mp2, function(x) lapply(x, function(y) cbind(y, 0)))
(mp4 = st_multipolygon(pts4))
(gc = st_geometrycollection(list(p1, ls1, pl1, mp1)))
st_geometrycollection() # empty geometry
c(st_point(1:2), st_point(5:6))
c(st_point(1:2), st_multipoint(matrix(5:8,2)))
c(st_multipoint(matrix(1:4,2)), st_multipoint(matrix(5:8,2)))
c(st_linestring(matrix(1:6,3)), st_linestring(matrix(11:16,3)))
c(st_multilinestring(list(matrix(1:6,3))), st_multilinestring(list(matrix(11:16,3))))
pl = list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))
c(st_polygon(pl), st_polygon(pl))
c(st_polygon(pl), st_multipolygon(list(pl)))
c(st_linestring(matrix(1:6,3)), st_point(1:2))
c(st_geometrycollection(list(st_point(1:2), st_linestring(matrix(1:6,3)))),
  st_geometrycollection(list(st_multilinestring(list(matrix(11:16,3))))))
c(st_geometrycollection(list(st_point(1:2), st_linestring(matrix(1:6,3)))),
  st_multilinestring(list(matrix(11:16,3))), st_point(5:6),
  st_geometrycollection(list(st_point(10:11))))





x <-  st_sf(list(x =1, st_sfc(mp1)))
st_cast(x, "POLYGON")
