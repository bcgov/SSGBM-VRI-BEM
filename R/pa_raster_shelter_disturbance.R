# disturbance <- "../SSGBM-VRI-BEM-data/Q12024/Disturbance" |> sf::st_read()
# vri_bem <- readRDS("../SSGBM-VRI-BEM-data/vri_bem_step5.rds")

###############

shelter_calculations <- function(vri_bem, disturbance, depth = units::as_units("400 m"), rasterize = TRUE) {

  # Check that input feature classes exist

  if (!"W_Shelter_1" %in% names(vri_bem)) {
    logger::log_error("**** Specified shelter polygon feature class does not contain required field W_Shelter_1.")
  }

  Shelter_EWH <- vri_bem[vri_bem$W_Shelter_1 == 1, "W_Shelter_1"] |> # Select shelter polygon
    sf::st_union() |> # combine intersecting shelters polygons
    sf::st_cast("POLYGON", warn = FALSE) # recast to single polygons

  # Select shelters with a greater than depth^2 area
  Shelter_EWH_Intact <- Shelter_EWH[which(sf::st_area(Shelter_EWH) >= depth^2L),]

  # If linear disturbance are provided, remove from shelters (assuming pre buffered)
  if (!missing(disturbance)) {
    # If a path is provided, read it
    if (inherits(disturbance, "character") && file.exists(disturbance)) {
      disturbance <- sf::read_sf(disturbance)
    }
    # Remove disturbance geometry from shelter polygons
    Shelter_EWH <- remove_polys(Shelter_EWH_Intact, disturbance)
  }

  # Select shelters with a greater than depth^2 area
  Shelter_EWH <- Shelter_EWH[which(sf::st_area(Shelter_EWH) >= depth^2L),]

  # Rasterize parameter
  if (isTRUE(rasterize)) {
    f <- rasterize_sf
  } else {
    f <- identity
  }

  # Return a structure
  structure(
    list(
      "Shelter_EWH" = Shelter_EWH |> f(),
      "Shelter_EWH_Intact" = Shelter_EWH_Intact |> f()
    ),
    depth = depth
  )
}

remove_polys <- function(x, y) {

  x <- sf::st_geometry(x) # Extract x geometry
  if (!length(x)) return(x) # Return if empty
  y <- sf::st_geometry(y) |> # Extract y geometry
    sf::st_cast("POLYGON", warn = FALSE) |> # Recast to POLYGON
    sf::st_crop(sf::st_bbox(x)) # Crop to x bbox to reduce compute area
  if (!length(y)) return(x) # Return if empty

  # Compute intersects
  inter <- sf:::CPL_geos_binop(x, y, "intersects", pattern = NA_character_, prepared = TRUE)

  res <- sf::st_sfc(crs = albers) # Results set

  # Loop on x
  for (i in seq_len(length(x))) {
    idx <- inter[[i]] # get indexes
    if (length(idx) > 0L) {
      x1 <- x[i]
      area_y <- y[idx]
      new_geo <- sf:::CPL_geos_union(area_y) |> # Combine y intersecting polys
        sf::st_sfc(crs = albers) |> # reclass to sfc
        sf::st_crop(sf::st_bbox(x1)) |>  # Crop to x1 bbox to reduce compute area
        sf:::CPL_geos_op2("difference", x1, sfcy = _) |> # Compute difference
        sf::st_sfc(crs = albers) |> # reclass to sfc
        sf::st_cast("POLYGON") # Recast to POLYGON
      if (length(new_geo)) {
        res <- c(res, new_geo) # Append to results set
      }
    }
  }
  return(res)
}

rasterize_sf <- function(x, crs = albers, resolution = units::as_units("100 m"), extent = terra::ext(159587.5, 1881187.5, 173787.5, 1748187.5), field = 1, background = 0) {

  ## Smaller extent
  # extent = terra::ext(1020387.5, 1030387.5, 960987.5, 970987.5)
  ## Full extents would be "159587.5, 1881187.5, 173787.5, 1748187.5"

  y <- terra::rast(
    crs = crs,
    resolution = resolution,
    extent = extent
  )

  x <- terra::vect(x)

  return(terra::rasterize(x, y, field = field, background = background))

}
