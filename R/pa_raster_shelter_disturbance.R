#' Disturbance around shelter
#' 
#' @param shelter vri-bem sf object.
#' @param disturbance sf object
#' @param depth unit
#' @param rasterize A logical. Should the results be rasterized.
#' @param ... 
depth_calculations <- function(shelter, disturbance, depth = units::as_units("400 m"), rasterize = TRUE, ...) {

  # Check that input feature classes exist

  if (!"W_Shelter_1" %in% names(shelter)) {
    logger::log_error("**** Specified shelter polygon feature class does not contain required field W_Shelter_1.")
  }

  Shelter_EWH <- shelter[shelter$W_Shelter_1 == 1, "W_Shelter_1"] |> # Select shelter polygon
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
    Shelter_EWH <- albers_polys_op(Shelter_EWH_Intact, disturbance, "difference")
    # Select shelters with a greater than depth^2 area
    Shelter_EWH <- Shelter_EWH[which(sf::st_area(Shelter_EWH) >= depth^2L),]
  }

  # Rasterize parameter
  if (isTRUE(rasterize)) {
    f <- rasterize_sf
  } else {
    f <- \(x, ...) identity(x)
  }

  # Return a structure
  structure(
    list(
      "Shelter_EWH" = Shelter_EWH |> f(...),
      "Shelter_EWH_Intact" = Shelter_EWH_Intact |> f(...)
    ),
    depth = depth
  )
}

static_within_from_shelter <- function(shelter, static_forage = shelter, AOI_WMUs_6, dist = units::as_units("100 m")) {

  if (!"W_Shelter_1" %in% names(shelter)) {
    logger::log_error("**** Specified shelter polygon feature class does not contain required field W_Shelter_1.")
  }

  if (!"Static_WFD_All" %in% names(static_forage)) {
    logger::log_error("**** Specified static forage polygon feature class does not contain required field Static_WFD_All.")
  }

  shelters <- shelter[shelter$W_Shelter_1 == 1, "W_Shelter_1"] # Select W_Shelter_1 == 1
  static_forage <- static_forage[static_forage$Static_WFD_All == 1, "Static_WFD_All"]

  intersecting <- shelters |>
    sf::st_buffer(dist = dist) |> # Buffer with dist
    albers_polys_op(shelters, "difference") |> # Create dist band around shelters
    albers_polys_op(static_forage, "intersect") |> # Compute intersecting
    terra::vect() # Turn into terra vect

  aoi6_r <- terra::rast(AOI_WMUs_6)

  # TODO: Continue. Intersect rast ...
  terra::rasterize(intersecting, aoi6_r, field = 1, background = 0)

}

albers_polys_op <- function(x, y, op) {

  res <- sf::st_sfc(crs = 3005) # Results set
  quickreturn <- switch(op, "difference" = x, "intersect" = res)

  x <- sf::st_geometry(x) # Extract x geometry
  if (!length(x)) return(quickreturn) # Return if empty
  y <- sf::st_geometry(y) |> # Extract y geometry
    sf::st_cast("POLYGON", warn = FALSE) |> # Recast to POLYGON
    sf::st_crop(sf::st_bbox(x)) # Crop to x bbox to reduce compute area
  if (!length(y)) return(quickreturn) # Return if empty

  # Compute intersects
  inter <- sf:::CPL_geos_binop(x, y, "intersects", pattern = NA_character_, prepared = TRUE)

  # Loop on x
  for (i in seq_along(x)) {
    idx <- inter[[i]] # get indexes
    if (length(idx) > 0L) {
      x1 <- x[i]
      area_y <- y[idx]
      new_geo <- sf:::CPL_geos_union(area_y) |> # Combine y intersecting polys
        sf::st_sfc(crs = 3005) |> # reclass to sfc
        sf::st_crop(sf::st_bbox(x1)) |>  # Crop to x1 bbox to reduce compute area
        sf:::CPL_geos_op2(op, x1, sfcy = _) |> # Compute op
        sf::st_sfc(crs = 3005) |> # reclass to sfc
        sf::st_cast("POLYGON") # Recast to POLYGON
      if (length(new_geo)) {
        res <- c(res, new_geo) # Append to results set
      }
    }
  }

  return(res)

}
