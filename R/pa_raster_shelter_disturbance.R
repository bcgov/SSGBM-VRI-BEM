#' Disturbance around shelter
#'
#' @param shelter vri-bem sf object.
#' @param disturbance sf object
#' @param depth unit
#' @param rasterize A logical. Should the results be rasterized.
#' @param ...
pa_Shelter_EWH <- function(shelter, disturbance, depth = units::as_units("400 m")) {

  # Check that input feature classes exist

  if (!"W_Shelter_1" %in% names(shelter)) {
    logger::log_error("**** Specified shelter polygon feature class does not contain required field W_Shelter_1.")
  }

  threshold <- (depth^2L) * 0.66 # To account for difference between a polygon and a raster

  Shelter_EWH <- shelter[shelter$W_Shelter_1 == 1, "W_Shelter_1"] |> # Select shelter polygon
    sf::st_union() |> # combine intersecting shelters polygons
    sf::st_cast("POLYGON", warn = FALSE) # recast to single polygons

  # Select shelters with a greater than depth^2 area
  Shelter_EWH <- Shelter_EWH[which(sf::st_area(Shelter_EWH) >= threshold),]

  # If linear disturbance are provided, remove from shelters (assuming pre buffered)
  if (!missing(disturbance)) {
    # If a path is provided, read it
    if (inherits(disturbance, "character") && file.exists(disturbance)) {
      disturbance <- sf::read_sf(disturbance)
      if (sf::st_crs(disturbance) != sf::st_crs(sf::st_sfc(crs = 3005))) {
        disturbance <- disturbance |> sf::st_transform(3005)
      }
    }
    # Remove disturbance geometry from shelter polygons
    Shelter_EWH <- albers_polys_op(Shelter_EWH, disturbance, "difference")
    # Select shelters with a greater than depth^2 area
    Shelter_EWH <- Shelter_EWH[which(sf::st_area(Shelter_EWH) >= threshold),]
  }

  return(Shelter_EWH)

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
    albers_polys_op(static_forage, "intersection") |> # Compute intersecting
    terra::vect() # Turn into terra vect

  aoi6_r <- terra::rast(AOI_WMUs_6)

  # TODO: Continue. Intersect rast ...
  terra::rasterize(intersecting, aoi6_r, field = 1, background = 0)

}

albers_polys_op <- function(x, y, op) {

  res <- sf::st_sfc(crs = 3005) # Results set
  quickreturn <- switch(op, "difference" = x, "intersection" = res)

  x <- sf::st_geometry(x) # Extract x geometry
  if (!length(x)) return(quickreturn) # Return if empty
  y <- sf::st_geometry(y) |> # Extract y geometry
    sf::st_cast("POLYGON", warn = FALSE) |> # Recast to POLYGON
    sf::st_crop(sf::st_bbox(x)) # Crop to x bbox to reduce compute area
  if (!length(y)) return(quickreturn) # Return if empty

  # Compute intersects
  inter <- sf:::CPL_geos_binop(x, y, "intersects", pattern = NA_character_, prepared = TRUE)

  algo <- function(i, inter, x, y, ...) {
    res <- sf::st_sfc(crs = 3005)
    idx <- inter[[i]]
    x1 <- x[i]
    if (length(idx) > 0L) {
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
    } else if (op %in% "difference") {
      res <- c(res, x1)
    }
    return(res)
  }

  res <- do.call(c, parlapply()(seq_along(x), algo, inter = inter, x = x, y = y, future.envir  = new.env()))

  return(res)

}
