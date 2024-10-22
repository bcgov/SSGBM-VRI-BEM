#' Effective Winter Habitat with or without disturbance
#'
#' @param shelter vri-bem sf object.
#' @param disturbance sf object
#' @param dist unit
#' @param rasterize A logical. Should the results be rasterized.
#' @rdname pa
#' @export
pa_Shelter_EWH <- function(shelter, disturbance, dist = units::as_units("400 m")) {

  # Check that input feature classes exist

  if (!"W_Shelter_1" %in% names(shelter)) {
    logger::log_error("**** Specified shelter polygon feature class does not contain required field W_Shelter_1.")
  }

  threshold <- (dist^2L) * 0.66 # To account for difference between a polygon and a raster

  Shelter_EWH <- shelter[shelter$W_Shelter_1 == 1, "W_Shelter_1"] |> # Select shelter polygon
    sf::st_union() |> # combine intersecting shelters polygons
    sf::st_cast("POLYGON", warn = FALSE) # recast to single polygons

  # Select shelters with a greater than threshold
  Shelter_EWH <- Shelter_EWH[which(sf::st_area(Shelter_EWH) >= threshold), ]

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
    # Select shelters with a greater than threshold
    Shelter_EWH <- Shelter_EWH[which(sf::st_area(Shelter_EWH) >= threshold),]
  }

  return(Shelter_EWH)

}

#' Static forage within distance from shelters
#'
#' @param AOI_WMUs_6 sf object
#' @param static_forage sf object
#' @rdname pa
#' @export
pa_Shelter_WFD <- function(shelter, static_forage = shelter, AOI_WMUs_6, dist = units::as_units("500 m")) {

  if (!"W_Shelter_1" %in% names(shelter)) {
    logger::log_error("**** Specified shelter polygon feature class does not contain required field W_Shelter_1.")
  }

  if (!"Static_WFD_All" %in% names(static_forage)) {
    logger::log_error("**** Specified static forage polygon feature class does not contain required field Static_WFD_All.")
  }

  shelters <- shelter[shelter$W_Shelter_1 == 1, "W_Shelter_1"] |> sf::st_geometry() # Select W_Shelter_1 == 1
  static_forage <- static_forage[static_forage$Static_WFD_All == 1, "Static_WFD_All"] |> sf::st_geometry()

  Shelter_WFD <- shelters |>
    sf::st_buffer(dist = dist) |> # Buffer with dist
    albers_polys_op(shelters, "difference") |> # Create dist band around shelters
    albers_polys_op(static_forage, "intersection") |> # Compute intersecting
    albers_polys_op(AOI_WMUs_6, "intersection") # Compute intersecting

  return(Shelter_WFD)

}

#' Dash security distance
#'
#' @rdname pa
#' @export
pa_dash_security <- function(shelter, AOI_WMUs_6, dist = units::as_units("100 m")) {

  for (f in c("W_Shelter_1", "Dynamic_WFD_All", "Security_1")) {
    if (!f %in% names(shelter)) {
      logger::log_error("**** Specified shelter polygon feature class does not contain required field `%s`." |> sprintf(f))
    }
  }

  security <- shelter[shelter$Security_1 == 1, "Security_1"] |> sf::st_geometry()
  dymamic_forage <- shelter[shelter$Dynamic_WFD_All == 1, "Dynamic_WFD_All"] |> sf::st_geometry()

  dash_security <- security |>
    sf::st_buffer(dist = dist) |> # Buffer with dist
    albers_polys_op(dymamic_forage, "intersection") |> # Compute intersecting
    albers_polys_op(AOI_WMUs_6, "intersection") # Compute intersecting

  return(dash_security)

}
