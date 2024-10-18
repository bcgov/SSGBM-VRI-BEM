#' Rename geometry attribute for sf object
#'
#' @param g sf object
#' @param name name of new sf column
#' @return sf object
#' @export
rename_geometry <- function(g, name){
  current <- attr(g, "sf_column")
  names(g)[names(g)==current] <- name
  st_geometry(g) <- name
  g
}

#' Union merge between CCB attributes and VRI features
#'
#' Performs a Union merge between y attributes on x features
#'
#' @param x sf object 
#' @param y sf object
#' @return sf object that represent the intersections of x and y
#' @export
merge_geometry <- function(x, y, tolerance = units::as_units("100 m2"), label = NULL) {

  if (!inherits(x, "sf") || !inherits(y, "sf")) {
    stop("Both x and y must be sf objects")
  }

  if (!sf::st_crs(x) == sf::st_crs(y)) {
    stop("Both x and y must have the same crs")
  }

  if (!nrow(y)) {
    return(x |> sanitize_geometry(tolerance))
  }
      
  sf::st_agr(x) <- "constant"
  sf::st_agr(y) <- "constant"

  x_y_intersection <- sf::st_intersection(x, y) |>
    sanitize_geometry(tolerance)

  if (!is.null(label)) {
    for (nm in names(label)) {
      x_y_intersection[[nm]] <- rep(label[[nm]], nrow(x_y_intersection))
    }
  }

  x_y_diff <- erase_geometry(x, y) |>
    sanitize_geometry(tolerance)

  res <- dplyr::bind_rows(x_y_intersection, x_y_diff)

  if (sf::st_crs(res) != albers) {
    res <- bcmaps::transform_bc_albers(res)
  }

  return(res)
}

#' Sanitize geometry
#'
#' @param x sf object
#' @return sf object
#' @export
sanitize_geometry <- function(x, tolerance) {
  sf::st_make_valid(x) |>
    sf::st_set_agr("constant") |>
    sf::st_collection_extract() |>
      sf::st_set_agr("constant") |>
    sf::st_cast("POLYGON") |>
    sf::st_make_valid() |>
    {\(x) x[which(sf::st_area(x) > tolerance),]}()
}

#' Erase geometry from target sf object
#'
#' @param target sf object
#' @param erase sf object
#' @return sf object
#' @export
erase_geometry <- function(target, erase) {

  if (nrow(erase) < 500L) {
    return(sf::st_difference(target, sf::st_union(sf::st_geometry(erase))))
  }

  # Extract geometry
  crs <- sf::st_crs(target)
  eg <- sf::st_geometry(erase)
  sf::st_crs(eg) <- NA
  tg <- sf::st_geometry(target)
  sf::st_crs(tg) <- NA

  # Find intersections using the spatial index
  intersections <- sf::st_intersects(tg, eg)
  idx <- which(lengths(intersections) > 0)
  inv_idx <- which(lengths(intersections) == 0)

  # Group overlapping erase polygons (could be run multi cpu to make it faster on Linux)
  empty_g <- sf::st_as_sfc("POLYGON EMPTY")
  if (Sys.info()[["sysname"]] == "Windows") {
    erased <- lapply(idx, function(i) sf:::CPL_geos_op2("difference", tg[i], sf::st_union(eg[intersections[[i]]])))
  } else {
    erased <- parallel::mclapply(idx, 
      function(i) {
        res <- sf::st_sfc(sf:::CPL_geos_op2("difference", tg[i], sf::st_union(eg[intersections[[i]]])))
        if (!length(res)) res <- empty_g
        return(res)
      },
      mc.cores = getOption("mc.cores", parallel::detectCores())
    )
  }

  outg <- list()
  outg[idx] <- erased
  outg[inv_idx] <- lapply(inv_idx, \(i) tg[i])

  res <- sf::st_set_geometry(target, sf::st_sfc(do.call(c, outg), crs = crs))
  return(res[!sf::st_is_empty(res),])

}

#' @noRd
#' @importFrom stringdist stringdist
match_labels <- function(x, y) {
  s1 <- names(x)
  s2 <- names(y)
  res <- character(length(s1))
  first_pass <- lapply(tolower(s1), \(x) head(s2[which(stringdist::stringdist(x,tolower(s2)) == 0)],1))
  i <- which(lengths(first_pass) > 0)
  res[i] <- first_pass[i] |> unlist()
  for (j in seq_along(s1)[-i]) {
    d <- stringdist::stringdist(tolower(s1[j]),tolower(s2[-i]))
    tol <- nchar(s1[j]) * 1.33
    wm <- which.min(d)
    cur_i <- wm[d[wm] < tol]
    if (length(cur_i)) {
      res[j] <- head(s2[-i][cur_i],1)
      i <- c(i,seq_along(s1)[-i][cur_i])
    } else {
      res[j] <- s1[j]
    }
  }
  return(res)
}


#' Rasterize an sf object
#' 
#' @param x sf object
#' @param crs Target crs
#' @param resolution Target resolution as unit.
#' @param extent Target raster extent.
#' @param field character or numeric. If field is a character, it should a variable name in `x`.
#' If field is numeric it typically is a single number or a vector of length `nrow(x)`.
#' The values are recycled to `nrow(x)`.
#' @param background numeric. Value to put in the cells that are not covered by any
#' of the features of `x`. Default is `NA`.
#' @param ... 
#' @export
rasterize_sf <- function(x, crs = albers, resolution = units::as_units("100 m"), extent = terra::ext(159587.5, 1881187.5, 173787.5, 1748187.5), field = 1, background = 0, ...) {

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