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

  x_y_intersection <- sf::st_intersection(x, y)
  if(nrow(x_y_intersection)==0){
    x_y_intersection <- x[0,]
  } else{
    x_y_intersection <- sanitize_geometry(x_y_intersection,tolerance)
  }

  if (!is.null(label) && nrow(x_y_intersection)>0) {
    for (nm in names(label)) {
      x_y_intersection[[nm]] <- rep(label[[nm]], nrow(x_y_intersection))
    }
  }

  x_y_diff <- erase_geometry(x, y)
  if(nrow(x_y_diff)>0){
    x_y_diff <-sanitize_geometry(x_y_diff,tolerance)
  }

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
    erased <- lapply(idx, function(i) {
      diff_result <- sf:::CPL_geos_op2("difference", tg[i], sf::st_union(eg[intersections[[i]]]))

    sfc_result <- sf::st_sfc(diff_result,crs=crs)

    cleaned <- sf::st_collection_extract(sfc_result,"POLYGON")

    if(length(cleaned) == 0) {return(empty_g[[1]])}

    return(sf::st_cast(cleaned,"MULTIPOLYGON")[[1]])
    })

  } else {
    erased <- parallel::mclapply(idx,
      function(i) {
        diff_result <- sf:::CPL_geos_op2("difference", tg[i], sf::st_union(eg[intersections[[i]]]))

        sfc_result <- sf::st_sfc(diff_result,crs=crs)

        cleaned <- sf::st_collection_extract(sfc_result,"POLYGON")

        if(length(cleaned) == 0) {return(empty_g[[1]])}

        return(sf::st_cast(cleaned,"MULTIPOLYGON")[[1]])},
      mc.cores = getOption("mc.cores",parallel::detectCores()))
  }

  #Make sure output list has same # as original geometries
  outg <- vector("list",length=length(tg))
  outg[idx] <- erased
  outg[inv_idx] <- tg[inv_idx]

  new_geom <-sf::st_sfc(outg,crs=crs)

  #Keep rows where geometry is not empty and matches new_geom length
  non_empty_idx <-which(!sf::st_is_empty(new_geom))

  res <- target[non_empty_idx, ,drop=FALSE]

  sf::st_geometry(res) <- new_geom[non_empty_idx]

  return(res)

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
#' @param ... Not used.
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

#' @noRd
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

#' @noRd
winos <- function() {
  isTRUE(Sys.info()["sysname"] == "Windows")
}

#' @noRd
multicpu <- function() {
  isTRUE(getOption("SSGBM.VRI.BEM.use.parallel", TRUE))
}

#' @importFrom future plan multisession
#' @importFrom parallel mclapply detectCores
#' @importFrom future.apply future_lapply
#' @noRd
parlapply <- function() {
  if (!multicpu()) {
    lapply
  } else if (winos()) {
    options("future.rng.onMisuse" = "ignore")
    future::plan(future::multisession, workers = parallel::detectCores())
    future.apply::future_lapply
  } else {
    options("mc.cores" = parallel::detectCores())
    parallel::mclapply
  }
}

#' @importFrom future plan multisession
#' @importFrom parallel mcmapply
#' @importFrom future.apply future_mapply
#' @noRd
parmapply <- function() {
  if (!multicpu()) {
    mapply
  } else if (winos()) {
    options("future.rng.onMisuse" = "ignore")
    future::plan(future::multisession, workers = parallel::detectCores())
    future.apply::future_mapply
  } else {
    options("mc.cores" = parallel::detectCores())
    parallel::mcmapply
  }
}
