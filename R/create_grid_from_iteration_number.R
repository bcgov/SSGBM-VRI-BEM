create_grid_from_iteration_number <- function(n_iterations, wkt_filter, as.text = TRUE) {
  sqrt_n_iterations <- sqrt(n_iterations)
  if ((sqrt_n_iterations %% 1) == 0) {
    grid <-  st_make_grid(st_as_sfc(wkt_filter), n = c(sqrt_n_iterations,sqrt_n_iterations))
  }
  else {
    base <- ceiling(sqrt_n_iterations)
    grid <- st_make_grid(st_as_sfc(wkt_filter), n = c(base, base/n_iterations))
  }

  if (as.text) {
    return(grid_text <- st_as_text(grid))
  }
  else {
    return(grid)
  }

}
