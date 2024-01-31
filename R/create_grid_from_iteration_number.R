#' @importFrom sf st_make_grid st_as_sfc st_as_text
#' @noRd
create_grid_from_iteration_number <- function(n_iterations, wkt_filter, as.text = TRUE) {

  aoi <- sf::st_as_sfc(wkt_filter)
  side <- sqrt(n_iterations)
  n <- ceiling(side)
  if (n > side) {
    warning("Square grid expanded to %sx%s [%s] to fit n_iterations [%s]." |> sprintf(n,n,n*n,n_iterations))
  }
  grid <-  sf::st_make_grid(aoi, n = ceiling(sqrt(n_iterations)))

  if (as.text) {
    grid <- sf::st_as_text(grid)
  }

  return(grid)

}
