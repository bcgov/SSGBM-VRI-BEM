#' Rename geometry attribute for sf object
#'
#' @param g sf object
#' @param name name of new sf column
#' @return sf object
#' @export
rename_geometry <- function(g, name){
  current = attr(g, "sf_column")
  names(g)[names(g)==current] = name
  st_geometry(g)=name
  g
}
