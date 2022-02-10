#' Read and format the unique ecosystem dt
#'
#' @param file File name in working directory, path to file (passed through path.expand for convenience), or a URL starting http://, file://, etc. Compressed files with extension ‘.gz’ and ‘.bz2’ are supported if the R.utils package is installed.
#' @return data.table object
#' @import data.table
#' @export
read_unique_ecosystem_dt <- function(file) {
  unique_ecosystem_dt <- fread(file)
  return(format_unique_ecosystem_dt(unique_ecosystem_dt))
}
