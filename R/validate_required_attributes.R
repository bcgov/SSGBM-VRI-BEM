#' Validate that required attributes are in the object
#'
#' @param ifc input feature class
#' @param required_attributes names of required attributes
#'
#' @return error if attribute is missing
#' @export
#'

validate_required_attributes <- function(ifc, required_attributes){

  missing_attributes <- setdiff(required_attributes,
                                names(ifc))

  if (length(missing_attributes) > 0) {
    stop("The following attributes were not found : ", paste(missing_attributes, collapse = ", "))
  }
}
