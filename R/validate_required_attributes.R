validate_required_attributes <- function(ifc, required_attributes){

  missing_attributes <- setdiff(required_attributes,
                                names(ifc))

  if (length(missing_attributes) > 0) {
    stop("The following attributes were not found in `ifc` : ", paste(missing_attributes, collapse = ", "))
  }
}
