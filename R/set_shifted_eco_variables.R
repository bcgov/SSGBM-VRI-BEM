#' shift eco variables
#'
#' @param input_dt data.table input data to be modified by reference
#' @param i numeric vector that identify which lines of the data we want to apply the shifting
#' @param shift_pattern list of vectors , each vector represent a shift, the first element of the vector is which variable we want to change and the second is what to use to feed the first variable
#' @param integer_variables_1 character vector that represent the name of the variables of type integer with index number 1 we want to shift
#' @param character_variables_1 character vector that represent the name of the variables of type character with index number 1 we want to shift
#' @import data.table
set_shifted_eco_variables <- function(input_dt, i, shift_pattern, integer_variables_1 = c("TREE_C1", "SHRUB_C1"), character_variables_1 = c("BEUMC_S1","REALM_1", "GROUP_1", "CLASS_1", "KIND_1", "SITE_S1", "SITEAM_S1A",
                                                                                                                                                         "SITEAM_S1B", "SITEAM_S1C", "SITEAM_S1D", "SITEMC_S1", "SITE_M1A", "SITE_M1B", "STRCT_S1", "STRCT_M1", "STAND_A1", "SERAL_1",
                                                                                                                                                         "DISTCLS_1", "DISTSCLS_1", "DISSSCLS_1", "SECL_1",
                                                                                                                                                         "SESUBCL_1", "COND_1", "VIAB_1", "FORESTED_1")) {

  # if the filter result in zero lines we simply exit the function
  # we use !isTRUE so that it will also quit if the i contains no TRUE value but some NA
  if (!isTRUE(any(i))) {
    return()
  }

  # create other variables names from arguments
  character_variables_2 <- sub("1", "2", character_variables_1)
  character_variables_3 <- sub("1", "3", character_variables_1)

  integer_variables_2 <- sub("1", "2", integer_variables_1)
  integer_variables_3 <- sub("1", "3", integer_variables_1)

  # initialize all final character and integer variable names
  all_to_character_variables <- character(0)
  all_to_integer_variables <- character(0)

  # create NA temp variables to simplify the code
  set(input_dt, j = "temp_integer_NA", value = NA_integer_)
  set(input_dt, j = "temp_character_NA", value = NA_character_)

  # compute temp variables for all combinations
  # we cannot assign the new result in the loop, because we want to allow shift pattern such as list(c(1,2), c(2,1)) where the variables_1 and variables_2 are switch
  for (combination in shift_pattern) {
    # separate created combination into from and to vectors
    from_number <- combination[2]
    to_number <- combination[1]
    if(is.na(from_number)) {
      from_number <- 0
    }
    if(is.na(to_number)) {
      to_number <- 0
    }
    # find which character and integer variables to use
    if (from_number == 1) {
      from_character_variables <- character_variables_1
      from_integer_variables <- integer_variables_1
    } else {
      if (from_number == 2) {
        from_character_variables <- character_variables_2
        from_integer_variables <- integer_variables_2
      } else {
        if (from_number == 3) {
          from_character_variables <- character_variables_3
          from_integer_variables <- integer_variables_3
        } else {
          from_character_variables <- rep("temp_character_NA", length(character_variables_1))
          from_integer_variables <- rep("temp_integer_NA", length(integer_variables_1))
        }
      }
    }

    if (to_number == 1) {
      to_character_variables_temp <- paste0("temp_", character_variables_1)
      to_integer_variables_temp <- paste0("temp_", integer_variables_1)
    } else {
      if (to_number == 2) {
        to_character_variables_temp <- paste0("temp_", character_variables_2)
        to_integer_variables_temp <- paste0("temp_", integer_variables_2)
      } else {
        if (to_number == 3) {
          to_character_variables_temp <- paste0("temp_", character_variables_3)
          to_integer_variables_temp <- paste0("temp_", integer_variables_3)
        } else {
          to_character_variables_temp <- rep("temp_temp_character_NA", length(character_variables_1))
          to_integer_variables_temp <- rep("temp_temp_integer_NA", length(integer_variables_1))
        }
      }
    }

    all_to_character_variables <- c(all_to_character_variables, to_character_variables_temp)

    all_to_integer_variables <- c(all_to_integer_variables, to_integer_variables_temp)

    set(input_dt, i = i, j = c(to_character_variables_temp, to_integer_variables_temp), value = input_dt[i, c(from_character_variables, from_integer_variables), with = FALSE])
  }

  # remove leading temp_ in created variables
  final_character_variables <- sapply(all_to_character_variables, function(x) substr(x, 6, nchar(x)))
  final_integer_variables <- sapply(all_to_integer_variables, function(x) substr(x, 6, nchar(x)))

  # assign new computed value in variables
  for (i in seq.int(along.with = all_to_character_variables)) {
    set(input_dt, i = i, j = final_character_variables[i], value = input_dt[[all_to_character_variables[i]]][i])
  }
  for (i in seq.int(along.with = final_integer_variables)) {
    set(input_dt, i = i, j = final_integer_variables[i], value = input_dt[[final_integer_variables[i]]][i])
  }

  # removed temp variables
  set(input_dt, j = c(all_to_character_variables, all_to_integer_variables, "temp_character_NA", "temp_integer_NA"), value = NULL)

}
