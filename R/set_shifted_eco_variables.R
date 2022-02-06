#' shift eco variables
#'
#' @param input_dt data.table input data to be modified by reference
#' @param boolean_filter boolean vector that identify which lines (true or false for each lines) of the data we want to apply the shifting
#' @param shift_pattern list of two elements, second element is a vector that represent the number of the eco variables that are going to replace the number in the vector of the first element
#' @param integer_variables_1 character vector that represent the name of the variables of type integer with index number 1 we want to shift
#' @param character_variables_1 character vector that represent the name of the variables of type character with index number 1 we want to shift
#' @import data.table
set_shifted_eco_variables <- function(input_dt, boolean_filter, shift_pattern, integer_variables_1 = c("TREE_C1", "SHRUB_C1"), character_variables_1 = c("REALM_1", "GROUP_1", "CLASS_1", "KIND_1", "SITE_S1", "SITEAM_S1A",
                                                                                                                                                         "SITEAM_S1B", "SITEAM_S1C", "SITEAM_S1D", "SITEMC_S1", "SITE_M1A", "SITE_M1B", "STRCT_S1", "STRCT_M1", "STAND_A1", "SERAL_1",
                                                                                                                                                         "DISTCLS_1", "DISTSCLS_1", "DISSSCLS_1", "SECL_1",
                                                                                                                                                         "SESUBCL_1", "COND_1", "VIAB_1")) {

  # verify if length of filter is the same as number of rows of data
  if (nrow(input_dt) != length(boolean_filter)) {
    stop(paste0("the filter boolean needs to be the same lenght as the number of row of the input dt which is ", nrow(input_dt)))
  }

  # replace NA by 0 so its easier to work with (later we don't want combination to end up being NA see line 35)
  shift_pattern[[1]][which(is.na(shift_pattern[[1]]))] <- 0
  shift_pattern[[2]][which(is.na(shift_pattern[[2]]))] <- 0

  # compute which lines needs to be updated
  which_lines <- which(boolean_filter)

  # create other variables names from arguments
  character_variables_2 <- sub("1", "2", character_variables_1)
  character_variables_3 <- sub("1", "3", character_variables_1)

  integer_variables_2 <- sub("1", "2", integer_variables_1)
  integer_variables_3 <- sub("1", "3", integer_variables_1)

  # create combination, it works because we have less than 10 variables (right now it only goes to 3)
  # and we make use of the natural recycling in the + function to create all the shift combination
  shift_combination <- shift_pattern[[1]] * 10 + shift_pattern[[2]]

  # initialize all final character and integer variable names
  all_to_character_variables <- character(length(character_variables_1) * length(unique(shift_pattern[[2]])))
  all_to_integer_variables <- character(length(integer_variables_1) * length(unique(shift_pattern[[2]])))

  # create NA temp variables to simplify the code
  set(input_dt, j = "temp_integer_NA", value = NA_integer_)
  set(input_dt, j = "temp_character_NA", value = NA_character_)

  # compute temp variables for all combinations
  # we cannot assign the new result in the loop, because we want to allow shift pattern such as list(c(1,2), c(2,1)) where the variables_1 and variables_2 are switch
  for (combination in shift_combination) {
    # separate created combination into from and to vectors
    from_number <- combination %% 10
    to_number <- (combination - to_number)/10

    # find which character variables to use
    from_character_variables <- fcase(from_number == 1, character_variables_1, from_number == 2, character_variables_2, from_number == 3, character_variables_3, default = "temp_character_NA")
    to_character_variables_temp <- paste0("temp_", fcase(to_number == 1, character_variables_1, to_number == 2, character_variables_2, to_number == 3, character_variables_3, default = "temp_character_NA"))
    all_to_character_variables <- c(all_to_character_variables, to_character_variables_temp)

    # find which integer variables to use
    from_integer_variables <- fcase(from_number == 1, integer_variables_1, from_number == 2, integer_variables_2, from_number == 3, integer_variables_3, default = "temp_integer_NA")
    to_integer_variables_temp <- paste0("temp_", fcase(to_number == 1, integer_variables_1, to_number == 2, integer_variables_2, from_number == 3, integer_variables_3, default = "temp_integer_NA"))
    all_to_integer_variables <- c(all_to_integer_variables, to_integer_variables_temp)

    set(input_dt, i = which_lines, j = to_character_variables_temp, value = input_dt[which_lines, from_character_variables])
    set(input_dt, i = which_lines, j = to_integer_variables_temp, value = input_dt[which_lines, from_integer_variables])
  }

  # remove leading temp_ in created variables
  final_character_variables <- sapply(all_to_character_variables, function(x) substr(x, 6, length(x)))
  final_integer_variables <- sapply(all_to_integer_variables, function(x) substr(x, 6, length(x)))

  # assign new computed value in variables
  for (i in seq.int(along.with = all_to_character_variables)) {
    set(input_dt, i = which_lines, j = final_character_variables[i], value = input_dt[[all_to_character_variables[i]]][which_lines])
  }
  for (i in seq.int(along.with = final_integer_variables)) {
    set(input_dt, i = which_lines, j = final_integer_variables[i], value = input_dt[[final_integer_variables[i]]][which_lines])
  }


  set(input_dt, i = which_lines, j = final_integer_variables, value = input_dt[which_lines, all_to_integer_variables])

  # removed temp variables
  set(input_dt, i = which_lines, j = c(all_to_character_variables, all_to_integer_variables, "temp_character_NA", "temp_integer_NA"), value = NULL)

}
