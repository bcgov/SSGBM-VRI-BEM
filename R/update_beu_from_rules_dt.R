#' update beu from rules
#'
#' update beumc values based on rules
#'
#' @param vri_bem sf object that represent VRI (vegetation ressource inventory) features
#' @param rules_dt data.table object that contains rule to apply to vri_bem to update beumc
#' @return sf object
#' @importFrom rlang parse_expr parse_exprs
#' @importFrom dplyr mutate_at across case_when mutate
#' @importFrom tidyr replace_na
#' @import data.table
#' @import sf
#' @export
update_beu_from_rules_dt <- function(vri_bem, rules_dt) {

  if (FALSE) {
    total_expr<-SDEC_1<-BEUMC_S2<-SDEC_2<-BEUMC_S3<-SDEC_3<-NULL
  }

  setDT(vri_bem)

  if (inherits(rules_dt, "character") && file.exists(rules_dt)) {
    sht <- readxl::excel_sheets(rules_dt) |> grep(pattern = "^comb.+script", ignore.case = TRUE, value = TRUE)
    rules_dt <- readxl::read_excel(rules_dt, sht) |> setDT()
  }

  rules_dt <- copy(rules_dt)

  # find rule columns except tree rules
  rules_dt_names <- copy(names(rules_dt))
  length_rules_dt_names <- length(rules_dt_names)
  which_name_input <- which(rules_dt_names == "INPUTS")
  if (length(which_name_input) != 1) {
    stop("One empty column named 'INPUTS' is expected in the rule file to mark the beginning of the columns use to create rules")
  }
  which_name_output <- which(rules_dt_names == "OUTPUTS")
  if (length(which_name_input) != 1) {
    stop("One empty column named 'OUTPUTS' is expected in the rule file to mark the end of the columns use to create rules and the beginning of the columns use to create outputs")
  }

  tree_list_var <- grep("^TREE_RL_SP_CD_[0-9]$", names(rules_dt))
  tree_pct_var <- grep("^TREE_RL_SP_PCT_[0-9]$", names(rules_dt))
  rule_columns <- setdiff((which_name_input + 1):(which_name_output - 1), c(tree_list_var, tree_pct_var))

  # all rule column must appear in the vri_bem
  rule_not_in_vri <- setdiff(rules_dt_names[rule_columns], names(vri_bem))
  if (length(rule_not_in_vri) > 0) {
    stop(paste0("Following rules are not a feature in vri-bem : ", rule_not_in_vri))
  }
  # create expession for all rule column except for tree rules

  # for non tree rules there is 4 possibilities
  # the CONTAINS keyword creates a rule that check if the variable contains the specified value
  # the DOES NOT CONTAINS keyword creates a rule that check if the variable does not contains the specified value
  # a list of values separate by commas creates a rule that check if the variable is in the list of values
  # a single value creates a rule that check if the variable is equal to the value

  for (column in rule_columns) {
    column_name <- rules_dt_names[column]
    column_name_expr <-  parse_expr(column_name)
    rules_dt[, paste0(rules_dt_names[column], "_expr") := fcase(grepl("^CONTAINS", eval(column_name_expr)), var_contains(var_name = column_name, rule = eval(column_name_expr)),
                                                                grepl("^DOES NOT CONTAIN", eval(column_name_expr)), var_does_not_contains(var_name = column_name, rule = eval(column_name_expr)),
                                                                grepl(",", eval(column_name_expr)), var_in_list(var_name = column_name, rule_list = eval(column_name_expr)),
                                                                !is.na(eval(column_name_expr)), var_equal_value(var_name = column_name, rule_value = eval(column_name_expr)),
                                                                default = "TRUE")]

  }

  # create expression for each pairs of tree list and percentage

  # for tree rules there is two possibilities
  # a list of values creates a rule that check if the total sum of percentage of any species in the list across all the pairs of species/pct variables is within the range in the corresponding pct rule column
  # a list of values separated by a > or a < sign creates a rule that check of the total sum of percentage of the species in the left-hand-side of the sign across all the pairs of species/pct variables is greater or less than the percentage for the species in the right-hand-side

  tree_list_var <- rules_dt_names[tree_list_var]
  tree_pct_var <- rules_dt_names[tree_pct_var]

  vri_bem_species_var <- grep("^SPEC_CD_[0-9]$", names(vri_bem), value = T)
  vri_bem_pct_var <- grep("^SPEC_PCT_[0-9]$", names(vri_bem), value = T)

  for (i in seq_along(tree_list_var)) {
    tree_list_var_parse <- rlang::parse_expr(tree_list_var[i])
    tree_pct_var_parse <- rlang::parse_expr(tree_pct_var[i])
    rules_dt[, paste0(tree_list_var[i], "_expr") := fcase(grepl("<|>", eval(tree_list_var_parse)), compare_pct_of_species_in_list(tree_list = eval(tree_list_var_parse), species_var = vri_bem_species_var, pct_var = vri_bem_pct_var),
                                                          !is.na(eval(tree_list_var_parse)), sum_pct_of_species_in_list_is_within_range(tree_list = eval(tree_list_var_parse), tree_range = eval(tree_pct_var_parse), species_var = vri_bem_species_var, pct_var = vri_bem_pct_var),
                                                          default = "TRUE")]

  }

  # create total expression
  rules_dt[ , total_expr := rlang::parse_exprs(do.call(paste, c(.SD, list(sep = " & ")))), .SDcols = c(paste0(rules_dt_names[rule_columns], "_expr"), paste0(tree_list_var, "_expr"))]

  # apply total expr on vri_bem and update output columns based on rules result
  for (rule in 1:nrow(rules_dt)) {
    which_lines <- vri_bem[, which(eval(rules_dt[["total_expr"]][[rule]]))]
    for (output_col in (which_name_output + 1):length_rules_dt_names) {
      # TODO find out which BEUMC to replace between the 3 deciles
      # I assume we update all decile
      # Apr 2023 update: only update first decile
      if (rules_dt_names[output_col] == "BEUMC") {
        for (i in 1) {
          set(vri_bem, i = which_lines, j = paste0(rules_dt_names[output_col], "_S", i), value = rules_dt[[rules_dt_names[output_col]]][rule])
        }
      } else {
        set(vri_bem, i = which_lines, j = rules_dt_names[output_col], value = rules_dt[[rules_dt_names[output_col]]][rule])
      }
    }
  }

  #correct for cases where BEUMC_S1 now equals BEUMC_S2 (or BEUMC_S3)
  vri_bem <- vri_bem |>

    dplyr::mutate_at(c('SDEC_1','SDEC_2','SDEC_3'), ~tidyr::replace_na(.,0)) |>

    dplyr::mutate(SDEC_1 = dplyr::case_when(
      BEUMC_S1 == BEUMC_S2 ~ rowSums(dplyr::across(c("SDEC_1","SDEC_2"))),
      BEUMC_S1 == BEUMC_S3 ~ rowSums(dplyr::across(c("SDEC_1","SDEC_3"))),
     .default = SDEC_1),

    BEUMC_S2 = dplyr::case_when(
        BEUMC_S1 == BEUMC_S2 & !is.na(BEUMC_S3) ~ BEUMC_S3,
        BEUMC_S1 == BEUMC_S2 & is.na(BEUMC_S3) ~ NA_character_,
        .default = BEUMC_S2),

    SDEC_2 = dplyr::case_when(
        BEUMC_S1 == BEUMC_S2 & !is.na(BEUMC_S3) ~ SDEC_3,
        BEUMC_S1 == BEUMC_S2 & is.na(BEUMC_S3) ~ 0,
        .default = SDEC_2),

    BEUMC_S3 = dplyr::case_when(
        BEUMC_S1 == BEUMC_S2 ~ NA_character_,
        BEUMC_S1 == BEUMC_S3 ~ NA_character_,
        .default = BEUMC_S3),

    SDEC_3 = dplyr::case_when(
          BEUMC_S1 == BEUMC_S2 ~ 0,
          BEUMC_S1 == BEUMC_S3 ~ 0,
          .default = SDEC_3))

  return(sf::st_as_sf(vri_bem))
}

#' var_contains
#'
#' create string expression to check if variable contains the rule
#'
#' @param var_name string name of variable
#' @param rule string of rule
#' @return string
#' @keywords internal
var_contains <- function(var_name, rule) {
  n_char_rule <- nchar(rule)
  paste0("grepl('",substr(rule, 10, n_char_rule),"', ", var_name, ")")
}

#' var_does_not_contains
#'
#' create string expression to check if variable does not contains the rule
#'
#' @param var_name string name of variable
#' @param rule string of rule
#' @return string
#' @keywords internal
var_does_not_contains <-  function(var_name, rule) {
  n_char_rule <- nchar(rule)
  paste0("!grepl('",substr(rule, 18, n_char_rule),"', ", var_name, ")")
}

#' convert_rule_list_to_string_vector
#'
#' create string expression that convert list separate by commas to vector
#'
#' @param rule_list string list of values
#' @return string
#' @keywords internal
convert_rule_list_to_string_vector <- function(rule_list) {
  which_na <- which(is.na(rule_list))
  res <- paste0("c('", sapply(strsplit(gsub(" ", "", rule_list), split = ",|>|<"), function(x) paste0(x, collapse = "','")), "')")
  res[which_na] <- ""
  return(res)
}

#' var_in_list
#'
#' create string expression that check if the variable is in the list
#'
#' @param var_name string name of variable
#' @param rule_list string list of values
#' @return string
#' @keywords internal
var_in_list <- function(var_name, rule_list) {
  paste0(var_name, " %in% ", convert_rule_list_to_string_vector(rule_list))
}

#' var_equal_value
#'
#' create string expression that check if the variable equal a value
#'
#' @param var_name string name of variable
#' @param rule_value string value to match
#' @return string
#' @keywords internal
var_equal_value <- function(var_name, rule_value) {
  paste0(var_name," == '", rule_value, "'")
}

#' sum_pct_for_species_in_list
#'
#' create string expression that sum the percentage of the species in the list across all pairs of species/pct variables
#'
#' @param tree_list string list of trees
#' @param vri_bem sf object of vri-bem, optional
#' @param species_var character vector of names of species variables to use in vri bem
#' @param pct_var character vector of names of percentage variables to use in vri bem
#' @return string
#' @keywords internal
sum_pct_for_species_in_list <-  function(tree_list, vri_bem = NULL, species_var = NULL, pct_var = NULL) {
  if (is.null(species_var)) {
    species_var <- grep("^SPEC_CD_", names(vri_bem), value = T)
  }
  if (is.null(pct_var)) {
    pct_var <- grep("^SPEC_PCT_", names(vri_bem), value = T)
  }

  species_list <- convert_rule_list_to_string_vector(tree_list)

  for (i in seq_along(species_var)) {
    if (i == 1) {
      string_expression <- paste0("(", species_var[i], " %in% ", species_list, ") * as.numeric(", pct_var[i], ") ")
    } else {
      string_expression <- paste0(string_expression, " + ", paste0("(", species_var[i], " %in% ", species_list, ") * as.numeric(", pct_var[i], ") "))
    }
  }

  return(string_expression)
}

#' sum_pct_of_species_in_list_is_within_range
#'
#' create string expression that check if the sum of the percentage of the species in the list across all pairs of species/pct variables is within a range
#'
#' @param tree_list string list of trees
#' @param tree_range string range
#' @param vri_bem sf object of vri-bem, optional
#' @param species_var character vector of names of species variables to use in vri bem
#' @param pct_var character vector of names of percentage variables to use in vri bem
#' @return string
#' @keywords internal
sum_pct_of_species_in_list_is_within_range <-  function(tree_list, tree_range, vri_bem = NULL , species_var = NULL, pct_var = NULL) {
  sum_pct_string_expression <-  sum_pct_for_species_in_list(tree_list = tree_list, vri_bem = vri_bem, species_var = species_var, pct_var = pct_var)
  range_pct <- strsplit(tree_range, split = "-")

  string_expression <- character(length(range_pct))
  for (i in seq_along(range_pct)) {
    string_expression[i] <- paste0("between(", sum_pct_string_expression[i], ",", range_pct[[i]][1], ",", range_pct[[i]][2], ")")
  }

  return(string_expression)
}

#' compare_pct_of_species_in_list
#'
#' create string expression that check if the sum of the percentage of the species in the list across all pairs of species/pct variables is greater or less than the percentage of the other species in the list
#'
#' @param tree_list string list of trees
#' @param vri_bem sf object of vri-bem, optional
#' @param species_var character vector of names of species variables to use in vri bem
#' @param pct_var character vector of names of percentage variables to use in vri bem
#' @return string
#' @keywords internal
compare_pct_of_species_in_list <- function(tree_list, vri_bem = NULL, species_var = NULL, pct_var = NULL) {
  tree_list_groups <- strsplit(tree_list, split = "<|>")
  tree_list_group_a <- sapply(tree_list_groups, function(x) x[1])
  tree_list_group_b <- sapply(tree_list_groups, function(x) x[2])

  sum_pct_group_a <-  sum_pct_for_species_in_list(tree_list = tree_list_group_a, vri_bem = vri_bem, species_var = species_var, pct_var = pct_var)
  sum_pct_group_b <-  sum_pct_for_species_in_list(tree_list = tree_list_group_b, vri_bem = vri_bem, species_var = species_var, pct_var = pct_var)

  return(paste0("(", sum_pct_group_a, ") ", fifelse(grepl("<", tree_list), "< ", "> "), "(", sum_pct_group_b, ")"))

}

