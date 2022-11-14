#' improve beu from rules
#'
#' update beumc values based on rules
#'
#' @param vri_bem sf object that represent VRI (vegetation ressource inventory) features
#' @param rules_dt data.table object that contains rule to apply to vri_bem to update beumc
#' @return sf object
#' @import rlang, data.table
#' @export
improve_beu_from_rule_dt <- function(vri_bem, rules_dt) {

  # create expression for BGC_ZONE rule
  rules_dt[, BGC_ZONE_expr := fcase(!is.na(BGC_ZONE), paste0("BGC_ZONE == '", BGC_ZONE, "'"),
                                    default = "TRUE")]

  # create expression for BGC_SUBZON rule
  # TODO shortcut of using hardcoded start and stop for substr only works for one letter condition
  rules_dt[, BGC_SUBZON_expr := fcase(grepl("^CONTAINS", BGC_SUBZON), paste0("grepl('",substr(BGC_SUBZON, 10, 10),"', BGC_SUBZON)"),
                                      grepl("^DOES NOT CONTAIN", BGC_SUBZON), paste0("!grepl('",substr(BGC_SUBZON, 10, 10),"', BGC_SUBZON)"),
                                      !is.na(BGC_SUBZON), paste0("BGC_SUBZON == '", BGC_SUBZON, "'"),
                                      default = "TRUE")]

  # create expression for BGC_VRT rule
  rules_dt[, BGC_VRT_expr := fcase(!is.na(BGC_VRT), paste0("BGC_VRT == ", BGC_VRT),
                                   default = "TRUE")]

  # create expression for each pairs of tree list and percentage
  tree_list_var <- grep("^TREE_RL_SP_CD_[0-9]$", names(rules_dt), value = T)
  tree_pct_var <- grep("^TREE_RL_SP_PCT_[0-9]$", names(rules_dt), value = T)

  vri_bem_species_var <- grep("^SPEC_CD_[0-9]$", names(vri_bem), value = T)
  vri_bem_pct_var <- grep("^SPEC_PCT_[0-9]$", names(vri_bem), value = T)

  for (i in seq_along(tree_list_var)) {
    tree_list_var_parse <- parse_expr(tree_list_var[i])
    tree_pct_var_parse <- parse_expr(tree_pct_var[i])
    rules_dt[, paste0(tree_list_var[i], "_expr") := fcase(grepl("<|>", eval(tree_list_var_parse)), compare_pct_of_species_in_list(tree_list = eval(tree_list_var_parse), species_var = vri_bem_species_var, pct_var = vri_bem_pct_var),
                                                          !is.na(eval(tree_list_var_parse)), sum_pct_of_species_in_list_is_within_range(tree_list = eval(tree_list_var_parse), tree_range = eval(tree_pct_var_parse), species_var = vri_bem_species_var, pct_var = vri_bem_pct_var),
                                                          default = "TRUE")]

  }

  # create expression for SOIL_MOISTURE_REGIME_1 rule
  rules_dt[ , SOIL_MOISTURE_REGIME_1_expr := fcase(!is.na(SOIL_MOISTURE_REGIME_1), paste0("SOIL_MOISTURE_REGIME_1 %in% ", convert_rule_list_to_string_vector(SOIL_MOISTURE_REGIME_1)),
                                                   default = "TRUE")]

  # create expression for slope mod
  rules_dt[ , SLOPE_MOD_expr := fcase(!is.na(SLOPE_MOD), paste0("SLOPE_MOD %in% ", convert_rule_list_to_string_vector(SLOPE_MOD)),
                                                   default = "TRUE")]

  # create total expression
  rules_dt[ , total_expr := parse_exprs(do.call(paste, c(.SD, list(sep = " & ")))), .SDcols = c("BGC_ZONE_expr", "BGC_SUBZON_expr", "BGC_VRT_expr", paste0(tree_list_var, "_expr"), "SOIL_MOISTURE_REGIME_1_expr", "SLOPE_MOD_expr")]

  # apply total expr on vri_bem and update BEUMC based on rules result
  for (rule in 1:nrow(rules_dt)) {
    which_lines <- vri_bem[, which(eval(rules_dt[["total_expr"]][[rule]]))]
    # TODO find out which BEUMC to replace between the 3 deciles
    set(vri_bem, i = which_lines, j = "BEUMC_test", value = rules_dt[["BEUMC"]][rule])
  }

  return(vri_bem)
}

convert_rule_list_to_string_vector <- function(rule_list) {
  which_na <- which(is.na(rule_list))
  res <- paste0("c('", sapply(strsplit(gsub(" ", "", rule_list), split = ",|>|<"), function(x) paste0(x, collapse = "','")), "')")
  res[which_na] <- ""
  return(res)
}

sum_pct_for_species_in_list <-  function(tree_list, vri_bem = NULL, species_var = NULL, pct_var = NULL) {
  if (is.null(species_var)) {
    species_var <- grep("^SPEC_CD_", names(vri_bem), value = T)
  }
  if (is.null(pct_var)) {
    pct_var <- grep("^SPEC_PCT_", names(vri_bem), value = T)
  }

  species_list <- convert_rule_list_to_string_vector(tree_list)

  for (i in seq_along(vri_bem_species_var)) {
    if (i == 1) {
      string_expression <- paste0("(", species_var[i], " %in% ", species_list, ") * as.numeric(", pct_var[i], ") ")
    } else {
      string_expression <- paste0(string_expression, " + ", paste0("(", species_var[i], " %in% ", species_list, ") * as.numeric(", pct_var[i], ") "))
    }
  }

  return(string_expression)
}

sum_pct_of_species_in_list_is_within_range <-  function(tree_list, tree_range, vri_bem = NULL , species_var = NULL, pct_var = NULL) {
  sum_pct_string_expression <-  sum_pct_for_species_in_list(tree_list = tree_list, vri_bem = vri_bem, species_var = species_var, pct_var = pct_var)
  range_pct <- strsplit(tree_range, split = "-")

  string_expression <- character(length(range_pct))
  for (i in seq_along(range_pct)) {
    string_expression[i] <- paste0("between(", sum_pct_string_expression[i], ",", range_pct[[i]][1], ",", range_pct[[i]][2], ")")
  }

  return(string_expression)
}

compare_pct_of_species_in_list <- function(tree_list, vri_bem = NULL, species_var = NULL, pct_var = NULL) {
  tree_list_groups <- strsplit(tree_list, split = "<|>")
  tree_list_group_a <- sapply(tree_list_groups, function(x) x[1])
  tree_list_group_b <- sapply(tree_list_groups, function(x) x[2])

  sum_pct_group_a <-  sum_pct_for_species_in_list(tree_list = tree_list_group_a, vri_bem = vri_bem, species_var = species_var, pct_var = pct_var)
  sum_pct_group_b <-  sum_pct_for_species_in_list(tree_list = tree_list_group_b, vri_bem = vri_bem, species_var = species_var, pct_var = pct_var)

  return(paste0("(", sum_pct_group_a, ") ", fifelse(grepl("<", tree_list), "< ", "> "), "(", sum_pct_group_b, ")"))

}

