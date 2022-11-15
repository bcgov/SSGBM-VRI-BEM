library(sf)
library(readxl)
library(data.table)
library(rlang)

rules_dt <- setDT(read_excel("/Users/nicolas/Documents/boostao/ssgbm/Improve_forested_BEU/Rules_for_scripting_improved_forested_BEUs_Skeena_07Mar2022.xlsx", sheet = "Combined_Rules_for_Script"))

rules_dt[, BGC_ZONE_expr := paste0("BGC_ZONE == '", BGC_ZONE, "'")]

rules_dt[, BGC_SUBZON_expr := fcase(grepl("^CONTAINS", BGC_SUBZON), paste0("grepl(",substr(BGC_SUBZON, 10, 10),", BGC_SUBZON)"),
                                    grepl("^DOES NOT CONTAIN", BGC_SUBZON), paste0("!grepl(",substr(BGC_SUBZON, 10, 10),", BGC_SUBZON)"),
                                    !is.na(BGC_SUBZON), paste0("BGC_SUBZON == ", BGC_SUBZON),
                                    default = "")]

rules_dt[, BGC_VRT_expr := fcase(!is.na(BGC_VRT), paste0("BGC_VRT == ", BGC_VRT),
                                 default = "")]

# create vector of string from rules for species
tree_species_var <- grep("^TREE_RL_SP_CD_", names(rules_dt), value = T)

convert_rule_list_to_string_vector <- function(rule_list) {
  which_na <- which(is.na(rule_list))
  res <- paste0("c('", sapply(strsplit(gsub(" ", "", rule_list), split = ",|>|<"), function(x) paste0(x, collapse = "','")), "')")
  res[which_na] <- ""
  return(res)
}

rules_dt[,  paste0("string_vector_", tree_species_var) :=  lapply(.SD, convert_rule_list_to_string_vector), .SDcols = (tree_species_var)]

vri_bem_species_var <- grep("^SPEC_CD_", names(vri_bem), value = T)
vri_bem_pct_var <- grep("^SPEC_PCT_", names(vri_bem), value = T)



rules_dt[, sum_pct_of_species_in_list_is_within_range(tree_list = TREE_RL_SP_CD_1, tree_range = TREE_RL_SP_PCT_1, species_var = vri_bem_species_var,
                                                      pct_var = vri_bem_pct_var)]


rules_dt[,  paste0(tree_species_var, "_expr") := lapply(.SD, function(tree_species) fcase(grepl("<|>", tree_species), paste0("TODO"),
                                                                                          !is.na(tree_species), sum_pct_for_species_in_list(tree_list = tree_species,
                                                                                                                                            vri_bem_species_var = vri_bem_species_var,
                                                                                                                                             vri_bem_pct_var = vri_bem_pct_var),
                                                                                          default = ""
                                                                                          )
                                                        ), .SDcols = (tree_species_var)
         ]

rules_dt[, fcase(grepl("<|>", SPECIES_CD_1), paste0("TODO"),
                 is.na(SPECIES_CD_1), "",
                 default = sum_pct_for_species_in_list())]

(SPECIES_CD_1 %in% SPECIES_CD_1)

# probablement plus simple de formatter les species cd de la table de rule en vecteur et generer des expressions avec des %in% que de faire des grep sur la grosse table VRIBEM
paste0("grep('(^eval(SPECIES_CD_1)$)|(^eval(SPECIES_CD_1)[^a-zA-Z]+)|([^a-zA-Z]+eval(SPECIES_CD_1)[^a-zA-Z]+)|([^a-zA-Z]+eval(SPECIES_CD_1)$)'", SPECIES_CD_1, ")")



help("grep")
(rules_dt$BGC_ZONE_expr[[1]])

setDT(vri_bem)
vri_bem[ , eval_tidy(rules_dt$BGC_ZONE_expr[1])]
with_env(vri_bem, parse_expr("BGC_ZONE == 'BWBS'"))
vri_bem[ , rule_expr := eval_tidy(parse_expr("BGC_ZONE == 'BWBS'"))]


my_rule <- parse_expr("BGC_ZONE == 'BWBS'")
eval_tidy(my_rule, vri_bem)
class(my_rule)
help(eval_tidy)
st_layers("/Users/nicolas/Documents/boostao/ssgbm/Improve_forested_BEU/vri_bem.gdb")
vri_bem <- st_read("/Users/nicolas/Documents/boostao/ssgbm/Improve_forested_BEU/vri_bem.gdb", layer = "fc2")

