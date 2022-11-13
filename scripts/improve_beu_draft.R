library(sf)
library(readxl)
library(data.table)
library(rlang)

rules_dt <- setDT(read_excel("/Users/nicolas/Documents/boostao/ssgbm/Improve_forested_BEU/Rules_for_scripting_improved_forested_BEUs_Skeena_07Mar2022.xlsx", sheet = "Combined_Rules_for_Script"))

# rename rules column to fit with vri-bem data
paste0("TREE_RL_SP_CD_", 1:4)
paste0("TREE_RL_SP_PCT_", 1:4)
paste0("SPECIES_CD_", 1:4)
paste0("SPECIES_PCT_", 1:4)

setnames(rules_dt,
         old = c(paste0("TREE_RL_SP_CD_", 1:4), paste0("TREE_RL_SP_PCT_", 1:4)),
         new = c(paste0("SPECIES_CD_", 1:4), paste0("SPECIES_PCT_", 1:4)),
         skip_absent = T)

rules_dt[, BGC_ZONE_expr := paste0("BGC_ZONE == '", BGC_ZONE, "'")]

rules_dt[, BGC_SUBZON_expr := fcase(grepl("^CONTAINS", BGC_SUBZON), paste0("grepl(",substr(BGC_SUBZON, 10, 10),", BGC_SUBZON)"),
                                    grepl("^DOES NOT CONTAIN", BGC_SUBZON), paste0("!grepl(",substr(BGC_SUBZON, 10, 10),", BGC_SUBZON)"),
                                    is.na(BGC_SUBZON), "",
                                    default = paste0("BGC_SUBZON == ", BGC_SUBZON))]

rules_dt[, BGC_VRT_expr := fcase(is.na(BGC_VRT), "",
                                 default = paste0("BGC_VRT == ", BGC_VRT))]

rules_dt[, fcase(grepl("<|>", SPECIES_CD_1), paste0("TODO"),
                 is.na(SPECIES_CD_1), "",
                 default = )]

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

