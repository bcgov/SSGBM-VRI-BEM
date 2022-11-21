# SSGBM.VRI.BEM 0.2.0

* added `merge_rrm_on_vri_moose` and `merge_rrm_on_vri_bear` functions
* added error message for incorrect input in rule file
* added `create_RRM_ecosystem_bear` function
* fix bug in creation of RRM so that we don't filter ecosystem that are not projections

# SSGBM.VRI.BEM 0.1.0

* added `update_beu_from_rule_dt` function

# SSGBM.VRI.BEM 0.0.2

* `read_vri`, `read_wetlands`, `read_rivers` and `read_ccb` functions will now fetch the information from the BC data catalog using the `bcdata` package when the `dsn` parameter is not provided. 

# SSGBM.VRI.BEM 0.0.1

* Create package
