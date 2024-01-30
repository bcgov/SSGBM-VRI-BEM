# SSGBM.VRI.BEM 0.2.3

* Address misassigned BEUs and structural stage 
* Added `calc_hem_fields`
* Added `create_hem_rasters`
* `create_unique_ecosystem_dt` now accept an existing csv to verify if it already covers all cases
* `merge_ccb_on_vri` now return intersection + difference
* Added `merge_salmon_on_vri`

# SSGBM.VRI.BEM 0.2.2

* added new argument `animal` to  `format_rrm_output` to handle moose data
* added change to rrm data in  `format_rrm_output` to handle moose data
* fix mean_slope calculation so that the result is in percentage (x100)


# SSGBM.VRI.BEM 0.2.1

* Added suffixes for rating variables
* New argument `animal` in `calc_capability_rating` to differentiate moose and bear
* Fix weighted average in `merge_rrm_on_vri` to handle missing decile or ratings


# SSGBM.VRI.BEM 0.2.0

* added `merge_rrm_on_vri` function
* added error message for incorrect input in rule file
* added `create_RRM_ecosystem_bear` function
* fix bug in creation of RRM so that we don't filter ecosystem that are not projections

# SSGBM.VRI.BEM 0.1.0

* added `update_beu_from_rule_dt` function

# SSGBM.VRI.BEM 0.0.2

* `read_vri`, `read_wetlands`, `read_rivers` and `read_ccb` functions will now fetch the information from the BC data catalog using the `bcdata` package when the `dsn` parameter is not provided. 

# SSGBM.VRI.BEM 0.0.1

* Create package
