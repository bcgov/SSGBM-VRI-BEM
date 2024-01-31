test_that("Test grid creation", {
  wkt_filter <- "POLYGON ((941827.7 932215.1, 1065018 932215.1, 1065018 1016988, 941827.7 1016988, 941827.7 932215.1))"
  expect_warning(create_grid_from_iteration_number(10, wkt_filter, FALSE), "")
})
