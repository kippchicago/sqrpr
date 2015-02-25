context("System data")

#data(nwea_cps_school_level_norms)
#data(cps_constants)

test_that("System data conform with expectations", {
  expect_is(nwea_cps_school_level_norms, "data.frame")
  expect_is(cps_constants, "data.frame")

  expect_equal(names(nwea_cps_school_level_norms),
               c("grade_start",
                 "grade_end",
                 "rit_start",
                 "typical_growth_mean",
                 "sd_growth",
                 "measurementscale",
                 "season_start",
                 "season_end"
                 )
               )
  expect_equal(names(cps_constants),
               c("grade",
                 "measurementscale",
                 "variable",
                 "value"
                 )
               )
  
  expect_equal(round(mean(nwea_cps_school_level_norms$rit_start),3), 197.097)
  expect_equal(round(mean(cps_constants$value),6), 4.402439)
})
