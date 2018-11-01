context("RSAGA-core")
library(RSAGA)

test_that("Link to saga", {
  testthat::skip_on_cran()

  env <- rsaga.env()

  expect_equal(typeof(env$version), "character")
})

test_that("Error message for wrong parameters in Geoprocessor", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env()

  params = list(ELEVATION = "dem.sgrd",
                SLOPES = "slope.sgrd",
                ASPECT = "aspect.sgrd")

  expect_error(rsaga.geoprocessor(lib = "ta_morphometry", module = 0,
                     param = params, check.module.exists=FALSE),
               regexp = "^.*Wrong paramters used.*$")
})





