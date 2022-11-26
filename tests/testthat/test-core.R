library(RSAGA)

# The following tests are not meant for CRAN.
# SAGA will normally be located automatically by RSAGA through
# a rsaga.env() call; however, I am hard-coding the paths of
# multiple SAGA versions in order to run multiple compatibility
# tests. Use NULL to let RSAGA try to find a SAGA installation.
# SAGA_PATH <- NULL
SAGA_PATH <- "C:/Progra~1/SAGA"
# SAGA_PATH <- "C:/Progra~1/saga_2.3.1_x64"

test_that("Link to saga", {
  testthat::skip_on_cran()

  env <- rsaga.env(path = SAGA_PATH)

  expect_equal(typeof(env$version), "character")
})

test_that("Error message for wrong arguments in Geoprocessor", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env(path = SAGA_PATH)

  params = list(ELEVATION = "dem.sgrd",
                SLOPES = "slope.sgrd",
                ASPECT = "aspect.sgrd")

  expect_error(rsaga.geoprocessor(lib = "ta_morphometry", module = 0,
                     param = params, check.module.exists=FALSE,
                     env = env),
               regexp = "^.*Wrong paramters used.*$")
})
