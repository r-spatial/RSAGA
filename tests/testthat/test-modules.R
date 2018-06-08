context("RSAGA-modules")

test_that("Slope", {
  testthat::skip_on_cran()

  env <- rsaga.env()

  print(env)

  rsaga.slope.asp.curv("./data/dem.sgrd", out.slope = file.path(tempdir(), "slope.sgrd"),
                       method = "poly2zevenbergen",env = env, check.module.exists=FALSE)

  expect_true(file.exists(file.path(tempdir(), "slope.sgrd")))

})
