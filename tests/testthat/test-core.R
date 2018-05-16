context("RSAGA-core")

test_that("Link to saga", {
  testthat::skip_on_cran()

  env <- rsaga.env()

  expect_equal(typeof(env$version), "character")
})





