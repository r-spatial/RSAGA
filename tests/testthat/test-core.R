context("RSAGA-core")

test_that("Link to saga", {
  
  env <- rsaga.env2()
  
  expect_equal(typeof(env$version), "character")
  
})

test_that("Modules", {
  testthat::skip_on_travis()
  
  env <- rsaga.env2()
  
  module <- rsaga.get.modules(lib = "ta_morphometry", env = env)
  
  expect_true("Slope, Aspect, Curvature" %in% module$ta_morphometry$name)
  
})

test_that("Usage", {
  testthat::skip_on_travis()
  
  env <- rsaga.env2()
  
  usage <- rsaga.get.usage(lib = "ta_morphometry", module = 1, env = env)
  
  expect_equal(typeof(usage), "character")
  
})

test_that("Check if module exists", {
  testthat::skip_on_travis()
  
  env <- rsaga.env2()
  
  expect_true(rsaga.module.exists(lib = "ta_morphometry", module = 1, env = env))
  
})






