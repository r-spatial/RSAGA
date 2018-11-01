context("RSAGA-modules")

library(RSAGA)
library(rgdal)

test_that("Write DEM to disc", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env()

  data(landslides)
  write.sgrd(
    data = dem, file = file.path(tempdir(), "dem.sgrd"), header = dem$header,
    env = env, check.module.exists = FALSE
  )
  expect_true(file.exists(file.path(tempdir(), "dem.sgrd")))
})

test_that("Slope", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env()

  rsaga.slope.asp.curv(file.path(tempdir(), "dem.sgrd"),
    out.slope = file.path(tempdir(), "slope.sgrd"),
    method = "poly2zevenbergen", env = env, check.module.exists = FALSE
  )
  expect_true(file.exists(file.path(tempdir(), "slope.sgrd")))
})

test_that("Fill Sinks", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env()

  rsaga.fill.sinks(file.path(tempdir(), "dem.sgrd"),
    out.dem = file.path(tempdir(), "fill_sinks.sgrd"),
    method = "planchon.darboux.2001", env = env, check.module.exists = FALSE
  )
  expect_true(file.exists(file.path(tempdir(), "fill_sinks.sgrd")))
})

test_that("Sink Route", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env()

  rsaga.sink.route(file.path(tempdir(), "dem.sgrd"),
    out.sink = file.path(tempdir(), "sink_route.sgrd"),
    env = env, check.module.exists = FALSE
  )
  expect_true(file.exists(file.path(tempdir(), "sink_route.sgrd")))
})

test_that("Sink Removal", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env()

  rsaga.sink.removal(file.path(tempdir(), "dem.sgrd"),
    in.sinkroute = file.path(tempdir(), "sink_route.sgrd"),
    out.dem = file.path(tempdir(), "sink_removal.sgrd"), env = env, check.module.exists = FALSE
  )
  expect_true(file.exists(file.path(tempdir(), "sink_removal.sgrd")))
})

test_that("Close Gaps", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env()

  rsaga.close.gaps(file.path(tempdir(), "dem.sgrd"),
    out.dem = file.path(tempdir(), "close_gaps.sgrd"),
    env = env, check.module.exists = FALSE
  )
  expect_true(file.exists(file.path(tempdir(), "close_gaps.sgrd")))
})

test_that("Hillshade", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env()

  rsaga.hillshade(file.path(tempdir(), "dem.sgrd"),
    out.grid = file.path(tempdir(), "hillshade.sgrd"),
    exaggeration = 10, env = env, check.module.exists = FALSE
  )
  expect_true(file.exists(file.path(tempdir(), "hillshade.sgrd")))
})

test_that("PISR2", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env()

  rsaga.pisr2(
    in.dem = file.path(tempdir(), "dem.sgrd"), out.direct.grid = file.path(tempdir(), "pisr2.sgrd"),
    out.diffuse.grid = file.path(tempdir(), "pisr2_diffuse.sgrd"),
    latitude = 43, unit = "kWh/m2", method = "lumped",
    lmp.transmittance = 60, time.range = c(0, 24), time.step = 3,
    start.date = list(day = 1, month = 10, year = 2016), end.date = list(day = 6, month = 12, year = 2016),
    day.step = 10, env = env, show = FALSE, check.module.exists = FALSE
  )
  expect_true(file.exists(file.path(tempdir(), "pisr2.sgrd")))
})

test_that("Topdown Processing", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env()

  rsaga.topdown.processing(
    in.dem = file.path(tempdir(), "dem.sgrd"),
    out.carea = file.path(tempdir(), "carea.sgrd"), env = env,
    check.module.exists = FALSE
  )
  expect_true(file.exists(file.path(tempdir(), "carea.sgrd")))
})

test_that("Wetness Index", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env()

  rsaga.wetness.index(
    in.dem = file.path(tempdir(), "dem.sgrd"),
    out.wetness.index = file.path(tempdir(), "wi.sgrd"), env = env,
    check.module.exists = FALSE
  )
  expect_true(file.exists(file.path(tempdir(), "wi.sgrd")))
})

test_that("Grid Calculus", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env()

  rsaga.grid.calculus(c(file.path(tempdir(), "dem.sgrd"), file.path(tempdir(), "dem.sgrd")),
    out.grid = file.path(tempdir(), "calculus.sgrd"), formula = "a + b",
    env = env, check.module.exists = FALSE
  )
  expect_true(file.exists(file.path(tempdir(), "calculus.sgrd")))
})

test_that("Contour", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env()

  rsaga.contour(file.path(tempdir(), "dem.sgrd"),
    out.shapefile = file.path(tempdir(), "contour"), zstep = 5, env = env,
    check.module.exists = FALSE
  )
  expect_true(file.exists(file.path(tempdir(), "contour.shp")))
})

test_that("Grid to Points Randomly", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env()

  rsaga.grid.to.points.randomly(
    in.grid = file.path(tempdir(), "dem.sgrd"),
    out.shapefile = file.path(tempdir(), "grid_to_points_randomly"),
    freq = 50, env = env, check.module.exists = FALSE
  )
  expect_true(file.exists(file.path(tempdir(), "grid_to_points_randomly.shp")))
})
