context("RSAGA-modules")

library(RSAGA)

test_that("Write DEM to disc", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env()

  data(landslides)
  out_fnm <- file.path(tempdir(), "dem.sgrd")
  write.sgrd(
    data = dem, file = out_fnm, header = dem$header,
    env = env, check.module.exists = FALSE
  )
  expect_true(file.exists(out_fnm))
})

test_that("Slope", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env()
  out_fnm <- file.path(tempdir(), "slope.sgrd")

  rsaga.slope.asp.curv(file.path(tempdir(), "dem.sgrd"),
    out.slope = out_fnm,
    method = "poly2zevenbergen", env = env, check.module.exists = FALSE
  )
  expect_true(file.exists(out_fnm))
})

test_that("Fill Sinks", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env()
  out_fnm <- file.path(tempdir(), "fill_sinks.sgrd")

  rsaga.fill.sinks(file.path(tempdir(), "dem.sgrd"),
    out.dem = out_fnm,
    method = "planchon.darboux.2001", env = env, check.module.exists = FALSE
  )
  expect_true(file.exists(out_fnm))
})

test_that("Sink Route", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env()
  out_fnm <- file.path(tempdir(), "sink_route.sgrd")

  rsaga.sink.route(file.path(tempdir(), "dem.sgrd"),
    out.sink = out_fnm,
    env = env, check.module.exists = FALSE
  )
  expect_true(file.exists(out_fnm))
})

test_that("Sink Removal", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env()
  out_fnm <- file.path(tempdir(), "sink_removal.sgrd")

  rsaga.sink.removal(file.path(tempdir(), "dem.sgrd"),
    in.sinkroute = file.path(tempdir(), "sink_route.sgrd"),
    out.dem = out_fnm, env = env, check.module.exists = FALSE
  )
  expect_true(file.exists(out_fnm))
})

test_that("Close Gaps", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env()
  out_fnm <- file.path(tempdir(), "close_gaps.sgrd")

  rsaga.close.gaps(file.path(tempdir(), "dem.sgrd"),
    out.dem = out_fnm,
    env = env, check.module.exists = FALSE
  )
  expect_true(file.exists(out_fnm))
})

test_that("Hillshade", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env()
  out_fnm <- file.path(tempdir(), "hillshade.sgrd")

  rsaga.hillshade(file.path(tempdir(), "dem.sgrd"),
    out.grid = out_fnm,
    exaggeration = 10, env = env, check.module.exists = FALSE
  )
  expect_true(file.exists(out_fnm))
})

test_that("PISR2", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env()
  out_fnm <- file.path(tempdir(), "pisr2.sgrd")

  rsaga.pisr2(
    in.dem = file.path(tempdir(), "dem.sgrd"), out.direct.grid = out_fnm,
    out.diffuse.grid = file.path(tempdir(), "pisr2_diffuse.sgrd"),
    latitude = 43, unit = "kWh/m2", method = "lumped",
    lmp.transmittance = 60, time.range = c(0, 24), time.step = 3,
    start.date = list(day = 1, month = 10, year = 2016), end.date = list(day = 6, month = 12, year = 2016),
    day.step = 10, env = env, show = FALSE, check.module.exists = FALSE
  )
  expect_true(file.exists(out_fnm))
})

test_that("Topdown Processing", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env()
  out_fnm <- file.path(tempdir(), "carea.sgrd")

  rsaga.topdown.processing(
    in.dem = file.path(tempdir(), "dem.sgrd"),
    out.carea = out_fnm, env = env,
    check.module.exists = FALSE
  )
  expect_true(file.exists(out_fnm))
})

test_that("Wetness Index", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env()
  out_fnm <- file.path(tempdir(), "wi.sgrd")

  rsaga.wetness.index(
    in.dem = file.path(tempdir(), "dem.sgrd"),
    out.wetness.index = out_fnm, env = env,
    check.module.exists = FALSE
  )
  expect_true(file.exists(out_fnm))
})

test_that("Grid Calculus", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env()
  out_fnm <- file.path(tempdir(), "calculus.sgrd")

  rsaga.grid.calculus(c(file.path(tempdir(), "dem.sgrd"),
                        file.path(tempdir(), "dem.sgrd")),
    out.grid = out_fnm, formula = "a + b",
    env = env, check.module.exists = FALSE
  )
  expect_true(file.exists(out_fnm))
})

test_that("Contour", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env()
  out_fnm <- file.path(tempdir(), "contour.shp")

  rsaga.contour(file.path(tempdir(), "dem.sgrd"),
    out.shapefile = out_fnm, zstep = 5, env = env,
    check.module.exists = FALSE
  )
  expect_true(file.exists(out_fnm))
})

test_that("Grid to Points Randomly", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env()
  out_fnm <- file.path(tempdir(), "grid_to_points_randomly.shp")

  rsaga.grid.to.points.randomly(
    in.grid = file.path(tempdir(), "dem.sgrd"),
    out.shapefile = out_fnm,
    freq = 50, env = env, check.module.exists = FALSE
  )
  expect_true(file.exists(out_fnm))
})
