library(RSAGA)

# The following tests are not meant for CRAN.
# SAGA will normally be located automatically by RSAGA through
# a rsaga.env() call; however, I am hard-coding the paths of
# multiple SAGA versions in order to run multiple compatibility
# tests. Use NULL to let RSAGA try to find a SAGA installation.
# SAGA_PATH <- NULL
SAGA_PATH <- "C:/Progra~1/SAGA"
# SAGA_PATH <- "C:/Progra~1/saga-9.7.2_x64"
# SAGA_PATH <- "C:/Progra~1/saga-9.3.3_x64"
# SAGA_PATH <- "C:/Progra~1/saga-9.0.0_x64"
# SAGA_PATH <- "C:/Progra~1/saga-8.5.0_x64"
# SAGA_PATH <- "C:/Progra~1/saga_8.1.3_x64"
# SAGA_PATH <- "C:/Progra~1/saga_2.3.1_x64"

test_that("Write DEM to disc", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env(path = SAGA_PATH)

  data(landslides)
  out_fnm <- file.path(tempdir(), "dem.sgrd")
  write.sgrd(
    data = dem, file = out_fnm, header = dem$header, prec = 2,
    env = env, check.module.exists = FALSE
  )
  expect_true(file.exists(out_fnm))
})

test_that("Read grid from disc", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env(path = SAGA_PATH)

  data(landslides)
  fnm <- file.path(tempdir(), "dem.sgrd")
  grd <- read.sgrd(fnm, prec = 2, env = env)

  expect_equal(sum(is.na(grd$data)), sum(is.na(dem$data)))
  maxdiff <- max(as.vector(grd$data - dem$data), na.rm = TRUE)
  expect_true(maxdiff <= 0.0051)
  expect_true(abs(dem$header$xllcenter - grd$header$xllcenter) < 0.005)
})

test_that("Slope", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env(path = SAGA_PATH)
  out_fnm <- file.path(tempdir(), "slope.sgrd")

  rsaga.slope.asp.curv(file.path(tempdir(), "dem.sgrd"),
    out.slope = out_fnm,
    method = "poly2zevenbergen", env = env, check.module.exists = FALSE
  )
  expect_true(file.exists(out_fnm))
  try(unlink(out_fnm))
})

test_that("Fill Sinks", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env(path = SAGA_PATH)
  out_fnm <- file.path(tempdir(), "fill_sinks.sgrd")

  rsaga.fill.sinks(file.path(tempdir(), "dem.sgrd"),
    out.dem = out_fnm,
    method = "planchon.darboux.2001", env = env, check.module.exists = FALSE
  )

  expect_true(file.exists(out_fnm))

  grd <- read.sgrd(out_fnm, prec = 3, env = env)
  meddiff <- median(as.vector(grd$data - dem$data), na.rm = TRUE)
  expect_true(meddiff <= 0.000001)
  expect_true(abs(dem$header$xllcenter - grd$header$xllcenter) < 0.005)

  try(unlink(out_fnm))
})

test_that("Sink Route", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env(path = SAGA_PATH)
  out_fnm <- file.path(tempdir(), "sink_route.sgrd")

  rsaga.sink.route(file.path(tempdir(), "dem.sgrd"),
    out.sink = out_fnm,
    env = env, check.module.exists = FALSE
  )
  expect_true(file.exists(out_fnm))
  try(unlink(out_fnm))
})

test_that("Sink Removal", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env(path = SAGA_PATH)
  out_fnm <- file.path(tempdir(), "sink_removal.sgrd")

  rsaga.sink.removal(file.path(tempdir(), "dem.sgrd"),
    in.sinkroute = file.path(tempdir(), "sink_route.sgrd"),
    out.dem = out_fnm, env = env, check.module.exists = FALSE
  )
  expect_true(file.exists(out_fnm))

  grd <- read.sgrd(out_fnm, prec = 3, env = env)
  meddiff <- median(as.vector(grd$data - dem$data), na.rm = TRUE)
  expect_true(meddiff <= 0.000001)
  expect_true(abs(dem$header$xllcenter - grd$header$xllcenter) < 0.005)
  try(unlink(out_fnm))
})

test_that("Close Gaps", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env(path = SAGA_PATH)
  out_fnm <- file.path(tempdir(), "close_gaps.sgrd")

  rsaga.close.gaps(file.path(tempdir(), "dem.sgrd"),
    out.dem = out_fnm,
    env = env, check.module.exists = FALSE
  )
  expect_true(file.exists(out_fnm))

  grd <- read.sgrd(out_fnm, prec = 3, env = env)
  meddiff <- median(as.vector(grd$data - dem$data), na.rm = TRUE)
  expect_true(meddiff <= 0.000001)
  expect_equal(sum(is.na(as.vector(grd$data))), 0)
  expect_true(abs(dem$header$xllcenter - grd$header$xllcenter) < 0.005)
  try(unlink(out_fnm))
})

test_that("Hillshade", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env(path = SAGA_PATH)
  out_fnm <- file.path(tempdir(), "hillshade.sgrd")

  rsaga.hillshade(file.path(tempdir(), "dem.sgrd"),
    out.grid = out_fnm,
    exaggeration = 10, env = env, check.module.exists = FALSE
  )
  expect_true(file.exists(out_fnm))
  try(unlink(out_fnm))
})

test_that("PISR2", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env(path = SAGA_PATH)
  out_fnm <- file.path(tempdir(), "pisr2.sgrd")

  rsaga.pisr2(
    in.dem = file.path(tempdir(), "dem.sgrd"), out.direct.grid = out_fnm,
    out.diffuse.grid = file.path(tempdir(), "pisr2_diffuse.sgrd"),
    latitude = 43, unit = "kWh/m2", method = "lumped",
    lmp.transmittance = 60, time.range = c(0, 24), time.step = 3,
    start.date = list(day = 1, month = 10, year = 2016),
    end.date = list(day = 6, month = 12, year = 2016),
    day.step = 5, env = env, show = FALSE, check.module.exists = FALSE
  )
  expect_true(file.exists(out_fnm))

  out_fnm2 <- file.path(tempdir(), "pisr22.sgrd")

  # same parameters, finer discretization:
  rsaga.pisr2(
    in.dem = file.path(tempdir(), "dem.sgrd"), out.direct.grid = out_fnm2,
    out.diffuse.grid = file.path(tempdir(), "pisr2_diffuse.sgrd"),
    latitude = 43, unit = "kWh/m2", method = "lumped",
    lmp.transmittance = 60, time.range = c(0, 24), time.step = 1,
    start.date = list(day = 1, month = 10, year = 2016),
    end.date = list(day = 6, month = 12, year = 2016),
    day.step = 2, env = env, show = FALSE, check.module.exists = FALSE
  )

  grd1 <- read.sgrd(out_fnm, prec = 5, env = env)
  grd2 <- read.sgrd(out_fnm2, prec = 5, env = env)
  expect_true(abs(grd1$header$xllcenter - grd2$header$xllcenter) < 0.0005)
  # median deviation <8%, but not 0:
  medratio <- median(as.vector(grd1$data / grd2$data), na.rm = TRUE)
  expect_true(abs(medratio - 1 ) < 0.08)
  expect_true(abs(medratio - 1 ) > 0)

  try(unlink(out_fnm))
  try(unlink(out_fnm2))
})

test_that("Topdown Processing", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env(path = SAGA_PATH)
  out_fnm <- file.path(tempdir(), "carea.sgrd")

  rsaga.topdown.processing(
    in.dem = file.path(tempdir(), "dem.sgrd"),
    out.carea = out_fnm, env = env,
    check.module.exists = FALSE
  )
  expect_true(file.exists(out_fnm))
  try(unlink(out_fnm))
})

test_that("Wetness Index", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env(path = SAGA_PATH)
  out_fnm <- file.path(tempdir(), "wi.sgrd")

  rsaga.wetness.index(
    in.dem = file.path(tempdir(), "dem.sgrd"),
    out.wetness.index = out_fnm, env = env,
    check.module.exists = FALSE
  )
  expect_true(file.exists(out_fnm))
  try(unlink(out_fnm))
})

test_that("Grid Calculus", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env(path = SAGA_PATH)
  out_fnm <- file.path(tempdir(), "calculus.sgrd")

  rsaga.grid.calculus(c(file.path(tempdir(), "dem.sgrd"),
                        file.path(tempdir(), "dem.sgrd")),
    out.grid = out_fnm, formula = "a + b",
    env = env, check.module.exists = FALSE
  )
  expect_true(file.exists(out_fnm))

  grd <- read.sgrd(out_fnm, prec = 3, env = env)
  meddiff <- median(as.vector(grd$data - 2*dem$data), na.rm = TRUE)
  expect_true(meddiff <= 0.000001)
  expect_equal(sum(is.na(as.vector(grd$data))),
               sum(is.na(as.vector(dem$data))))
  expect_true(abs(dem$header$xllcenter - grd$header$xllcenter) < 0.005)
  try(unlink(out_fnm))
})

test_that("Contour", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env(path = SAGA_PATH)
  out_fnm <- file.path(tempdir(), "contour.shp")

  rsaga.contour(file.path(tempdir(), "dem.sgrd"),
    out.shapefile = out_fnm, zstep = 5, env = env,
    check.module.exists = FALSE
  )
  expect_true(file.exists(out_fnm))

  shp <- sf::read_sf(out_fnm)
  expect_equal(as.character(sf::st_geometry_type(shp)[1]), "LINESTRING")
  try(unlink(out_fnm))
})

test_that("Grid to Points Randomly", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env(path = SAGA_PATH)
  out_fnm <- file.path(tempdir(), "grid_to_points_randomly.shp")

  rsaga.grid.to.points.randomly(
    in.grid = file.path(tempdir(), "dem.sgrd"),
    out.shapefile = out_fnm,
    freq = 50, env = env, check.module.exists = FALSE
  )
  expect_true(file.exists(out_fnm))

  shp <- sf::read_sf(out_fnm)
  expect_equal(as.character(sf::st_geometry_type(shp)[1]), "POINT")
  # need to be tolerant here because actual number of sampled points is random:
  expect_true(nrow(shp) > 0.8*(dem$header$ncols * dem$header$nrows)/50)
  try(unlink(out_fnm))
})

test_that("Grid to Points", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env(path = SAGA_PATH)
  out_fnm <- file.path(tempdir(), "grid_to_points.shp")

  rsaga.grid.to.points(
    in.grid = file.path(tempdir(), "dem.sgrd"),
    out.shapefile = out_fnm,
    exclude.nodata = TRUE,
    env = env, check.module.exists = FALSE
  )
  expect_true(file.exists(out_fnm))

  shp <- sf::read_sf(out_fnm)
  expect_equal(as.character(sf::st_geometry_type(shp)[1]), "POINT")
  # DEM contains ~0.4% nodata values:
  expect_true(nrow(shp) != (dem$header$ncols * dem$header$nrows))
  expect_true(nrow(shp) > 0.995*(dem$header$ncols * dem$header$nrows))
  try(unlink(out_fnm))
})

try(unlink(file.path(tempdir(), "dem.sgrd")))
