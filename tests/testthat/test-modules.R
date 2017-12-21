context("RSAGA-modules")

library(digest)
library(rgdal)

test_that("Write DEM to disc", {
  testthat::skip_on_travis()
  
  env <- rsaga.env2()
  
  data(landslides)
  write.sgrd(data = dem, file = file.path(tempdir(), "dem.sgrd"), header = dem$header, 
             env = env, check.module.exists=FALSE)
  test <- read.sgrd(file.path(tempdir(), "dem.sgrd"), env=env, check.module.exists=FALSE)
  
  expect_true(digest(test, algo="md5") %in% c("137181ee7294515ceb6ccf04fe975cfd"))
  
})

test_that("Slope", {
  testthat::skip_on_travis()
  
  env <- rsaga.env2()
  
  rsaga.slope.asp.curv(file.path(tempdir(), "dem.sgrd"), out.slope = file.path(tempdir(), "slope.sgrd"), 
                       method = "poly2zevenbergen",env = env, check.module.exists=FALSE) 
  test <- read.sgrd(file.path(tempdir(), "slope.sgrd"), env=env, check.module.exists=FALSE)
  
  expect_true(digest(test, algo="md5") %in% c("b2852c8fa289636908e1322ee33c3c0b",
                                              "b1b0c8c02db274cad530d2021abd7032"))
})

test_that("Fill Sinks", {
  testthat::skip_on_travis()
  
  env <- rsaga.env2()
  
  rsaga.fill.sinks(file.path(tempdir(), "dem.sgrd"), out.dem= file.path(tempdir(), "fill_sinks.sgrd"), 
                   method="planchon.darboux.2001", env=env, check.module.exists=FALSE)
  test <- read.sgrd(file.path(tempdir(), "fill_sinks.sgrd"), env=env, check.module.exists=FALSE)
  
  expect_true(digest(test, algo="md5") %in% c("b2852c8fa289636908e1322ee33c3c0b",
                                              "7b67ea5c169b07dd6965c88aa22a48ee"))
})

test_that("Sink Route", {
  testthat::skip_on_travis()
  
  env <- rsaga.env2()
  
  rsaga.sink.route(file.path(tempdir(), "dem.sgrd"), out.sink= file.path(tempdir(), "sink_route.sgrd"),  
                   env=env, check.module.exists=FALSE)
  
  test <- read.sgrd(file.path(tempdir(), "sink_route.sgrd"), env=env, check.module.exists=FALSE)
  
  expect_true(digest(test, algo="md5") %in% c("1f538bc0a415dc127c69d882e191cafc"))
})

test_that("Sink Removal", {
  testthat::skip_on_travis()
  
  env <- rsaga.env2()
  
  rsaga.sink.removal(file.path(tempdir(), "dem.sgrd"), in.sinkroute = file.path(tempdir(), "sink_route.sgrd"),  
                     out.dem= file.path(tempdir(), "sink_removal.sgrd"), env=env, check.module.exists=FALSE)
  test <- read.sgrd(file.path(tempdir(), "sink_removal.sgrd"), env=env, check.module.exists=FALSE)
  
  expect_true(digest(test, algo="md5") %in% c("5feb7dbe2d3e947557d38f194ad37310", 
                                              "b8f14181cdad9bb304c3c0a7e889afe7"))
})

test_that("Close Gaps", {
  testthat::skip_on_travis()
  
  env <- rsaga.env2()
  
  rsaga.close.gaps(file.path(tempdir(), "dem.sgrd"), out.dem= file.path(tempdir(), "close_gaps.sgrd"), 
                   env=env, check.module.exists=FALSE)
  test <- read.sgrd(file.path(tempdir(), "close_gaps.sgrd"), env=env, check.module.exists=FALSE)
  
  expect_true(digest(test, algo="md5") %in% c("f110d3106bc8c54554b97b6642a39978", 
                                              "b8f14181cdad9bb304c3c0a7e889afe7", 
                                              "fedc13e7fe041b8a1d7dae5e5fbb729d"
  ))
})

test_that("Hillshade", {
  testthat::skip_on_travis()
  
  env <- rsaga.env2()
  
  rsaga.hillshade(file.path(tempdir(), "dem.sgrd"), out.grid= file.path(tempdir(), "hillshade.sgrd"), 
                  exaggeration=10, env=env, check.module.exists=FALSE)
  test <- read.sgrd(file.path(tempdir(), "hillshade.sgrd"), env=env, check.module.exists=FALSE)
  
  expect_true(digest(test, algo="md5") %in% c("ff369e210cc14ac36eeafca86003f5c0"))
})

test_that("PISR2", {
  testthat::skip_on_travis()
  
  env <- rsaga.env2()
  
  rsaga.pisr2(in.dem = file.path(tempdir(), "dem.sgrd"), out.direct.grid = file.path(tempdir(), "pisr2.sgrd"), 
              out.diffuse.grid = file.path(tempdir(), "pisr2_diffuse.sgrd"),
              latitude = 43, unit = "kWh/m2", method = "lumped",
              lmp.transmittance = 60, time.range = c(0,24), time.step = 3,
              start.date = list(day=1,month=10,year=2016), end.date = list(day=6,month=12,year=2016),
              day.step = 10, env = env, show = FALSE, check.module.exists=FALSE)
  test <- read.sgrd(file.path(tempdir(), "pisr2.sgrd"), env=env, check.module.exists=FALSE)
  
  expect_true(digest(test, algo="md5") %in% c("ef3290bb060d890a4c07430c7ef9bf24",
                                              "4249dcc756f3bcc80d05e5224959e66d",
                                              "e204a7c8ff803575b0e4c0371600038b"))
})

test_that("Topdown Processing", {
  testthat::skip_on_travis()
  
  env <- rsaga.env2()
  
  rsaga.topdown.processing(in.dem = file.path(tempdir(), "dem.sgrd"), 
                           out.carea = file.path(tempdir(), "carea.sgrd"), env=env, 
                           check.module.exists=FALSE)
  test <- read.sgrd(file.path(tempdir(), "carea.sgrd"), env=env, check.module.exists=FALSE)
  
  expect_true(digest(test, algo="md5") %in% c("39b364b3605a1422ff6c96a1c5b60319" ,
                                              "6fdcd2d52bc35870600aa1505bb49791"
  ))
})

test_that("Wetness Index", {
  testthat::skip_on_travis()
  
  env <- rsaga.env2()
  
  rsaga.wetness.index(in.dem = file.path(tempdir(), "dem.sgrd"), 
                      out.wetness.index = file.path(tempdir(), "wi.sgrd"), env=env, 
                      check.module.exists=FALSE)
  
  test <- read.sgrd(file.path(tempdir(), "wi.sgrd"), env=env, check.module.exists=FALSE)
  
  expect_true(digest(test, algo="md5") %in% c("98fb807d7ec0bf787de6dc4fa88ca35d",
                                              "6d96e79007f8ffd64aeb69111ee052f5"
  ))
})

test_that("Grid Calculus", {
  testthat::skip_on_travis()
  
  env <- rsaga.env2()
  
  rsaga.grid.calculus(c(file.path(tempdir(), "dem.sgrd"), file.path(tempdir(), "dem.sgrd")), 
                      out.grid = file.path(tempdir(), "calculus.sgrd"), formula = "a + b", 
                      env=env, check.module.exists=FALSE)
  test <- read.sgrd(file.path(tempdir(), "calculus.sgrd"), env=env, check.module.exists=FALSE)
  
  expect_true(digest(test, algo="md5") %in% c("ca98e329dbd22d6cd310271f55f60740",
                                              "cb20edb9c5a71d88fb1af9c63c4c33d8"
  ))
})

test_that("Contour", {
  testthat::skip_on_travis()
  
  env <- rsaga.env2()
  
  rsaga.contour(file.path(tempdir(), "dem.sgrd"), 
                out.shapefile = file.path(tempdir(), "contour"), zstep=5, env=env, 
                check.module.exists=FALSE)
  test <- readOGR(tempdir(), "contour")
  
  expect_true(digest(test, algo="md5") %in% c("de4bfbe3b89fca5c831d14fc65498f10"
  ))
})

test_that("Grid to Points Randomly", {
  testthat::skip_on_travis()
  
  env <- rsaga.env2()
  
  rsaga.grid.to.points.randomly(in.grid = file.path(tempdir(), "dem.sgrd"), 
                                out.shapefile = file.path(tempdir(), "grid_to_points_randomly"), 
                                freq = 50, env = env, check.module.exists=FALSE)
  test <- readOGR(tempdir(), "grid_to_points_randomly")
  
  expect_equal(typeof(test@data$VALUE[1]), "double")
  
  
})






