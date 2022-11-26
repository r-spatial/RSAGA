library(RSAGA)

# The following tests are not meant for CRAN.
# SAGA will normally be located automatically by RSAGA through
# a rsaga.env() call; however, I am hard-coding the paths of
# multiple SAGA versions in order to run multiple compatibility
# tests. Use NULL to let RSAGA try to find a SAGA installation.
# SAGA_PATH <- NULL
SAGA_PATH <- "C:/Progra~1/SAGA"
# SAGA_PATH <- "C:/Progra~1/saga_8.1.3_x64"
# SAGA_PATH <- "C:/Progra~1/saga_2.3.1_x64"

test_that("Link to saga", {
  testthat::skip_on_cran()

  env <- rsaga.env(path = SAGA_PATH)

  expect_equal(typeof(env$version), "character")
})


required_modules <- list(
  io_gdal = c("Import Raster"),
  io_grid = c("Import ESRI Arc/Info Grid", "Export ESRI Arc/Info Grid"),
  ta_morphometry = c("Slope, Aspect, Curvature"),
  ta_preprocessor = c("Fill Sinks (Wang & Liu)", "Fill Sinks XXL (Wang & Liu)", "Sink Drainage Route Detection", "Sink Removal"),
  grid_tools = c("Close Gaps", "Close One Cell Gaps"),
  ta_lighting = c("Analytical Hillshading"),
  ta_lighting = c("Potential Incoming Solar Radiation"),
  grid_filter = c("Simple Filter", "Gaussian Filter"),
  ta_hydrology = c("Flow Accumulation (Top-Down)", "SAGA Wetness Index"),
  grid_calculus = c("Grid Calculator"),
  shapes_grid = c("Contour Lines from Grid", "Add Grid Values to Points",
                  "Grid Values to Points", "Grid Values to Points (randomly)"),
  grid_gridding = c("Inverse Distance Weighted", "Nearest Neighbour",
                    "Modifed Quadratic Shepard", "Triangulation"),
  shapes_polygons = c("Intersect", "Union")
)


test_that("All required SAGA libraries could be found", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env(path = SAGA_PATH)

  expect_true(all(names(required_modules) %in% rsaga.get.libraries(path = env$modules))
)
})

test_that("All required SAGA modules could be found", {
  testthat::skip_on_travis()
  testthat::skip_on_cran()

  env <- rsaga.env(path = SAGA_PATH)

  CHK <- TRUE
  for (i in 1:length(required_modules)) {
    lib <- names(required_modules)[i]
    chk <- required_modules[[i]] %in% rsaga.get.lib.modules(lib = lib, env = env)$name
    if (!all(chk))
      cat("The following module(s) is/are missing from library '", lib, "':\n",
          paste0("- ", required_modules[[i]][!chk], collapse = "\n"),
          "\n", sep = "")
    CHK <- CHK & all(chk)
  }

  expect_true(CHK)
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
