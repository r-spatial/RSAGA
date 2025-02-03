#' @title DEM for the `landslides` dataset
#' @description Digital elevation model (DEM) for the `landslides` dataset's study area in southern Ecuador
#' @format Digital elevation model given as a list
#'   consisting of the elements `header` (nine properties) and `data`
#'   (grid elevation values in m a.s.l.). The 10 m x 10 m digital elevation model
#'   was triangulated from aerial imagery as described by Jordan *et al.*
#'   (2005) and provided as a courtesy of Lars Ungerechts (2010).
#'
#' @note Loading `landslides` replaces any existing objects in the
#' current work environment named `dem`, `landslides`, and `study_area`.
#'
#' @details Landslide data provided here are a subset of that used by Muenchow
#'   *et al.* (2012) to predict spatially landslide susceptibility using
#'   generalized additive models (GAMs). Specifically, the here provided
#'   landslides belong to the "natural" part of the *RBSF* area. Please
#'   refer also to the accompanying vignette for an introductory tutorial on the
#'   use of the RSAGA package for terrain analysis, geoprocessing, and
#'   model-building using these data.
#' @seealso [landslides]
#'
#' @source Ungerechts, L. (2010): DEM 10m (triangulated from aerial photo - b/w).
#'   Retrieved from `http://vhrz669.hrz.uni-marburg.de/tmf_respect/data_pre.do?citid=901`
#'
#'   Jordan, E., Ungerechts, L., Caceres, B. Penafiel, A. and Francou, B.
#'   (2005): Estimation by photogrammetry of the glacier recession on the
#'   Cotopaxi Volcano (Ecuador) between 1956 and 1997. *Hydrological
#'   Sciences* 50, 949-961.
#' @examples
#' \dontrun{
#' library("RSAGA")
#' data(dem)
#'
#' # Print the DEM header:
#' dem$header
#'
#' # Write the DEM to a SAGA grid:
#' write.sgrd(data = dem, file = "dem", header = dem$header, env = env)
#'
#' # Calculate slope of DEM:
#' rsaga.slope(in.dem = "dem", out.slope = "slope", method = "poly2zevenbergen")
#' }
"dem"
