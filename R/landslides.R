#' @title Landslide inventory, study area mask and DEM
#' @description  Landslide data
#' @format The `landslides` dataset consists of three objects:
#' \enumerate{
#'   \item{`landslides`} {A dataframe of 1535 rows and 3 variables
#'    representing landslide initiation points in the
#'    *Reserva Biologica San Francisco* (RBSF) area of the tropical Andes
#'    in Southern Ecuador. The variables are:
#'    \itemize{
#'      \item{`lslpts`} {landslide initiation point (boolean)}
#'      \item{`x` and `y`} {Coordinates of coordinate reference system
#'       UTM zone 17S (EPSG: 32717)}
#'       }
#'    The landslide inventory was mapped by Stoyan (2000) in the field and by
#'    the presence of landslide scars in aerial imagery.}
#'   \item `dem` {Digital elevation model given as a .Rd grid, i.e. a list
#'   consisting of the elements `header` (nine properties) and `data`
#'   (grid elevation values in m a.s.l.). The 10 m x 10 m digital elevation model
#'   was triangulated from aerial imagery as described by Jordan *et al.*
#'   (2005) and provided as a courtesy of Lars Ungerechts (2010).}
#'   \item `study_area` {An `sf`-object representing the outlines of
#'   the natural part of the RBSF study area.}
#'   }
#'
#' @note Please note that loading `landslides` overwrites existing objects named
#' `dem`, `landslides` and `study_area`.
#'
#' @details Landslide data provided here are a subset of that used by Muenchow
#'   *et al.* (2012) to predict spatially landslide susceptibility using
#'   generalized additive models (GAMs). Specifically, the here provided
#'   landslides belong to the "natural" part of the *RBSF* area. Please
#'   refer also to the accompanying vignette for an introductory tutorial on the
#'   use of the RSAGA package for terrain analysis, geoprocessing, and
#'   model-building using these data.
#' @name landslides
#' @aliases dem study_area
#'
#' @source \strong{DEM:}
#'
#'   Ungerechts, L. (2010): DEM 10m (triangulated from aerial photo - b/w).
#'   Available online:
#'   <http://vhrz669.hrz.uni-marburg.de/tmf_respect/data_pre.do?citid=901>
#'
#'   Jordan, E., Ungerechts, L., Caceres, B. Penafiel, A. and Francou, B.
#'   (2005): Estimation by photogrammetry of the glacier recession on the
#'   Cotopaxi Volcano (Ecuador) between 1956 and 1997. *Hydrological
#'   Sciences* 50, 949-961.
#'
#'   \strong{Landslide Data:}
#'
#'   Muenchow, J., Brenning, A., Richter, R. (2012): Geomorphic process rates of
#'   landslides along a humidity gradient in the tropical Andes, Geomorphology
#'   139-140, 271-284. DOI: 10.1016/j.geomorph.2011.10.029.
#'
#'   Stoyan, R. (2000): Aktivitaet, Ursachen und Klassifikation der Rutschungen
#'   in San Francisco/Suedecuador. Unpublished diploma thesis, University of
#'   Erlangen-Nuremberg, Germany.
#' @examples
#' \dontrun{
#' library("RSAGA")
#' data(landslides)
#'
#' # Print the DEM header:
#' dem$header
#'
#' # Write the DEM to a SAGA grid:
#' write.sgrd(data = dem, file = "dem", header = dem$header, env = env)
#'
#' # Calculate slope of DEM:
#' rsaga.slope(in.dem = "dem", out.slope = "slope", method = "poly2zevenbergen")
#'
#' # Pick slope values at landslide points,
#' # added to landslides data.frame as variable "slope":
#' landslides <- pick.from.saga.grid(data = landslides,
#'                                   filename = "slope",
#'                                   varname = "slope")
#' }
NULL
