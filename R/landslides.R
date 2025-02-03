#' @title Landslide inventory data
#' @description Landslide data from southern Ecuador
#' @description Landslide data from southern Ecuador
#' @format The landslide dataset consists of three objects: `landslides`, `dem`, and `study_area`.
#'
#' - `landslides`: A `data.frame` with 1535 rows and 3 variables representing landslide initiation points in the
#'   *Reserva Biologica San Francisco* (RBSF) area of the tropical Andes in Southern Ecuador. The variables are:
#'   \itemize{
#'     \item `lslpts`: Landslide initiation point (boolean)
#'     \item `x` and `y`: Coordinates in UTM zone 17S (EPSG: 32717)
#'   }
#'   The landslide inventory was mapped by Stoyan (2000) in the field and by the presence of landslide scars in aerial imagery.
#'
#' @details Landslide data provided here are a subset of that used by Muenchow
#'   *et al.* (2012) to predict spatially landslide susceptibility using
#'   generalized additive models (GAMs). Specifically, the here provided
#'   landslides belong to the "natural" part of the *RBSF* area. Please
#'   refer also to the accompanying vignette for an introductory tutorial on the
#'   use of the RSAGA package for terrain analysis, geoprocessing, and
#'   model-building using these data.
#' @seealso [dem], [study_area]
#'
#' @source Muenchow, J., Brenning, A., Richter, R. (2012): Geomorphic process rates of
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
#'
#' # Pick slope values at landslide points,
#' # added to landslides data.frame as variable "slope":
#' landslides <- pick.from.saga.grid(data = landslides,
#'                                   filename = "slope",
#'                                   varname = "slope")
#' }
"landslides"
