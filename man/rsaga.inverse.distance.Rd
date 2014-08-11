\name{rsaga.inverse.distance}
\alias{rsaga.inverse.distance}
\alias{rsaga.modified.quadratic.shephard}
\alias{rsaga.nearest.neighbour}
\alias{rsaga.triangulation}
\title{Spatial Interpolation Methods}
\usage{
  rsaga.inverse.distance(in.shapefile, out.grid, field,
    power = 1, maxdist, nmax = 100, target,
    env = rsaga.env(), ...)

  rsaga.nearest.neighbour(in.shapefile, out.grid, field,
    target, env = rsaga.env(), ...)

  rsaga.modified.quadratic.shephard(in.shapefile, out.grid,
    field, quadratic.neighbors = 13,
    weighting.neighbors = 19, target, env = rsaga.env(),
    ...)

  rsaga.triangulation(in.shapefile, out.grid, field,
    target, env = rsaga.env(), ...)
}
\arguments{
  \item{in.shapefile}{Input: point shapefile (default
  extension: \code{.shp}).}

  \item{out.grid}{Output: filename for interpolated grid
  (SAGA grid file). Existing files will be overwritten!}

  \item{field}{numeric or character: number or name of
  attribute in the shapefile's attribute table to be
  interpolated; the first attribute is represented by a
  zero.}

  \item{power}{numeric (>0): exponent used in inverse
  distance weighting (usually 1 or 2)}

  \item{maxdist}{numeric: maximum distance of points to be
  used for inverse distance interpolation (search radius);
  no search radius is applied when this argument is missing
  or equals \code{Inf}}

  \item{nmax}{Maximum number of nearest points to be used
  for interpolation; \code{nmax=Inf} is a valid value (no
  upper limit)}

  \item{quadratic.neighbors}{integer >=5; ??}

  \item{weighting.neighbors}{integer >=3; ??}

  \item{target}{required argument of type list: parameters
  identifying the target area, e.g. the x/y extent and
  cellsize, or name of a reference grid; see
  \code{\link{rsaga.target}}.}

  \item{env}{RSAGA geoprocessing environment created by
  \code{\link{rsaga.env}}, required because module(s)
  depend(s) on SAGA version}

  \item{...}{Optional arguments to be passed to
  \code{\link{rsaga.geoprocessor}}, including the
  \code{env} RSAGA geoprocessing environment.}
}
\description{
  Spatial interpolation of point data using inverse
  distance to a power (inverse distance weighting, IDW),
  nearest neighbors, or modified quadratic shephard.
}
\details{
  These functions use modules from the \code{grid_gridding}
  SAGA GIS library. They do not support SAGA GIS 2.0.4,
  which differs in some argument names and
  parameterizations. Target grid parameterization by grid
  file name currently doesn't work with SAGA GIS 2.1.0
  Release Candidate 1 (see also
  \code{\link{rsaga.target}}); stay tuned for future
  updates and fixes.
}
\note{
  The 'Inverse Distance Weighted' module of SAGA GIS not
  only support inverse-distance weighted interpolation, but
  also exponential and other weighting schemes (command
  line argument WEIGHTING); these are however not
  accessible through this function, but only through the
  \code{rsaga.geoprocessor}, if needed. See
  \code{rsaga.get.usage("grid_gridding","Inverse Distance
  Weighted")} for details.

  See the example section in the help file for
  \code{\link[shapefiles]{write.shapefile}} in package
  \code{shapefiles} to learn how to apply these
  interpolation functions to a shapefile exported from a
  data.frame.

  Modified Quadratic Shephard method: based on module 660
  in TOMS (see references).
}
\author{
  Alexander Brenning (R interface), Andre Ringeler and Olaf
  Conrad (SAGA modules)
}
\references{
  QSHEP2D: Fortran routines implementing the Quadratic
  Shepard method for bivariate interpolation of scattered
  data (see R. J. Renka, ACM TOMS 14 (1988) pp.149-150).
  Classes: E2b. Interpolation of scattered, non-gridded
  multivariate data.
}
\seealso{
  \code{\link{rsaga.target}}; \code{\link[gstat]{idw}} in
  package \code{gstat}.
}
\keyword{interface}
\keyword{spatial}

