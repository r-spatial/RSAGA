\name{rsaga.local.morphometry}
\alias{rsaga.aspect}
\alias{rsaga.curvature}
\alias{rsaga.local.morphometry}
\alias{rsaga.plan.curvature}
\alias{rsaga.profile.curvature}
\alias{rsaga.slope}
\title{Local Morphometry}
\usage{
  rsaga.local.morphometry(in.dem, out.slope, out.aspect,
    out.curv, out.hcurv, out.vcurv,
    method = "poly2zevenbergen", env = rsaga.env(), ...)

  rsaga.slope(in.dem, out.slope,
    method = "poly2zevenbergen", ...)

  rsaga.aspect(in.dem, out.aspect,
    method = "poly2zevenbergen", ...)

  rsaga.curvature(in.dem, out.curv,
    method = "poly2zevenbergen", ...)

  rsaga.plan.curvature(in.dem, out.hcurv,
    method = "poly2zevenbergen", ...)

  rsaga.profile.curvature(in.dem, out.vcurv,
    method = "poly2zevenbergen", ...)
}
\arguments{
  \item{in.dem}{input: digital elevation model (DEM) as
  SAGA grid file (default file extension: \code{.sgrd})}

  \item{out.slope}{optional output: slope (in radian)}

  \item{out.aspect}{optional output: aspect (in radian;
  north=0, clockwise angles)}

  \item{out.curv}{optional output: curvature}

  \item{out.hcurv}{optional output: horizontal curvature
  (plan curvature)}

  \item{out.vcurv}{optional output: vertical curvature
  (profile curvature)}

  \item{method}{character or numeric: algorithm (see
  References): Maximum Slope - Travis et al. (1975)
  (\code{"maxslope"}, or 0), Max. Triangle Slope - Tarboton
  (1997) (\code{"maxtriangleslope"}, or 1), Least Squares
  Fit Plane - Costa-Cabral and Burgess (1996)
  (\code{"lsqfitplane"}, or 2), Fit 2nd Degree Polynomial -
  Bauer et al. (1985) (\code{"poly2bauer"}, or 3), Fit 2nd
  Degree Polynomial - Heerdegen and Beran (1982)
  (\code{"poly2heerdegen"}, or 4), default: Fit 2nd Degree
  Polynomial - Zevenbergen and Thorne (1987)
  (\code{"poly2zevenbergen"}, or 5), Fit 3rd Degree
  Polynomial - Haralick (1983) (\code{"poly3haralick"}, or
  6).}

  \item{env}{list, setting up a SAGA geoprocessing
  environment as created by \code{\link{rsaga.env}}}

  \item{...}{further arguments to
  \code{\link{rsaga.geoprocessor}}}
}
\value{
  The type of object returned depends on the \code{intern}
  argument passed to the \code{\link{rsaga.geoprocessor}}.
  For \code{intern=FALSE} it is a numerical error code (0:
  success), or otherwise (default) a character vector with
  the module's console output.
}
\description{
  Calculates local morphometric terrain attributes (i.e.
  slope, aspect and curvatures).
}
\examples{
\dontrun{
# a simple slope algorithm:
rsaga.slope("lican.sgrd","slope","maxslope")
# same for ASCII grids (default extension .asc):
rsaga.esri.wrapper(rsaga.slope,in.dem="lican",out.slope="slope",method="maxslope")
}
}
\author{
  Alexander Brenning (R interface), Olaf Conrad (SAGA
  module)
}
\references{
  General references:

  Jones KH (1998) A comparison of algorithms used to
  compute hill slope as a property of the DEM. Computers
  and Geosciences. 24 (4): 315-323.

  References on specific methods:

  Maximum Slope:

  Travis, M.R., Elsner, G.H., Iverson, W.D., Johnson, C.G.
  (1975): VIEWIT: computation of seen areas, slope, and
  aspect for land-use planning. USDA F.S. Gen. Tech. Rep.
  PSW-11/1975, 70 p. Berkeley, California, U.S.A.

  Maximum Triangle Slope:

  Tarboton, D.G. (1997): A new method for the determination
  of flow directions and upslope areas in grid digital
  elevation models. Water Ressources Research, 33(2):
  309-319.

  Least Squares or Best Fit Plane:

  Beasley, D.B., Huggins, L.F. (1982): ANSWERS: User's
  manual. U.S. EPA-905/9-82-001, Chicago, IL, 54 pp.

  Costa-Cabral, M., Burges, S.J. (1994): Digital Elevation
  Model Networks (DEMON): a model of flow over hillslopes
  for computation of contributing and dispersal areas.
  Water Resources Research, 30(6): 1681-1692.

  Fit 2nd Degree Polynomial:

  Bauer, J., Rohdenburg, H., Bork, H.-R. (1985): Ein
  Digitales Reliefmodell als Vorraussetzung fuer ein
  deterministisches Modell der Wasser- und Stoff-Fluesse.
  Landschaftsgenese und Landschaftsoekologie, H. 10,
  Parameteraufbereitung fuer deterministische
  Gebiets-Wassermodelle, Grundlagenarbeiten zur Analyse von
  Agrar-Oekosystemen, eds.: Bork, H.-R., Rohdenburg, H., p.
  1-15.

  Heerdegen, R.G., Beran, M.A. (1982): Quantifying source
  areas through land surface curvature. Journal of
  Hydrology, 57.

  Zevenbergen, L.W., Thorne, C.R. (1987): Quantitative
  analysis of land surface topography. Earth Surface
  Processes and Landforms, 12: 47-56.

  Fit 3.Degree Polynom:

  Haralick, R.M. (1983): Ridge and valley detection on
  digital images. Computer Vision, Graphics and Image
  Processing, 22(1): 28-38.

  For a discussion on the calculation of slope by ArcGIS
  check these links:

  \url{http://forums.esri.com/Thread.asp?c=93&f=1734&t=239914}

  \url{http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?topicname=how_slope_works}
}
\seealso{
  \code{\link{rsaga.parallel.processing}},
  \code{\link{rsaga.geoprocessor}}, \code{\link{rsaga.env}}
}
\keyword{interface}
\keyword{spatial}

