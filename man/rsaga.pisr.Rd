% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RSAGA-modules.R
\name{rsaga.pisr}
\alias{rsaga.pisr}
\title{Potential incoming solar radiation}
\usage{
rsaga.pisr(
  in.dem,
  in.svf.grid = NULL,
  in.vapour.grid = NULL,
  in.latitude.grid = NULL,
  in.longitude.grid = NULL,
  out.direct.grid,
  out.diffuse.grid,
  out.total.grid = NULL,
  out.ratio.grid = NULL,
  out.duration,
  out.sunrise,
  out.sunset,
  local.svf = TRUE,
  latitude,
  unit = c("kWh/m2", "kJ/m2", "J/cm2"),
  solconst = 1367,
  enable.bending = FALSE,
  bending.radius = 6366737.96,
  bending.lat.offset = "user",
  bending.lat.ref.user = 0,
  bending.lon.offset = "center",
  bending.lon.ref.user = 0,
  method = c("height", "components", "lumped"),
  hgt.atmosphere = 12000,
  hgt.water.vapour.pressure = 10,
  cmp.pressure = 1013,
  cmp.water.content = 1.68,
  cmp.dust = 100,
  lmp.transmittance = 70,
  time.range = c(0, 24),
  time.step = 0.5,
  start.date = list(day = 21, month = 3),
  end.date = NULL,
  day.step = 5,
  env = rsaga.env(),
  ...
)
}
\arguments{
\item{in.dem}{name of input digital elevation model (DEM) grid in SAGA grid format (default extension: \code{.sgrd})}

\item{in.svf.grid}{Optional input grid in SAGA format:  Sky View Factor; see also \code{local.svf}}

\item{in.vapour.grid}{Optional input grid in SAGA format:  Water vapour pressure (mbar); see also argument \code{hgt.water.vapour.pressure}}

\item{in.latitude.grid}{Optional input grid in SAGA format: Latitude (degree) of each grid cell}

\item{in.longitude.grid}{see \code{in.latitude.grid}}

\item{out.direct.grid}{Output grid: Direct insolation (unit selected by \code{unit} argument)}

\item{out.diffuse.grid}{Output grid: Diffuse insolation}

\item{out.total.grid}{Optional output grid: Total insolation, i.e. sum of direct and diffuse incoming solar radiation}

\item{out.ratio.grid}{Optional output grid: Direct to diffuse ratio}

\item{out.duration}{Optional output grid: Duration of insolation}

\item{out.sunrise}{Optional output grid: time of sunrise; only calculated if time span is set to single day}

\item{out.sunset}{Time of sunset; see \code{out.sunrise}}

\item{local.svf}{logical (default: \code{TRUE}; if TRUE, use sky view factor based on local slope (after Oke, 1988), if no sky view factor grid is provided in \code{in.svf.grid}}

\item{latitude}{Geographical latitude in degree North (negative values indicate southern hemisphere)}

\item{unit}{unit of insolation output grids: \code{"kWh/m2"} (default) \code{"kJ/m2"}, or \code{"J/cm2"}}

\item{solconst}{solar constant, defaults to 1367 W/m2}

\item{enable.bending}{logical (default: \code{FALSE}): incorporate effects of planetary bending?}

\item{bending.radius}{Planetary radius, default \code{6366737.96}}

\item{bending.lat.offset}{if bending is enabled: latitudinal reference  is \code{"user"}-defined (default), or relative to \code{"top"}, \code{"center"} or \code{"bottom"} of grid?}

\item{bending.lat.ref.user}{user-defined lat. reference for bending, see \code{bending.lat.offset}}

\item{bending.lon.offset}{longitudinal reference, i.e. local time,  is \code{"user"}-defined, or relative to \code{"top"}, \code{"center"} (default) or \code{"bottom"} of grid?}

\item{bending.lon.ref.user}{user-defined reference for local time (Details??)}

\item{method}{specifies how the atmospheric components should be  accounted for: either based on the height of atmosphere and vapour pressure (\code{"height"}, or numeric code 0), or air pressure, water and dust content (\code{"components"}, code 1), or lumped atmospheric transmittance (\code{"lumped"}, code \code{0})}

\item{hgt.atmosphere}{Height of atmosphere (in m); default 12000 m}

\item{hgt.water.vapour.pressure}{Water vapour pressure in mbar (default 10 mbar); This value is used if no vapour pressure grid is given in  argument \code{in.vapour.grid}}

\item{cmp.pressure}{atmospheric pressure in mbar, defaults to 1013 mbar}

\item{cmp.water.content}{water content of a vertical slice of the atmosphere in cm: between 1.5 and 1.7cm, average 1.68cm (default)}

\item{cmp.dust}{dust factor in ppm; defaults to 100 ppm}

\item{lmp.transmittance}{transmittance of the atmosphere in percent; usually between 60 (humid areas) and 80 percent (deserts)}

\item{time.range}{numeric vector of length 2:  time span (hours of the day) for numerical integration}

\item{time.step}{time step in hours for numerical integration}

\item{start.date}{list of length two, giving the start date in \code{day} and \code{month} components as numbers; these numbers are one-based (SAGA_CMD uses zero-based numbers internally), i.e. Jan. 1st is \code{list(day=1,month=1)}}

\item{end.date}{see \code{start.date}}

\item{day.step}{if \code{days} indicates a range of days, this specifies the time step (number of days) for calculating the incoming solar radiation}

\item{env}{RSAGA geoprocessing environment obtained with \code{\link[=rsaga.env]{rsaga.env()}}; this argument is required for version control (see Note)}

\item{...}{optional arguments to be passed to \code{\link[=rsaga.geoprocessor]{rsaga.geoprocessor()}}}
}
\description{
This function calculates the potential incoming solar radiation in an area using different atmospheric models; module available in SAGA GIS 2.0.6+.
}
\details{
According to SAGA GIS 2.0.7 documentation, "Most options should do well, but TAPES-G based diffuse irradiance calculation ("Atmospheric Effects" methods 2 and 3) needs further revision!" I.e. be careful with \code{method = "components"} and \code{method = "lumped"}.
}
\note{
This module is computationally very intensive (depending on the size of the grid and the time resolution, of course). The performance seems to have much improved in SAGA GIS 2.1.0, which by default runs this module in multicore mode (at the release candidate 1 for Windows does).

SAGA_CMD uses zero-based days and months, but this R function uses the standard one-based days and months (e.g. day 1 is the first day of the month, month 1 is January) and translates to the SAGA system.

This function uses module Potential Incoming Solar Radiation from SAGA library \code{ta_lighting} in SAGA version 2.0.6+.
}
\references{
Boehner, J., Antonic, O. (2009): Land surface parameters specific to topo-climatology. In: Hengl, T. and Reuter, H. I. (eds.): Geomorphometry - Concepts, Software, Applications. Elsevier.

Oke, T.R. (1988): Boundary layer climates. London, Taylor and Francis.

Wilson, J.P., Gallant, J.C. (eds.), 2000: Terrain analysis - principles and applications. New York, John Wiley and Sons.
}
\seealso{
\code{\link[=rsaga.hillshade]{rsaga.hillshade()}}; for similar modules in older SAGA versions (pre-2.0.6) see \code{\link[=rsaga.solar.radiation]{rsaga.solar.radiation()}} and \code{\link[=rsaga.insolation]{rsaga.insolation()}}
}
\author{
Alexander Brenning (R interface), Olaf Conrad (SAGA module)
}
\keyword{interface}
\keyword{spatial}
