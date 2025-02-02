#' Define target grid for interpolation
#'
#' Define the resolution and extent of a target grid for interpolation by SAGA
#' modules based on (1) user-provided x/y coordinates, (2) an existing SAGA grid
#' file, or (3) the header data of an ASCII grid. Intended to be used with
#' RSAGA's interpolation functions.
#' @name rsaga.target
#' @param target character: method used for defining the target grid
#' @param user.cellsize Only for `target="user.defined"`: raster resolution
#'   (in the grid's map units)
#' @param user.x.extent See `user.y.extent`
#' @param user.y.extent Only for `target="user.defined"`: numeric vectors
#'   of length 2: minimum and maximum coordinates of grid cell center points
#' @param target.grid Only for `target="target.grid"`: character string
#'   giving the name of a SAGA grid file that specifies the extent and
#'   resolution of the target grid; this target grid file may be overwritten,
#'   depending on the specifics of the SAGA GIS module used.
#' @param header Only for `target="header"`: list: ASCII grid header (as
#'   returned e.g. by [read.ascii.grid.header()]) or defined manually;
#'   must at least have components `ncols`, `nrows`, `cellsize`,
#'   and either `x/yllcorner` or `x/yllcenter`.
#' @param env A SAGA geoprocessing environment, see [rsaga.env()].)
#' @note This function is to be used with RSAGA functions
#'   [rsaga.inverse.distance()], [rsaga.nearest.neighbour()]
#'   and [rsaga.modified.quadratic.shephard()]. Note that these are
#'   currently only compatible with SAGA GIS 2.0.5 and higher.
#' @seealso [read.ascii.grid.header()]
#' @examples
#' \dontrun{
#' # IDW interpolation of attribute "z" from the point shapefile
#' # 'points.shp' to a grid with the same extent and resolution
#' # as the (pre-existing) geology grid:
#' rsaga.inverse.distance("points", "dem", field = "z", maxdist = 1000,
#'     target = rsaga.target(target="target.grid",
#'     target.grid = "geology"))
#' }
#' @keywords spatial interface
#' @export
rsaga.target = function(
    target = c("user.defined", "target.grid", "header"),
    user.cellsize = 100,
    user.x.extent, user.y.extent,
    target.grid, header, env = rsaga.env() )
{
    if (any(c("2.0.4","2.0.5","2.0.6","2.0.7","2.0.8",
            "2.1.0","2.1.1","2.1.2","2.1.3","2.1.4",
            "2.2.0","2.2.1","2.2.2","2.2.3") == env$version)){
        stop("rsaga.target doesn't support SAGA GIS Versions older than 2.3.1 any longer")
    }

    target = match.arg.ext(target, base = 0, numeric = TRUE)

    if (target == 2) {
        stopifnot(missing(user.x.extent) & missing(user.y.extent) & missing(target.grid))
        target = 0
        user.cellsize = header$cellsize
        if (!any(names(header) == "xllcenter"))
            header$xllcenter = header$xllcorner + header$cellsize / 2
        if (!any(names(header) == "yllcenter"))
            header$yllcenter = header$yllcorner + header$cellsize / 2
        user.x.extent = c(header$xllcenter, header$xllcenter + header$cellsize * (header$ncols-1))
        user.y.extent = c(header$yllcenter, header$yllcenter + header$cellsize * (header$nrows-1))
    }

    param = list(TARGET_DEFINITION = target)

    if (target == 0) {
        param = c(param,
            TARGET_USER_SIZE = user.cellsize,
            TARGET_USER_XMIN = min(user.x.extent),
            TARGET_USER_XMAX = max(user.x.extent),
            TARGET_USER_YMIN = min(user.y.extent),
            TARGET_USER_YMAX = max(user.y.extent))
    } else if (target == 1) {
        stopifnot(missing(user.x.extent) & missing(user.y.extent))
        target.grid = default.file.extension(target.grid, ".sgrd")
        param = c(param, TARGET_TEMPLATE = target.grid)
    }
    return(param)
}



########     Module io_grid_gdal    ########



#' Import Grid Files to SAGA grid format using GDAL
#'
#' These functions provide simple interfaces for reading and writing grids
#' from/to ASCII grids and Rd files. Grids are stored in matrices, their headers
#' in lists.
#' @name rsaga.import.gdal
#' @param in.grid file name of a grid in a format supported by GDAL
#' @param out.grid output SAGA grid file name; defaults to `in.grid` with
#'   the file extension being removed; file extension should not be specified,
#'   it defaults to `.sgrd`
#' @param env RSAGA geoprocessing environment created by [rsaga.env()]
#' @param ... additional arguments to be passed to `rsaga.geoprocessor`
#' @details The GDAL Raster Import module of SAGA imports grid data from various
#'   file formats using the Geospatial Data Abstraction Library (GDAL) by Frank
#'   Warmerdam. GDAL Versions are specific to SAGA versions:
#' + SAGA 2.1.2 - 2.2.0: GDAL v.1.11.0
#' + SAGA 2.2.1 - 2.2.3: GDAL v.2.1.0 dev
#' + ...
#' + SAGA 8.4.1: GDAL v3.3.0
#' More information is available at <https://gdal.org/>.
#'
#' If `in.grid` has more than one band (e.g. RGB GEOTIFF), then output
#' grids with file names of the form \eqn{in.grid{\_}01.sgrd}{in.grid_01.sgrd},
#' \eqn{in.grid{\_}02.sgrd}{in.grid_02.sgrd} etc. are written, one for each
#' band.
#'
#' Numerous raster formats are currently supported. For SAGA 8.4.1 see e.g.
#' <https://saga-gis.sourceforge.io/saga_tool_doc/8.4.1/io_gdal_0.html>
#' @references GDAL website: <https://gdal.org/>
#' @author Alexander Brenning (R interface), Olaf Conrad / Andre Ringeler (SAGA module), Frank Warmerdam (GDAL)
#' @seealso `read.ascii.grid`, `rsaga.esri.to.sgrd`, `read.sgrd`, `read.Rd.grid`
#' @keywords spatial interface file
#' @export
rsaga.import.gdal = function( in.grid, out.grid, env = rsaga.env(), ... )
{
    if(!missing(out.grid)) {
      out.grid = default.file.extension(out.grid, ".sgrd")
    }
    if (missing(out.grid)) {
        out.grid = set.file.extension(in.grid, "")
        out.grid = substr(out.grid, 1, nchar(out.grid) - 1)
    }
    if (env$version == "2.0.4") {
        param = list( GRIDS = out.grid, FILE = in.grid )
    } else {
        param = list( GRIDS = out.grid, FILES = in.grid)
    }

    module = "Import Raster"

    if (any(c("2.0.4","2.0.5","2.0.6","2.0.7","2.0.8",
              "2.1.0","2.1.1","2.1.2","2.1.3","2.1.4",
              "2.2.0","2.2.1","2.2.2") == env$version)) {
      module = "GDAL: Import Raster"
    }

    rsaga.geoprocessor("io_gdal", module = module,
        param = param, env = env, check.parameters = FALSE, ...)
}




########       Module io_grid       ########


#' Convert ESRI ASCII/binary grids to SAGA grids
#'
#' `rsaga.esri.to.sgrd` converts grid files from ESRI's ASCII (.asc) and binary (.flt) format to SAGA's (version 2) grid format (.sgrd).
#' @name rsaga.esri.to.sgrd
#' @param in.grids character vector of ESRI ASCII/binary grid files (default file extension: `.asc`); files should be located in folder `in.path`
#' @param out.sgrds character vector of output SAGA grid files; defaults to `in.grids` with file extension being replaced by `.sgrd`, which is also the default extension if file names without extension are specified; files will be placed in the current SAGA workspace (default: \code{\link{rsaga.env}()$workspace}, or `env$workspace` if an `env` argument is provided
#' @param in.path folder with `in.grids`
#' @param ... optional arguments to be passed to [rsaga.geoprocessor()], including the `env` RSAGA geoprocessing environment
#' @return The type of object returned depends on the `intern` argument passed to the [rsaga.geoprocessor()]. For `intern=FALSE` it is a numerical error code (0: success), or otherwise (default) a character vector with the module's console output.
#'
#' If multiple `in.grids` are converted, the result will be a vector of numerical error codes of the same length, or the combination of the console outputs with `c()`.
#' @author Alexander Brenning (R interface), Olaf Conrad (SAGA module)
#' @note This function uses module 1 from the SAGA library `io_grid`.
#' @seealso [rsaga.esri.wrapper()] for an efficient way of applying RSAGA to ESRI ASCII/binary grids; [rsaga.env()]
#' @keywords spatial interface file
#' @export
rsaga.esri.to.sgrd = function( in.grids,
    out.sgrds=set.file.extension(in.grids,".sgrd"), in.path, ... )
{
    in.grids = default.file.extension(in.grids,".asc")
    out.sgrds = default.file.extension(out.sgrds,".sgrd")
    if (!missing(in.path))
        in.grids = file.path(in.path,in.grids)
    if (length(in.grids) != length(out.sgrds))
        stop("must have the same number of input and outpute grids")
    res = c()
    for (i in seq_along(in.grids)) try({
        res = c(res, rsaga.geoprocessor("io_grid", "Import ESRI Arc/Info Grid",
            list(FILE=in.grids[i],GRID=out.sgrds[i]), check.parameters = FALSE, ...) )
    })
    invisible(res)
}



#' Convert SAGA grids to ESRI ASCII/binary grids
#'
#' `rsaga.sgrd.to.esri` converts grid files from SAGA's (version 2) grid
#' format (.sgrd) to ESRI's ASCII (.asc)  and binary (.flt) format.
#' @name rsaga.sgrd.to.esri
#' @param in.sgrds character vector of SAGA grid files (`.sgrd`) to be
#'   converted;  files are expected to be found in folder
#'   \code{\link{rsaga.env}()$workspace}, or, if an optional `env` argument
#'   is provided, in `env$workspace`
#' @param out.grids character vector of ESRI ASCII/float output file names;
#'   defaults to `in.sgrds` with the file extension being replaced by
#'   `.asc` or `.flt`, depending on `format`. Files will be
#'   placed in folder `out.path`, existing files will be overwritten
#' @param out.path folder for `out.grids`
#' @param format output file format, either `"ascii"` (default; equivalent:
#'   `format=1`) for ASCII grids or `"binary"` (equivalent: `0`)
#'   for binary ESRI grids (`.flt`).
#' @param georef character: `"corner"` (equivalent numeric code: `0`)
#'   or `"center"` (default; equivalent: `1`). Determines whether the
#'   georeference will be related to the center or corner of its extreme lower
#'   left grid cell.
#' @param prec number of digits when writing floating point values to ASCII grid
#'   files; either a single number (to be replicated if necessary), or a numeric
#'   vector of length `length(in.grids)`
#' @param ... optional arguments to be passed to
#'   [rsaga.geoprocessor()], including the `env` RSAGA
#'   geoprocessing environment
#' @return The type of object returned depends on the `intern` argument
#'   passed to the [rsaga.geoprocessor()]. For `intern=FALSE` it
#'   is a numerical error code (0: success), or otherwise (default) a character
#'   vector with the module's console output.
#' @author Alexander Brenning (R interface), Olaf Conrad (SAGA module)
#' @note This function uses module 0 from the SAGA library `io_grid`.
#' @seealso [rsaga.esri.wrapper()] for an efficient way of applying
#'   RSAGA to ESRI ASCII/binary grids; [rsaga.env()]
#' @keywords spatial interface file
#' @export
rsaga.sgrd.to.esri = function( in.sgrds, out.grids, out.path,
    format="ascii", georef="corner", prec=5, ... )
{
    in.sgrds = default.file.extension(in.sgrds,".sgrd")
    format = match.arg.ext(format,choices=c("binary","ascii"),base=0,ignore.case=TRUE,numeric=TRUE)
    georef = match.arg.ext(georef,choices=c("corner","center"),base=0,ignore.case=TRUE,numeric=TRUE)
    if (missing(out.grids))
        out.grids = set.file.extension(in.sgrds, c(".flt",".asc")[format+1])
    out.grids = default.file.extension(out.grids, c(".flt",".asc")[format+1])
    if (!missing(out.path))
        out.grids = file.path(out.path,out.grids)
    if (length(out.grids) != length(in.sgrds))
        stop("must have the same number of input and outpute grids")
    if ((length(prec)==1) & (length(in.sgrds)>1))
        prec = rep(prec,length(in.sgrds))
    if (length(prec) != length(in.sgrds))
        stop("must have same number of in-/output grids and 'prec' parameters (or length(prec)==1)")
    res = c()
    for (i in seq_along(in.sgrds)) try({
        res = c(res, rsaga.geoprocessor("io_grid", "Export ESRI Arc/Info Grid",
            list( GRID=in.sgrds[i], FILE=out.grids[i], FORMAT=format, GEOREF=georef, PREC=prec[i]), check.parameters = FALSE,
            ...))
    })
    invisible(res)
}


#
########    Module ta_morphometry   ########

#' Slope, Aspect, Curvature
#'
#' Calculates local morphometric terrain attributes (i.e. slope, aspect, and curvatures). Intended for use with SAGA v 2.1.1+. For older versions use [rsaga.local.morphometry()].
#' @name rsaga.slope.asp.curv
#' @param in.dem input: digital elevation model as SAGA grid file (`.sgrd`)
#' @param out.slope optional output: slope
#' @param out.aspect optional output: aspect
#' @param out.cgene optional output: general curvature (1 / map units)
#' @param out.cprof optional output: profile curvature (vertical curvature; 1 / map units)
#' @param out.cplan optional output: plan curvature (horizontal curvature; 1 / map units)
#' @param out.ctang optional output: tangential curvature (1 / map units)
#' @param out.clong optional output: longitudinal curvature (1 / map units) Zevenbergen & Thorne (1987) refer to this as profile curvature
#' @param out.ccros optional output: cross-sectional curvature (1 / map units) Zevenbergen & Thorne (1987) refer to this as the plan curvature
#' @param out.cmini optional output: minimal curvature (1 / map units)
#' @param out.cmaxi optional output: maximal curvature (1 / map units)
#' @param out.ctota optional output: total curvature (1 / map units)
#' @param out.croto optional output: flow line curvature (1 / map units)
#' @param method character algorithm (see References):
#' - 0 Maximum Slope - Travis et al. (1975) (`"maxslope"`)
#' - 1 Max. Triangle Slope - Tarboton (1997) (`"maxtriangleslope"`)
#' - 2 Least Squares Fit Plane - Costa-Cabral & Burgess (1996) (`"lsqfitplane"`)
#' - 3 Fit 2nd Degree Polynomial - Evans (1979) (`"poly2evans"`)
#' - 4 Fit 2nd Degree Polynomial - Heerdegen and Beran (1982) (`"poly2heerdegen"`)
#' - 5 Fit 2nd Degree Polynomial - Bauer et al. (1985) (`"poly2bauer"`)
#' - 6 default: Fit 2nd Degree Polynomial - Zevenbergen & Thorne (1987) (`"poly2zevenbergen"`)
#' - 7 Fit 3rd Degree Polynomial - Haralick (1983) (`"poly3haralick"`)
#' @param unit.slope character or numeric (default `"radians"`):
#' - 0 `"radians"`
#' - 1 `"degrees"`
#' - 2 `"percent"`
#' @param unit.aspect character or numeric (default is 0, or `"radians"`):
#' - 0 `"radians"`
#' - 1 `"degrees"`
#' @param env list, setting up a SAGA geoprocessing environment as created by [rsaga.env()]
#' @param ... further arguments to [rsaga.geoprocessor()]
#' @details Profile and plan curvature calculation (`out.cprof`, `out.cplan`) changed in SAGA GIS 2.1.1+ compared to earlier versions. See the following thread on sourceforge.net for an ongoing discussion: <https://sourceforge.net/p/saga-gis/discussion/354013/thread/e9d07075/#5727>
#' @return The type of object returned depends on the `intern` argument passed to the [rsaga.geoprocessor()]. For `intern=FALSE` it is a numerical error code (0: success), or otherwise (default) a character vector with the module's console output.
#' @references General references:
#'
#' Jones KH (1998) A comparison of algorithms used to compute hill slope as a property of the DEM. Computers and Geosciences. 24 (4): 315-323.
#'
#' References on specific methods:
#'
#' Maximum Slope:
#'
#' Travis, M.R., Elsner, G.H., Iverson, W.D., Johnson, C.G. (1975): VIEWIT: computation of seen areas, slope, and aspect for land-use planning. USDA F.S. Gen. Tech. Rep. PSW-11/1975, 70 p. Berkeley, California, U.S.A.
#'
#' Maximum Triangle Slope:
#'
#' Tarboton, D.G. (1997): A new method for the determination of flow directions and upslope areas in grid digital elevation models. Water Ressources Research, 33(2): 309-319.
#'
#' Least Squares or Best Fit Plane:
#'
#' Beasley, D.B., Huggins, L.F. (1982): ANSWERS: User's manual. U.S. EPA-905/9-82-001, Chicago, IL, 54 pp.
#'
#' Costa-Cabral, M., Burges, S.J. (1994): Digital Elevation Model Networks (DEMON): a model of flow over hillslopes for computation of contributing and dispersal areas. Water Resources Research, 30(6): 1681-1692.
#'
#' Fit 2nd Degree Polynomial:
#'
#' Evans, I.S. (1979): An integrated system of terrain analysis and slope mapping. Final Report on grant DA-ERO-591-73-G0040. University of Durham, England.
#'
#' Bauer, J., Rohdenburg, H., Bork, H.-R. (1985): Ein Digitales Reliefmodell als Vorraussetzung fuer ein deterministisches  Modell der Wasser- und Stoff-Fluesse. Landschaftsgenese und Landschaftsoekologie, H. 10, Parameteraufbereitung fuer deterministische Gebiets-Wassermodelle, Grundlagenarbeiten zur Analyse von Agrar-Oekosystemen, eds.: Bork, H.-R., Rohdenburg, H., p. 1-15.
#'
#' Heerdegen, R.G., Beran, M.A. (1982): Quantifying source areas through land surface curvature. Journal of Hydrology, 57.
#'
#' Zevenbergen, L.W., Thorne, C.R. (1987): Quantitative analysis of land surface topography. Earth Surface Processes and Landforms, 12: 47-56.
#'
#' Fit 3.Degree Polynomial:
#'
#' Haralick, R.M. (1983): Ridge and valley detection on digital images. Computer Vision, Graphics and Image Processing, 22(1): 28-38.
#'
#' For a discussion on the calculation of slope by ArcGIS check these links:
#'
#' <https://community.esri.com/?c=93&f=1734&t=239914>
#'
#' <https://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?topicname=how_slope_works>
#' @author Alexander Brenning and Donovan Bangs (R interface), Olaf Conrad (SAGA module)
#' @seealso [rsaga.local.morphometry()], [rsaga.parallel.processing()], [rsaga.geoprocessor()],  [rsaga.env()]
#' @examples
#' \dontrun{
#' # Simple slope, aspect, and general curvature in degrees:
#' rsaga.slope.asp.curv("lican.sgrd", "slope", "aspect", "curvature",
#'                      method = "maxslope", unit.slope = "degrees", unit.aspect = "degrees")
#' # same for ASCII grids (default extension .asc):
#' rsaga.esri.wrapper(rsaga.slope.asp.curv,
#'                    in.dem="lican", out.slope="slope",
#'                    out.aspect = "aspect", out.cgene = "curvature",
#'                    method="maxslope", unit.slope = "degrees", unit.aspect = "degrees")
#' }
#' @keywords spatial interface
#' @export
rsaga.slope.asp.curv = function(in.dem,
                              out.slope, out.aspect, out.cgene,
                              out.cprof, out.cplan, out.ctang,
                              out.clong, out.ccros, out.cmini,
                              out.cmaxi, out.ctota, out.croto,
                              method = "poly2zevenbergen",
                              unit.slope = "radians", unit.aspect = "radians",
                              env = rsaga.env(), ...) {

  if (any(c("2.0.4","2.0.5","2.0.6","2.0.7","2.0.8","2.0.9","2.1.0") == env$version)) {
    stop("rsaga.slope.asp.curv only for SAGA GIS 2.1.1+;\n",
         "use rsaga.local.morphometry for older versions of SAGA GIS")
  }

  in.dem = default.file.extension(in.dem, ".sgrd")

  if(!missing(out.slope)){
    out.slope = default.file.extension(out.slope, ".sgrd")
  }
  if(!missing(out.aspect)){
    out.aspect = default.file.extension(out.aspect, ".sgrd")
  }

  method.choices = c("maxslope","maxtriangleslope","lsqfitplane", "poly2evans",
                     "poly2bauer","poly2heerdegen","poly2zevenbergen","poly3haralick")
  if(is.numeric(method) == TRUE)
    stop("Numeric 'method' argument not supported with SAGA GIS 2.1.1+;\n",
         "Use character name of methods - see help(rsaga.slope.asp.curv) for options")
  method = match.arg.ext(method, method.choices, numeric=TRUE, base=0)

  unit.slope.choices = c("radians", "degrees", "percent")
  unit.slope = match.arg.ext(unit.slope, unit.slope.choices, numeric=TRUE, base=0)

  unit.aspect.choices = c("radians", "degrees")
  unit.aspect = match.arg.ext(unit.aspect, unit.aspect.choices, numeric=TRUE, base=0)

  if (missing(out.aspect)) {
    out.aspect = tempfile()
    on.exit(unlink(paste(out.aspect,".*",sep="")), add = TRUE)
  }
  if (missing(out.slope)) {
    out.slope = tempfile()
    on.exit(unlink(paste(out.slope,".*",sep="")), add = TRUE)
  }

  param = list(ELEVATION=in.dem, SLOPE=out.slope, ASPECT = out.aspect)
  if(!missing(out.cgene)) {
    out.cgene = default.file.extension(out.cgene, ".sgrd")
    param = c(param, C_GENE = out.cgene)}
  if(!missing(out.cprof)) {
    out.cprof = default.file.extension(out.cprof, ".sgrd")
    param = c(param, C_PROF = out.cprof)}
  if(!missing(out.cplan)) {
    out.cplan = default.file.extension(out.cplan, ".sgrd")
    param  =c(param, C_PLAN = out.cplan)}
  if(!missing(out.ctang)) {
    out.ctang = default.file.extension(out.ctang, ".sgrd")
    param = c(param, C_TANG = out.ctang)}
  if(!missing(out.clong)) {
    out.clong = default.file.extension(out.clong, ".sgrd")
    param = c(param, C_LONG = out.clong)}
  if(!missing(out.ccros)) {
    out.ccros = default.file.extension(out.ccros, ".sgrd")
    param = c(param, C_CROS = out.ccros)}
  if(!missing(out.cmini)){
    out.cmini = default.file.extension(out.cmini, ".sgrd")
    param = c(param, C_MINI = out.cmini)}
  if(!missing(out.cmaxi)){
    out.cmaxi = default.file.extension(out.cmaxi, ".sgrd")
    param = c(param, C_MAXI = out.cmaxi)}
  if(!missing(out.ctota)){
    out.ctota = default.file.extension(out.ctota, ".sgrd")
    param = c(param, C_TOTA = out.ctota)}
  if(!missing(out.croto)){
    out.croto = default.file.extension(out.croto, ".sgrd")
    param = c(param, C_ROTO = out.croto)}

  param = c(param, METHOD=method, UNIT_SLOPE=unit.slope, UNIT_ASPECT=unit.aspect)

  module = "Slope, Aspect, Curvature"

  rsaga.geoprocessor("ta_morphometry", module, param, env = env, check.parameters = FALSE, ...)

  if (!missing(out.cprof) | !missing(out.cplan))
    warning("Plan and profile curvature calculations have changed with SAGA 2.1.1+\n",
            "See help(rsaga.slope.asp.curv) for more information")
}


#' Local Morphometry
#'
#' Calculates local morphometric terrain attributes (i.e. slope, aspect and curvatures). Intended for use with SAGA versions 2.1.0 and older. Use [rsaga.slope.asp.curv()] for SAGA 2.1.1+
#' @name rsaga.local.morphometry
#' @param in.dem input: digital elevation model (DEM) as SAGA grid file (default file extension: `.sgrd`)
#' @param out.slope optional output: slope (in radians)
#' @param out.aspect optional output: aspect (in radians; north=0, clockwise angles)
#' @param out.curv optional output: curvature
#' @param out.hcurv optional output: horizontal curvature (plan curvature)
#' @param out.vcurv optional output: vertical curvature (profile curvature)
#' @param method character (or numeric): algorithm (see References):
#' - 0 Maximum Slope - Travis et al. (1975) (`"maxslope"`, or 0)
#' - 1 Max. Triangle Slope - Tarboton (1997) (`"maxtriangleslope"`, or 1)
#' - 2 Least Squares Fit Plane - Costa-Cabral and Burgess (1996) (`"lsqfitplane"`, or 2)
#' - 3 Fit 2nd Degree Polynomial - Bauer et al. (1985) (`"poly2bauer"`, or 3)
#' - 4 Fit 2nd Degree Polynomial - Heerdegen and Beran (1982) (`"poly2heerdegen"`, or 4)
#' - 5 default: Fit 2nd Degree Polynomial - Zevenbergen and Thorne (1987) (`"poly2zevenbergen"`, or 5)
#' - 6 Fit 3rd Degree Polynomial - Haralick (1983) (`"poly3haralick"`, or 6).
#' @param env list, setting up a SAGA geoprocessing environment as created by [rsaga.env()]
#' @param ... further arguments to [rsaga.geoprocessor()]
#' @return The type of object returned depends on the `intern` argument passed to the [rsaga.geoprocessor()]. For `intern=FALSE` it is a numerical error code (0: success), or otherwise (default) a character vector with the module's console output.
#' @references For references and algorithm changes in SAGA GIS 2.1.1+ see [rsaga.slope.asp.curv()].
#' @author Alexander Brenning and Donovan Bangs (R interface), Olaf Conrad (SAGA module)
#' @seealso [rsaga.slope.asp.curv()], [rsaga.parallel.processing()], [rsaga.geoprocessor()],  [rsaga.env()]
#' @examples
#' \dontrun{
#' # a simple slope algorithm:
#' rsaga.slope("lican.sgrd","slope","maxslope")
#' # same for ASCII grids (default extension .asc):
#' rsaga.esri.wrapper(rsaga.slope,in.dem="lican",out.slope="slope",method="maxslope")
#' }
#' @keywords spatial interface
#' @export
rsaga.local.morphometry = function( in.dem,
    out.slope, out.aspect, out.curv, out.hcurv, out.vcurv,
    method = "poly2zevenbergen", env = rsaga.env(), ...)
{
  if (!(env$version %in% c("2.0.4","2.0.5","2.0.6","2.0.7","2.0.8","2.0.9","2.1.0"))) {
    rsaga.slope.asp.curv( in.dem=in.dem, out.slope=out.slope, out.aspect=out.aspect,
        out.cgene=out.curv, out.cplan=out.hcurv, out.cprof=out.vcurv,
        method=method, env=env, ... )
    warning("rsaga.local.morphometry specific to SAGA versions < 2.1.1\n",
            "Translating provided arguments and using rsaga.slope.asp.curv\n",
            "Note: order of numeric methods have changed with SAGA 2.1.1+")
  } else {

    in.dem = default.file.extension(in.dem,".sgrd")
    choices = c("maxslope","maxtriangleslope","lsqfitplane",
        "poly2bauer","poly2heerdegen","poly2zevenbergen","poly3haralick")
    method = match.arg.ext(method,choices,numeric=TRUE,base=0)
    if (missing(out.aspect)) {
        out.aspect = tempfile()
        on.exit(unlink(paste(out.aspect,".*",sep="")), add = TRUE)
    }
    if (missing(out.slope)) {
        out.slope = tempfile()
        on.exit(unlink(paste(out.slope,".*",sep="")), add = TRUE)
    }
    param = list(ELEVATION=in.dem, SLOPE=out.slope, ASPECT=out.aspect)
    if (!missing(out.curv))
        param = c(param, CURV=out.curv)
    if (!missing(out.hcurv))
        param = c(param, HCURV=out.hcurv)
    if (!missing(out.vcurv))
        param = c(param, VCURV=out.vcurv)
    param = c(param, METHOD=method)

    module = "Slope, Aspect, Curvature"
    if (any(c("2.0.4","2.0.5","2.0.6") == env$version)) module = "Local Morphometry"

    rsaga.geoprocessor("ta_morphometry", module, param, env = env, check.parameters = FALSE, ...)
  }
    if (!(env$version %in% c("2.0.4","2.0.5","2.0.6","2.0.7","2.0.8","2.0.9","2.1.0"))){
        if (!missing(out.hcurv) | !missing(out.vcurv))
            warning("Plan and profile curvature calculations have changed with SAGA 2.1.1+\n",
                    "See help(rsaga.slope.asp.curv) for more information")
    }
}

#' @rdname rsaga.local.morphometry
#' @name rsaga.slope
#' @export
rsaga.slope = function( in.dem, out.slope, method = "poly2zevenbergen", env = rsaga.env(), ... ) {
    stopifnot(!missing(out.slope))
    if (!(env$version %in% c("2.0.4","2.0.5","2.0.6","2.0.7","2.0.8","2.0.9","2.1.0"))) {
      rsaga.slope.asp.curv( in.dem=in.dem, out.slope=out.slope, method=method, env = env, ... )
    }
    else {
      rsaga.local.morphometry( in.dem=in.dem, out.slope=out.slope, method=method, env = env, ... )
    }
}

#' @rdname rsaga.local.morphometry
#' @name rsaga.aspect
#' @export
rsaga.aspect = function( in.dem, out.aspect, method = "poly2zevenbergen", env = rsaga.env(), ... ) {
    stopifnot(!missing(out.aspect))
    if (!(env$version %in% c("2.0.4","2.0.5","2.0.6","2.0.7","2.0.8","2.0.9","2.1.0"))) {
      rsaga.slope.asp.curv( in.dem=in.dem, out.aspect=out.aspect, method=method, env = env, ... )
    }
    else {
      rsaga.local.morphometry( in.dem=in.dem, out.aspect=out.aspect, method=method, env = env, ... )
    }
}


#' @rdname rsaga.local.morphometry
#' @name rsaga.curvature
#' @export
rsaga.curvature = function( in.dem, out.curv, method = "poly2zevenbergen", env = rsaga.env(), ... ) {
    stopifnot(!missing(out.curv))
    if (!(env$version %in% c("2.0.4","2.0.5","2.0.6","2.0.7","2.0.8","2.0.9","2.1.0"))) {
      rsaga.slope.asp.curv( in.dem=in.dem, out.cgene=out.curv, method=method, env = env, ... )
    }
    else {
      rsaga.local.morphometry( in.dem=in.dem, out.curv=out.curv, method=method, env = env,  ... )
    }
}

#' @rdname rsaga.local.morphometry
#' @name rsaga.plan.curvature
#' @export
rsaga.plan.curvature = function( in.dem, out.hcurv, method = "poly2zevenbergen", env = rsaga.env(), ... ) {
    stopifnot(!missing(out.hcurv))
    if (!(env$version %in% c("2.0.4","2.0.5","2.0.6","2.0.7","2.0.8","2.0.9","2.1.0"))) {
      rsaga.slope.asp.curv( in.dem=in.dem, out.cplan=out.hcurv, method=method, env = env,  ... )
    }
    else {
      rsaga.local.morphometry( in.dem=in.dem, out.hcurv=out.hcurv, method=method, env = env,  ... )
    }
}

#' @rdname rsaga.local.morphometry
#' @name rsaga.profile.curvature
#' @export
rsaga.profile.curvature = function( in.dem, out.vcurv, method = "poly2zevenbergen", env = rsaga.env(), ... ) {
    stopifnot(!missing(out.vcurv))
    if (!(env$version %in% c("2.0.4","2.0.5","2.0.6","2.0.7","2.0.8","2.0.9","2.1.0"))) {
      rsaga.slope.asp.curv( in.dem=in.dem, out.cprof=out.vcurv, method=method, env = env, ... )
    }
    else {
      rsaga.local.morphometry( in.dem=in.dem, out.vcurv=out.vcurv, method=method, env = env, ... )
    }
}


########   Module ta_preprocessor   ########



#' Fill Sinks
#'
#' Several methods for filling closed depressions in digital elevation models that would affect hydrological modeling.
#' @name rsaga.fill.sinks
#' @param in.dem Input: digital elevation model (DEM) as SAGA grid file (default extension: `.sgrd`).
#' @param out.dem Output: filled, depression-free DEM (SAGA grid file). Existing files will be overwritten!
#' @param method The depression filling algorithm to be used (character). One of `"planchon.darboux.2001"` (default), `"wang.liu.2006"`, or `"xxl.wang.liu.2006"`.
#' @param out.flowdir (only for `"wang.liu.2001"`): Optional output grid file for computed flow directions (see Notes).
#' @param out.wshed (only for `"wang.liu.2001"`): Optional output grid file for watershed basins.
#' @param minslope Minimum slope angle (in degree) preserved between adjacent grid cells (default value of `0.01` only for `method="planchon.darboux.2001"`, otherwise no default).
#' @param ... Optional arguments to be passed to [rsaga.geoprocessor()], including the `env` RSAGA geoprocessing environment.
#' @details This function bundles three SAGA modules for filling sinks using three different algorithms (`method` argument).
#'
#' `"planchon.darboux.2001"`: The algorithm of Planchon and Darboux (2001) consists of increasing the elevation of pixels in closed depressions until the sink disappears and a minimum slope angle of `minslope` (default: `0.01` degree) is established.
#'
#' `"wang.liu.2006"`: This module uses an algorithm proposed by Wang and Liu (2006) to identify and fill surface depressions in DEMs. The method was enhanced to allow the creation of hydrologically sound elevation models, i.e. not only to fill the depressions but also to  preserve a downward slope along the flow path.  If desired, this  is accomplished by preserving a minimum slope gradient (and thus elevation difference) between cells. This is the fully featured version of the module creating a depression-free DEM, a flow path grid and a grid with watershed basins. If you encounter problems processing large data sets (e.g. LIDAR data) with this module try the basic version (`xxl.wang.lui.2006`).
#'
#' `"xxl.wang.liu.2006"`: This modified algorithm after Wang and Liu (2006) is designed to work on large data sets.
#' @return The type of object returned depends on the `intern` argument passed to the [rsaga.geoprocessor()]. For `intern=FALSE` it is a numerical error code (0: success), or otherwise (default) a character vector with the module's console output.
#'
#' The function writes SAGA grid files containing of the depression-free preprocessed DEM, and optionally the flow directions and watershed basins.
#' @references Planchon, O., and F. Darboux (2001): A fast, simple and versatile algorithm to fill the depressions of digital elevation models. Catena 46: 159-176.
#'
#' Wang, L. & H. Liu (2006): An efficient method for identifying and filling surface depressions in digital elevation models for hydrologic analysis and modelling. International Journal of Geographical Information Science, Vol. 20, No. 2: 193-213.
#' @author Alexander Brenning (R interface), Volker Wichmann (SAGA module)
#' @note The flow directions are coded as 0 = north, 1 = northeast, 2 = east, ..., 7 = northwest.
#'
#' If `minslope=0`, depressions will only be filled until a horizontal surface is established, which may not be helpful for hydrological modeling.
#' @seealso [rsaga.sink.removal()], [rsaga.sink.route()].
#' @keywords spatial interface
#' @export
rsaga.fill.sinks = function(in.dem,out.dem,
    method="planchon.darboux.2001", out.flowdir, out.wshed, minslope, ...)
{
    stopifnot(is.character(method))
    method = match.arg.ext(method, ignore.case=TRUE, numeric=TRUE, base=2,
        choices=c("planchon.darboux.2001","wang.liu.2006","xxl.wang.liu.2006"))
    in.dem = default.file.extension(in.dem,".sgrd")

    if(!missing("out.dem")){
      out.dem = default.file.extension(out.dem, "sgrd")
    }

    if(!missing("out.flowdir")){
      out.flowdir = default.file.extension(out.flowdir, "sgrd")
    }

    if(!missing("out.wshed")){
      out.wshed = default.file.extension(out.wshed, "sgrd")
    }

    stopifnot(!missing(out.dem))
    if (missing(minslope)) minslope = NULL
    if (method==2) {
        param = list( DEM=in.dem, RESULT=out.dem )
        if (missing(minslope)) minslope = 0.01
        minslope = as.numeric(minslope)
        method = "Fill Sinks (Planchon/Darboux, 2001)"
    } else if (method==3) {
        if (missing(out.flowdir)) {
            out.flowdir = tempfile()
            on.exit(unlink(paste(out.flowdir,".*",sep="")), add = TRUE)
        }
        if (missing(out.wshed)) {
            out.wshed = tempfile()
            on.exit(unlink(paste(out.wshed,".*",sep="")), add = TRUE)
        }
        param = list(ELEV=in.dem, FILLED=out.dem, FDIR=out.flowdir, WSHED=out.wshed)
        method = "Fill Sinks (Wang & Liu)"
    } else if (method==4) {
        param = list(ELEV=in.dem, FILLED=out.dem)
        method = "Fill Sinks XXL (Wang & Liu)"
    }
    if (!is.null(minslope)) param = c( param, MINSLOPE=minslope )
    rsaga.geoprocessor("ta_preprocessor", method, param, check.parameters = FALSE, ...)
}



#' Sink Drainage Route Detection
#'
#' Sink drainage route detection.
#' @name rsaga.sink.route
#' @param in.dem input: digital elevation model (DEM) as SAGA grid file (default file extension: `.sgrd`)
#' @param out.sinkroute output: sink route grid file: non-sinks obtain a value of 0, sinks are assigned an integer between 0 and 8 indicating the direction to which flow from this sink should be routed
#' @param threshold logical: use a threshold value?
#' @param thrsheight numeric: threshold value (default: `100`)
#' @param ... optional arguments to be passed to [rsaga.geoprocessor()], including the `env` RSAGA geoprocessing environment
#' @return The type of object returned depends on the `intern` argument passed to the [rsaga.geoprocessor()]. For `intern=FALSE` it is a numerical error code (0: success), or otherwise (default) a character vector with the module's console output.
#' @author Alexander Brenning (R interface), Olaf Conrad (SAGA module)
#' @note I assume that flow directions are coded as 0 = north, 1 = northeast,  2 = east, ..., 7 = northwest, as in [rsaga.fill.sinks()].
#' @seealso  [rsaga.sink.removal()]
#' @examples
#' \dontrun{rsaga.sink.route("dem","sinkroute")
#' rsaga.sink.removal("dem","sinkroute","dem-preproc",method="deepen")}
#' @keywords spatial interface
#' @export
rsaga.sink.route = function(in.dem, out.sinkroute,
    threshold, thrsheight = 100, ...)
{
    in.dem = default.file.extension(in.dem,".sgrd")
    out.sinkroute = default.file.extension(out.sinkroute,".sgrd")

    param = list( ELEVATION=in.dem, SINKROUTE=out.sinkroute )
    if (!missing(threshold)) {
        if (threshold)   param = c( param, THRESHOLD="" )
    }
    # I guess thrsheight is redundant if threshold is missing/false:
    param = c( param, THRSHEIGHT=as.numeric(thrsheight) )
    rsaga.geoprocessor("ta_preprocessor", "Sink Drainage Route Detection", param, check.parameters = FALSE, ...)
    # was: module = 0
}



#' Sink Removal
#' Remove sinks from a digital elevation model by deepening drainage routes or filling sinks.
#' @name rsaga.sink.removal
#' @param in.dem input: digital elevation model (DEM) as SAGA grid file (default file extension: `.sgrd`)
#' @param in.sinkroute optional input: sink route grid file
#' @param out.dem output: modified DEM
#' @param method character string or numeric value specifying the algorithm (partial string matching will be applied): `"deepen drainage route"` (or 0): reduce the elevation of pixels in order to achieve drainage out of the former sinks `"fill sinks"` (or 1): fill sinks until none are left
#' @param ... optional arguments to be passed to [rsaga.geoprocessor()], including the `env` RSAGA geoprocessing environment
#' @return The type of object returned depends on the `intern` argument passed to the [rsaga.geoprocessor()]. For `intern=FALSE` it is a numerical error code (0: success), or otherwise (default) a character vector with the module's console output.
#' @author Alexander Brenning (R interface), Olaf Conrad (SAGA module)
#' @note This function uses module 1 from SAGA library `ta_preprocessor`.
#' @seealso  [rsaga.sink.route()], [rsaga.fill.sinks()]
#' @examples
#' \dontrun{rsaga.sink.route("dem","sinkroute")
#' rsaga.sink.removal("dem","sinkroute","dem-preproc",method="deepen")}
#' @keywords spatial interface
#' @export
rsaga.sink.removal = function(in.dem,in.sinkroute,out.dem,method="fill",...)
{
    in.dem = default.file.extension(in.dem,".sgrd")
    out.dem = default.file.extension(out.dem, ".sgrd")

    method = match.arg.ext(method,c("deepen drainage routes","fill sinks"),ignore.case=TRUE,numeric=TRUE)
    param = list( DEM=in.dem )
    if (!missing(in.sinkroute)) {
        in.sinkroute = default.file.extension(in.sinkroute,".sgrd")
        param = c(param, SINKROUTE=in.sinkroute)
    }
    param = c( param, DEM_PREPROC=out.dem, METHOD=method )
    rsaga.geoprocessor("ta_preprocessor", "Sink Removal", param, check.parameters = FALSE, ...)
}





########     Module grid_tools      ########




#' SAGA Modules Close Gaps and Close One Cell Gaps
#'
#' Close (Interpolate) Gaps
#' @name rsaga.close.gaps
#' @param in.dem input: digital elevation model (DEM) as SAGA grid file (default file extension: `.sgrd`)
#' @param out.dem output: DEM grid file without no-data values (gaps). Existing files will be overwritten!
#' @param threshold tension threshold for adjusting the interpolator (default: 0.1)
#' @param ... optional arguments to be passed to [rsaga.geoprocessor()], including the `env` RSAGA geoprocessing environment
#' @details `rsaga.close.one.cell.gaps` only fill gaps whose neighbor grid cells have non-missing data.
#'
#' In `rsaga.close.gaps`, larger tension thresholds can be used to reduce overshoots and undershoots in the surfaces used to fill (interpolate) the gaps.
#' @return The type of object returned depends on the `intern` argument passed to the [rsaga.geoprocessor()]. For `intern=FALSE` it is a numerical error code (0: success), or otherwise (default) a character vector with the module's console output.
#' @author Alexander Brenning (R interface), Olaf Conrad (SAGA module)
#' @note This function uses modules 7 (`rsaga.close.gaps` and 6 `rsaga.close.one.cell.gaps` from the SAGA library `grid_tools`.
#'
#' SAGA GIS 2.0.5+ has a new additional module `Close Gaps with Spline`, which
#' can be accessed using [rsaga.geoprocessor()] (currently no R wrapper
#' available). See `rsaga.get.usage("grid_tools","Close Gaps with Spline")`
#' or in version 2.1.0+ call `rsaga.html.help("grid_tools","Close Gaps with Spline")`.
#' @seealso [rsaga.geoprocessor()], [rsaga.env()]
#' @examples
#' \dontrun{
#' # using SAGA grids:
#' rsaga.close.gaps("rawdem.sgrd","dem.sgrd")
#' # using ASCII grids:
#' rsaga.esri.wrapper(rsaga.close.gaps,in.dem="rawdem",out.dem="dem")
#' }
#' @keywords spatial interface
#' @export
rsaga.close.gaps = function(in.dem,out.dem,threshold=0.1,...)
{
    in.dem = default.file.extension(in.dem,".sgrd")
    out.dem = default.file.extension(out.dem, ".sgrd")
    param = list( INPUT=in.dem, RESULT=out.dem, THRESHOLD=as.numeric(threshold) )
    rsaga.geoprocessor("grid_tools", "Close Gaps", param, check.parameters = FALSE, ...)
}


#' @rdname rsaga.close.gaps
#' @name rsaga.close.one.cell.gaps
#' @keywords spatial interface
#' @export
rsaga.close.one.cell.gaps = function(in.dem,out.dem,...)
{
    in.dem = default.file.extension(in.dem,".sgrd")
    out.dem = default.file.extension(out.dem, ".sgrd")
    param = list( INPUT = in.dem, RESULT = out.dem )
    rsaga.geoprocessor("grid_tools", "Close One Cell Gaps",
        param, ...)
}



########     Module ta_lighting     ########



#' Analytical hillshading
#' Analytical hillshading calculation.
#' @name rsaga.hillshade
#' @param in.dem Input digital elevation model (DEM) as SAGA grid file (default extension: `.sgrd`).
#' @param out.grid Output hillshading grid (SAGA grid file). Existing files will be overwritten!
#' @param method Available choices (character or numeric): `"standard"` (or `0` - default), `"max90deg.standard"` (`1`), `"combined.shading"` (`2`), `"ray.tracing"` (`3`). See Details.
#' @param azimuth Direction of the light source, measured in degree  clockwise from the north direction; default 315, i.e. northwest.
#' @param declination Declination of the light source, measured in degree above the horizon (default 45).
#' @param exaggeration Vertical exaggeration of elevation (default: 4). The terrain exaggeration factor allows to increase the shading contrasts in flat areas.
#' @param ... Optional arguments to be passed to [rsaga.geoprocessor()], including the `env` RSAGA geoprocessing environment.
#' @details The Analytical Hillshading algorithm is based on the angle between the surface and the incoming light beams, measured in radians.
#' @return The type of object returned depends on the `intern` argument passed to the [rsaga.geoprocessor()]. For `intern=FALSE` it is a numerical error code (0: success), or otherwise (default) a character vector with the module's console output.
#' @author Alexander Brenning (R interface), Olaf Conrad (SAGA module)
#' @note While the default azimuth of 315 degree (northwest) is not physically meaningful on the northern hemisphere, a northwesterly light source is required to properly depict relief in hillshading images. Physically correct southerly light sources results a hillshade that would be considered by most people as inverted: hills look like depressions, mountain chains like troughs.
#' @seealso [rsaga.solar.radiation()], [rsaga.insolation()]
#' @examples
#' \dontrun{rsaga.hillshade("dem.sgrd","hillshade")}
#' @keywords spatial interface
#' @export
rsaga.hillshade = function(in.dem, out.grid,
    method="standard", azimuth=315, declination=45, exaggeration=4, ...)
{
    in.dem = default.file.extension(in.dem,".sgrd")
    out.grid = default.file.extension(out.grid,".sgrd")
    method = match.arg.ext(method, numeric=TRUE, ignore.case=TRUE, base=0,
        choices=c("standard","max90deg.standard","combined.shading","ray.tracing"))
    param = list(ELEVATION=in.dem, SHADE=out.grid, METHOD=method,
        AZIMUTH=azimuth, DECLINATION=declination, EXAGGERATION=exaggeration)
    rsaga.geoprocessor("ta_lighting", "Analytical Hillshading", param, check.parameters = FALSE, ...)
    # was: module = 0
}



#' Potential incoming solar radiation
#'
#' This function calculates the potential incoming solar radiation in an area using different atmospheric models; module available in SAGA GIS 2.0.6+.
#' @name rsaga.pisr
#' @param in.dem name of input digital elevation model (DEM) grid in SAGA grid format (default extension: `.sgrd`)
#' @param in.svf.grid Optional input grid in SAGA format:  Sky View Factor; see also `local.svf`
#' @param in.vapour.grid Optional input grid in SAGA format:  Water vapour pressure (mbar); see also argument `hgt.water.vapour.pressure`
#' @param in.latitude.grid Optional input grid in SAGA format: Latitude (degree) of each grid cell
#' @param in.longitude.grid see `in.latitude.grid`
#' @param out.direct.grid Output grid: Direct insolation (unit selected by `unit` argument)
#' @param out.diffuse.grid Output grid: Diffuse insolation
#' @param out.total.grid Optional output grid: Total insolation, i.e. sum of direct and diffuse incoming solar radiation
#' @param out.ratio.grid Optional output grid: Direct to diffuse ratio
#' @param out.duration Optional output grid: Duration of insolation
#' @param out.sunrise Optional output grid: time of sunrise; only calculated if time span is set to single day
#' @param out.sunset Time of sunset; see `out.sunrise`
#' @param local.svf logical (default: `TRUE`; if TRUE, use sky view factor based on local slope (after Oke, 1988), if no sky view factor grid is provided in `in.svf.grid`
#' @param latitude Geographical latitude in degree North (negative values indicate southern hemisphere)
#' @param unit unit of insolation output grids: `"kWh/m2"` (default) `"kJ/m2"`, or `"J/cm2"`
#' @param solconst solar constant, defaults to 1367 W/m2
#' @param enable.bending logical (default: `FALSE`): incorporate effects of planetary bending?
#' @param bending.radius Planetary radius, default `6366737.96`
#' @param bending.lat.offset if bending is enabled: latitudinal reference  is `"user"`-defined (default), or relative to `"top"`, `"center"` or `"bottom"` of grid?
#' @param bending.lat.ref.user user-defined lat. reference for bending, see `bending.lat.offset`
#' @param bending.lon.offset longitudinal reference, i.e. local time,  is `"user"`-defined, or relative to `"top"`, `"center"` (default) or `"bottom"` of grid?
#' @param bending.lon.ref.user  user-defined reference for local time (Details??)
#' @param method specifies how the atmospheric components should be  accounted for: either based on the height of atmosphere and vapour pressure (`"height"`, or numeric code 0), or air pressure, water and dust content (`"components"`, code 1), or lumped atmospheric transmittance (`"lumped"`, code `0`)
#' @param hgt.atmosphere Height of atmosphere (in m); default 12000 m
#' @param hgt.water.vapour.pressure Water vapour pressure in mbar (default 10 mbar); This value is used if no vapour pressure grid is given in  argument `in.vapour.grid`
#' @param cmp.pressure atmospheric pressure in mbar, defaults to 1013 mbar
#' @param cmp.water.content water content of a vertical slice of the atmosphere in cm: between 1.5 and 1.7cm, average 1.68cm (default)
#' @param cmp.dust dust factor in ppm; defaults to 100 ppm
#' @param lmp.transmittance transmittance of the atmosphere in percent; usually between 60 (humid areas) and 80 percent (deserts)
#' @param time.range numeric vector of length 2:  time span (hours of the day) for numerical integration
#' @param time.step time step in hours for numerical integration
#' @param start.date list of length two, giving the start date in `day` and `month` components as numbers; these numbers are one-based (SAGA_CMD uses zero-based numbers internally), i.e. Jan. 1st is `list(day=1,month=1)`
#' @param end.date see `start.date`
#' @param day.step if `days` indicates a range of days, this specifies the time step (number of days) for calculating the incoming solar radiation
#' @param env RSAGA geoprocessing environment obtained with [rsaga.env()]; this argument is required for version control (see Note)
#' @param ... optional arguments to be passed to [rsaga.geoprocessor()]
#' @details According to SAGA GIS 2.0.7 documentation, "Most options should do well, but TAPES-G based diffuse irradiance calculation ("Atmospheric Effects" methods 2 and 3) needs further revision!" I.e. be careful with `method = "components"` and `method = "lumped"`.
#' @references
#' Boehner, J., Antonic, O. (2009): Land surface parameters specific to topo-climatology. In: Hengl, T. and Reuter, H. I. (eds.): Geomorphometry - Concepts, Software, Applications. Elsevier.
#'
#' Oke, T.R. (1988): Boundary layer climates. London, Taylor and Francis.
#'
#' Wilson, J.P., Gallant, J.C. (eds.), 2000: Terrain analysis - principles and applications. New York, John Wiley and Sons.
#' @author Alexander Brenning (R interface), Olaf Conrad (SAGA module)
#' @note This module is computationally very intensive (depending on the size of the grid and the time resolution, of course). The performance seems to have much improved in SAGA GIS 2.1.0, which by default runs this module in multicore mode (at the release candidate 1 for Windows does).
#'
#' SAGA_CMD uses zero-based days and months, but this R function uses the standard one-based days and months (e.g. day 1 is the first day of the month, month 1 is January) and translates to the SAGA system.
#'
#' This function uses module Potential Incoming Solar Radiation from SAGA library `ta_lighting` in SAGA version 2.0.6+.
#' @seealso [rsaga.hillshade()]; for similar modules in older SAGA versions (pre-2.0.6) see [rsaga.solar.radiation()] and [rsaga.insolation()]
#' @keywords spatial interface
#' @export
rsaga.pisr = function(in.dem, in.svf.grid = NULL, in.vapour.grid = NULL,
    in.latitude.grid = NULL, in.longitude.grid = NULL,
    out.direct.grid, out.diffuse.grid, out.total.grid = NULL,
    out.ratio.grid = NULL, out.duration, out.sunrise, out.sunset,
    local.svf = TRUE, latitude,
    unit=c("kWh/m2","kJ/m2","J/cm2"), solconst=1367.0,
    enable.bending = FALSE, bending.radius = 6366737.96,
    bending.lat.offset = "user", bending.lat.ref.user = 0,
    bending.lon.offset = "center", bending.lon.ref.user = 0,
    method = c("height","components","lumped"),
    hgt.atmosphere = 12000, hgt.water.vapour.pressure = 10,
    cmp.pressure = 1013, cmp.water.content = 1.68, cmp.dust = 100,
    lmp.transmittance = 70,
    time.range = c(0,24), time.step = 0.5,
    start.date = list(day=21, month=3), end.date = NULL, day.step = 5,
    env = rsaga.env(), ...)
{
    if ( (env$version == "2.0.4" | env$version == "2.0.5") ) {
        stop("rsaga.pisr only for SAGA GIS 2.0.6 - 2.2.1;\n",
             " use rsaga.solar.radiation for older versions of SAGA GIS")
    }
    if (!any(c("2.0.6","2.0.7","2.0.8", "2.1.0","2.1.1","2.1.2","2.1.3","2.1.4",
               "2.2.0","2.2.1") == env$version)) {
        stop("rsaga.pisr only for SAGA GIS 2.0.6 - 2.2.1:\n",
             " use rsaga.pisr2 for newer versions of SAGA GIS")
    }

    in.dem = default.file.extension(in.dem,".sgrd")
    if (!is.null(in.svf.grid)) in.svf.grid = default.file.extension(in.svf.grid,".sgrd")
    if (!is.null(in.vapour.grid)) in.vapour.grid = default.file.extension(in.vapour.grid,".sgrd")
    if (!is.null(in.latitude.grid)) in.latitude.grid = default.file.extension(in.latitude.grid,".sgrd")
    if (!is.null(in.longitude.grid)) in.longitude.grid = default.file.extension(in.longitude.grid,".sgrd")
    if (missing(out.direct.grid)) {
        out.direct.grid = tempfile()
        on.exit(unlink(paste(out.direct.grid,".*",sep="")), add = TRUE)
    }
    if (missing(out.diffuse.grid)) {
        out.diffuse.grid = tempfile()
        on.exit(unlink(paste(out.diffuse.grid,".*",sep="")), add = TRUE)
    }
    if (missing(out.total.grid)) {
        out.total.grid = tempfile()
        on.exit(unlink(paste(out.total.grid,".*",sep="")), add = TRUE)
    }
    if (missing(out.ratio.grid)) {
        out.ratio.grid = tempfile()
        on.exit(unlink(paste(out.ratio.grid,".*",sep="")), add = TRUE)
    }
    if (missing(out.duration)) {
        out.duration = tempfile()
        on.exit(unlink(paste(out.duration,".*",sep="")), add = TRUE)
    }
    if (missing(out.sunrise)) {
        out.sunrise = tempfile()
        on.exit(unlink(paste(out.sunrise,".*",sep="")), add = TRUE)
    }
    if (missing(out.sunset)) {
        out.sunset = tempfile()
        on.exit(unlink(paste(out.sunset,".*",sep="")), add = TRUE)
    }

    unit = match.arg.ext(unit,numeric=TRUE,ignore.case=TRUE,base=0)
    method = match.arg.ext(method, numeric = TRUE, ignore.case = TRUE, base = 0)
    bending.lat.offset = match.arg.ext(bending.lat.offset, c("bottom","center","top","user"),
        numeric = TRUE, ignore.case = TRUE, base = 0)
    bending.lon.offset = match.arg.ext(bending.lon.offset, c("left","center","right","user"),
        numeric = TRUE, ignore.case = TRUE, base = 0)

    if (!is.null(latitude))
        stopifnot( (latitude>=-90) & (latitude<=90) )
    stopifnot( length(time.range)==2 )
    stopifnot( all(time.range>=0) & all(time.range<=24) & (time.range[1]<time.range[2]) )
    stopifnot( (time.step>0) & (time.step<=12) )
    stopifnot( (day.step>0) & (day.step<=100) )
    stopifnot( is.logical(local.svf) )
    stopifnot( is.logical(enable.bending) )

    param = list( GRD_DEM=in.dem,
        GRD_DIRECT = out.direct.grid, GRD_DIFFUS = out.diffuse.grid,
        GRD_TOTAL = out.total.grid, GRD_RATIO = out.ratio.grid,
        DURATION = out.duration,
        SUNRISE = out.sunrise, SUNSET = out.sunset,
        UNITS = unit, SOLARCONST = as.numeric(solconst), LOCALSVF = local.svf,
        BENDING_BENDING = enable.bending,
        METHOD = method,
        #LATITUDE = as.numeric(latitude),  # removed 27 Dec 2011
        DHOUR = time.step )

    # Added 27 Dec 2011:
    if (!is.null(latitude)) {
        stopifnot((latitude >= -90) & (latitude <= 90))
        param = c(param, LATITUDE = as.numeric(latitude))
    }

    if (!is.null(in.svf.grid)) param = c( param, GRD_SVF=in.svf.grid )
    if (!is.null(in.vapour.grid)) param = c( param, GRD_VAPOUR=in.vapour.grid )
    stopifnot( !is.null(latitude) | !is.null(in.latitude.grid) ) # added 27 Dec 2011
    if (!is.null(in.latitude.grid)) param = c( param, GRD_LAT=in.latitude.grid )
    if (!is.null(in.longitude.grid)) param = c( param, GRD_LON=in.longitude.grid )

    if (enable.bending) {
        param = c( param,
            BENDING_RADIUS = bending.radius,
            BENDING_LAT_OFFSET = bending.lat.offset,
            BENDING_LAT_REF_USER = bending.lat.ref.user,
            BENDING_LON_OFFSET = bending.lon.offset,
            BENDING_LON_REF_USER = bending.lon.ref.user )
    }

    if (method == 0) {
        param = c(param, ATMOSPHERE = as.numeric(hgt.atmosphere),
            VAPOUR = as.numeric(hgt.water.vapour.pressure))
    } else if (method == 1) {
        param = c(param, PRESSURE = as.numeric(cmp.pressure),
            WATER = as.numeric(cmp.water.content), DUST = as.numeric(cmp.dust))
    } else if (method == 2) {
        stopifnot( (lmp.transmittance>=0) & (lmp.transmittance<=100) )
        param = c(param, LUMPED = as.numeric(lmp.transmittance))
    } else stopifnot( method %in% c(0:2) )

    if (is.null(start.date)) { # one year
        stopifnot( is.null(end.date) )
        param = c( param, PERIOD = 2, DAY_A = 0, MONTH_A = 0,
                      DAY_B = 30, MONTH_B = 11 )
    } else {
        if (is.null(end.date)) {
            param = c( param, PERIOD = 1 ) # single day ... or moment (later)
        } else param = c( param, PERIOD = 2 )
        stopifnot(is.list(start.date))
        stopifnot(length(start.date) == 2)
        stopifnot(all(names(start.date %in% c("day","month"))))
        stopifnot( (start.date$day>=1) & (start.date$day<=31) )
        stopifnot( (start.date$month>=1) & (start.date$month<=12) )
        param = c( param, DAY_A = start.date$day - 1,
                    MON_A = start.date$month - 1 )
        if (is.null(end.date)) {
            # check if moment:
            stopifnot(length(time.range) <= 2)
            if (length(time.range) == 2) {
                if (time.range[2] == time.range[1])
                    time.range = time.range[1]
            }
            if (length(time.range) == 1) {
                # moment
                param$PERIOD = 0
                stopifnot(time.range >= 0 & time.range <= 24)
                param = c(param, MOMENT = round(time.range,3))
            } else {
                stopifnot(time.range[1] >= 0 & time.range[1] <= 24)
                stopifnot(time.range[2] >= 0 & time.range[2] <= 24)
                stopifnot(time.range[1] < time.range[2])
                param = c(param, HOUR_RANGE_MIN = time.range[1],
                    HOUR_RANGE_MAX = time.range[2])
            }
        } else {
            # range of days:
            stopifnot(is.list(end.date))
            stopifnot(length(end.date) == 2)
            stopifnot(all(names(end.date %in% c("day","month"))))
            stopifnot( (end.date$day>=1) & (end.date$day<=31) )
            stopifnot( (end.date$month>=1) & (end.date$month<=12) )
            param = c( param, DAY_B = end.date$day - 1,
                        MON_B = end.date$month - 1,
                        DDAYS = day.step )
            if (is.null(time.range)) time.range = c(0,24)
            stopifnot(length(time.range) == 2)
            stopifnot(time.range[1] >= 0 & time.range[1] <= 24)
            stopifnot(time.range[2] >= 0 & time.range[2] <= 24)
            stopifnot(time.range[1] < time.range[2])
            param = c(param, HOUR_RANGE_MIN = time.range[1],
                HOUR_RANGE_MAX = time.range[2])
        }
    }

    rsaga.geoprocessor(lib = "ta_lighting",
        module = "Potential Incoming Solar Radiation",  # = 2
        param = param, env = env, check.parameters = FALSE, ...)
}

#' Potential incoming solar radiation SAGA 2.2.2+
#'
#' This function calculates the potential incoming solar radiation in an area using different atmospheric models; This function reflects changes to the module with SAGA 2.2.2+.
#' For SAGA versions 2.0.6 to 2.2.1 please see [rsaga.pisr()].
#' @name rsaga.pisr2
#' @param in.dem name of input digital elevation model (DEM) grid in SAGA grid format (default extension: `.sgrd`)
#' @param in.svf.grid Optional input grid in SAGA format:  Sky View Factor; see also `local.svf`
#' @param in.vapour.grid Optional input grid in SAGA format:  Water vapour pressure (mbar), for use with `method = "height"`; default 10 mbar
#' @param in.linke.grid Optional input grid in SAGA format: Linke turbidity coefficient, for use with `method = "hofierka"`; default 3.0
#' @param out.direct.grid Output grid: Direct insolation (unit selected by `unit` argument)
#' @param out.diffuse.grid Output grid: Diffuse insolation
#' @param out.total.grid Optional output grid: Total insolation, i.e. sum of direct and diffuse incoming solar radiation
#' @param out.ratio.grid Optional output grid: Direct to diffuse ratio
#' @param out.duration Optional output grid: Duration of insolation.
#' @param out.sunrise Optional output grid: time of sunrise; only calculated if time span is set to single day
#' @param out.sunset Time of sunset; see `out.sunrise`
#' @param vapour.default SAGA argument `GRD_VAPOUR_DEFAULT`, defaults to 10.0
#' @param linke.default SAGA argument `GRD_LINKE_DEFAULT`, defaults to 3.0
#' @param local.svf logical (default: `TRUE`; if TRUE, use sky view factor based on local slope (after Oke, 1988), if no sky view factor grid is provided in `in.svf.grid`
#' @param location specified whether to use constant latitude supplied by `latitude` below (`"latitude"` or code `0`; default) or as calculated from the grid system (`"grid"` or code `1`)
#' @param latitude Geographical latitude in degree North (negative values indicate southern hemisphere)
#' @param unit unit of insolation output grids: `"kWh/m2"` (default) `"kJ/m2"`, or `"J/cm2"`
#' @param solconst solar constant, defaults to 1367 W/m2
#' @param shadow specifies how topographic shading is modeled: `"slim"` (or numeric code 0), `"fat"` (or code 1; the default), or `"none"` (code `2`). Quoting SAGA 7.8.2 help: Choose 'slim' to trace grid node's shadow, 'fat' to trace the whole cell's shadow, or ignore shadowing effects. The first is slightly faster but might show some artifacts. (End quote.)
#' @param method specifies how the atmospheric components should be accounted for: either based on the height of atmosphere and vapour pressure (`"height"`, or numeric code 0), or air pressure, water and dust content (`"components"`, code 1), or lumped atmospheric transmittance (`"lumped"`, code `2`), or by the method of Hofierka and Suri, 2009 (`"hofierka"`, code `3`). Default: `"lumped"`.
#' @param hgt.atmosphere Height of atmosphere (in m); default 12000 m. For use with `method = "height"`
#' @param cmp.pressure atmospheric pressure in mbar, defaults to 1013 mbar. For use with `method = "components"`
#' @param cmp.water.content water content of a vertical slice of the atmosphere in cm: between 1.5 and 1.7cm, average 1.68cm (default). For use with `method = "components"`
#' @param cmp.dust dust factor in ppm; defaults to 100 ppm. For use with `method = "components"`
#' @param lmp.transmittance transmittance of the atmosphere in percent; usually between 60 (humid areas) and 80 percent (deserts)
#' @param time.range numeric vector of length 2:  time span (hours of the day) for numerical integration
#' @param time.step time step in hours for numerical integration
#' @param start.date list of length three, giving the start date in `day`, `month`, and `year` components as numbers, i.e. Jan. 1st 2015 is `list(day=1,month=1,year=2015)`
#' @param end.date see `start.date`
#' @param day.step if `days` indicates a range of days, this specifies the time step (number of days) for calculating the incoming solar radiation
#' @param env RSAGA geoprocessing environment obtained with [rsaga.env()]; this argument is required for version control (see Note)
#' @param ... optional arguments to be passed to [rsaga.geoprocessor()]
#' @details According to SAGA GIS 2.0.7 documentation, "Most options should do well, but TAPES-G based diffuse irradiance calculation ("Atmospheric Effects" methods 2 and 3) needs further revision!" I.e. be careful with `method = "components"` and `method = "lumped"`.
#' @references
#' Boehner, J., Antonic, O. (2009): Land surface parameters specific to topo-climatology. In: Hengl, T. and Reuter, H. I. (eds.): Geomorphometry - Concepts, Software, Applications. Elsevier.
#'
#' Oke, T.R. (1988): Boundary layer climates. London, Taylor and Francis.
#'
#' Wilson, J.P., Gallant, J.C. (eds.), 2000: Terrain analysis - principles and applications. New York, John Wiley and Sons.
#'
#' Hofierka, J., Suri, M. (2002): The solar radiation model for Open source GIS: implementation and applications. International GRASS users conference in Trento, Italy, September 2002
#' @author Alexander Brenning & Donovan Bangs (R interface), Olaf Conrad (SAGA module)
#' @note
#' This function uses module Potential Incoming Solar Radiation from SAGA library `ta_lighting` in SAGA versions >2.2.3.
#' Duration of insolation (`"out.duration"`) is only calculated when the time period is set to a single day.
#'
#' The SAGA module in version 8.5.x does not correctly use the date arguments; it is therefore not supported. Several SAGA versions >=8.6.0 and <8.5.0 did, however, produce plausible results and correctly interpreted the date arguments according to output shown when `show.output.on.console=TRUE` was used.
#' @seealso [rsaga.pisr()]; for similar modules in older SAGA versions (pre-2.0.6) see [rsaga.solar.radiation()] and [rsaga.insolation()]; [rsaga.hillshade()]
#' @keywords spatial interface
#' @export
rsaga.pisr2 <- function(in.dem, in.svf.grid = NULL, in.vapour.grid = NULL,
                         in.linke.grid = NULL,
                         out.direct.grid, out.diffuse.grid, out.total.grid = NULL,
                         out.ratio.grid = NULL, out.duration, out.sunrise, out.sunset,
                         local.svf = TRUE,
                         location = c("latitude", "grid"), latitude = 53,
                         unit = c("kWh/m2", "kJ/m2", "J/cm2"),
                         shadow = c("slim", "fat", "none"),
                         solconst = 1367.0,
                         method = c("height", "components", "lumped", "hofierka"),
                         linke.default = 3.0,
                         vapour.default = 10.0,
                         hgt.atmosphere = 12000.0,
                         cmp.pressure = 1013.0, cmp.water.content = 1.68, cmp.dust = 100.0,
                         lmp.transmittance = 70.0,
                         time.range = c(0,24), time.step = 0.5,
                         start.date = list(day=31, month=10, year=2015),
                         end.date = NULL, day.step = 5,
                         env = rsaga.env(), ...)
{
  if (any(c("2.0.4","2.0.5","2.0.6","2.0.7","2.0.8",
            "2.1.0","2.1.1","2.1.2","2.1.3","2.1.4",
            "2.2.0","2.2.1","2.2.2","2.2.3") == env$version)) {
    stop("This SAGA version is not supported by the current version of rsaga.pisr2")
  }
  if ((env$numeric_version >= 850) & (env$numeric_version < 860)) {
    stop("rsaga.pisr2 does not spport SAGA version 8.5.x as this version does not correctly read the date arguments.")
  }

  # Function for creating date character strings in format expected by SAGA:
  rsaga.format.date <- function(x) {
    stopifnot(is.list(x))
    stopifnot(length(x) == 3)
    stopifnot(all(names(x %in% c("day","month","year"))))
    stopifnot( (x$day>=1) & (x$day<=31) )
    stopifnot( (x$month>=1) & (x$month<=12) )
    stopifnot(x$year >= 0)

    ## Changed this to YYYY-MM-DD format as displayed in
    ## SAGA help, but format MM/DD/YYYY apparently continues
    ## to work. - 2025-02-02
    # paste0(
    #   formatC(x$month, format = "d", flag = "0", width = 2),
    #   "/",
    #   formatC(x$day, format = "d", flag = "0", width = 2),
    #   "/",
    #   formatC(x$year, format = "d", flag = "0", width = 4)
    # )
    paste0(
      formatC(x$year, format = "d", flag = "0", width = 4),
      "-",
      formatC(x$month, format = "d", flag = "0", width = 2),
      "-",
      formatC(x$day, format = "d", flag = "0", width = 2)
    )
  }

  in.dem = default.file.extension(in.dem,".sgrd")
  if (!is.null(in.svf.grid)) in.svf.grid = default.file.extension(in.svf.grid,".sgrd")
  if (!is.null(in.vapour.grid)) in.vapour.grid = default.file.extension(in.vapour.grid,".sgrd")
  if (!is.null(in.linke.grid)) in.linke.grid = default.file.extension(in.linke.grid,".sgrd")
  if (missing(out.direct.grid)) {
    out.direct.grid = tempfile()
    on.exit(unlink(paste(out.direct.grid,".*",sep="")), add = TRUE)
  } else {
    out.direct.grid = default.file.extension(out.direct.grid, ".sgrd")
  }
  if (missing(out.diffuse.grid)) {
    out.diffuse.grid = tempfile()
    on.exit(unlink(paste(out.diffuse.grid,".*",sep="")), add = TRUE)
  } else {
    out.diffuse.grid = default.file.extension(out.diffuse.grid, ".sgrd")
  }
  if (missing(out.total.grid)) {
    out.total.grid = tempfile()
    on.exit(unlink(paste(out.total.grid,".*",sep="")), add = TRUE)
  } else {
    out.total.grid = default.file.extension(out.total.grid, ".sgrd")
  }
  if (missing(out.ratio.grid)) {
    out.ratio.grid = tempfile()
    on.exit(unlink(paste(out.ratio.grid,".*",sep="")), add = TRUE)
  } else {
    out.ratio.grid = default.file.extension(out.ratio.grid, ".sgrd")
  }
  if (missing(out.duration)) {
    out.duration = tempfile()
    on.exit(unlink(paste(out.duration,".*",sep="")), add = TRUE)
  } else {
    out.duration = default.file.extension(out.duration, ".sgrd")
  }
  if (missing(out.sunrise)) {
    out.sunrise = tempfile()
    on.exit(unlink(paste(out.sunrise,".*",sep="")), add = TRUE)
  } else {
    out.sunrise = default.file.extension(out.sunrise, ".sgrd")
  }
  if (missing(out.sunset)) {
    out.sunset = tempfile()
    on.exit(unlink(paste(out.sunset,".*",sep="")), add = TRUE)
  } else {
    out.sunset = default.file.extension(out.sunset, ".sgrd")
  }

  unit = match.arg.ext(unit, numeric = TRUE, ignore.case = TRUE, base = 0)
  if (length(method) > 1) method <- "lumped"
  method = match.arg.ext(method, numeric = TRUE, ignore.case = TRUE, base = 0)
  if (length(shadow) > 1) shadow <- "fat"
  shadow = match.arg.ext(shadow, numeric = TRUE, ignore.case = TRUE, base = 0)
  location = match.arg.ext(location, numeric = TRUE, ignore.case = TRUE, base = 0)

  if (!is.null(latitude))
    stopifnot( (latitude>=-90) & (latitude<=90) )
  stopifnot( length(time.range)==2 )
  stopifnot( all(time.range>=0) & all(time.range<=24) & (time.range[1]<time.range[2]) )
  stopifnot( (time.step>0) & (time.step<=12) )
  stopifnot( (day.step>0) & (day.step<=100) )
  stopifnot( is.logical(local.svf) )

  param = list( GRD_DEM=in.dem,
                GRD_DIRECT = out.direct.grid, GRD_DIFFUS = out.diffuse.grid,
                GRD_TOTAL = out.total.grid, GRD_RATIO = out.ratio.grid,
                GRD_DURATION = out.duration,
                GRD_SUNRISE = out.sunrise,
                GRD_SUNSET = out.sunset,
                GRD_LINKE_DEFAULT = linke.default,
                GRD_VAPOUR_DEFAULT = vapour.default,
                UNITS = unit,
                SOLARCONST = as.numeric(solconst),
                LOCALSVF = local.svf,
                METHOD = method,
                HOUR_STEP = time.step )

  if (location == 0) {
    if (!is.null(latitude)) {
      stopifnot((latitude >= -90) & (latitude <= 90))
      param = c(param, LATITUDE = as.numeric(latitude))
    }
  } else {
    param = c(param, LOCATION = as.numeric(location))
  }

  if (!is.null(in.svf.grid)) param = c( param, GRD_SVF=in.svf.grid )
  if (!is.null(in.vapour.grid)) param = c( param, GRD_VAPOUR=in.vapour.grid )
  if (!is.null(in.linke.grid)) param = c( param, GRD_LINKE=in.linke.grid )

  if (method == 0) {
    param = c(param, ATMOSPHERE = as.numeric(hgt.atmosphere))
  } else if (method == 1) {
    param = c(param, PRESSURE = as.numeric(cmp.pressure),
              WATER = as.numeric(cmp.water.content), DUST = as.numeric(cmp.dust))
  } else if (method == 2) {
    stopifnot( (lmp.transmittance>=0) & (lmp.transmittance<=100) )
    param = c(param, LUMPED = as.numeric(lmp.transmittance))
  } else if (method == 3) {
    param = param
  } else stopifnot( method %in% c(0:3) )


  if (is.null(end.date)) { # Just Start Date but no end date
    param = c( param, PERIOD = 1 ) # single day ... or moment (later)
  } else
    param = c( param, PERIOD = 2 )

  start_date <- rsaga.format.date(start.date)
  param = c( param, DAY = start_date)

  if (is.null(end.date)) {
    # check if moment:
    stopifnot(length(time.range) <= 2)
    if (length(time.range) == 2) {
      if (time.range[2] == time.range[1])
        time.range = time.range[1]
    }
    if (length(time.range) == 1) {
      # moment
      param$PERIOD = 0
      stopifnot(time.range >= 0 & time.range <= 24)
      param = c(param, MOMENT = round(time.range,3))
    } else {
      stopifnot(time.range[1] >= 0 & time.range[1] <= 24)
      stopifnot(time.range[2] >= 0 & time.range[2] <= 24)
      stopifnot(time.range[1] < time.range[2])
      param = c(param, HOUR_RANGE_MIN = time.range[1],
                HOUR_RANGE_MAX = time.range[2])
    }
  } else {
    # range of days:
    end_date <- rsaga.format.date(end.date)

    param = c( param,
               DAY_STOP = end_date,
               DAYS_STEP = day.step)

    if (is.null(time.range)) time.range = c(0,24)
    stopifnot(length(time.range) == 2)
    stopifnot(time.range[1] >= 0 & time.range[1] <= 24)
    stopifnot(time.range[2] >= 0 & time.range[2] <= 24)
    stopifnot(time.range[1] < time.range[2])
    param = c(param, HOUR_RANGE_MIN = time.range[1],
              HOUR_RANGE_MAX = time.range[2])
  }


  rsaga.geoprocessor(lib = "ta_lighting",
                     module = "Potential Incoming Solar Radiation",  # = 2
                     param = param, env = env, check.parameters = FALSE, ...)
}


#' Potential incoming solar radiation
#'
#' This function calculates the potential incoming solar radiation in an area either using a lumped atmospheric transmittance model or estimating it based on water and dust content. Use [rsaga.pisr()] instead with SAGA GIS 2.0.6+.
#' @name rsaga.solar.radiation
#' @param in.dem name of input digital elevation model (DEM) grid in SAGA grid format (default extension: `.sgrd`)
#' @param out.grid output grid file for potential incoming solar radiation sums
#' @param out.duration Optional output grid file for duration of insolation
#' @param latitude Geographical latitude in degree North (negative values indicate southern hemisphere)
#' @param unit unit of the `out.grid` output: `"kWh/m2"` (default) or `"J/m2"`
#' @param solconst solar constant, defaults to 1367 W/m2
#' @param method specifies how the atmospheric components should be accounted for: either based on a lumped atmospheric transmittance as specified by argument `transmittance` (`"lumped"`, or numeric code `0`; default); or by calculating the components corresponding to water and dust (`"components"`, code `1`)
#' @param transmittance transmittance of the atmosphere in percent; usually between 60 (humid areas) and 80 percent (deserts)
#' @param pressure atmospheric pressure in mbar
#' @param water.content water content of a vertical slice of the atmosphere in cm: between 1.5 and 1.7cm, average 1.68cm (default)
#' @param dust dust factor in ppm; defaults to 100ppm
#' @param time.range numeric vector of length 2:  time span (hours of the day) for numerical integration
#' @param time.step time step in hours for numerical integration
#' @param days either a list with components `day` and `month` specifying a single day of the year for radiation modeling; OR a numeric vector of length 2 specifying the start and end date (see Note below)
#' @param day.step if `days` indicates a range of days, this specifies the time step (number of days) for calculating the incoming solar radiation
#' @param env RSAGA geoprocessing environment obtained with [rsaga.env()]; this argument is required for version control (see Note)
#' @param ... optional arguments to be passed to [rsaga.geoprocessor()]
#' @references Wilson, J.P., Gallant, J.C. (eds.), 2000: Terrain analysis - principles and applications. New York, John Wiley & Sons.
#' @author Alexander Brenning (R interface), Olaf Conrad (SAGA module)
#' @note This module ceased to exist under SAGA GIS 2.0.6+, which has a similar (but more flexible) module Potential Solar Radiation that is interfaced by [rsaga.pisr()].
#'
#' SAGA_CMD uses zero-based days and months, but this R function uses the standard one-based days and months (e.g. day 1 is the first day of the month, month 1 is January) and translates to the SAGA system.
#'
#' In SAGA 2.0.2, solar radiation sums calculated for a range of days, say `days=c(a,b)` actually calculate radiation only for days `a,...,b-1` (in steps of `day.step` - I used `day.step=1` in this example).  The setting `a=b` however gives the same result as `b=a+1`, and indeed `b=a+2` gives twice the radiation sums and potential sunshine duration that `a=b` and `b=a+1` both give.
#'
#' The solar radiation module of SAGA 2.0.1 had a bug that made it impossible to pass a range of `days` of the year or a range of hours of the day (`time.range`) to SAGA. These options work in SAGA 2.0.1.
#'
#' This function uses module Incoming Solar Radiation from SAGA GIS library `ta_lighting`.
#' @seealso [rsaga.hillshade()], [rsaga.insolation()]
#' @examples
#' \dontrun{
#' # potential solar radiation on Nov 7 in Southern Ontario...
#' rsaga.solar.radiation("dem","solrad","soldur",latitude=43,
#'     days=list(day=7,month=11),time.step=0.5)
#' }
#' @keywords spatial interface
#' @export
rsaga.solar.radiation = function(in.dem, out.grid, out.duration, latitude,
    unit=c("kWh/m2","J/m2"), solconst=1367.0, method=c("lumped","components"),
    transmittance=70, pressure=1013, water.content=1.68, dust=100,
    time.range=c(0,24), time.step=1,
    days=list(day=21,month=3), day.step=5,
    env = rsaga.env(), ...)
{
    if ( !(env$version == "2.0.4" | env$version == "2.0.5") ) {
        stop("rsaga.solar.radiation only for SAGA GIS 2.0.4 / 2.0.5;\n",
             " use rsaga.pisr for SAGA GIS 2.0.6+")
    }

    in.dem = default.file.extension(in.dem,".sgrd")
    if (missing(out.duration)) {
        out.duration = tempfile()
        on.exit(unlink(paste(out.duration,".*",sep="")), add = TRUE)
    }
    unit = match.arg.ext(unit,numeric=TRUE,ignore.case=TRUE,base=0)
    method = match.arg.ext(method,numeric=TRUE,ignore.case=TRUE,base=0)
    stopifnot( (transmittance>=0) & (transmittance<=100) )
    stopifnot( (latitude>=-90) & (latitude<=90) )
    stopifnot( length(time.range)==2 )
    stopifnot( all(time.range>=0) & all(time.range<=24) & (time.range[1]<time.range[2]) )
    stopifnot( (time.step>0) & (time.step<=12) )
    stopifnot( (day.step>0) & (day.step<=100) )

    param = list( ELEVATION=in.dem, INSOLAT=out.grid, DURATION=out.duration,
        UNIT=unit, SOLCONST=as.numeric(solconst), METHOD=method,
        TRANSMITT=as.numeric(transmittance), PRESSURE=as.numeric(pressure),
        WATER=as.numeric(water.content), DUST=as.numeric(dust),
        LATITUDE=as.numeric(latitude),
        HOUR_RANGE_MIN=time.range[1], HOUR_RANGE_MAX=time.range[2],
        HOUR_STEP=time.step )

    if (is.null(days)) { # one year
        param = c( param, TIMESPAN=2 )
    } else if (is.list(days)) { # single day
        stopifnot(length(days)==2)
        stopifnot( (days$day>=1) & (days$day<=31) )
        stopifnot( (days$month>=1) & (days$month<=12) )
        param = c( param, TIMESPAN=0,
            SINGLE_DAY_DAY=days$day-1, SINGLE_DAY_MONTH=days$month-1 )
    } else if (is.numeric(days)) { # range of days
        stopifnot(length(days)==2)
        stopifnot( days[1] <= days[2] )
        stopifnot( (days[1]>=1) & (days[2]<=366) )
        param = c( param, TIMESPAN=1,
            DAY_RANGE_MIN=days[1], DAY_RANGE_MAX=days[2],
            DAY_STEP=day.step )
    }
    rsaga.geoprocessor(lib = "ta_lighting",
        module = "Incoming Solar Radiation",  # = 2
        param = param, env = env, check.parameters = FALSE, ...)
}



#' Incoming Solar Radiation (Insolation)
#'
#' This function calculates the amount of incoming solar radiation (insolation) depending on slope, aspect, and atmospheric properties. Module not available in SAGA GIS 2.0.6 and 2.0.7.
#' @name rsaga.insolation
#' @param in.dem Name of input digital elevation model (DEM) grid in SAGA grid format (default extension: `.sgrd`)
#' @param in.vapour Optional input: SAGA grid file giving the water vapour pressure in mbar
#' @param in.latitude Optional input: SAGA grid file giving for each pixel the latitude in degree
#' @param in.longitude Optional input: SAGA grid file giving for each pixel the longitude in degree
#' @param out.direct Optional output grid file for direct insolation
#' @param out.diffuse Optional output grid file for diffuse insolation
#' @param out.total Optional output grid file for total insolation, i.e. the sum of direct and diffuse insolation
#' @param horizontal logical; project radiation onto a horizontal surface? (default: `FALSE`, i.e. use the actual inclined surface as a reference area)
#' @param solconst solar constant in Joule; default: 8.164 J/cm2/min (=1360.7 kWh/m2; the more commonly used solar constant of 1367 kWh/m2 corresponds to 8.202 J/cm2/min)
#' @param atmosphere height of atmosphere in m; default: 12000m
#' @param water.vapour.pressure if no water vapour grid is given, this argument specifies a constant water vapour pressure that is uniform in space; in mbar, default 10 mbar
#' @param type type of time period: `"moment"` (equivalent: `0`) for a single instant, `"day"` (or `1`) for a single day, `"range.of.days"` (or `2`), or `"same.moment.range.of.days"` (or `3`) for the same moment in a range of days; default: `"moment"`
#' @param time.step time resolution in hours for discretization within a day
#' @param day.step time resolution in days for a range of days
#' @param days numeric vector of length 2, specifying the first and last day of a range of days (for `type`s 2 and 3)
#' @param moment if `type="moment"` or `"same.moment.range.of.days"`, `moment` specifies the time of the day (hour between 0 and 24) for which the insolation is to be calculated
#' @param latitude if no `in.latitude` grid is given, this will specify a fixed geographical latitude for the entire grid
#' @param bending should planetary bending be modeled? (default: `FALSE`)
#' @param radius planetary radius
#' @param lat.offset `latitude` relates to grids `"bottom"`(equivalent code: `0`), `"center"` (1), `"top"` (2), or `"user"`-defined reference (default: `"user"`); in the latter case, `lat.ref.user` defines the reference
#' @param lat.ref.user if `in.latitude` is missing and `lat.offset="user"`, then this numeric value defines the latitudinal reference (details??)
#' @param lon.offset local time refers to grid's `"left"` edge (code 0), `"center"` (1), `"right"` edge (2), or a  `"user"`-defined reference.
#' @param lon.ref.user if `in.longitude` is missing and `lon.offset="user"`, then this numeric value defines the reference of the local time (details??)
#' @param env RSAGA geoprocessing environment obtained with [rsaga.env()]; this argument is required for version control (see Note)
#' @param ... optional arguments to be passed to [rsaga.geoprocessor()], including the `env` RSAGA geoprocessing environment
#' @details Calculation of incoming solar radiation (insolation). Based on the SADO (System for the Analysis of Discrete Surfaces) routines developed  by Boehner & Trachinow.
#' @return The type of object returned depends on the `intern` argument passed to the [rsaga.geoprocessor()]. For `intern=FALSE` it is a numerical error code (0: success), or otherwise (default) a character vector with the module's console output.
#' @author Alexander Brenning (R interface), Olaf Conrad (SAGA module)
#' @note This function uses module `Insolation` (code: 3) from SAGA library `ta_lighting`. It is available in SAGA GIS 2.0.4 and 2.0.5 but not 2.0.6 and 2.0.7; see [rsaga.pisr()].
#' @seealso [rsaga.solar.radiation()], [rsaga.pisr()],  [rsaga.hillshade()]
#' @keywords spatial interface
#' @export
rsaga.insolation = function(in.dem, in.vapour, in.latitude, in.longitude,
    out.direct, out.diffuse, out.total,
    horizontal=FALSE, solconst=8.1640, atmosphere=12000, water.vapour.pressure=10.0,
    type=c("moment","day","range.of.days","same.moment.range.of.days"),
    time.step=1, day.step=5, days, moment, latitude, bending=FALSE,
    radius=6366737.96,
    lat.offset="user", lat.ref.user=0,
    lon.offset="center", lon.ref.user=0,
    env = rsaga.env(),
    ...)
{
    if (!rsaga.module.exists(libs = "ta_lighting", module = "Insolation", env = env)) {
      stop("Module 'Insolation' in library 'ta_lighting' not available in this version of SAGA GIS (",
           env$version, "). Consider using function rsaga.pisr2() instead.")
    }

    in.dem = default.file.extension(in.dem,".sgrd")
    param = list( GRD_DEM=in.dem )
    type = match.arg.ext(type,numeric=TRUE,ignore.case=TRUE,base=0)
    stopifnot( (!missing(out.direct)) | (!missing(out.diffuse)) | (!missing(out.total)) )
    stopifnot( !missing(latitude) )
    if (!missing(moment)) {
        if (!(type==0 | type==3)) {
            warning("'moment' argument only relevant for 'type=\"moment\"'\n",
                    "or 'type=\"same.moment.range.of.days\"' -\n",
                    "ignoring the 'moment' argument")
        }
    }
    if (!missing(in.vapour)) {
        in.vapour = default.file.extension(in.vapour,".sgrd")
        param = c(param, GRD_VAPOUR=in.vapour)
    }
    if (!missing(in.latitude)) {
        in.latitude = default.file.extension(in.latitude,".sgrd")
        param = c(param, GRD_LAT=in.latitude)
    }
    if (!missing(in.longitude)) {
        in.longitude = default.file.extension(in.longitude,".sgrd")
        param = c(param, GRD_LON=in.longitude)
    }
    if (!missing(out.direct)) param = c(param, GRD_DIRECT=out.direct)
    if (!missing(out.diffuse)) param = c(param, GRD_DIFFUS=out.diffuse)
    if (!missing(out.total)) param = c(param, GRD_TOTAL=out.total)
    stopifnot( (days[1]>=0) & (days[1]<=366) )
    param = c(param, BHORIZON=horizontal, SOLARCONST=solconst,
        ATMOSPHERE=atmosphere, VAPOUR=water.vapour.pressure,
        PERIOD=type, DHOUR=time.step, DDAYS=day.step,
        DAY_A=days[1])
    if (type>=2) { # range of days / same moment in a range of days
        stopifnot( (days[2]>=days[1]) & (days[2]<=366) )
        param = c(param, DAY_B=days[2])
    }
    if ((type==0) | (type==3)) {
        stopifnot( (moment>=0) & (moment<=24) )
        param = c(param, MOMENT=moment)
    }
    param = c(param, LATITUDE=latitude, BENDING=bending, RADIUS=radius)
    lat.offset = match.arg.ext(lat.offset, c("bottom","center","top","user"),
        numeric=TRUE, ignore.case=TRUE, base=0)
    lon.offset = match.arg.ext(lon.offset, c("left","center","right","user"),
        numeric=TRUE, ignore.case=TRUE, base=0)
    param = c(param, LAT_OFFSET=lat.offset)
    if (lat.offset==3) { # user-defined
        #stopifnot(!missing(lat.ref.user))
        param = c(param, LAT_REF_USER=as.numeric(lat.ref.user))
    }
    param = c(param, LON_OFFSET=lon.offset)
    if (lon.offset==3) { # user-defined
        #stopifnot(!missing(lon.ref.user))
        param = c(param, LON_REF_USER=as.numeric(lon.ref.user))
    }
    rsaga.geoprocessor(lib = "ta_lighting",
        module = "Insolation", # = 3
        param = param, check.parameters = FALSE, env = env, ...)
}





########     Module grid_filter     ########



#' Simple Filters
#'
#' Apply a smoothing, sharpening or edge filter to a SAGA grid.
#' @name rsaga.filter.simple
#' @param in.grid input: SAGA grid file (default file extension: `.sgrd`)
#' @param out.grid output: SAGA grid file
#' @param mode character or numeric: shape of moving window, either `"square"` (=0) or `"circle"` (=1, default)
#' @param method character or numeric: `"smooth"` (=0), `"sharpen"` (=1), or `"edge"` (=2)
#' @param radius positive integer: radius of moving window
#' @param env list, setting up a SAGA geoprocessing environment as created by [rsaga.env()]
#' @param ... optional arguments to be passed to [rsaga.geoprocessor()], including the `env` RSAGA geoprocessing environment
#' @return The type of object returned depends on the `intern` argument passed to the [rsaga.geoprocessor()]. For `intern=FALSE` it is a numerical error code (0: success), or otherwise (the default) a character vector with the module's console output.
#' @author Alexander Brenning (R interface), Olaf Conrad (SAGA module)
#' @seealso [rsaga.filter.gauss()]
#' @examples \dontrun{rsaga.filter.simple("dem","dem-smooth",radius=4)}
#' @keywords spatial interface
#' @export
rsaga.filter.simple = function(in.grid, out.grid, mode="circle",
    method=c("smooth","sharpen","edge"), radius, env = rsaga.env(), ...)
{
    in.grid = default.file.extension(in.grid,".sgrd")
    out.grid = default.file.extension(out.grid, ".sgrd")
    mode = match.arg.ext(mode,choices=c("square","circle"),
        numeric=TRUE,base=0,ignore.case=TRUE)
    method = match.arg.ext(method,numeric=TRUE,base=0,ignore.case=TRUE)
    if (missing(radius)) stop("the search 'radius' argument (in # pixels) must be specified")
    if (round(radius) != radius) {
        warning("'radius' must be an integer >=1 (# pixels); rounding it...")
        radius = round(radius)
    }
    if (radius<1) {
        warning("'radius' must be an integer >=1 (# pixels); setting 'radius=1'...")
        radius = 1
    }

    if (any(c("2.0.4","2.0.5","2.0.6","2.0.7","2.0.8",
              "2.1.0","2.1.1","2.1.2","2.1.3","2.1.4",
              "2.2.0","2.2.1","2.2.2","2.2.3", "2.3.1",
              "2.3.2","3.0.0") == env$version)) {
      param = list(INPUT=in.grid, RESULT=out.grid, MODE=mode,
                   METHOD=method, RADIUS=radius)
    } else {
      param = list(INPUT=in.grid, RESULT=out.grid, KERNEL_TYPE=mode,
                   METHOD=method, KERNEL_RADIUS=radius)
    }

    rsaga.geoprocessor(lib = "grid_filter",
        module = "Simple Filter",
        param = param, env = env, check.parameters = FALSE, ...)
}



#' Gauss Filter
#'
#' Smooth a grid using a Gauss filter.
#' @name rsaga.filter.gauss
#' @param in.grid input: SAGA GIS grid file (default file extension: `.sgrd`)
#' @param out.grid output: SAGA GIS grid file
#' @param sigma numeric, >0.0001: standard deviation parameter of Gauss filter
#' @param radius positive integer: radius of moving window
#' @param env list, setting up a SAGA geoprocessing environment as created by [rsaga.env()]
#' @param ... optional arguments to be passed to [rsaga.geoprocessor()], including the `env` RSAGA geoprocessing environment
#' @return The type of object returned depends on the `intern` argument passed to the [rsaga.geoprocessor()]. For `intern=FALSE` it is a numerical error code (0: success), or otherwise (the default) a character vector with the module's console output.
#' @author Alexander Brenning (R interface), Olaf Conrad (SAGA module)
#' @seealso [rsaga.filter.simple()]
#' @keywords spatial interface
#' @export
rsaga.filter.gauss = function(in.grid, out.grid, sigma,
    radius=ceiling(2*sigma), env = rsaga.env(), ...)
{
    in.grid = default.file.extension(in.grid,".sgrd")
    out.grid = default.file.extension(out.grid, ".sgrd")
    if (missing(sigma)) stop("the 'sigma' standard deviation argument (in # pixels) must be specified")
    stopifnot(sigma>0.0001)
    if (round(radius) != radius) stop("'radius' must be an integer (# pixels)")
    stopifnot(radius>=1)

    if (any(c("2.0.4","2.0.5","2.0.6","2.0.7","2.0.8",
              "2.1.0","2.1.1","2.1.2","2.1.3","2.1.4",
              "2.2.0","2.2.1","2.2.2","2.2.3", "2.3.1",
              "2.3.2", "3.0.0") == env$version)) {
      param = list(INPUT=in.grid, RESULT=out.grid, SIGMA=sigma, RADIUS=radius)
    } else {
      param = list(INPUT=in.grid, RESULT=out.grid, SIGMA=sigma, KERNEL_RADIUS=radius)
    }


    rsaga.geoprocessor(lib = "grid_filter",
        module = "Gaussian Filter", # = 1,
        param, env = env, check.parameters = FALSE, ...)
}





########     Module ta_hydrology    ########



#' Parallel Processing
#'
#' Calculate the size of the local catchment area (contributing area), the catchment height, catchment slope and aspect, and flow path length, using parallel processing algorithms including the recommended multiple flow direction algorithm. This set of algorithms processes a digital elevation model (DEM) downwards from the highest to the lowest cell.\cr No longer supported with SAGA GIS 2.1.3+. See [rsaga.topdown.processing()].
#' @name rsaga.parallel.processing
#' @param in.dem input: digital elevation model (DEM) as SAGA grid file (default file extension: `.sgrd`)
#' @param in.sinkroute optional input: SAGA grid with sink routes
#' @param in.weight optional intput: SAGA grid with weights
#' @param out.carea output: catchment area grid
#' @param out.cheight optional output: catchment height grid
#' @param out.cslope optional output: catchment slope grid
#' @param out.caspect optional output: catchment aspect grid
#' @param out.flowpath optional output: flow path length grid
#' @param step integer >=1: step parameter
#' @param method character or numeric: choice of processing algorithm: Deterministic 8 (`"d8"` or 0), Rho 8 (`"rho8"` or 1), Braunschweiger Reliefmodell (`"braunschweig"` or 2), Deterministic Infinity (`"dinf"` or 3), Multiple Flow Direction (`"mfd"` or 4, the default), Multiple Triangular Flow Direction (`"mtfd"`, or 5).
#' @param linear.threshold numeric (number of grid cells): threshold above which linear flow (i.e. the Deterministic 8 algorithm) will be used; linear flow is disabled for `linear.threshold=Inf` (the default)
#' @param convergence numeric >=0: a parameter for tuning convergent/ divergent flow; default value of `1.1` gives realistic results and should not be changed
#' @param env list, setting up a SAGA geoprocessing environment as created by [rsaga.env()]
#' @param ... further arguments to [rsaga.geoprocessor()]
#' @details Refer to the references for details on the available algorithms.
#' @return The type of object returned depends on the `intern` argument passed to the [rsaga.geoprocessor()]. For `intern=FALSE` it is a numerical error code (0: success), or otherwise (the default) a character vector with the module's console output.
#' @references
#' Deterministic 8:
#'
#' O'Callaghan, J.F., Mark, D.M. (1984): The extraction of drainage networks from digital elevation data. Computer Vision, Graphics and Image Processing, 28: 323-344.
#'
#' Rho 8:
#'
#' Fairfield, J., Leymarie, P. (1991): Drainage networks from grid digital elevation models. Water Resources Research, 27: 709-717.
#'
#' Braunschweiger Reliefmodell:
#'
#' Bauer, J., Rohdenburg, H., Bork, H.-R. (1985): Ein Digitales Reliefmodell als Vorraussetzung fuer ein deterministisches Modell der Wasser- und Stoff-Fluesse. Landschaftsgenese und Landschaftsoekologie, H. 10, Parameteraufbereitung fuer deterministische Gebiets-Wassermodelle, Grundlagenarbeiten zu Analyse von Agrar-Oekosystemen, eds.: Bork, H.-R., Rohdenburg, H., p. 1-15.
#'
#' Deterministic Infinity:
#'
#' Tarboton, D.G. (1997): A new method for the determination of flow directions and upslope areas in grid digital elevation models. Water Ressources Research, 33(2): 309-319.
#'
#' Multiple Flow Direction:
#'
#' Freeman, G.T. (1991): Calculating catchment area with divergent flow based on a regular grid. Computers and Geosciences, 17: 413-22.
#'
#' Quinn, P.F., Beven, K.J., Chevallier, P., Planchon, O. (1991): The prediction of hillslope flow paths for distributed hydrological modelling using digital terrain models. Hydrological Processes, 5: 59-79.
#'
#' Multiple Triangular Flow Direction:
#'
#' Seibert, J., McGlynn, B. (2007): A new triangular multiple flow direction algorithm for computing upslope areas from gridded digital elevation models. Water Ressources Research, 43, W04501.
#'
#' @author Alexander Brenning (R interface), Olaf Conrad (SAGA module), Thomas Grabs (MTFD algorithm)
#' @note This function uses module `Parallel Processing` (version 2.0.7+: `Catchment Area (Parallel)` from SAGA library `ta_hydrology`.
#'
#' The SAGA GIS 2.0.6+ version of the module adds more (optional) input and
#' output grids that are currently not supported by this wrapper function.
#' Use [rsaga.geoprocessor()] for access to these options,
#' and see `rsaga.get.usage("ta_hydrology","Catchment Area (Parallel)")`
#' for information on new arguments.
#' @seealso [rsaga.topdown.processing()], [rsaga.wetness.index()], [rsaga.geoprocessor()], [rsaga.env()]
#' @examples
#' \dontrun{
#' # SAGA GIS 2.0.6+:
#' rsaga.get.usage("ta_hydrology","Catchment Area (Parallel)")
#' # earlier versions of SAGA GIS:
#' #rsaga.get.usage("ta_hydrology","Parallel Processing")
#' # execute model with typical settings:
#' rsaga.parallel.processing(in.dem = "dem", out.carea = "carea", out.cslope = "cslope")
#' # cslope is in radians - convert to degree:
#' fac = round(180/pi, 4)
#' formula = paste(fac, "*a", sep = "")
#' rsaga.grid.calculus("cslope", "cslopedeg", formula)
#' }
#' @keywords spatial interface
#' @export
rsaga.parallel.processing = function(in.dem, in.sinkroute, in.weight,
    out.carea, out.cheight, out.cslope, out.caspect, out.flowpath,
    step, method="mfd", linear.threshold=Inf, convergence=1.1,
    env = rsaga.env(), ...)
{
    ## Version Stop - tool no longer supported SAGA 2.1.3
    if (env$version == "2.1.3" | env$version == "2.1.4" | env$version == "2.2.0" | env$version == "2.2.1" |
        env$version == "2.2.2" | env$version == "2.2.3") {
      stop("Parallel processing not supported with SAGA GIS 2.1.3 and higher;\n",
           "See help(rsaga.topdown.processing) for similar function with SAGA 2.1.3+")
    }
    in.dem = default.file.extension(in.dem,".sgrd")
    pp.choices = c("d8","rho8","braunschweig","dinf","mfd", "mtfd")
    method = match.arg.ext(method, choices=pp.choices,
        numeric=TRUE, ignore.case=TRUE, base=0)
    param = list( ELEVATION=in.dem )
    if (!missing(in.sinkroute)) {
        in.sinkroute = default.file.extension(in.sinkroute,".sgrd")
        param = c(param, SINKROUTE=in.sinkroute)
    }
    if (!missing(in.weight)) {
        in.weight = default.file.extension(in.weight,".sgrd")
        param = c(param, SINKROUTE=in.weight)
    }
    if (!missing(out.carea))
        param = c(param, CAREA=out.carea)
    if (!missing(out.cheight))
        param = c(param, CHEIGHT=out.cheight)
    if (!missing(out.cslope))
        param = c(param, CSLOPE=out.cslope)
    if (!missing(step))
        param = c(param, STEP=step)
    if (!missing(out.caspect))
        param = c(param, CASPECT=out.caspect)
    if (!missing(out.flowpath))
        param = c(param, FLWPATH=out.flowpath)
    param = c(param, Method=method)
    if (is.finite(linear.threshold)) {
        param = c(param, DOLINEAR=TRUE, LINEARTHRS=linear.threshold)
    } else param = c(param, DOLINEAR=FALSE)

    param = c(param, CONVERGENCE=convergence)

    module = "Catchment Area (Parallel)"
    if (env$version == "2.0.4" | env$version == "2.0.5" | env$version == "2.0.6")
        module = "Parallel Processing"

    rsaga.geoprocessor(lib = "ta_hydrology", module = module, param, env = env, check.parameters = FALSE, ...)
}

#' Top-Down Processing
#'
#' Calculate the size of the local catchment area (contributing area), accumulated material, and flow path length, using top-down processing algorithms from the highest to the lowest cell. \cr Top-Down Processing is new with SAGA GIS 2.1.3. See [rsaga.parallel.processing()] with older versions.
#' @name rsaga.topdown.processing
#' @param in.dem input: digital elevation model (DEM) as SAGA grid file (default file extension: `.sgrd`)
#' @param in.sinkroute optional input: SAGA grid with sink routes
#' @param in.weight optional input: SAGA grid with weights
#' @param in.mean optional input: SAGA grid for mean over catchment calculation
#' @param in.material optional input: SAGA grid with material
#' @param in.target optional input: SAGA grid of accumulation target
#' @param in.lin.val optional input: SAGA grid providing values to be compared with linear flow threshold instead of catchment area
#' @param in.lin.dir optional input: SAGA grid to be used for linear flow routing, if the value is a valid direction (0-7 = N, NE, E, SE, S, SW, W, NW)
#' @param out.carea output: catchment area grid
#' @param out.mean optional output: mean over catchment grid
#' @param out.tot.mat optional output: total accumulated material grid
#' @param out.acc.left optional output: accumulated material from left side grid
#' @param out.acc.right optional output: accumulated material from right side grid
#' @param out.flowpath optional output: flow path length grid
#' @param step integer >=1: step parameter
#' @param method character or numeric: choice of processing algorithm (default `"mfd"`, or 4):
#' - 0 Deterministic 8 (`"d8"` or 0)
#' - 1 Rho 8 (`"rho8"`, or 1)
#' - 2 Braunschweiger Reliefmodell (`"braunschweig"` or 2)
#' - 3 Deterministic Infinity (`"dinf"` or 3)
#' - 4 Multiple Flow Direction (`"mfd"` or 4)
#' - 5 Multiple Triangular Flow Direction (`"mtfd"`, or 5)
#' - 6 Multiple Maximum Gradient Based Flow Direction (`"mdg"`, or 6)
#' @param linear.threshold numeric (number of grid cells): threshold above which linear flow (i.e. the Deterministic 8 algorithm) will be used; linear flow is disabled for `linear.threshold=Inf` (the default)
#' @param convergence numeric >=0: a parameter for tuning convergent/ divergent flow; default value of `1.1` gives realistic results and should not be changed
#' @param env list, setting up a SAGA geoprocessing environment as created by [rsaga.env()]
#' @param ... further arguments to [rsaga.geoprocessor()]
#' @details Refer to the references for details on the available algorithms.
#' @return The type of object returned depends on the `intern` argument passed to the [rsaga.geoprocessor()]. For `intern=FALSE` it is a numerical error code (0: success), or otherwise (the default) a character vector with the module's console output.
#' @references
#' Deterministic 8:
#'
#' O'Callaghan, J.F., Mark, D.M. (1984): The extraction of drainage networks from digital elevation data. Computer Vision, Graphics and Image Processing, 28: 323-344.
#'
#' Rho 8:
#'
#' Fairfield, J., Leymarie, P. (1991): Drainage networks from grid digital elevation models. Water Resources Research, 27: 709-717.
#'
#' Braunschweiger Reliefmodell:
#'
#' Bauer, J., Rohdenburg, H., Bork, H.-R. (1985): Ein Digitales Reliefmodell als Vorraussetzung fuer ein deterministisches Modell der Wasser- und Stoff-Fluesse. Landschaftsgenese und Landschaftsoekologie, H. 10, Parameteraufbereitung fuer deterministische Gebiets-Wassermodelle, Grundlagenarbeiten zu Analyse von Agrar-Oekosystemen, eds.: Bork, H.-R., Rohdenburg, H., p. 1-15.
#'
#' Deterministic Infinity:
#'
#' Tarboton, D.G. (1997): A new method for the determination of flow directions and upslope areas in grid digital elevation models. Water Ressources Research, 33(2): 309-319.
#'
#' Multiple Flow Direction:
#'
#' Freeman, G.T. (1991): Calculating catchment area with divergent flow based on a regular grid. Computers and Geosciences, 17: 413-22.
#'
#' Quinn, P.F., Beven, K.J., Chevallier, P., Planchon, O. (1991): The prediction of hillslope flow paths for distributed hydrological modelling using digital terrain models. Hydrological Processes, 5: 59-79.
#'
#' Multiple Triangular Flow Direction:
#'
#' Seibert, J., McGlynn, B. (2007): A new triangular multiple flow direction algorithm for computing upslope areas from gridded digital elevation models. Water Ressources Research, 43, W04501.
#'
#' Multiple Flow Direction Based on Maximum Downslope Gradient:
#'
#' Qin, C.Z., Zhu, A-X., Pei, T., Li, B.L., Scholten, T., Zhou, C.H. (2011): An approach to computing topographic wetness index based on maximum downslope gradient. Precision Agriculture, 12(1): 32-43.
#'
#' @author Alexander Brenning and Donovan Bangs (R interface), Olaf Conrad (SAGA module), Thomas Grabs (MTFD algorithm)
#' @examples
#' \dontrun{
#' # Calculation of contributing area with default settings:
#' rsaga.topdown.processing(in.dem = "dem", out.carea = "carea")
#' # Calculation of contributing area by maximunm downslope gradient:
#' rsaga.topdown.processing(in.dem = "dem", out.carea = "carea",
#'                          method = "mdg")
#' }
#' @seealso [rsaga.parallel.processing()], [rsaga.wetness.index()], [rsaga.geoprocessor()], [rsaga.env()]
#' @keywords spatial interface
#' @export
rsaga.topdown.processing = function(in.dem, in.sinkroute, in.weight, in.mean, in.material, in.target,
                                    in.lin.val, in.lin.dir,
                                    out.carea, out.mean, out.tot.mat, out.acc.left, out.acc.right,
                                    out.flowpath, step, method = "mfd", linear.threshold = Inf, convergence = 1.1,
                                    env = rsaga.env(), ...) {

    ## Version Stop - SAGA GIS Version < 2.1.3
    if (any(c("2.0.4","2.0.5","2.0.6","2.0.7","2.0.8",
              "2.1.0","2.1.1","2.1.2") == env$version)) {
        stop("rsaga.topdown.processing requires SAGA GIS 2.1.3 or higher;\n",
             "see help(rsaga.parallel.processing) for similar function in earlier versions")
    }

    in.dem = default.file.extension(in.dem,".sgrd")
    pp.choices = c("d8","rho8","braunschweig","dinf","mfd", "mtfd", "mdg")
    method = match.arg.ext(method, choices=pp.choices,
                           numeric=TRUE, ignore.case=TRUE, base=0)
    param = list( ELEVATION=in.dem )
    if (!missing(in.sinkroute)) {
        in.sinkroute = default.file.extension(in.sinkroute,".sgrd")
        param = c(param, SINKROUTE=in.sinkroute)
    }
    if (!missing(in.weight)) {
        in.weight = default.file.extension(in.weight,".sgrd")
        param = c(param, SINKROUTE=in.weight)
    }
    if (!missing(in.mean)) {
        in.mean = default.file.extension(in.mean, ".sgrd")
        param = c(param,VAL_INPUT=in.mean)
    }
    if (!missing(in.material)) {
        in.material = default.file.extension(in.material, ".sgrd")

        if (any(c("2.1.3","2.1.4","2.2.0","2.2.1","2.2.2","2.2.3") == env$version)){
          param = c(param, MATERIAL=in.material)
        } else {
          param = c(param, ACCU_MATERIAL=in.material)
        }
    }
    if (!missing(in.target)) {
        in.target = default.file.extension(in.target, ".sgrd")

        if (any(c("2.1.3","2.1.4","2.2.0","2.2.1","2.2.2","2.2.3") == env$version)){
          param = c(param, TARGET=in.target)
        } else {
          param = c(param, ACCU_TARGET=in.target)
        }

    }
    if (!missing(in.lin.val)) {
        in.lin.val = default.file.extension(in.lin.val, ".sgrd")
        param = c(param, LINEAR_VAL=in.lin.val)
    }
    if (!missing(in.lin.dir)){
        in.lin.dir = default.file.extension(in.lin.dir, ".sgrd")
        param = c(param, LINEAR_DIR=in.lin.dir)
    }
    if (!missing(out.carea)){
        out.carea = default.file.extension(out.carea, ".sgrd")
        if (any(c("2.1.3","2.1.4","2.2.0","2.2.1","2.2.2","2.2.3") == env$version)){
          param = c(param, CAREA=out.carea)
        } else {
          param = c(param, FLOW=out.carea)
        }
    }
    if (!missing(out.mean)){
        out.mean = default.file.extension(out.mean, ".sgrd")
        param = c(param, VAL_MEAN=out.mean)}
    if (!missing(out.tot.mat)) {
      out.tot.mat = default.file.extension(out.tot.mat, ".sgrd")
      if (any(c("2.1.3","2.1.4","2.2.0","2.2.1","2.2.2","2.2.3") == env$version)){
        param = c(param, ACCU_TOT=out.tot.mat)
      } else {
        param = c(param, ACCU_TOTAL=out.tot.mat)
      }
    }
    if (!missing(out.acc.left)){
        out.acc.left = default.file.extension(out.acc.left, ".sgrd")
        param = c(param, ACCU_LEFT=out.acc.left)}
    if (!missing(out.acc.right)){
        out.acc.right = default.file.extension(out.acc.right, ".sgrd")
        param = c(param, ACCU_RIGHT=out.acc.right)}
    if (!missing(out.flowpath)) {
      out.flowpath = default.file.extension(out.flowpath, ".sgrd")
      if (any(c("2.1.3","2.1.4","2.2.0","2.2.1","2.2.2","2.2.3") == env$version)){
        param = c(param, FLOWLEN=out.flowpath)
      } else {
        param = c(param, FLOW_LENGTH=out.flowpath)
      }
    }

    param = c(param, METHOD=method)
    if (is.finite(linear.threshold)) {
        param = c(param, LINEAR_DO=TRUE, LINEAR_MIN=linear.threshold)
    } else param = c(param, LINEAR_DO=FALSE)

    param = c(param, CONVERGENCE=convergence)

    if (any(c("2.1.3","2.1.4") == env$version)) {
      module = "Catchment Area (Top-Down)"
    } else {
      module = "Flow Accumulation (Top-Down)"
    }

    rsaga.geoprocessor(lib = "ta_hydrology", module = module, param, env = env, check.parameters = FALSE, ...)
}

#' SAGA Modules SAGA Wetness Index
#'
#' Calculate the SAGA Wetness Index (SWI), a modified topographic wetness index (TWI)
#' @name rsaga.wetness.index
#' @param in.dem input: digital elevation model (DEM) as SAGA grid file (default file extension: `.sgrd`)
#' @param out.wetness.index output file (optional): wetness index grid file name. Existing files of the same name will be overwritten!
#' @param out.carea output file (optional): catchment area grid file name
#' @param out.cslope output file (optional): catchment slope grid file name
#' @param out.mod.carea output file (optional): file name of modified catchment area grid
#' @param suction SAGA GIS 2.1.0+: positive numeric value (optional): the lower this value is the stronger is the suction effect; defaults to a value of 10 (more detailed information is currently not available  in the SAGA GIS documentation
#' @param area.type character or numeric (optional): type of area: `"absolute"` (or numeric code 0): absolute catchment area; `"square root"` (code 1; the default e.g. in SAGA 2.3.1): square root of catchment area; `"specific"` (code 2; the default e.g. in SAGA 8.4.1): specific catchment area
#' @param slope.type character or numeric (optional): type of slope: `"local"` (or numeric code 0): local slope; `"catchment"` (or code 1; the default): catchment slope.
#' @param slope.min numeric (optional): minimum slope; default: 0
#' @param slope.offset numeric (optional): offset slope; default: 0.1
#' @param slope.weight numeric (optional): weighting factor for slope in index calculation; default: 1
#' @param t.param SAGA GIS up to version 2.0.8: positive numeric value (optional): undocumented
#' @param env A SAGA geoprocessing environment, see [rsaga.env()].)
#' @param ... optional arguments to be passed to [rsaga.geoprocessor()]
#' @details The SAGA Wetness Index is similar to the  Topographic Wetness Index (TWI), but it is based on a modified  catchment area calculation (`out.mod.carea`), which does not treat the flow as a thin film as done in the calculation of catchment areas in conventional algorithms. As a result, the SWI tends to assign a more realistic, higher potential soil wetness than the TWI to grid cells situated in valley floors with a small vertical distance to a channel.
#'
#' This module and its arguments changed substantially from SAGA GIS 2.0.8 to version 2.1.0. It appears to me that the new algorithm is similar (but not identical) to the old one when using `area.type="absolute"` and `slope.type="local"` but I haven't tried out all possible options. This help file will be updated as soon as additional documentation becomes available.
#' @return The type of object returned depends on the `intern` argument passed to the [rsaga.geoprocessor()]. For `intern=FALSE` it is a numerical error code (0: success), or otherwise (the default) a character vector with the module's console output.
#' @references Boehner, J., Koethe, R. Conrad, O., Gross, J.,  Ringeler, A., Selige, T. (2002): Soil Regionalisation by Means of Terrain Analysis and Process Parameterisation. In: Micheli, E., Nachtergaele, F., Montanarella, L. (ed.): Soil Classification 2001. European Soil Bureau, Research Report No. 7, EUR 20398 EN, Luxembourg. pp.213-222.
#'
#' Boehner, J. and Selige, T. (2006): Spatial prediction of soil attributes using terrain analysis and climate regionalisation. In: Boehner, J., McCloy, K.R., Strobl, J. \[Ed.: SAGA - Analysis and Modelling Applications, Goettinger Geographische Abhandlungen, Goettingen: 13-28.
#' @author Alexander Brenning (R interface), Juergen Boehner and Olaf Conrad (SAGA module)
#' @seealso [rsaga.parallel.processing()], [rsaga.geoprocessor()], [rsaga.env()]
#' @examples
#' \dontrun{
#' # using SAGA grids:
#' rsaga.wetness.index("dem.sgrd","swi.sgrd")
#' }
#' @keywords spatial interface
#' @export
rsaga.wetness.index = function( in.dem,
    out.wetness.index, out.carea, out.cslope,
    out.mod.carea,
    # since SAGA GIS 2.1.0:
    suction, area.type, slope.type, slope.min, slope.offset, slope.weight,
    # up to SAGA GIS 2.0.8:
    t.param,
    env = rsaga.env(), ...)
{
    in.dem = default.file.extension(in.dem,".sgrd")
    if(missing(out.wetness.index)) {
      out.wetness.index = tempfile()
      on.exit(unlink(paste(out.wetness.index,".*",sep="")), add = TRUE)
    } else {
      out.wetness.index = default.file.extension(out.wetness.index, ".sgrd")
    }
    if (missing(out.carea)) {
        out.carea = tempfile()
        on.exit(unlink(paste(out.carea,".*",sep="")), add = TRUE)
    } else {
      out.carea = default.file.extension(out.carea, ".sgrd")
    }
    if (missing(out.cslope)) {
        out.cslope = tempfile()
        on.exit(unlink(paste(out.cslope,".*",sep="")), add=TRUE)
    } else {
      out.cslope = default.file.extension(out.cslope, ".sgrd")
    }
    if (missing(out.mod.carea)) {
        out.mod.carea = tempfile()
        on.exit(unlink(paste(out.mod.carea,".*",sep="")), add=TRUE)
    } else {
      out.mod.carea = default.file.extension(out.mod.carea, ".sgrd")
    }
    if (!any(c("2.0.4","2.0.5","2.0.6","2.0.7","2.0.8") == env$version)) {
        param = list(DEM=in.dem, AREA=out.carea, SLOPE=out.cslope,
                     AREA_MOD=out.mod.carea, TWI=out.wetness.index)
        if (!missing(suction)) {
            suction = as.numeric(suction)
            if (suction <= 0) stop("'suction' argument must be >0")
            param = c(param, SUCTION=suction)
        }
        if (!missing(area.type)) {
            area.type = match.arg.ext(area.type,choices=c("absolute","square root","specific"),base=0,ignore.case=TRUE,numeric=TRUE)
            param = c(param, AREA_TYPE=area.type)
        }
        if (!missing(slope.type)) {
            slope.type = match.arg.ext(slope.type,choices=c("local","catchment"),base=0,ignore.case=TRUE,numeric=TRUE)
            param = c(param, SLOPE_TYPE=slope.type)
        }
        if (!missing(slope.min)) {
            slope.min = as.numeric(slope.min)
            if (slope.min < 0) stop("'slope.min' argument must be >=0")
            param = c(param, SLOPE.MIN=slope.min)
        }
        if (!missing(slope.offset)) {
            slope.offset = as.numeric(slope.offset)
            if (slope.offset < 0) stop("'slope.offset' argument must be >=0")
            param = c(param, SLOPE.OFF=slope.offset)
        }
        if (!missing(slope.weight)) {
            slope.weight = as.numeric(slope.weight)
            if (slope.weight < 0) stop("'slope.weight' argument must be >=0")
            param = c(param, SLOPE.WEIGHT=slope.weight)
        }
        if (!missing(t.param))
            warning("argument 't.param' (in saga_cmd: T) supported only up to SAGA GIS 2.0.8")
    } else {
        param = list(DEM=in.dem, C=out.carea, GN=out.cslope,
                     CS=out.mod.carea, SB=out.wetness.index)
        if (!missing(t.param))
            param = c(param, T=as.numeric(t.param))
        if (!missing(suction) | !missing(area.type) | !missing(slope.type) | !missing(slope.min) | !missing(slope.offset) | !missing(slope.weight))
            warning("arguments 'suction', 'area.type', 'slope.min', 'slope.type', 'slope.offset'\n",
                    "and 'slope.weight' not supported prior to SAGA GIS 2.1.0")
    }
    rsaga.geoprocessor(lib = "ta_hydrology",
        module = "SAGA Wetness Index",
        param, check.parameters = FALSE, env=env, ...)
}






########    Module grid_calculus    ########



#' SAGA Module Grid Calculus
#'
#' Perform Arithmetic Operations on Grids
#' @name rsaga.grid.calculus
#' @param in.grids input character vector: SAGA grid files (default file extension: `.sgrd`)
#' @param out.grid output: grid file resulting from the cell-by-cell application of 'formula' to the grids. Existing files will be overwritten!
#' @param formula character string of formula specifying the arithmetic operation to be performed on the `in.grids` (see Details); if this is a formula, only the right hand side will be used.
#' @param coef numeric: coefficient vector to be used for the linear combination of the `in.grids`. If `coef` as one more element than `in.grids`, the first one will be interpreted as an intercept.
#' @param cf.digits integer: number of digits used when converting the `coef`ficients to character strings (trailing zeros will be removed)
#' @param remove.zeros logical: if `TRUE`, terms (grids) with coefficient (numerically) equal to zero (after rounding to `cf.digits` digits) will be removed from the formula
#' @param remove.ones logical: if `TRUE` (the default), factors equal to 1 (after rounding to `cf.digits` digits) will be removed from the formula
#' @param env RSAGA geoprocessing environment, generated by a call to [rsaga.env()]
#' @param ... optional arguments to be passed to [rsaga.geoprocessor()]
#' @details The `in.grids` are represented in the `formula` by the letters `a` (for `in.grids[1]`), `b` etc. Thus, if `in.grids[1]` is Landsat TM channel 3 and `in.grids[2]` is channel 4, the NDVI formula (TM3-TM4)/(TM3+TM4) can be represented  by the character string `"(a-b)/(a+b)"` (any spaces are removed) or the formula `~(a-b)/(a+b)` in the `formula` argument.
#'
#' In addition to +, -, *, and /, the following operators and functions are available for the `formula` definition:
#'     + \eqn{\hat{\ }}{^} power
#'     + `sin(a)` sine
#'     + `cos(a)` cosine
#'     + `tan(a)` tangent
#'     + `asin(a)` arc sine
#'     + `acos(a)` arc cosine
#'     + `atan(a)` arc tangent
#'     + `atan2(a,b)` arc tangent of b/a
#'     + `abs(a)` absolute value
#'     + `int(a)` convert to integer
#'     + `sqr(a)` square
#'     + `sqrt(a)` square root
#'     + `ln(a)` natural logarithm
#'     + `log(a)` base 10 logarithm
#'     + `mod(a,b)` modulo
#'     + `gt(a, b)` returns 1 if a greater b
#'     + `lt(a, b)` returns 1 if a lower b
#'     + `eq(a, b)` returns 1 if a equal b
#'     + `ifelse(switch, x, y)` returns x if switch equals 1 else y
#'
#' Using `remove.zeros=FALSE` might have the side effect that no data areas in the grid with coefficient 0 are passed on to the results grid. (To be confirmed.)
#' @return The type of object returned depends on the `intern` argument passed to the [rsaga.geoprocessor()]. For `intern=FALSE` it is a numerical error code (0: success), or otherwise (the default) a character vector with the module's console output.
#' @author Alexander Brenning (R interface), Olaf Conrad (SAGA module)
#' @seealso [local.function()], [focal.function()], and [multi.focal.function()] for a more flexible framework for combining grids or applying local and focal functions; [rsaga.geoprocessor()], [rsaga.env()]
#' @examples
#' \dontrun{
#' # using SAGA grids:
#' # calculate the NDVI from Landsat TM bands 3 and 4:
#' rsaga.grid.calculus(c("tm3.sgrd","tm4.sgrd"), "ndvi.sgrd", ~(a-b)/(a+b))
#' # apply a linear regression equation to grids:
#' coefs = c(20,-0.6)
#' # maybe from a linear regression of mean annual air temperature (MAAT)
#' # against elevation - something like:
#' # coefs = coef( lm( maat ~ elevation ) )
#' rsaga.linear.combination("elevation.sgrd", "maat.sgrd", coefs)
#' # equivalent:
#' rsaga.grid.calculus("elevation.sgrd", "maat.sgrd", "20 - 0.6*a")
#' }
#' @keywords spatial interface
#' @export
rsaga.grid.calculus = function(in.grids, out.grid, formula,
    env = rsaga.env(), ...)
{
    out.grid = default.file.extension(out.grid, ".sgrd")
    in.grids = default.file.extension(in.grids, ".sgrd")
    in.grids = paste(in.grids, collapse = ";")
    if (inherits(formula, "formula"))
        formula = rev( as.character(formula) )[1]
    formula = gsub(" ", "", formula)
    if (env$version == "2.0.4") {
        param = list( INPUT = in.grids, RESULT = out.grid,
                    FORMUL = formula )
    } else {
        param = list( GRIDS = in.grids, RESULT = out.grid,
                    FORMULA = formula )
    }
    rsaga.geoprocessor(lib = "grid_calculus",
        module = "Grid Calculator", # was = 1
        param = param, env = env, check.parameters = FALSE, ...)
}



#' @rdname rsaga.grid.calculus
#' @name rsaga.linear.combination
#' @export
rsaga.linear.combination = function(in.grids, out.grid, coef,
    cf.digits = 16, remove.zeros = FALSE, remove.ones = TRUE,
    env = rsaga.env(), ...)
{
    out.grid = default.file.extension(out.grid, ".sgrd")
    fmt = paste("%.", cf.digits, "f", sep = "")
    coef = sprintf(fmt, coef)
    zero = sprintf(fmt, 0)
    omit = rep(FALSE, length(coef))

    if (length(coef) == length(in.grids)) { # no intercept provided
        coef = c(NA, coef)
        omit = c(TRUE, omit)
    }
    nvars = length(coef)
    if (nvars != length(in.grids) + 1)
        stop("'coef' must have length 'length(in.grids)' or 'length(in.grids)+1'")

    # Simplify the formula by removing terms that are zero
    # (after rounding to the specified number of digits):
    if (remove.zeros)
        omit = omit | (coef == zero)
    # Zero intercept is always removed:
    omit[1] = omit[1] | (coef[1] == zero)

    # Remove zeros at the end of the coefficients:
    for (i in 1:nvars) {
        if (omit[i]) next
        # Are there any digits at all?
        if (length(grep(".", coef[i], fixed = TRUE)) == 0) next
        nc = nchar(coef[i])
        # Remove all trailing zeros:
        while (substr(coef[i], nc, nc) == "0") {
            coef[i] = substr(coef[i], 1, nc - 1)
            nc = nchar(coef[i])
        }
        # Remove trailing decimal point:
        if (substr(coef[i], nc, nc) == ".")
            coef[i] = substr(coef[i], 1, nc - 1)
    }

    # Set up the formula:
    ltrs = letters[ 1 : sum(!omit[-1]) ]
    if (!omit[1]) ltrs = c("intercept", ltrs)
    formula = paste(coef[ !omit ], ltrs,
                    collapse = "+", sep = "*")
    formula = gsub("*intercept", "", formula, fixed = TRUE)
    formula = gsub("+-", "-", formula, fixed = TRUE)
    if (remove.ones) {
        formula = gsub("-1*", "-", formula, fixed = TRUE)
        formula = gsub("+1*", "+", formula, fixed = TRUE)
    }

    rsaga.grid.calculus(in.grids = in.grids[!omit[-1]], out.grid = out.grid,
        formula = formula, env = env, ...)
}





########     Module shapes_grid     ########



#' Contour Lines from a Grid
#'
#' Creates a contour lines shapefile from a grid file in SAGA grid format.
#' @name rsaga.contour
#' @param in.grid input: digital elevation model (DEM) as SAGA grid file (default file extension: `.sgrd`)
#' @param out.shapefile output: contour line shapefile. Existing files will be overwritten!
#' @param zstep,zmin,zmax lower limit, upper limit, and equidistance of contour lines
#' @param vertex optional parameter: vertex type for resulting contours. Default `"xy"` (or 0). Only available with SAGA GIS 2.1.3+.
#' - 0 `"xy"`
#' - 1 `"xyz"`
#' @param env A SAGA geoprocessing environment, see [rsaga.env()]
#' @param ... arguments to be passed to [rsaga.geoprocessor()]
#' @return The type of object returned depends on the `intern` argument passed to the [rsaga.geoprocessor()]. For `intern=FALSE` it is a numerical error code (0: success), or otherwise (the default) a character vector with the module's console output.
#' @author Alexander Brenning (R interface), Olaf Conrad (SAGA module)
#' @seealso [rsaga.geoprocessor()]
#' @keywords spatial interface
#' @export
rsaga.contour = function(in.grid,out.shapefile,zstep,zmin,zmax,vertex="xy",env=rsaga.env(),...) {
    in.grid = default.file.extension(in.grid,".sgrd")
    # 'INPUT' changed to 'GRID' with SAGA 2.1.3
    if(any(c("2.0.4","2.0.5","2.0.6","2.0.7","2.0.8",
             "2.1.0","2.1.1","2.1.2") == env$version)){
        param = list(INPUT=in.grid,CONTOUR=out.shapefile)
    } else {
        param = list(GRID=in.grid,CONTOUR=out.shapefile)
    }
    if (!missing(zmin))  param = c(param, ZMIN=as.numeric(zmin))
    if (!missing(zmax))  param = c(param, ZMAX=as.numeric(zmax))
    if (!missing(zstep)) {
        stopifnot(as.numeric(zstep)>0)
        param = c(param, ZSTEP=as.numeric(zstep))
    }
    v.choices = c("xy", "xyz")
    vertex = match.arg.ext(vertex, choices=v.choices,
                           numeric=TRUE, ignore.case=TRUE, base=0)
    if (!missing(vertex)) {
        if (env$version == "2.1.3" | env$version == "2.1.4") {
            param = c(param, VERTEX=vertex)
        }
    }
    rsaga.geoprocessor(lib = "shapes_grid",
        module = "Contour Lines from Grid",
        param, env = env, check.parameters = FALSE, ...)
}



#' Add Grid Values to Point Shapefile
#'
#' Pick values from SAGA grids and attach them as a new variables to a point shapefile.
#' @name rsaga.add.grid.values.to.points
#' @param in.grids Input: character vector with names of (one or more) SAGA GIS grid files to be converted into a point shapefile.
#' @param in.shapefile Input point shapefile (default extension: `.shp`).
#' @param out.shapefile Output point shapefile (default extension: `.shp`).
#' @param method interpolation method to be used; choices: nearest neighbour interpolation (default), bilinear interpolation, inverse distance weighting, bicubic spline interpolation, B-splines.
#' @param ... Optional arguments to be passed to [rsaga.geoprocessor()], including the `env` RSAGA geoprocessing environment.
#' @details Retrieves information from the selected grids at the positions of the points of the selected points layer and adds it to the resulting layer.
#' @author Alexander Brenning (R interface), Olaf Conrad (SAGA modules)
#' @note This function uses module `Add Grid Values to Points` in SAGA GIS library `shapes_grid`.
#' @seealso [pick.from.points()], [pick.from.ascii.grid()], [pick.from.saga.grid()], [rsaga.grid.to.points()]
#' @keywords spatial interface
#' @export
rsaga.add.grid.values.to.points = function(in.shapefile,
    in.grids, out.shapefile,
    method = c("nearest.neighbour", "bilinear",
      "idw", "bicubic.spline", "b.spline"), ...)
{
    in.grids = default.file.extension(in.grids,".sgrd")
    in.grids = paste(in.grids, collapse = ";")
    # check if this is SAGA version dependent:
    in.shapefile = default.file.extension(in.shapefile,".shp")
    out.shapefile = default.file.extension(out.shapefile,".shp")
    method = match.arg.ext(method, base = 0, ignore.case = TRUE, numeric = TRUE)
    param = list(SHAPES = in.shapefile, GRIDS = in.grids,
                RESULT = out.shapefile, INTERPOL = method)
    rsaga.geoprocessor(lib = "shapes_grid",
        module = "Add Grid Values to Points", # was: = 0
        param, check.parameters = FALSE, ...)
}


#' Convert SAGA grid file to point shapefile
#'
#' Convert SAGA grid file to point (or polygon) shapefile - either completely or only a random sample of grid cells.
#' @name rsaga.grid.to.points
#' @param in.grids Input: names of (possibly several) SAGA GIS grid files to be converted into a point shapefile.
#' @param in.grid Input: SAGA grid file from which to sample.
#' @param out.shapefile Output: point shapefile (default extension: `.shp`). Existing files will be overwritten!
#' @param in.clip.polygons optional polygon shapefile to be used for clipping/masking an area
#' @param exclude.nodata logical (default: `TRUE`): skip 'nodata' grid cells?
#' @param type character string: `"nodes"`: create point shapefile of grid center points; `"cells"` (only supported by SAGA GIS 2.0.6+): create polygon shapefile with grid cell boundaries
#' @param freq integer >=1: sampling frequency: on average 1 out of 'freq' grid cells are selected
#' @param env RSAGA geoprocessing environment created by [rsaga.env()]; required by `rsaga.grid.to.points` to determine version-dependent SAGA module name and arguments
#' @param ... Optional arguments to be passed to [rsaga.geoprocessor()]
#' @author Alexander Brenning (R interface), Olaf Conrad (SAGA modules)
#' @note These functions use modules `Grid Cells to Points/Polygons` (previously called `Grid Values to Points` and in some earlier versions `Grid Values to Shapes`) and `Grid Values to Points (randomly)` in SAGA library `shapes_grid`.
#'
#' The SAGA 2.0.6+ version of this module is more flexible as it allows to create grid cell polygons instead of center points (see argument `type`).
#' @seealso [rsaga.add.grid.values.to.points()]
#' @examples
#' \dontrun{
#' # one point per grid cell, exclude nodata areas:
#' rsaga.grid.to.points("dem", "dempoints")
#' # take only every 20th point, but to not exclude nodata areas:
#' rsaga.grid.to.points.randomly("dem", "dempoints20", freq = 20)
#' }
#' @keywords spatial interface
#' @export
rsaga.grid.to.points = function(in.grids, out.shapefile,
    in.clip.polygons, exclude.nodata = TRUE,
    type = "nodes", env = rsaga.env(), ...)
{
    in.grids = default.file.extension(in.grids,".sgrd")
    in.grids = paste(in.grids, collapse = ";")
    type = match.arg.ext(type, numeric=TRUE, ignore.case=TRUE, base=0,
        choices=c("nodes","cells"))
    if (type == 1 & (env$version == "2.0.4" | env$version == "2.0.5")) {
        type = 0
        warning("type == 'cells' not supported by SAGA 2.0.4 and 2.0.5; using type = 'nodes'")
    }
    param = list(GRIDS = in.grids)
    if (env$version == "2.0.4" | env$version == "2.0.5") {
        param = c(param, POINTS = out.shapefile)
    } else param = c(param, SHAPES = out.shapefile)
    param = c(param, NODATA = exclude.nodata)
    if (!missing(in.clip.polygons))
        param = c(param, POLYGONS = in.clip.polygons)
    if (!(env$version == "2.0.4" | env$version == "2.0.5"))
        param = c(param, TYPE = type)

    # Module name changed to 'Grid Cells to Points/Polygons'
    # somewhere between SAGA 9.3.3 and SAGA 9.5.0:
    module <- "Grid Cells to Points/Polygons"
    check_module <- TRUE
    if (!is.na(env$numeric_version))
      check_module <- env$numeric_version < 950
    if (!rsaga.module.exists("shapes_grid",module,env=env)) {
        module = "Grid Values to Shapes"
        if (!rsaga.module.exists("shapes_grid",module,env=env))
            module = "Grid Values to Shapes"
    }

    rsaga.geoprocessor(lib = "shapes_grid",
        module = module, # was: = 3
        param, env = env, check.parameters = FALSE, ...)
}


#' @rdname rsaga.grid.to.points
#' @name rsaga.grid.to.points.randomly
#' @export
rsaga.grid.to.points.randomly = function(in.grid,
    out.shapefile, freq, ...)
{
    in.grid = default.file.extension(in.grid, ".sgrd")
    out.shapefile = default.file.extension(out.shapefile, ".shp")
    if (freq < 1) stop("'freq' must be an integer >=1")
    param = list(GRID = in.grid, FREQ = freq, POINTS = out.shapefile)
    rsaga.geoprocessor(lib = "shapes_grid",
        module = "Grid Values to Points (randomly)", # was: = 4
        param, check.parameters = FALSE, ...)
}



#' Spatial Interpolation Methods
#'
#' Spatial interpolation of point data using inverse distance to a power (inverse distance weighting, IDW), nearest neighbors, or modified quadratic shephard.
#' @name rsaga.inverse.distance
#' @param in.shapefile Input: point shapefile (default extension: `.shp`).
#' @param out.grid Output: filename for interpolated grid (SAGA grid file). Existing files will be overwritten!
#' @param field numeric or character: number or name of attribute in the shapefile's attribute table to be interpolated; the first attribute is represented by a zero.
#' @param power numeric (>0): exponent used in inverse distance  weighting (usually 1 or 2)
#' @param maxdist numeric: maximum distance of points to be used for inverse distance interpolation (search radius); no search radius is applied when this argument is missing or equals `Inf`
#' @param nmax Maximum number of nearest points to be used for interpolation; `nmax=Inf` is a valid value (no upper limit)
#' @param quadratic.neighbors integer >=5; default 13.
#' @param weighting.neighbors integer >=3; default 19.
#' @param target required argument of type list: parameters identifying the target area, e.g. the x/y extent and cellsize, or name of a reference grid; see [rsaga.target()].
#' @param env RSAGA geoprocessing environment created by [rsaga.env()], required because module(s) depend(s) on SAGA version
#' @param ... Optional arguments to be passed to [rsaga.geoprocessor()], including the `env` RSAGA geoprocessing environment.
#' @details These functions use modules from the `grid_gridding` SAGA GIS library. They do not support SAGA GIS 2.0.4, which differs in some argument names and parameterizations. Target grid parameterization by grid file name currently doesn't work with SAGA GIS 2.1.0  Release Candidate 1 (see also [rsaga.target()]); stay tuned for future updates and fixes.
#' @references QSHEP2D: Fortran routines implementing the Quadratic Shepard method for bivariate interpolation of scattered data  (see R. J. Renka, ACM TOMS 14 (1988) pp.149-150). Classes: E2b. Interpolation of scattered, non-gridded  multivariate data.
#' @author Alexander Brenning (R interface), Andre Ringeler and Olaf Conrad (SAGA modules)
#' @note The 'Inverse Distance Weighted' module of SAGA GIS not only support inverse-distance weighted interpolation, but also exponential and other weighting schemes (command line argument WEIGHTING); these are however not accessible through this function, but only through the `rsaga.geoprocessor`, if needed. See `rsaga.get.usage("grid_gridding","Inverse Distance Weighted")` for details.
#'
#' See the example section in the help file for [shapefiles::write.shapefile()] in package `shapefiles` to learn how to apply these interpolation functions to a shapefile exported from a data.frame.
#'
#' Modified Quadratic Shephard method: based on module 660 in TOMS (see references).
#' @seealso [rsaga.target()]; [gstat::idw()] in package `gstat`.
#' @keywords spatial interface
#' @export
rsaga.inverse.distance = function(in.shapefile, out.grid, field,
        power = 1, maxdist, nmax = 100,
        target, env = rsaga.env(), ...)
{
    if (any(c("2.0.4","2.0.5","2.0.6","2.0.7","2.0.8",
              "2.1.0","2.1.1","2.1.2","2.1.3","2.1.4",
              "2.2.0","2.2.1","2.2.2","2.2.3") == env$version))
        stop("rsaga.inverse.distance doesn't support SAGA GIS Versions older than 2.3.1 any longer")

    stopifnot(!missing(target))

    if (power <= 0) stop("'power' must be >0")
    if (field < 0) stop("'field' must be an integer >=0")

    in.shapefile = default.file.extension(in.shapefile, ".shp")
    out.grid = default.file.extension(out.grid, ".sgrd")

    if (target$TARGET_DEFINITION== 1) {
        if (target$TARGET_TEMPLATE != out.grid) {
            rsaga.copy.sgrd(target$TARGET_TEMPLATE, out.grid, env = env)
            target$TARGET_TEMPLATE = out.grid
        }
    }

    param = list(
        TARGET_OUT_GRID = out.grid,
        POINTS = in.shapefile,
        FIELD = field,
        DW_WEIGHTING = 0,
        SEARCH_DIRECTION = 0,
        DW_IDW_POWER = power)


    is.global = (missing(maxdist))
    if (!missing(maxdist)) {
        if (maxdist <= 0) stop("'maxdist' must be >0")
        if (maxdist == Inf) is.global = TRUE
    }
    if (is.global) {
        param = c(param, list(SEARCH_RANGE = 1))
    } else
        param = c(param, list(SEARCH_RANGE = 0, SEARCH_RADIUS = maxdist))

    if (nmax <= 0) stop("'nmax' must be an integer >0, or Inf")
    use.all = (nmax == Inf)

    if (use.all) {
        param = c(param, list(SEARCH_POINTS_ALL = 1))
    } else
        param = c(param, list(SEARCH_POINTS_ALL = 0, SEARCH_POINTS_MAX = nmax))

    param = c(param, target)

    if (any(c("2.3.1","2.3.2", "3.0.0") == env$version)) {

      nm = names(param)
      nm[ nm == "POINTS" ] = "SHAPES"

      names(param) = nm
    }

    rsaga.geoprocessor(lib = "grid_gridding",
        module = "Inverse Distance Weighted",
        param = param, env = env, check.parameters = FALSE, ...)
}


#' @rdname rsaga.inverse.distance
#' @name rsaga.nearest.neighbour
#' @export
rsaga.nearest.neighbour = function(in.shapefile, out.grid, field,
    target, env = rsaga.env(), ...)
{
  if (any(c("2.0.4","2.0.5","2.0.6","2.0.7","2.0.8",
            "2.1.0","2.1.1","2.1.2","2.1.3","2.1.4",
            "2.2.0","2.2.1","2.2.2","2.2.3") == env$version))
      stop("rsaga.inverse.distance doesn't support SAGA GIS Versions older than 2.3.1 any longer")

   stopifnot(!missing(target))

    if (field < 0)
        stop("'field' must be an integer >=0")

    in.shapefile = default.file.extension(in.shapefile, ".shp")
    out.grid = default.file.extension(out.grid, ".sgrd")

    if (target$TARGET_DEFINITION== 1) {
      if (target$TARGET_TEMPLATE != out.grid) {
        rsaga.copy.sgrd(target$TARGET_TEMPLATE, out.grid, env = env)
        target$TARGET_TEMPLATE = out.grid
      }
    }

    param = list(
        TARGET_OUT_GRID  = out.grid,
        POINTS = in.shapefile,
        FIELD = field)
    param = c(param, target)


    if (any(c("2.3.1","2.3.2", "3.0.0") == env$version)) {

      nm = names(param)
      nm[ nm == "POINTS" ] = "SHAPES"

      names(param) = nm
    }


    rsaga.geoprocessor(lib = "grid_gridding",
        module = "Nearest Neighbour",
        param, env = env, check.parameters = FALSE, ...)
}

#' @rdname rsaga.inverse.distance
#' @name rsaga.modified.quadratic.shephard
#' @export
rsaga.modified.quadratic.shephard = function(in.shapefile, out.grid, field,
    quadratic.neighbors = 13, weighting.neighbors = 19,
    target, env = rsaga.env(), ...)
{
    if (any(c("2.0.4","2.0.5","2.0.6","2.0.7","2.0.8",
            "2.1.0","2.1.1","2.1.2","2.1.3","2.1.4",
            "2.2.0","2.2.1","2.2.2","2.2.3") == env$version))
        stop("rsaga.modified.quadratic.shephard doesn't support SAGA GIS Versions older than 2.3.1 any longer")


    stopifnot(!missing(target))

    if (field < 0)
        stop("'field' must be an integer >=0")
    if (quadratic.neighbors < 5)
        stop("'quadratic.neighbors' must be an integer >=5")
    if (weighting.neighbors < 5)
        stop("'weighting.neighbors' must be an integer >=3")

    in.shapefile = default.file.extension(in.shapefile, ".shp")
    out.grid = default.file.extension(out.grid, ".sgrd")

    if (target$TARGET_DEFINITION== 1) {
      if (target$TARGET_TEMPLATE != out.grid) {
        rsaga.copy.sgrd(target$TARGET_TEMPLATE, out.grid, env = env)
        target$TARGET_TEMPLATE = out.grid
      }
    }

    param = list(
        TARGET_OUT_GRID = out.grid,
        POINTS = in.shapefile,
        FIELD = field,
        QUADRATIC_NEIGHBORS = quadratic.neighbors,
        WEIGHTING_NEIGHBORS = weighting.neighbors)

    param = c(param, target)

    if (any(c("2.3.1","2.3.1", "3.0.0") == env$version)) {

      nm = names(param)
      nm[ nm == "POINTS" ] = "SHAPES"

      names(param) = nm
    }

    # Earlier versions had a typo:
    module <- "Modified Quadratic Shepard"
    if (!is.na(env$numeric_version)) {
      if (env$numeric_version < 970)
        module <- "Modifed Quadratic Shepard"
    }

    rsaga.geoprocessor(lib = "grid_gridding",
        module = module,
        param, env = env, check.parameters = FALSE, ...)
}


#' @rdname rsaga.inverse.distance
#' @name rsaga.triangulation
#' @export
rsaga.triangulation = function(in.shapefile, out.grid, field,
    target, env = rsaga.env(), ...)
{
  if (any(c("2.0.4","2.0.5","2.0.6","2.0.7","2.0.8",
            "2.1.0","2.1.1","2.1.2","2.1.3","2.1.4",
            "2.2.0","2.2.1","2.2.2","2.2.3") == env$version))
    stop("rsaga.modified.quadratic.shephard doesn't support SAGA GIS Versions older than 2.3.1 any longer")

    stopifnot(!missing(target))

    if (field < 0)
        stop("'field' must be an integer >=0")

    in.shapefile = default.file.extension(in.shapefile, ".shp")
    out.grid = default.file.extension(out.grid, ".sgrd")

    if (target$TARGET_DEFINITION== 1) {
      if (target$TARGET_TEMPLATE != out.grid) {
        rsaga.copy.sgrd(target$TARGET_TEMPLATE, out.grid, env = env)
        target$TARGET_TEMPLATE = out.grid
      }
    }

    param = list(
      TARGET_OUT_GRID = out.grid,
      POINTS = in.shapefile,
      FIELD = field)

    param = c(param, target)

    if (any(c("2.3.1","2.3.2", "3.0.0") == env$version)) {

      nm = names(param)
      nm[ nm == "POINTS" ] = "SHAPES"

      names(param) = nm
    }

    rsaga.geoprocessor(lib = "grid_gridding",
        module = "Triangulation",
        param, env = env, check.parameters = FALSE, ...)
}


### Module shapes_polygons##########

#' @title Spatial intersection of two polygon layers
#' @description The function `rsaga.intersect.polygons` calculates the
#'   geometric intersection of two overlayed polygon layers using SAGA module
#'   "`Intersect`".
#' @param layer_a A `character` string representing the path to a polygon
#'   shapefile.
#' @param layer_b A `character` string representing the path to a polygon
#'   shapefile with which to intersect layer_a.
#' @param result A `character` string indicating where the resulting
#'   shapefile should be stored.
#' @param split If `TRUE`, multipart polygons become separated polygons
#'   (default: FALSE).
#' @param load Deprecated, will be removed in a future release. Ignored
#'   if `FALSE`, and causes an error if `TRUE` (default: NULL).
#' @param env RSAGA geoprocessing environment created by
#'   [rsaga.env()].
#' @return The function saves the output shapefile to the path indicated in
#'   function argument `result`.
#' @details Function `gIntersection` in `rgeos` package can also be used to
#'   define the intersection between two polygon layers. However,
#'   [rsaga.intersect.polygons()] will be usually much faster,
#'   especially when intersecting thousands of polygons.
#' @author Jannes Muenchow and Alexander Brenning (R interface), Olaf Conrad and Angus Johnson (SAGA
#'   modules)
#' @keywords vector operations polygons
#' @export
#'
rsaga.intersect.polygons <-
  function(layer_a = NULL, layer_b = NULL, result = NULL,
           split = FALSE, load = NULL, env = rsaga.env()) {
    # check if all necessary function arguments were supplied
    if (any(mapply(is.null, list(layer_a, layer_b, result)))) {
      stop("Please specify layer_a, layer_b and a result layer!")
    }

    if (!is.character(layer_a)) {
      stop("layer_a must be the name of a shapefile; SpatialPolygonsDataFrames are no longer supported")
    }

    if (!is.character(layer_b)) {
      stop("layer_b must be the name of a shapefile; SpatialPolygonsDataFrames are no longer supported")
    }

    if (!is.null(load)) {
      if (load) {
        stop("argument 'load' is deprecated, and 'load = TRUE' is no longer supported")
      } else {
        warning("argument 'load' is deprecated and will cause an error in future versions")
      }
    }

    # execute the 'Intersect'-function
    rsaga.geoprocessor(lib = "shapes_polygons", module = "Intersect",
                       list(A = layer_a,
                            B = layer_b,
                            RESULT = result,
                            SPLIT = split),
                       env = env, check.parameters = FALSE)
  }

#' @title Spatial union of two polygon layers
#' @description The function `rsaga.union.polygons` uses SAGA function
#'   "`Union`" to calculate the geometric union of two polygon layers. This
#' corresponds to the intersection and the symmetrical difference of the two
#' layers.
#' @param layer_a A `character` string representing the path to a polygon
#'   shapefile.
#' @param layer_b A `character` string representing the path to a polygon
#'   shapefile with which to union layer_a.
#' @param result `character`, path indicating where to store the output
#'   shapefile.
#' @param split If `TRUE`, multipart polygons become separated polygons
#'   (default: FALSE).
#' @param load Deprecated, will be removed in a future release. Ignored
#'   if `FALSE`, and causes an error if `TRUE`  (default: NULL)
#' @param env RSAGA geoprocessing environment created by
#'   [rsaga.env()], required because module(s) depend(s) on SAGA
#'   version.
#' @return The function saves the output shapefile to the path indicated in
#'   function argument `result`.
#' @details Function `gUnion()` in `rgeos` package can also be used for joining
#'   intersecting polygon geometries. However,
#'   [rsaga.union.polygons()] will be usually much faster,
#'   especially when joining thousands of polygons.
#' @author Jannes Muenchow and Alexander Brenning (R interface), Olaf Conrad and Angus Johnson (SAGA
#'   modules)
#' @keywords vector operations polygons
#' @export

rsaga.union.polygons <-
  function(layer_a = NULL, layer_b = NULL,
           result = NULL, split = FALSE, load = NULL,
           env = rsaga.env()) {
    # check if all necessary function arguments were provided
    if (any(mapply(is.null, list(layer_a, layer_b, result)))) {
      stop("Please specify layer_a, layer_b and a result layer!")
      }

    if (!is.character(layer_a)) {
      stop("layer_a must be the name of a shapefile; SpatialPolygonsDataFrames are no longer supported")
    }

    if (!is.character(layer_b)) {
      stop("layer_b must be the name of a shapefile; SpatialPolygonsDataFrames are no longer supported")
    }

    if (!is.null(load)) {
      if (load) {
        stop("argument 'load' is deprecated, and 'load = TRUE' is no longer supported")
      } else {
        warning("argument 'load' is deprecated and will cause an error in future versions")
      }
    }

  # execute SAGA function "Union"
  rsaga.geoprocessor(lib = "shapes_polygons", module = "Union",
                     list(A = layer_a,
                          B = layer_b,
                          RESULT = result,
                          SPLIT = split),
                     env = env, check.parameters = FALSE)
}

