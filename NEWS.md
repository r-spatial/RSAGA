# RSAGA 1.4.2

* Package was updated and (successfully) tested with SAGA GIS 9.7.2, but continues to support earlier versions of SAGA GIS. Tests were also run with several SAGA versions in the 8.5.0 to 9.7.2 range.
* Changes in module names in recent SAGA versions: grid_gridding "Modified Quadratic Shepard" and shapes_grid "Grid Values to Points"
* Module "Potential Incoming Solar Radiation" is not working properly in SAGA 8.5.x; `rsaga.pisr2` therefore refuses to call it and generates an error.


# RSAGA 1.4.1

## Bug fixes

* Fixed data format in `rsaga.pisr2` arguments passed to `saga_cmd`

# RSAGA 1.4.0

## General

* Package was (successfully) tested with SAGA GIS 8.4.1, but continues to support earlier versions of SAGA GIS. No tests made with SAGA >7.0.0 and <=8.4.0.
* Dependency on rgdal was removed by eliminating the support of spatial object from package sp by intersect and union functions.
* Now also looks for SAGA installation in C:/Progra~?/SAGA, which seems to be used by SAGA 8.4.1 (and other recent versions) by default.
* Additional test were implemented to verify that package works with recent (as well as older) versions of SAGA GIS.

## TO DO

* Ensure that help file contents are still consistent with current SAGA GIS help files.

# RSAGA 1.3.0

## General

* RSAGA checks if the correct parameters are used in `rsaga.geoprocessor`
* RSAGA supports SAGA GIS 7.0.0

## Bug fixes

* Fixed `out.wetness` optional parameter bug in `rsaga.wetness.index`

# RSAGA 1.2.0

## General

* RSAGA supports SAGA GIS 6.4.0

## Bug fixes

* Warning messages for RSAGA functions that are not supported by certain SAGA GIS versions

# RSAGA 1.1.1

## Bug fixes

* Fix for CRAN warning about digest package

# RSAGA 1.1.0

## General

* We integrated RSAGA into the r-spatial.org community. If you find any bugs please report them at our new development repository (<https://github.com/r-spatial/RSAGA>).
* RSAGA supports SAGA GIS 6.3.0
* RSAGA locates the SAGA command line program and the SAGA module libraries on Unix systems also with the PATH and SAGA_MLB environment variable

## Bug fixes

* change curvature units form degrees to 1 / map units #4

# RSAGA 1.0.0

## General

* RSAGA 1.0.0 is now available on CRAN. It is a major update that adds support for SAGA GIS 2.3.1 LTS - 6.2.0. Older SAGA GIS versions might still work but are not supported anymore. Please report bugs to (<https://github.com/r-spatial/RSAGA>).
* RSAGA supports now SAGA GIS versions up to 6.2.0

## Misc

* rsaga.env is rewritten to automatically detect SAGA GIS on Windows and Unix again

# RSAGA 0.94-5

## General

* RSAGA 0.94-5 is now available on CRAN. This release is largely version updates to support SAGA GIS 2.2.3

# RSAGA 0.94-4

## General

* RSAGA 0.94-4 is now available on CRAN. Included in this release are updates to support SAGA GIS versions 2.2.1 and 2.2.2

* An additional function `rsaga.pisr2` has been added to support the Potential Incoming Solar Radiation module with SAGA 2.2.2

* **Unix note** - on some systems users have reported that `rsaga.*` functions with SAGA 2.2.0+ produce a warning message: "Error: select a tool". This can be suppressed by adding the argument `check.module.exists = FALSE` until the bug is addressed.

# RSAGA 0.94-3

## General

* RSAGA 0.94-3 is now available on CRAN. Included in this release are updates to support SAGA GIS version 2.2.0

* An introductory vignette and associated data file `landslides` are now included with RSAGA. The vignette demonstrates how to initialize and run RSAGA, perform terrain analysis, and execute RSAGA grid functions using an example of generalized additive modeling for landslide susceptibility modeling with the included dataset.

# RSAGA 0.94-1 

## General

* RSAGA 0.94-1 is now available on CRAN. Updates to the package support changes in SAGA GIS, particularly from SAGA GIS 2.1.0 - 2.1.4.

* **N.B.:** Some changes may affect older code:

## Argument changes for SAGA GIS 2.1.1+

  * New function `rsaga.slope.asp.curv` has been added for use with SAGA 2.1.1+
  * Continue to use `rsaga.local.morphometry` with SAGA versions below 2.1.1
  * Calls using `rsaga.local.morphometry` with SAGA 2.1.1+ are redirected to `rsaga.slope.asp.curv`
  * An additional method has been added in `rsaga.slope.asp.curv`, in the `3` position. Numeric arguments for `method`
    are not supported with this function, and will stop with an error message.
  * Plan and profile curvature calculations have changed with SAGA 2.1.1+. See [SAGA forum discussion]
  (http://sourceforge.net/p/saga-gis/discussion/354013/thread/e9d07075/?limit=25#5727) for details.
  * `rsaga.slope`, `rsaga.aspect`, `rsaga.curvature`, `rsaga.plan.curvature`, and `rsaga.profile.curvature`
  use either `rsaga.slope.asp.curv` (version 2.1.1+) or `rsaga.local.morphometry` (version < 2.1.1).
  
## Parallel Processing

* `rsaga.parallel.processing` is not supported with SAGA 2.1.3+. See `rsaga.topdown.processing` for a similar function.

## The handling of Boolean parameters

* When using `rsaga.geoprocessor`, *not* passing a Boolean parameter will leave it as default, not necessarily `FALSE`. 
  See more details in [SAGA Release Wiki] (http://sourceforge.net/p/saga-gis/wiki/Compatibility%202.1.3/).
