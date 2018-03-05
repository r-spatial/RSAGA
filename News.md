# RSAGA News

# RSAGA 1.0.1

We integrated RSAGA into the r-spatial.org community. If you find any bugs please report them at our new development repository (https://github.com/r-spatial/RSAGA).

## Features
* Bug fixes for the describtion file
* RSAGA supports SAGA GIS 6.3.0

# RSAGA 1.0.0
RSAGA 1.0.0 is now available on CRAN. It is a major update that adds support for SAGA GIS 2.3.1 LTS - 6.2.0. Older SAGA GIS versions might still work but are not supported anymore. Please report bugs to (https://github.com/be-marc/RSAGA).

## Features
* RSAGA supports now the latest SAGA GIS versions up to 6.2.0
* rsaga.env is rewritten to automatically detect SAGA GIS on Windows and Unix again


# RSAGA 0.94-5
RSAGA 0.94-5 is now available on CRAN. This release is largely version updates to support SAGA GIS 2.2.3

# RSAGA 0.94-4
RSAGA 0.94-4 is now available on CRAN. Included in this release are updates to support SAGA GIS versions 2.2.1 and 2.2.2

An additional function `rsaga.pisr2` has been added to support the Potential Incoming Solar Radiation module with SAGA 2.2.2

**Unix note** - on some systems users have reported that `rsaga.*` functions with SAGA 2.2.0+ produce a warning message: "Error: select a tool". This can be suppressed by adding the argument `check.module.exists = FALSE` until the bug is addressed.

# RSAGA 0.94-3
RSAGA 0.94-3 is now available on CRAN. Included in this release are updates to support SAGA GIS version 2.2.0

An [introductory vignette](https://cran.r-project.org/web/packages/RSAGA/vignettes/RSAGA-landslides.pdf) and associated data file `landslides` are now included with RSAGA. The vignette demonstrates how to initialize and run RSAGA, perform terrain analysis, and execute RSAGA grid functions using an example of generalized additive modeling for landslide susceptibility modeling with the included dataset.

# RSAGA 0.94-1 

RSAGA 0.94-1 is now available on CRAN. Updates to the package support changes in SAGA GIS, particularly from SAGA version
2.1.0 to 2.1.4.

**N.B.:** Some changes may affect older code:

1. Arguments and calculations using the `Slope, Aspect, Curvature` module have changed with SAGA 2.1.1+ 
  * New function `rsaga.slope.asp.curv` has been added for use with SAGA 2.1.1+
  * Continue to use `rsaga.local.morphometry` with SAGA versions below 2.1.1
  * Calls using `rsaga.local.morphometry` with SAGA 2.1.1+ are redirected to `rsaga.slope.asp.curv`
    * An additional method has been added in `rsaga.slope.asp.curv`, in the `3` position. Numeric arguments for `method`
    are not supported with this function, and will stop with an error message.
  * Plan and profile curvature calculations have changed with SAGA 2.1.1+. See [SAGA forum discussion]
  (http://sourceforge.net/p/saga-gis/discussion/354013/thread/e9d07075/?limit=25#5727) for details.
  * `rsaga.slope`, `rsaga.aspect`, `rsaga.curvature`, `rsaga.plan.curvature`, and `rsaga.profile.curvature`
  use either `rsaga.slope.asp.curv` (version 2.1.1+) or `rsaga.local.morphometry` (version < 2.1.1).
  
2. Parallel Processing (Terrain Analysis - Hydrology) no longer in SAGA 2.1.3+, replaced with Top-Down Processing.
`rsaga.parallel.processing` is not supported with SAGA 2.1.3+. See `rsaga.topdown.processing` for a similar
function.

3. The handling of Boolean parameters has changed with SAGA 2.1.3+. When using `rsaga.geoprocessor`, *not* passing a
Boolean parameter will leave it as default, not necessarily `FALSE`. See more details in [SAGA Release Wiki]
(http://sourceforge.net/p/saga-gis/wiki/Compatibility%202.1.3/).
