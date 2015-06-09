RSAGA News
====
#### 09 June 2015

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
