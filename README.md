
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RSAGA

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/r-spatial/RSAGA.svg?branch=master)](https://travis-ci.org/r-spatial/RSAGA)
[![Lifecycle:
retired](https://img.shields.io/badge/lifecycle-dormant-blue.svg)](https://www.tidyverse.org/lifecycle/#dormant)
<!-- badges: end -->

# Status

RSAGA is no longer under active development and no support is available.
Try [Rsagacmd](https://github.com/stevenpawley/Rsagacmd).

# Introduction

Latest RSAGA version with support for SAGA GIS 2.3 LTS - 7.0.0

Provides access to geocomputing and terrain analysis functions of the
geographical information system (GIS) [‘SAGA’ (System for Automated
Geoscientific Analyses)](http://saga-gis.org/en/index.html) from within
R by running the command line version of SAGA. This package furthermore
provides several R functions for handling ASCII grids, including a
flexible framework for applying local functions (including predict
methods of fitted models) and focal functions to multiple grids. SAGA
GIS is available under GPLv2 / LGPLv2 licence from
<http://sourceforge.net/projects/saga-gis/>.

## Installation

You can install RSAGA 1.3.0 from CRAN with:

``` r
install.packages("RSAGA", dependencies = TRUE)
```

Or you can get the latest version from GitHub with:

``` r
devtools::install_github("r-spatial/RSAGA", dependencies = TRUE)
```
