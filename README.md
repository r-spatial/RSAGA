
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RSAGA

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/r-spatial/RSAGA.svg?branch=master)](https://app.travis-ci.com/r-spatial/RSAGA)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

# Introduction

Latest RSAGA version with support for SAGA GIS 2.3 LTS - 9.7.2

Provides access to geocomputing and terrain analysis functions of the
geographical information system (GIS) [SAGA (System for Automated
Geoscientific Analyses)](https://saga-gis.sourceforge.io/en/index.html) from within
R by running the command line version of SAGA. This package furthermore
provides several R functions for handling ASCII grids, including a
flexible framework for applying local functions (including predict
methods of fitted models) and focal functions to multiple grids. SAGA
GIS is available under GPLv2 / LGPLv2 licence from
<https://sourceforge.net/projects/saga-gis/>.

## Installation

You can install RSAGA from CRAN with:

``` r
install.packages("RSAGA", dependencies = TRUE)
```

Or you can get the latest version from GitHub with:

``` r
devtools::install_github("r-spatial/RSAGA", dependencies = TRUE)
```
