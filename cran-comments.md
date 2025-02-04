## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* The NOTE is due to worldclockapi not being available: 'unable to verify current time'
* The `attachment::att_amend_desc()` call removes `tibble` from `Suggests`, but then `devtools::check(args = c("--no-manual", "--as-cran"))` fails with an error because `sf` uses `tibble` in the package's test code. It therefore seems necessary to keep `tibble` in the `Suggests` list. This is what I did, and so the CRAN checks run without an error.
