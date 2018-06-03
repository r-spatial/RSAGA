## Test environments
* local OS X install, R-release
* r-hub Debian, R-devel
* r-hub Debian, R-release
* r-hub Fedora, R-devel
* win-builder, R-devel
* win-builder, R-release

## R CMD check results

0 errors | 0 warnings | 0 note

## Note
We got an email about CRAN Warning:

checking for unstated dependencies in ‘tests’ ... WARNING
'library' or 'require' call not declared from: ‘digest’

Unfortunately we could not reproduce this warning on r-hub or locally. 

Tried to fix this in 1.1.1 by adding 'digest' to 'Suggest' in DESCRIPTION
---
