## Test environments

* local x86_64-w64-mingw32 install, R-4.2.1
* r-hub Debian, R-devel
* win-builder, R-devel

## R CMD check results

0 errors | 0 warnings | 1 note

## Note

The win-builder NOTE concerns possibly misspelled words in the DESCRIPTION; these are false positives.

On Fedora (and only on Fedora), there's also a note concerning the referencing of package `sp` in an unspecified Rd file. Adding `sp` to the Suggests did not solve this. I am also unaware of any possible references to `sp` in the help files. I therefore believe that this is a false positive.
