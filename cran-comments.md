## Release summary

This is a feature release (0.2.0) with four new geometries/stats,
breaking changes (datasets removed), and raised minimums of
R >= 4.2.0 and ggplot2 >= 4.0.0.

## Breaking changes

* The bundled datasets `co2_ml`, `covid_vac`, and `female_leaders` have been
  removed. The `vignette("examples")` that relied on them has also been removed.

## R CMD check results

0 errors | 0 warnings | 1 note

* checking for future file timestamps ... NOTE
  unable to verify current time
  (Network access is restricted on this machine; not a package issue.)

The local check also produced two warnings caused by missing system tools
(qpdf, inconsolata.sty) that are not present on this machine but are standard
on CRAN infrastructure. The package was additionally checked on win-builder
and macOS builder with 0 errors, 0 warnings, 0 notes.

## Test environments

* local Ubuntu 24.04, R 4.4.x
* win-builder (R devel, R release)
* macOS builder (R release)

## Reverse dependencies

There are no reverse dependencies.
