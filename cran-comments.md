In this version I have:
* fixes HTML validation problems
* add new geom/stat combo

## Test environments
* Windows Server 2022, R-devel, 64 bit
* Windows Server 2022, R-release, 32/64 bit
* Apple Silicon (M1), macOS 11.6 Big Sur, R-release
* macOS 10.13.6 High Sierra, R-release, CRAN's setup
* Fedora Linux, R-devel, clang, gfortran
* Ubuntu Linux 20.04.1 LTS, R-release, GCC


## R CMD check results

0 errors | 0 warnings | 1 note

There was 1 note for platform Windows Server 2022, R-devel, 64 bit
that I could not otherwise reproduce. See also:
https://github.com/r-hub/rhub/issues/503

* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'

## Downstream dependencies

There are no downstream dependencies.
