## Test environments
* local R installation, R 4.0.2
* ubuntu 20.04 (on github actions), release
* ubuntu 20.04 (on github actions), devel
* Windows Server 2019 (on github actions), release
* MacOS 10.15 (on github actions), release
* win-builder (devel)
* mac-builder (release)

## R CMD check results

0 errors v | 0 warnings v | 0 notes v

* This is a resubmission that fixes warnings on CRAN R CMD check for linux systems
* Updated the vignette to ignore memory allocation data from bench::mark() which was returning NAs on CRAN build system and causing build failure.
