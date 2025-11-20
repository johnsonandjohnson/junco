## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local Windows install, R 4.4.0
* ubuntu 22.04 (on GitHub Actions), R 4.4.0
* win-builder (devel)

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Changes in this version

* Moved 'rbmi' package from 'Imports' to 'Suggests' to reduce dependencies
* Added conditional checks in all functions and tests that use 'rbmi'
* Package can now be installed and used without 'rbmi' being available
* Functions that depend on 'rbmi' will provide informative error messages if 'rbmi' is not installed
