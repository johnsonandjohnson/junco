## R CMD check results

❯ checking package dependencies ... ERROR
  Package suggested but not available: 'rbmi'
  
  The suggested packages are required for a complete check.
  Checking can be attempted without them by setting the environment
  variable _R_CHECK_FORCE_SUGGESTS_ to a false value.
  
  See section 'The DESCRIPTION file' in the 'Writing R Extensions'
  manual.

❯ checking CRAN incoming feasibility ... [35s] NOTE
  Maintainer: 'Gabriel Becker <gabembecker@gmail.com>'
  
  New submission
  
  Package was archived on CRAN
  
  CRAN repository db overrides:
    X-CRAN-Comment: Archived on 2025-11-19 as requires archived package
      'rbmi'.
  
  Suggests or Enhances not in mainstream repositories:
    rbmi
  Availability using Additional_repositories specification:
    rbmi   yes   https://insightsengineering.r-universe.dev/

1 error ✖ | 0 warnings ✔ | 1 note ✖

* We've moved 'rbmi' to Suggests and the package now works without it.

## Test environments

* local Windows install, R 4.5.0
* ubuntu 22.04 (on GitHub Actions), R 4.5.0
* win-builder (devel)

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Changes in this version

* Moved 'rbmi' from 'Imports' to 'Suggests' and made package fully functional without it
