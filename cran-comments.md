## R CMD check results

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

0 errors ✖ | 0 warnings ✔ | 1 note ✖

* We've moved 'rbmi' from 'Imports' to 'Suggests' and made the package fully functional without it.

## Test environments

* local Windows install, R 4.5.0
* ubuntu 22.04 (on GitHub Actions), R 4.5.0
* windows-latest (on GitHub Actions), R 4.5.0
* win-builder (devel)

## Downstream dependencies

There are currently no downstream dependencies for this package.

