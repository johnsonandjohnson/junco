# junco

<!-- start badges -->
[![Check 🛠](https://github.com/johnsonandjohnson/junco/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/johnsonandjohnson/junco/actions/workflows/R-CMD-check.yaml)
[![Docs 📚](https://github.com/johnsonandjohnson/junco/actions/workflows/pkgdown.yaml/badge.svg)](https://johnsonandjohnson.github.io/junco/)
[![Code Coverage 📔](https://raw.githubusercontent.com/johnsonandjohnson/junco/refs/heads/gh-pages/_xml_coverage_reports/badge.svg)](https://johnsonandjohnson.github.io/junco/_xml_coverage_reports/coverage.html)

![GitHub forks](https://img.shields.io/github/forks/johnsonandjohnson/junco?style=social)
![GitHub repo stars](https://img.shields.io/github/stars/johnsonandjohnson/junco?style=social)

![GitHub commit activity](https://img.shields.io/github/commit-activity/m/johnsonandjohnson/junco)
![GitHub contributors](https://img.shields.io/github/contributors/johnsonandjohnson/junco)
![GitHub last commit](https://img.shields.io/github/last-commit/johnsonandjohnson/junco)
![GitHub pull requests](https://img.shields.io/github/issues-pr/johnsonandjohnson/junco)
![GitHub repo size](https://img.shields.io/github/repo-size/johnsonandjohnson/junco)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Current Version](https://img.shields.io/github/r-package/v/johnsonandjohnson/junco/main?color=purple&label=package%20version)](https://github.com/johnsonandjohnson/junco/tree/main)
[![Open Issues](https://img.shields.io/github/issues-raw/johnsonandjohnson/junco?color=red&label=open%20issues)](https://github.com/johnsonandjohnson/junco/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc)
<!-- end badges -->



## Overview

The junco R package contains analysis functions to create tables and listings used for clinical trial reporting.
It complements the tern package by providing additional statistical analysis capabilities.

The package provides a range of functionality, such as:

- Statistical analysis (ANCOVA, MMRM, Cox regression, Kaplan-Meier)
- Calculation of odds ratios, relative risks, and proportion differences
- Event incidence rate analysis
- Frequency tabulations and summarizations
- Reference-based multiple imputation (RBMI) for handling missing data
- Production-ready RTF exporter for listings and tables (see [tt_to_tbldf](https://johnsonandjohnson.github.io/junco/reference/tt_to_tlgrtf.html))
- Creation of tables, listings, and graphs (TLGs)

## Installation

You can install the development version of junco from [GitHub](https://github.com/johnsonandjohnson/junco) with:

```r
# install.packages("remotes")
remotes::install_github("johnsonandjohnson/junco")
```

## Usage

To understand how to use this package, please refer to the [junco article](https://johnsonandjohnson.github.io/junco/articles/junco.html), which provides multiple examples of code implementation.

See package vignettes `browseVignettes(package = "junco")` for usage of this package.
