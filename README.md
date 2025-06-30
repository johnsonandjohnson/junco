# junco

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

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


