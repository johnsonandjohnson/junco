# junco <a href='https://github.com/johnsonandjohnson/junco'><img src="man/figures/logo.png" align="right" width="200"/></a>

<!-- start badges -->
[![Check 🛠](https://github.com/johnsonandjohnson/junco/actions/workflows/inspect.yaml/badge.svg)](https://johnsonandjohnson.github.io/junco/unit-test-report-non-cran/)
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

- Statistical analysis (ANCOVA, MMRM, Cox regression, Kaplan-Meier, CMH stratified proportion difference estimation)
- Calculation of odds ratios, relative risks, and proportion differences
- Event incidence rate analysis
- Support optional SAS (to-nearest-value) and IEC (to-nearest-even) rounding
- Frequency tabulations and summarizations
- Reference-based multiple imputation (RBMI) for handling missing data
- Production-ready RTF and DOCX exporter for listings and tables (see [tt_to_tbldf](https://johnsonandjohnson.github.io/junco/reference/tt_to_tlgrtf.html) and [export_as_docx_j](https://johnsonandjohnson.github.io/junco/reference/export_as_docx_j.html))
- Creation of tables, listings, and graphs (TLGs)

## Installation

Either you can install the stable CRAN version

```r
install.packages("junco") #CRAN Release
```

Or you can install the development version from GitHub:

```r
require('remotes')
remotes::install_github("johnsonandjohnson/junco", ref = "dev") #for the rolling dev release
```

## Usage

To understand how to use this package, please refer to the vignettes (also available on the pkgdown site). You can list them locally with:

```r
browseVignettes(package = "junco")
```

Key vignettes and what they cover:

- Getting started with junco — overview and basic workflow
  - Article: https://johnsonandjohnson.github.io/junco/articles/junco.html
  - What you’ll learn: how to set up data, create common analyses, and produce TLG-ready outputs using core helpers.

- Table and listing customizations — controlling layouts and styles
  - Article: https://johnsonandjohnson.github.io/junco/articles/table_and_listing_customizations.html
  - What you’ll learn: how to customize headers, footers, column formatting, pagination, and how to export tables and listing to RTF and DOCX.

- Auto column widths — making table columns fit content automatically
  - Article: https://johnsonandjohnson.github.io/junco/articles/auto_colwidths.html
  - What you’ll learn: techniques and options for automatically sizing column widths for cleaner RTF/DOCX outputs.

### TLG Catalog — browse examples and outputs

Explore a curated catalog of Tables, Listings, and Graphs (TLGs) generated with `junco` and companion tooling:

- TLG Catalog: https://johnsonandjohnson.github.io/TLG_Catalog

What you’ll find there:

- Realistic TLG outputs to help you preview what specific functions produce.
- Pointers to the underlying function calls and key arguments used to create each artifact.
- Context on when to use each approach (e.g., by analysis type or endpoint).

Tip: Use the catalog alongside the vignettes above and the function reference to quickly identify the right helper for your analysis, 
then adapt the showcased code to your study’s data and conventions.
