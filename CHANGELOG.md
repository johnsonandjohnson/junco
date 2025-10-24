# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.2] - 2025-08-20

### Added
- Add `a_maxlev()` to be able calculate count and percentage of the maximum level of an ordered factor per subject
- fix bug for not selecting NA records in `h_subset_combo()`
- Remove `brackets_to_rtf()`
- Update pruning_functions.R
- update `string_to_title()` to handle factors (#26)
- Export `rbmi_pool()` #22

### Changed
- Remove some unused functions (jj_uc_map, postfun_cog, postfun_eq5d, column_N, non_blank_sentinel, null_fn, unicodify
- Replace {pharmaverseadam} with {pharmaverseadamjnj}

## [0.1.1] - 2025-07-28

### Added
- First release of the junco package for Table and Listing (TL) Reporting
- Functions to produce tables and listings in R
- Built on the rtables package framework
- Integration with formatters, rtables, tidytlg, and tern packages

### Changed
- Initial CRAN release

[0.1.2]: https://github.com/johnsonandjohnson/junco/releases/tag/v0.1.2
[0.1.1]: https://github.com/johnsonandjohnson/junco/releases/tag/v0.1.1
