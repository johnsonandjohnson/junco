# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.4] - Unreleased

### Fixed
- Fixed `s_kaplan_meier()` range censoring indicator handling to no longer produce `NA` values in the output when either all subjects are censored or none are censored.

## [0.1.3] - 2026-01-12

### Changed
- Address CRAN NOTES.


## [0.1.2] - 2025-12-10

### Added and Removed
- Add `a_two_tier()` analysis function
- Add `a_maxlev()` to be able to calculate count and percentage of the maximum level of an ordered factor per subject
- Remove `brackets_to_rtf()`
- Export `rbmi_pool()` #22
- Add functionality to `a_freq_j()` to process `val = NULL` from levels specified in `label_map`, as well as the modified version `a_freq_j_with_exclude()` that allows to exclude the analysis from specified row splits. #95
- Add `a_cmhrms_j()` to support p-value from CMH row mean score test, as well as the modified version `a_cmhrms_j_with_exclude()` that allows to exclude the analysis from specified row splits. #97
- Added standard error (SE) column for each treatment arm's least square (LS) means estimate to the `summarize_lsmeans_wide()` layout.
- Added the internal function `do_exclude_split()` to facilitate the exclusion of specified row splits from analysis functions.
- Remove some unused functions (jj_uc_map, postfun_cog, postfun_eq5d, column_N, non_blank_sentinel, null_fn, unicodify
- Add extra check for existence of `.alt_df_full` when layout has risk difference column and a row-split (h_create_alt_df) #120.
- Add docx exporter for Tables, Listings and Figures.
- Add `alignments` argument in `tt_to_tlgrtf()`
- Removed `s_test_proportion_diff()` and corresponding helper functions, as they are now available as needed in the `tern` package.
- Added `cmh_sato` and `cmh_mn` (Cochran-Mantel-Haenszel stratified proportion difference estimation with Sato variance and Miettinen Nurminen method, respectively) `method` options to the `s_proportion_diff_j()` function.
- Added formatting function round type (#76)

### Changed
- Replace {pharmaverseadam} with {pharmaverseadamjnj}
- Update pruning_functions.R
- Update `string_to_title()` to handle factors (#26)
- Moved rbmi to suggest
- Replaced `denom_df` with `.alt_df_full` in `a_maxlev()`.

### Fixed
- Fixes #102 bug inappropriate warnings from `cond_rm_facets` function
- Fix bug for not selecting NA records in `h_subset_combo()`
- Consistent `tt_to_tbldf()` function behavior with invalid structures #116


## [0.1.1] - 2025-07-28

### Added
- First release of the junco package for Table and Listing (TL) Reporting
- Functions to produce tables and listings in R
- Built on the rtables package framework
- Integration with formatters, rtables, tidytlg, and tern packages

### Changed
- Initial CRAN release

[0.1.3]: https://github.com/johnsonandjohnson/junco/releases/tag/v0.1.3
[0.1.2]: https://github.com/johnsonandjohnson/junco/releases/tag/v0.1.2
[0.1.1]: https://github.com/johnsonandjohnson/junco/releases/tag/v0.1.1
