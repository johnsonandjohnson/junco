# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.5] - Unreleased

### Fixed

- Fixed `s_kaplan_meier()` range censoring indicator handling to no longer produce `NA` values in the output when either all subjects are censored or none are censored.
- Aligned `a_freq_resp_var_j()` with `a_freq_j()` by adding `.formats` and `na_str` parameters to allow user customization of output formatting and NA string representation (#67).
- Fixed the hanging indent in the first column of the body of the table (#138)
- Export `leftside()`, `postfun_eq5d` `ac_blank_line` and `tt_to_tblfile`
- Minor bugfix in DOCX exporter when calculating the number of pages (#188)
- Fixed `tt_to_flextable_j()` to have correct left-indentation in header col 1 (#171) and footer first line starting with newline (#171)
- Fixed relative risk difference derivations for combined column facets in `a_freq_j()`, `a_freq_resp_var_j()` and `a_eair100_j()` (#195)

### Changed

- refactored functions `tt_to_flextable_j()` and `export_as_docx_j()`
- created generic wrapper function `export_TLG_as_docx()`, which now calls `export_as_docx_j()` and `export_graph_as_docx()` (#173)
- Functions `export_as_docx_j()` and `export_graph_as_docx()` still exist but are not internal, i.e. not exported
- updated vignette to explain correctly how to insert newlines in the headers of Tables and Listings (#179)
- "watermark" argument in the docx exporter is now a String instead of a Boolean (#181)
- faster docx unit tests (#197)
- LS means tabulation uses very slightly updated column header (no blanks around the hyphen in "Testing-Reference")
- Bump flextable and remove the skip() units tests (#193)

### Added

- Added option to switch on/off the export of the csv in both `tt_to_tlgrtf()` and `export_as_docx_j()`
- Added option to specify the output folder for the csv
- Added argument 'validate' to `export_TLG_as_docx()` and `tt_to_flextable_j()` (#213)
- added "watermark" argument in the docx exporter for Figures (#181)
- Export helpers for LS means tabulation: `lsmeans_wide_cfun`, `lsmeans_wide_first_split_fun_fct`, `lsmeans_wide_second_split_fun_fct`

## [0.1.4] - 2026-02-05

### Fixed

- "caption" paragraph style in the docx exporter is now handled by flextable (#182)
- Fixed `tt_to_tlgrtf()`, when exporting an empty listing do not lose Title and Footers
- Fixed `tt_to_tlgrtf()` argument `label_width_ins` which was not applying the change in the row label column width (#166).

### Changed

- Reinstate rbmi as dependency

## [0.1.3][0.1.3] - 2026-01-12

### Changed

- Address CRAN NOTES.

## [0.1.2][0.1.2] - 2025-12-10

### Added and Removed

- Add `a_two_tier()` analysis function
- Add `a_maxlev()` to be able to calculate count and percentage of the maximum level of an ordered factor per subject
- Remove `brackets_to_rtf()`
- Export `rbmi_pool()` #22
- Add functionality to `a_freq_j()` to process `val = NULL` from levels specified in `label_map`, as well as the modified version `a_freq_j_with_exclude()` that allows to exclude the analysis from specified row splits. #95
- Add `a_cmhrms_j()` to support p-value from CMH row mean score test, as well as the modified version `a_cmhrms_j_with_exclude()` that allows to exclude the analysis from specified row splits. #97
- Added standard error (SE) column for each treatment arm's least square (LS) means estimate to the `summarize_lsmeans_wide()` layout.
- Added the internal function `do_exclude_split()` to facilitate the exclusion of specified row splits from analysis functions.
- Remove some unused functions (jj_uc_map, postfun_cog, column_N, non_blank_sentinel, null_fn, unicodify
- Add extra check for existence of `.alt_df_full` when layout has risk difference column and a row-split (h_create_alt_df) #120.
- Add docx exporter for Tables, Listings and Figures.
- Add `alignments` argument in `tt_to_tlgrtf()`
- Removed `s_test_proportion_diff()` and corresponding helper functions, as they are now available as needed in the `tern` package.
- Added `cmh_sato` and `cmh_mn` (Cochran-Mantel-Haenszel stratified proportion difference estimation with Sato variance and Miettinen Nurminen method, respectively) `method` options to the `s_proportion_diff_j()` function.
- Added formatting function round type (#76)

* Fixed bug in s_summarize_desc_j function to handle sparse data (zero variance) cases (#78)

## [0.1.1][0.1.1] - 2025-07-28

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
