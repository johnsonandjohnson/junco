# junco 0.1.6.9000

### Fixed
- Fixed `get_ref_info()` to accept ref_path = NULL (#359).
- Fixed `junco_get_stats()` to inherit any default stats from `tern` that are not explicitly defined in junco.
- Fixed `get_ref_info()` so that is works in the presence of "overall" column (#332)
- CRITICAL: hotfixed `tt_to_tlgrtf()` lost titles in certain cases (#373)

### Changed
- Added a default value for the `label` argument in `c_summary_subset_label()`.
- Updated the documentation of `a_summary_subset()`.
- Refactored `prepend_label_cell()`; Only `RowsVerticalSection` is now supported.
- Removed `filter_df_prior_afun()`, `a_summary_diff_mvars()`, `a_summary_diff_mvars_label()`.
- Removed formatters exports #317
- Deprecate `a_coxph_hr` for `tern:::a_coxph_pairwise()` and `s_coxph_hr` for `tern:::s_coxph_pairwise()` #158
- Reduce sampling of `rbmi` test to make tests shorter (#323)
- Optimize shared tables in `test-tt_to_tblfile` (#323)
- Changed forked `h_ancova` for the `tern` one
- Add extra statistics to `a_eair100_j` and introduce scaling factor `num_p_year` (default = 100) (#361)
- Removed ellipsis argument from `a_freq_resp_var_j` (#236) 
- Removed `dplyr` from `junco` and replaced by base R #201
- Replaced `assertthat` by `checkmate` for consistency #201
- Remove `stringi` from dependencies #201
- Deprecate `rbmi_analyse()`, `make_rbmi_cluster()`, `par_lapply()` for `rbmi` equivalent functions #367
- Deprecate `a_kaplan_meier()` for `tern::a_surv_time()`
- Changed the label for `range_with_cens_info` from `"Min, max"` (junco) to `"Min - Max (with censoring)"` and the argument from `lsmean_diffci` to `lsmean_diff_with_ci`
- Deprecate `s_coxph_hr()` for `tern:::a_coxph_pairwise()`
- Changed stop message in `a_freq_j()` when `label_map` option is used in a rowsplit with no data on a character analysis var #386



### Added
- Updated documentation and examples for `label_map` in `a_freq_j` (#235)

## [0.1.6] - 2026-05-05 (CRAN release)


### Fixed

- Fixed `s_kaplan_meier()` range censoring indicator handling to no longer produce `NA` values in the output when either all subjects are censored or none are censored.
- Aligned `a_freq_resp_var_j()` with `a_freq_j()` by adding `.formats` and `na_str` parameters to allow user customization of output formatting and NA string representation (#67).
- Fixed the hanging indent in the first column of the body of the table (#138)
- Export `leftside()`, `postfun_eq5d` `ac_blank_line` and `tt_to_tblfile`
- Minor bugfix in DOCX exporter when calculating the number of pages (#188)
- Fixed `tt_to_flextable_j()` to have correct left-indentation in header col 1 (#171) and footer first line starting with newline (#171)
- Fixed relative risk difference derivations for combined column facets in `a_freq_j()`, `a_freq_resp_var_j()` and `a_eair100_j()` (#195)
- Fixed calculation of 'reduce_first_col_indentation' and section break positions in `insert_keepNext_vertical_pagination()` (#218)
- Fixed the little gaps in neighboring spanning headers in the docx exporter (#216)
- Fixed blank pages happening at the end of the docx (#177)
- DOCX exporter fixed watermark location in case of listings
- DOCX exporter updated table border width from 0.75 to 0.875 inches
- DOCX exporter when having vertical pagination in tables or listings, fixed the rows misalignment from page 2 and below compared to RTF
- Fixed bug in `s_summarize_desc_j()` when applied to almost constant data due to behavior from `t.test.default()` (#257)
- `tt_to_tlgrtf()` now is passing vectorized colwidths when exporting 'allparts' to match the colwidths of the individual parts (#225)

### Changed

- Updated `c_summary_subset_label()`: removed `.spl_context` argument and renamed `subset_expr` to `filter_expr`.
- Renamed `a_summary_j()` to `a_summary_subset()` and updated its purpose, implementation, and arguments.
- Added new stats/labels/formats/indents to `junco_utils_default_stats_formats_labels.r` file.
- Renamed `s_diff_mean_ci()` to `s_diff_means()`, and added new statistics to `s_diff_means()`.
- Moved `safe_t_test()` from `a_summarize_aval_chg_diff.R` to a new file `safe_t_test.R`,
- Moved `add_blank_line_rcells()` from `s_functions.R` to `blank_line.R`.
- changed return value `n` for `s_ancova_j` into `n_fit`, to differentiate between these two statistics for combined function `s_summarize_ancova_j()` (#117)
- added default label for median_range "Median (min, max)" in `junco_default_labels_start`
- changed label from "experimental" to "stable" for `default_stats_formats_labels`
- refactored functions `tt_to_flextable_j()` and `export_as_docx_j()`
- created generic wrapper function `export_TLG_as_docx()`, which now calls `export_as_docx_j()` and `export_graph_as_docx()` (#173)
- Functions `export_as_docx_j()` and `export_graph_as_docx()` still exist but are now internal, i.e. not exported
- updated vignette to explain correctly how to insert newlines in the headers of Tables and Listings (#179)
- "watermark" argument in the docx exporter is now a String instead of a Boolean (#181)
- faster docx unit tests (#197)
- LS means tabulation uses very slightly updated column header (no blanks around the hyphen in "Testing-Reference")
- Bump flextable and remove the skip() units tests (#193)
- Update colwidths tests to derive values
- Fix old TODOs in tests
- docx exporter added missing defaults (#186)
- Changed all snapshot tests to `cran = TRUE`
- Footnotes in docx and RTF outputs are now 1 table row/cell (#285)
- removed label "experimental" for `theme_docx_default_j`, `tt_to_flextable_j` and `export_TLG_as_docx`

### Added

- Added `a_diff_means()`, which is based on the existing `s_diff_means()`.
- Added multi-comparator functionality (#271)
- Hotfix: Added several new functions for creating the Vital Sign tables for core and clinpharm:
  `a_summary_diff_mvars()`, `a_summary_j()`, `c_summary_subset_label()`, `filter_df_prior_afun()`,
  `prepend_label_cell()`, `s_diff_mean_ci()`, `s_summary_diff()`, `safe_t_test()` (#304).
- Added option to perform `a_summarize_ancova_j` in a layout with a combined column facet (#117) + slight change in return value for `s_ancova_j()`, now we have n_fit instead of n returned
- Added option to switch on/off the export of the csv in both `tt_to_tlgrtf()` and `export_as_docx_j()`
- Added option to specify the output folder for the csv
- Added argument 'validate' to `export_TLG_as_docx()` and `tt_to_flextable_j()` (#213)
- Added "watermark" argument in the docx exporter for Figures (#181)
- Added argument `mult_adj_emmeans` in `fit_mmrm_j()` to enable (single-step or step-down) Dunnett multiplicity adjustment for LS means contrasts (p-values and confidence intervals) for more than one experimental arm within visits.
- Added option for subgroup variable in the `vars` argument of `fit_mmrm_j()` to enable fitting an overall MMRM with subgroup interaction terms.
- Export helpers for LS means tabulation: `lsmeans_wide_cfun`, `lsmeans_wide_first_split_fun_fct`, `lsmeans_wide_second_split_fun_fct`
- Added `lifecycle` to suggests

## [0.1.5] - 2026-03-03 (minor hotfix release)

### Added and Removed
- Added new behavior for nested `countsource` in h_a_freq_dataprep `altdf_subset` (#200)


## [0.1.4] - 2026-02-02 (minor hotfix release)

### Fixed

- "caption" paragraph style in the docx exporter is now handled by flextable (#182)
- Fixed `tt_to_tlgrtf()`, when exporting an empty listing do not lose Title and Footers
- Fixed `tt_to_tlgrtf()` argument `label_width_ins` which was not applying the change in the row label column width (#166).

### Changed

- Reinstate rbmi as dependency


## [0.1.3] - 2026-01-12 (minor hotfix release)

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
- Remove some unused functions (jj_uc_map, postfun_cog, column_N, non_blank_sentinel, null_fn, unicodify
- Add extra check for existence of `.alt_df_full` when layout has risk difference column and a row-split (h_create_alt_df) #120.
- Add docx exporter for Tables, Listings and Figures.
- Add `alignments` argument in `tt_to_tlgrtf()`
- Removed `s_test_proportion_diff()` and corresponding helper functions, as they are now available as needed in the `tern` package.
- Added `cmh_sato` and `cmh_mn` (Cochran-Mantel-Haenszel stratified proportion difference estimation with Sato variance and Miettinen Nurminen method, respectively) `method` options to the `s_proportion_diff_j()` function.
- Added formatting function round type (#76)

* Fixed bug in s_summarize_desc_j function to handle sparse data (zero variance) cases (#78)

## [0.1.1] - 2025-07-28

### Added

- First release of the junco package for Table and Listing (TL) Reporting
- Functions to produce tables and listings in R
- Built on the rtables package framework
- Integration with formatters, rtables, tidytlg, and tern packages

### Changed

- Initial CRAN release


## Changelog

[0.1.6]: https://github.com/johnsonandjohnson/junco/releases/tag/v0.1.6-rc
[0.1.5]: https://github.com/johnsonandjohnson/junco/releases/tag/v-0.1.5
[0.1.4]: https://github.com/johnsonandjohnson/junco/releases/tag/0.1.4
[0.1.3]: https://github.com/johnsonandjohnson/junco/releases/tag/v0.1.3
[0.1.2]: https://github.com/johnsonandjohnson/junco/releases/tag/v0.1.2-rc1
[0.1.1]: https://github.com/johnsonandjohnson/junco/releases/tag/v0.1.1

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).
