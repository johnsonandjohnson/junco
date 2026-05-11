# Changelog

## junco 0.1.6

CRAN release: 2026-05-10

#### Fixed

- Fixed
  [`s_kaplan_meier()`](https://johnsonandjohnson.github.io/junco/reference/kaplan_meier.md)
  range censoring indicator handling to no longer produce `NA` values in
  the output when either all subjects are censored or none are censored.
- Aligned
  [`a_freq_resp_var_j()`](https://johnsonandjohnson.github.io/junco/reference/a_freq_resp_var_j.md)
  with
  [`a_freq_j()`](https://johnsonandjohnson.github.io/junco/reference/a_freq_j.md)
  by adding `.formats` and `na_str` parameters to allow user
  customization of output formatting and NA string representation
  ([\#67](https://github.com/johnsonandjohnson/junco/issues/67)).
- Fixed the hanging indent in the first column of the body of the table
  ([\#138](https://github.com/johnsonandjohnson/junco/issues/138))
- Export
  [`leftside()`](https://johnsonandjohnson.github.io/junco/reference/leftside.md),
  `postfun_eq5d` `ac_blank_line` and `tt_to_tblfile`
- Minor bugfix in DOCX exporter when calculating the number of pages
  ([\#188](https://github.com/johnsonandjohnson/junco/issues/188))
- Fixed
  [`tt_to_flextable_j()`](https://johnsonandjohnson.github.io/junco/reference/tt_to_flextable_j.md)
  to have correct left-indentation in header col 1
  ([\#171](https://github.com/johnsonandjohnson/junco/issues/171)) and
  footer first line starting with newline
  ([\#171](https://github.com/johnsonandjohnson/junco/issues/171))
- Fixed relative risk difference derivations for combined column facets
  in
  [`a_freq_j()`](https://johnsonandjohnson.github.io/junco/reference/a_freq_j.md),
  [`a_freq_resp_var_j()`](https://johnsonandjohnson.github.io/junco/reference/a_freq_resp_var_j.md)
  and
  [`a_eair100_j()`](https://johnsonandjohnson.github.io/junco/reference/a_eair100_j.md)
  ([\#195](https://github.com/johnsonandjohnson/junco/issues/195))
- Fixed calculation of ‘reduce_first_col_indentation’ and section break
  positions in `insert_keepNext_vertical_pagination()`
  ([\#218](https://github.com/johnsonandjohnson/junco/issues/218))
- Fixed the little gaps in neighboring spanning headers in the docx
  exporter
  ([\#216](https://github.com/johnsonandjohnson/junco/issues/216))
- Fixed blank pages happening at the end of the docx
  ([\#177](https://github.com/johnsonandjohnson/junco/issues/177))
- DOCX exporter fixed watermark location in case of listings
- DOCX exporter updated table border width from 0.75 to 0.875 inches
- DOCX exporter when having vertical pagination in tables or listings,
  fixed the rows misalignment from page 2 and below compared to RTF
- Fixed bug in `s_summarize_desc_j()` when applied to almost constant
  data due to behavior from `t.test.default()`
  ([\#257](https://github.com/johnsonandjohnson/junco/issues/257))
- [`tt_to_tlgrtf()`](https://johnsonandjohnson.github.io/junco/reference/tt_to_tlgrtf.md)
  now is passing vectorized colwidths when exporting ‘allparts’ to match
  the colwidths of the individual parts
  ([\#225](https://github.com/johnsonandjohnson/junco/issues/225))

#### Changed

- Moved
  [`safe_t_test()`](https://johnsonandjohnson.github.io/junco/reference/safe_t_test.md)
  from `a_summarize_aval_chg_diff.R` to a new file `safe_t_test.R`,
- Moved `add_blank_line_rcells()` from `s_functions.R` to
  `blank_line.R`.
- changed return value `n` for `s_ancova_j` into `n_fit`, to
  differentiate between these two statistics for combined function
  [`s_summarize_ancova_j()`](https://johnsonandjohnson.github.io/junco/reference/s_summarize_ancova_j.md)
  ([\#117](https://github.com/johnsonandjohnson/junco/issues/117))
- added default label for median_range “Median (min, max)” in
  `junco_default_labels_start`
- changed label from “experimental” to “stable” for
  `default_stats_formats_labels`
- refactored functions
  [`tt_to_flextable_j()`](https://johnsonandjohnson.github.io/junco/reference/tt_to_flextable_j.md)
  and
  [`export_as_docx_j()`](https://johnsonandjohnson.github.io/junco/reference/export_as_docx_j.md)
- created generic wrapper function
  [`export_TLG_as_docx()`](https://johnsonandjohnson.github.io/junco/reference/export_TLG_as_docx.md),
  which now calls
  [`export_as_docx_j()`](https://johnsonandjohnson.github.io/junco/reference/export_as_docx_j.md)
  and
  [`export_graph_as_docx()`](https://johnsonandjohnson.github.io/junco/reference/export_graph_as_docx.md)
  ([\#173](https://github.com/johnsonandjohnson/junco/issues/173))
- Functions
  [`export_as_docx_j()`](https://johnsonandjohnson.github.io/junco/reference/export_as_docx_j.md)
  and
  [`export_graph_as_docx()`](https://johnsonandjohnson.github.io/junco/reference/export_graph_as_docx.md)
  still exist but are now internal, i.e. not exported
- updated vignette to explain correctly how to insert newlines in the
  headers of Tables and Listings
  ([\#179](https://github.com/johnsonandjohnson/junco/issues/179))
- “watermark” argument in the docx exporter is now a String instead of a
  Boolean
  ([\#181](https://github.com/johnsonandjohnson/junco/issues/181))
- faster docx unit tests
  ([\#197](https://github.com/johnsonandjohnson/junco/issues/197))
- LS means tabulation uses very slightly updated column header (no
  blanks around the hyphen in “Testing-Reference”)
- Bump flextable and remove the skip() units tests
  ([\#193](https://github.com/johnsonandjohnson/junco/issues/193))
- Update colwidths tests to derive values
- Fix old TODOs in tests
- docx exporter added missing defaults
  ([\#186](https://github.com/johnsonandjohnson/junco/issues/186))
- Changed all snapshot tests to `cran = TRUE`
- Footnotes in docx and RTF outputs are now 1 table row/cell
  ([\#285](https://github.com/johnsonandjohnson/junco/issues/285))
- removed label “experimental” for `theme_docx_default_j`,
  `tt_to_flextable_j` and `export_TLG_as_docx`

#### Added

- Added multi-comparator functionality
  ([\#271](https://github.com/johnsonandjohnson/junco/issues/271))
- Hotfix: Added several new functions for creating the Vital Sign tables
  for core and clinpharm:
  [`a_summary_diff_mvars()`](https://johnsonandjohnson.github.io/junco/reference/a_summary_diff_mvars.md),
  [`a_summary_j()`](https://johnsonandjohnson.github.io/junco/reference/a_summary_j.md),
  [`c_summary_subset_label()`](https://johnsonandjohnson.github.io/junco/reference/c_summary_subset_label.md),
  [`filter_df_prior_afun()`](https://johnsonandjohnson.github.io/junco/reference/filter_df_prior_afun.md),
  [`prepend_label_cell()`](https://johnsonandjohnson.github.io/junco/reference/prepend_label_cell.md),
  [`s_diff_mean_ci()`](https://johnsonandjohnson.github.io/junco/reference/s_diff_mean_ci.md),
  [`s_summary_diff()`](https://johnsonandjohnson.github.io/junco/reference/s_summary_diff.md),
  [`safe_t_test()`](https://johnsonandjohnson.github.io/junco/reference/safe_t_test.md)
  ([\#304](https://github.com/johnsonandjohnson/junco/issues/304)).
- Added option to perform `a_summarize_ancova_j` in a layout with a
  combined column facet
  ([\#117](https://github.com/johnsonandjohnson/junco/issues/117)) +
  slight change in return value for
  [`s_ancova_j()`](https://johnsonandjohnson.github.io/junco/reference/s_ancova_j.md),
  now we have n_fit instead of n returned
- Added option to switch on/off the export of the csv in both
  [`tt_to_tlgrtf()`](https://johnsonandjohnson.github.io/junco/reference/tt_to_tlgrtf.md)
  and
  [`export_as_docx_j()`](https://johnsonandjohnson.github.io/junco/reference/export_as_docx_j.md)
- Added option to specify the output folder for the csv
- Added argument ‘validate’ to
  [`export_TLG_as_docx()`](https://johnsonandjohnson.github.io/junco/reference/export_TLG_as_docx.md)
  and
  [`tt_to_flextable_j()`](https://johnsonandjohnson.github.io/junco/reference/tt_to_flextable_j.md)
  ([\#213](https://github.com/johnsonandjohnson/junco/issues/213))
- Added “watermark” argument in the docx exporter for Figures
  ([\#181](https://github.com/johnsonandjohnson/junco/issues/181))
- Added argument `mult_adj_emmeans` in
  [`fit_mmrm_j()`](https://johnsonandjohnson.github.io/junco/reference/fit_mmrm_j.md)
  to enable (single-step or step-down) Dunnett multiplicity adjustment
  for LS means contrasts (p-values and confidence intervals) for more
  than one experimental arm within visits.
- Added option for subgroup variable in the `vars` argument of
  [`fit_mmrm_j()`](https://johnsonandjohnson.github.io/junco/reference/fit_mmrm_j.md)
  to enable fitting an overall MMRM with subgroup interaction terms.
- Export helpers for LS means tabulation: `lsmeans_wide_cfun`,
  `lsmeans_wide_first_split_fun_fct`,
  `lsmeans_wide_second_split_fun_fct`
- Added `lifecycle` to suggests

### [0.1.5](https://github.com/johnsonandjohnson/junco/releases/tag/v-0.1.5) - 2026-03-03 (minor hotfix release)

#### Added and Removed

- Added new behavior for nested `countsource` in h_a_freq_dataprep
  `altdf_subset`
  ([\#200](https://github.com/johnsonandjohnson/junco/issues/200))

### [0.1.4](https://github.com/johnsonandjohnson/junco/releases/tag/0.1.4) - 2026-02-02 (minor hotfix release)

#### Fixed

- “caption” paragraph style in the docx exporter is now handled by
  flextable
  ([\#182](https://github.com/johnsonandjohnson/junco/issues/182))
- Fixed
  [`tt_to_tlgrtf()`](https://johnsonandjohnson.github.io/junco/reference/tt_to_tlgrtf.md),
  when exporting an empty listing do not lose Title and Footers
- Fixed
  [`tt_to_tlgrtf()`](https://johnsonandjohnson.github.io/junco/reference/tt_to_tlgrtf.md)
  argument `label_width_ins` which was not applying the change in the
  row label column width
  ([\#166](https://github.com/johnsonandjohnson/junco/issues/166)).

#### Changed

- Reinstate rbmi as dependency

### [0.1.3](https://github.com/johnsonandjohnson/junco/releases/tag/v0.1.3) - 2026-01-12 (minor hotfix release)

#### Changed

- Address CRAN NOTES.

### [0.1.2](https://github.com/johnsonandjohnson/junco/releases/tag/v0.1.2-rc1) - 2025-12-10

#### Added and Removed

- Add
  [`a_two_tier()`](https://johnsonandjohnson.github.io/junco/reference/a_two_tier.md)
  analysis function

- Add
  [`a_maxlev()`](https://johnsonandjohnson.github.io/junco/reference/a_maxlev.md)
  to be able to calculate count and percentage of the maximum level of
  an ordered factor per subject

- Remove `brackets_to_rtf()`

- Export
  [`rbmi_pool()`](https://johnsonandjohnson.github.io/junco/reference/rbmi_pool.md)
  [\#22](https://github.com/johnsonandjohnson/junco/issues/22)

- Add functionality to
  [`a_freq_j()`](https://johnsonandjohnson.github.io/junco/reference/a_freq_j.md)
  to process `val = NULL` from levels specified in `label_map`, as well
  as the modified version
  [`a_freq_j_with_exclude()`](https://johnsonandjohnson.github.io/junco/reference/a_freq_j.md)
  that allows to exclude the analysis from specified row splits.
  [\#95](https://github.com/johnsonandjohnson/junco/issues/95)

- Add
  [`a_cmhrms_j()`](https://johnsonandjohnson.github.io/junco/reference/cmhrms.md)
  to support p-value from CMH row mean score test, as well as the
  modified version
  [`a_cmhrms_j_with_exclude()`](https://johnsonandjohnson.github.io/junco/reference/cmhrms.md)
  that allows to exclude the analysis from specified row splits.
  [\#97](https://github.com/johnsonandjohnson/junco/issues/97)

- Added standard error (SE) column for each treatment arm’s least square
  (LS) means estimate to the
  [`summarize_lsmeans_wide()`](https://johnsonandjohnson.github.io/junco/reference/summarize_lsmeans_wide.md)
  layout.

- Added the internal function
  [`do_exclude_split()`](https://johnsonandjohnson.github.io/junco/reference/do_exclude_split.md)
  to facilitate the exclusion of specified row splits from analysis
  functions.

- Remove some unused functions (jj_uc_map, postfun_cog, column_N,
  non_blank_sentinel, null_fn, unicodify

- Add extra check for existence of `.alt_df_full` when layout has risk
  difference column and a row-split (h_create_alt_df)
  [\#120](https://github.com/johnsonandjohnson/junco/issues/120).

- Add docx exporter for Tables, Listings and Figures.

- Add `alignments` argument in
  [`tt_to_tlgrtf()`](https://johnsonandjohnson.github.io/junco/reference/tt_to_tlgrtf.md)

- Removed
  [`s_test_proportion_diff()`](https://insightsengineering.github.io/tern/latest-tag/reference/prop_diff_test.html)
  and corresponding helper functions, as they are now available as
  needed in the `tern` package.

- Added `cmh_sato` and `cmh_mn` (Cochran-Mantel-Haenszel stratified
  proportion difference estimation with Sato variance and Miettinen
  Nurminen method, respectively) `method` options to the
  [`s_proportion_diff_j()`](https://johnsonandjohnson.github.io/junco/reference/prop_diff.md)
  function.

- Added formatting function round type
  ([\#76](https://github.com/johnsonandjohnson/junco/issues/76))

- Fixed bug in s_summarize_desc_j function to handle sparse data (zero
  variance) cases
  ([\#78](https://github.com/johnsonandjohnson/junco/issues/78))

### [0.1.1](https://github.com/johnsonandjohnson/junco/releases/tag/v0.1.1) - 2025-07-28

#### Added

- First release of the junco package for Table and Listing (TL)
  Reporting
- Functions to produce tables and listings in R
- Built on the rtables package framework
- Integration with formatters, rtables, tidytlg, and tern packages

#### Changed

- Initial CRAN release

All notable changes to this project will be documented in this file.

The format is based on [Keep a
Changelog](https://keepachangelog.com/en/1.0.0/), and this project
adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).
