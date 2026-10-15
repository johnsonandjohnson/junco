# Changelog

## junco 0.1.7009

### \[0.1.7\] - 2026-10-15 (CRAN release)

#### Fixed

- Fixed
  [`get_ref_info()`](https://johnsonandjohnson.github.io/junco/reference/get_ref_info.md)
  to accept ref_path = NULL
  ([\#359](https://github.com/johnsonandjohnson/junco/issues/359)).
- Fixed
  [`junco_get_stats()`](https://johnsonandjohnson.github.io/junco/reference/default_stats_formats_labels.md)
  to inherit any default stats from `tern` that are not explicitly
  defined in junco.
- Fixed
  [`get_ref_info()`](https://johnsonandjohnson.github.io/junco/reference/get_ref_info.md)
  so that is works in the presence of “overall” column
  ([\#332](https://github.com/johnsonandjohnson/junco/issues/332))
- CRITICAL: hotfixed
  [`tt_to_tlgrtf()`](https://johnsonandjohnson.github.io/junco/reference/tt_to_tlgrtf.md)
  lost titles in certain cases
  ([\#373](https://github.com/johnsonandjohnson/junco/issues/373))
- CRITICAL: hotfixed
  [`def_colwidths()`](https://johnsonandjohnson.github.io/junco/reference/def_colwidths.md)
  when a column label is too long
  ([\#281](https://github.com/johnsonandjohnson/junco/issues/281))

#### Changed

- Added `"unique_fraction"` as a new statistic supported by
  [`s_freq_j()`](https://johnsonandjohnson.github.io/junco/reference/a_freq_j.md)
  and
  [`a_freq_j()`](https://johnsonandjohnson.github.io/junco/reference/a_freq_j.md)
  ([\#519](https://github.com/johnsonandjohnson/junco/issues/519)).
- Added `lowercase_words` argument to
  [`string_to_title()`](https://johnsonandjohnson.github.io/junco/reference/string_to_title.md)
  to keep specified words lowercase
  ([\#524](https://github.com/johnsonandjohnson/junco/issues/524)).
- Added `.cell_footnotes` and `.row_footnotes` arguments to
  [`format_stats()`](https://johnsonandjohnson.github.io/junco/reference/default_stats_formats_labels.md)
  ([\#470](https://github.com/johnsonandjohnson/junco/issues/470)).
- Removed `s_proportion_diff_j()` which was a wrapper around
  [`tern::s_proportion_diff()`](https://pharmaverse.github.io/tern/latest-tag/reference/prop_diff.html).
  The `label` and `conf_level` parameters moved to
  [`a_proportion_diff_j()`](https://johnsonandjohnson.github.io/junco/reference/prop_diff.md)
  ([\#442](https://github.com/johnsonandjohnson/junco/issues/442),
  [\#467](https://github.com/johnsonandjohnson/junco/issues/467)).
- Renamed internal functions `s_rel_risk_val_j()` and
  `s_rel_risk_levii_j()` to `s_risk_diff_val_j()` and
  `s_risk_diff_levii_j()` to correctly reflect that they compute risk
  difference (p1 - p2), not relative risk (p1 / p2)
  ([\#442](https://github.com/johnsonandjohnson/junco/issues/442)).
- Updated several analysis functions to use
  [`h_get_cur_trt_grp()`](https://johnsonandjohnson.github.io/junco/reference/h_get_cur_trt_grp.md)
  ([\#295](https://github.com/johnsonandjohnson/junco/issues/295)).
- Replaced `h_get_trtvar_refpath()` with
  [`h_get_cur_trt_grp()`](https://johnsonandjohnson.github.io/junco/reference/h_get_cur_trt_grp.md)
  ([\#295](https://github.com/johnsonandjohnson/junco/issues/295)).
- Updated
  [`get_ref_info()`](https://johnsonandjohnson.github.io/junco/reference/get_ref_info.md)
  for matching column split paths
  ([\#295](https://github.com/johnsonandjohnson/junco/issues/295)).
- Added the new helper function
  [`factor_by_order()`](https://johnsonandjohnson.github.io/junco/reference/factor_by_order.md)
  ([\#425](https://github.com/johnsonandjohnson/junco/issues/425)).
- Renamed `in_ref_col()` to
  [`in_column()`](https://johnsonandjohnson.github.io/junco/reference/cur_col_split_path_utils.md)
  and renamed its `ref_path` argument to `col_path`.
- Updated `in_ref_col()` to accept `ref_path = NULL`
  ([\#404](https://github.com/johnsonandjohnson/junco/issues/404)).
- Added the new helper functions
  [`cur_col_split_path()`](https://johnsonandjohnson.github.io/junco/reference/cur_col_split_path_utils.md)
  and
  [`in_column()`](https://johnsonandjohnson.github.io/junco/reference/cur_col_split_path_utils.md)
  to support custom analysis functions that depend on the current column
  split context
  ([\#404](https://github.com/johnsonandjohnson/junco/issues/404)).
- Added a default value for the `label` argument in
  [`c_summary_subset_label()`](https://johnsonandjohnson.github.io/junco/reference/summary_subset.md).
- Updated the documentation of
  [`a_summary_subset()`](https://johnsonandjohnson.github.io/junco/reference/summary_subset.md).
- Refactored
  [`prepend_label_cell()`](https://johnsonandjohnson.github.io/junco/reference/prepend_label_cell.md);
  Only `RowsVerticalSection` is now supported.
- Removed `filter_df_prior_afun()`, `a_summary_diff_mvars()`,
  `a_summary_diff_mvars_label()`.
- Removed formatters exports
  [\#317](https://github.com/johnsonandjohnson/junco/issues/317)
- Deprecate `a_coxph_hr` for `tern:::a_coxph_pairwise()` and
  `s_coxph_hr` for `tern:::s_coxph_pairwise()`
  [\#158](https://github.com/johnsonandjohnson/junco/issues/158)
- Reduce sampling of `rbmi` test to make tests shorter
  ([\#323](https://github.com/johnsonandjohnson/junco/issues/323))
- Optimize shared tables in `test-tt_to_tblfile`
  ([\#323](https://github.com/johnsonandjohnson/junco/issues/323))
- Changed forked `h_ancova` for the `tern` one
- Add extra statistics to `a_eair100_j` and introduce scaling factor
  `num_p_year` (default = 100)
  ([\#361](https://github.com/johnsonandjohnson/junco/issues/361))
- Removed ellipsis argument from `a_freq_resp_var_j`
  ([\#236](https://github.com/johnsonandjohnson/junco/issues/236))
- Removed `dplyr` from `junco` and replaced by base R
  [\#201](https://github.com/johnsonandjohnson/junco/issues/201)
- Replaced `assertthat` by `checkmate` for consistency
  [\#201](https://github.com/johnsonandjohnson/junco/issues/201)
- Remove `stringi` from dependencies
  [\#201](https://github.com/johnsonandjohnson/junco/issues/201)
- Deprecate
  [`rbmi_analyse()`](https://johnsonandjohnson.github.io/junco/reference/rbmi_analyse.md),
  [`make_rbmi_cluster()`](https://johnsonandjohnson.github.io/junco/reference/make_rbmi_cluster.md),
  [`par_lapply()`](https://johnsonandjohnson.github.io/junco/reference/par_lapply.md)
  for `rbmi` equivalent functions
  [\#367](https://github.com/johnsonandjohnson/junco/issues/367)
- Deprecate
  [`a_kaplan_meier()`](https://johnsonandjohnson.github.io/junco/reference/a_kaplan_meier.md)
  for
  [`tern::a_surv_time()`](https://pharmaverse.github.io/tern/latest-tag/reference/survival_time.html)
- Changed the label for `range_with_cens_info` from `"Min, max"` (junco)
  to `"Min - Max (with censoring)"` and the argument from
  `lsmean_diffci` to `lsmean_diff_with_ci`
- Deprecate
  [`s_coxph_hr()`](https://johnsonandjohnson.github.io/junco/reference/s_coxph_hr.md)
  for `tern:::a_coxph_pairwise()`
- Changed stop message in
  [`a_freq_j()`](https://johnsonandjohnson.github.io/junco/reference/a_freq_j.md)
  when `label_map` option is used in a rowsplit with no data on a
  character analysis var
  [\#386](https://github.com/johnsonandjohnson/junco/issues/386)
- Use new exported splv_extra and value_expr accessors
  (insightsengineering#1098)
- Update new exported calls from rtables.officer
- update documentation to `roxygen2` 8.0.0
- Add extra statistics to `a_eair100_j` and introduce scaling factor
  `num_p_year` (default = 100)
  ([\#361](https://github.com/johnsonandjohnson/junco/issues/361))
- Updated behavior of formatting functions
  `jjcsformat_count_denom_fraction` and
  `jjcsformat_fraction_count_denom` and introduce functions
  `jjcsformat_count_denom_fraction_legacy` and
  `jjcsformat_fraction_count_denom_legacy` for backward compatibility
  reasons
  ([\#450](https://github.com/johnsonandjohnson/junco/issues/450))
- Updated `insightsengineering` urls to `pharmaverse`
- Renamed and refactored `postfun_eq5d` into `stats_in_cols_setup`
- Added factor assertion on `.var` in `s_freq_j`
  ([\#453](https://github.com/johnsonandjohnson/junco/issues/453)).
- Added assertions for the `id` argument in
  [`a_freq_j()`](https://johnsonandjohnson.github.io/junco/reference/a_freq_j.md)
  and
  [`s_freq_j()`](https://johnsonandjohnson.github.io/junco/reference/a_freq_j.md)
  ([\#424](https://github.com/johnsonandjohnson/junco/issues/424)).

#### Added

- Added `jjcsformat_percentage()` for formatting percentage values
  ([\#519](https://github.com/johnsonandjohnson/junco/issues/519)).
- Added a new analysis function
  [`a_combo_prop_diff_pval_mf()`](https://johnsonandjohnson.github.io/junco/reference/a_combo_prop_diff_pval_mf.md)
  for proportions, risk differences, and risk difference tests with
  adaptive Mantel-Fleiss-based method selection
  ([\#517](https://github.com/johnsonandjohnson/junco/issues/517)).
- Added
  [`s_test_proportion_diff_mf()`](https://johnsonandjohnson.github.io/junco/reference/test_proportion_diff_mf.md)
  and
  [`a_test_proportion_diff_mf()`](https://johnsonandjohnson.github.io/junco/reference/test_proportion_diff_mf.md)
  adaptive test of stratified response-proportion differences using the
  Mantel–Fleiss criterion
  ([\#457](https://github.com/johnsonandjohnson/junco/issues/457)).
- Added
  [`s_proportion_diff_mf()`](https://johnsonandjohnson.github.io/junco/reference/proportion_diff_mf.md)
  and
  [`a_proportion_diff_mf()`](https://johnsonandjohnson.github.io/junco/reference/proportion_diff_mf.md)
  adaptive estimation of stratified response-proportion differences
  using the Mantel–Fleiss criterion
  ([\#456](https://github.com/johnsonandjohnson/junco/issues/456)).
- Added
  [`a_cond_proportion_j()`](https://johnsonandjohnson.github.io/junco/reference/cond_proportion_j.md)
  which implements conditional method selection between Wald and
  Clopper-Pearson confidence intervals based on the number of responses
  and the denominator (and the method selection can either be done on
  cell level or on row level).
- Added
  [`a_sum_ratio_j()`](https://johnsonandjohnson.github.io/junco/reference/a_sum_ratio_j.md)
  analysis function for computing sum, unique-row sum, and their ratios
  against a denominator column.
- Added
  [`strict_match()`](https://johnsonandjohnson.github.io/junco/reference/strict_match.md)
  for uniquely matching a value in the odd or even positions of a
  character vector.
- Added
  [`categorize_pval()`](https://johnsonandjohnson.github.io/junco/reference/categorize_pval.md)
  for assigning p-values to validated, user-defined categories.
- Added
  [`pool_rubin_scalar()`](https://johnsonandjohnson.github.io/junco/reference/pool_rubin_scalar.md)
  and
  [`pool_z_stat()`](https://johnsonandjohnson.github.io/junco/reference/pool_z_stat.md)
  for pooling scalar estimates and z statistics across imputations.
- Added
  [`resp_multiple_imputation()`](https://johnsonandjohnson.github.io/junco/reference/resp_multiple_imputation.md)
  to impute missing binary responses across scenarios and pool CMH
  risk-difference and p-value results.
- Updated documentation and examples for `label_map` in `a_freq_j`
  ([\#235](https://github.com/johnsonandjohnson/junco/issues/235))
- Added `tern` methods for difference in proportions in `a_freq_j` and
  `a_freq_resp_var_j` : `cmh_sato`, `cmh_mn`, `uncond_exact_diff`
  ([\#389](https://github.com/johnsonandjohnson/junco/issues/389))
- Added
  [`a_summary_j_with_exclude()`](https://johnsonandjohnson.github.io/junco/reference/a_summary_j.md)
  to allow
  [`tern::a_summary()`](https://pharmaverse.github.io/tern/latest-tag/reference/analyze_variables.html)
  analyses to be skipped for selected row split levels.
- Added
  [`a_summarize_mmrm_with_exclude()`](https://johnsonandjohnson.github.io/junco/reference/summarize_mmrm.md)
  to allow MMRM summaries to be skipped for selected row split levels.
- Added
  [`rightside()`](https://johnsonandjohnson.github.io/junco/reference/rightside.md)
  to extract the right-hand side of a formula as a scalar character
  value.
- Added
  [`a_three_tier()`](https://johnsonandjohnson.github.io/junco/reference/a_two_tier.md)
  as extension to
  [`a_two_tier()`](https://johnsonandjohnson.github.io/junco/reference/a_two_tier.md).
- Added formatting function
  [`format_sigfig_j()`](https://johnsonandjohnson.github.io/junco/reference/format_sigfig_j.md)
  as alternative to
  [`tern::format_sigfig()`](https://pharmaverse.github.io/tern/latest-tag/reference/format_sigfig.html).
  ([\#436](https://github.com/johnsonandjohnson/junco/issues/436))
- Added new standard column structure functions:
  [`make_multicomp_splfun()`](https://johnsonandjohnson.github.io/junco/reference/make_multicomp_splfun.md),
  [`grouped_cols_w_diffs()`](https://johnsonandjohnson.github.io/junco/reference/grouped_cols_w_diffs.md),
  [`grouped_cols_w_subgrps()`](https://johnsonandjohnson.github.io/junco/reference/grouped_cols_w_diffs.md),
  [`shift_tbl_col_struct()`](https://johnsonandjohnson.github.io/junco/reference/shift_tbl_col_struct.md),
  and
  [`some_v_all_col_struct()`](https://johnsonandjohnson.github.io/junco/reference/some_v_all_col_struct.md).
- Added varying decimal precision utility functions `fmt_spec_single_d`,
  `fmt_spec_df_d`, `fmt_spec_var_d`.
  ([\#474](https://github.com/johnsonandjohnson/junco/issues/474))
- Added utility functions `get_fmt_details` and `compare_fmt_specs` for
  reviewing format specification objects.

### [0.1.6](https://github.com/johnsonandjohnson/junco/releases/tag/v0.1.6-rc) - 2026-05-05 (CRAN release)

#### Fixed

- Fixed
  [`s_kaplan_meier()`](https://johnsonandjohnson.github.io/junco/reference/s_kaplan_meier.md)
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

- Updated
  [`c_summary_subset_label()`](https://johnsonandjohnson.github.io/junco/reference/summary_subset.md):
  removed `.spl_context` argument and renamed `subset_expr` to
  `filter_expr`.
- Renamed
  [`a_summary_j()`](https://johnsonandjohnson.github.io/junco/reference/a_summary_j.md)
  to
  [`a_summary_subset()`](https://johnsonandjohnson.github.io/junco/reference/summary_subset.md)
  and updated its purpose, implementation, and arguments.
- Added new stats/labels/formats/indents to
  `junco_utils_default_stats_formats_labels.r` file.
- Renamed `s_diff_mean_ci()` to
  [`s_diff_means()`](https://johnsonandjohnson.github.io/junco/reference/diff_means.md),
  and added new statistics to
  [`s_diff_means()`](https://johnsonandjohnson.github.io/junco/reference/diff_means.md).
- Moved `safe_t_test()` from `a_summarize_aval_chg_diff.R` to a new file
  `safe_t_test.R`,
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

- Added
  [`a_diff_means()`](https://johnsonandjohnson.github.io/junco/reference/diff_means.md),
  which is based on the existing
  [`s_diff_means()`](https://johnsonandjohnson.github.io/junco/reference/diff_means.md).
- Added multi-comparator functionality
  ([\#271](https://github.com/johnsonandjohnson/junco/issues/271))
- Hotfix: Added several new functions for creating the Vital Sign tables
  for core and clinpharm: `a_summary_diff_mvars()`,
  [`a_summary_j()`](https://johnsonandjohnson.github.io/junco/reference/a_summary_j.md),
  [`c_summary_subset_label()`](https://johnsonandjohnson.github.io/junco/reference/summary_subset.md),
  `filter_df_prior_afun()`,
  [`prepend_label_cell()`](https://johnsonandjohnson.github.io/junco/reference/prepend_label_cell.md),
  `s_diff_mean_ci()`,
  [`s_summary_diff()`](https://johnsonandjohnson.github.io/junco/reference/s_summary_diff.md),
  `safe_t_test()`
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
  [`s_test_proportion_diff()`](https://pharmaverse.github.io/tern/latest-tag/reference/prop_diff_test.html)
  and corresponding helper functions, as they are now available as
  needed in the `tern` package.

- Added `cmh_sato` and `cmh_mn` (Cochran-Mantel-Haenszel stratified
  proportion difference estimation with Sato variance and Miettinen
  Nurminen method, respectively) `method` options to the
  `s_proportion_diff_j()` function.

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

### Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a
Changelog](https://keepachangelog.com/en/1.0.0/), and this project
adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).
