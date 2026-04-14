# junco 0.1.6

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

### Changed

- added default label for median_range "Median (min, max)" in `junco_default_labels_start`
- changed label from "experimental" to "stable" for `default_stats_formats_labels`
- refactored functions `tt_to_flextable_j()` and `export_as_docx_j()`
- created generic wrapper function `export_TLG_as_docx()`, which now calls `export_as_docx_j()` and `export_graph_as_docx()` (#173)
- Functions `export_as_docx_j()` and `export_graph_as_docx()` still exist but are not internal, i.e. not exported
- updated vignette to explain correctly how to insert newlines in the headers of Tables and Listings (#179)
- "watermark" argument in the docx exporter is now a String instead of a Boolean (#181)
- faster docx unit tests (#197)
- LS means tabulation uses very slightly updated column header (no blanks around the hyphen in "Testing-Reference")
- Bump flextable and remove the skip() units tests (#193)
- Update colwidths tests to derive values
- Fix old TODOs in tests
- docx exporter added missing defaults (#186)
- Changed all snapshot tests to `cran = TRUE`

### Added

- Added option to switch on/off the export of the csv in both `tt_to_tlgrtf()` and `export_as_docx_j()`
- Added option to specify the output folder for the csv
- Added argument 'validate' to `export_TLG_as_docx()` and `tt_to_flextable_j()` (#213)
- Added "watermark" argument in the docx exporter for Figures (#181)
- Added argument `mult_adj_emmeans` in `fit_mmrm_j()` to enable (single-step or step-down) Dunnett multiplicity adjustment for LS means contrasts (p-values and confidence intervals) for more than one experimental arm within visits.
- Export helpers for LS means tabulation: `lsmeans_wide_cfun`, `lsmeans_wide_first_split_fun_fct`, `lsmeans_wide_second_split_fun_fct`