# junco 0.1.3


### New Major features
- Add docx exporter for Tables, Listings and Figures.
- Add `a_cmhrms_j()` to support p-value from CMH row mean score test, as well as the modified version `a_cmhrms_j_with_exclude()` that allows to exclude the analysis from specified row splits. #97
- Added standard error (SE) column for each treatment arm's least square (LS) means estimate to the `summarize_lsmeans_wide()` layout.
- Add `a_maxlev()` to be able to calculate count and percentage of the maximum level of an ordered factor per subject
- Added `cmh_sato` and `cmh_mn` (Cochran-Mantel-Haenszel stratified proportion difference estimation with Sato variance and Miettinen Nurminen method, respectively) `method` options to the `s_proportion_diff_j()` function.
- Added formatting function round type (#76)

### Changed
- Vignettes: switch to rmarkdown::html_vignette and fix internal link to package index to address CRAN NOTES.

### Minor changes
- Add `a_two_tier()` analysis function
- Remove `brackets_to_rtf()`
- Export `rbmi_pool()` #22
- Add functionality to `a_freq_j()` to process `val = NULL` from levels specified in `label_map`, as well as the modified version `a_freq_j_with_exclude()` that allows to exclude the analysis from specified row splits. #95
- Added the internal function `do_exclude_split()` to facilitate the exclusion of specified row splits from analysis functions.
- Remove some unused functions (jj_uc_map, postfun_cog, postfun_eq5d, column_N, non_blank_sentinel, null_fn, unicodify
- Add extra check for existence of `.alt_df_full` when layout has risk difference column and a row-split (h_create_alt_df) #120.
- Add `alignments` argument in `tt_to_tlgrtf()`
- Removed `s_test_proportion_diff()` and corresponding helper functions, as they are now available as needed in the `tern` package.
- Replace {pharmaverseadam} with {pharmaverseadamjnj}
- Update pruning_functions.R
- Update `string_to_title()` to handle factors (#26)
- Moved rbmi to suggest
- Replaced `denom_df` with `.alt_df_full` in `a_maxlev()`.

### Fixed
- Fixes #102 bug inappropriate warnings from `cond_rm_facets` function
- Fix bug for not selecting NA records in `h_subset_combo()`
- Consistent `tt_to_tbldf()` function behavior with invalid structures #116


## Other changes
* Moved 'rbmi' from 'Imports' to 'Suggests' and made package fully functional without it
