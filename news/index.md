# Changelog

## junco 0.1.3

CRAN release: 2026-01-15

#### New Major features

- Add docx exporter for Tables, Listings and Figures.
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
- Add
  [`a_maxlev()`](https://johnsonandjohnson.github.io/junco/reference/a_maxlev.md)
  to be able to calculate count and percentage of the maximum level of
  an ordered factor per subject
- Added `cmh_sato` and `cmh_mn` (Cochran-Mantel-Haenszel stratified
  proportion difference estimation with Sato variance and Miettinen
  Nurminen method, respectively) `method` options to the
  [`s_proportion_diff_j()`](https://johnsonandjohnson.github.io/junco/reference/prop_diff.md)
  function.
- Added formatting function round type
  ([\#76](https://github.com/johnsonandjohnson/junco/issues/76))

#### Changed

- Vignettes: switch to rmarkdown::html_vignette and fix internal link to
  package index to address CRAN NOTES.

#### Minor changes

- Add
  [`a_two_tier()`](https://johnsonandjohnson.github.io/junco/reference/a_two_tier.md)
  analysis function
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
- Added the internal function
  [`do_exclude_split()`](https://johnsonandjohnson.github.io/junco/reference/do_exclude_split.md)
  to facilitate the exclusion of specified row splits from analysis
  functions.
- Remove some unused functions (jj_uc_map, postfun_cog, postfun_eq5d,
  column_N, non_blank_sentinel, null_fn, unicodify
- Add extra check for existence of `.alt_df_full` when layout has risk
  difference column and a row-split (h_create_alt_df)
  [\#120](https://github.com/johnsonandjohnson/junco/issues/120).
- Add `alignments` argument in
  [`tt_to_tlgrtf()`](https://johnsonandjohnson.github.io/junco/reference/tt_to_tlgrtf.md)
- Removed
  [`s_test_proportion_diff()`](https://insightsengineering.github.io/tern/latest-tag/reference/prop_diff_test.html)
  and corresponding helper functions, as they are now available as
  needed in the `tern` package.
- Replace {pharmaverseadam} with {pharmaverseadamjnj}
- Update pruning_functions.R
- Update
  [`string_to_title()`](https://johnsonandjohnson.github.io/junco/reference/string_to_title.md)
  to handle factors
  ([\#26](https://github.com/johnsonandjohnson/junco/issues/26))
- Moved rbmi to suggest
- Replaced `denom_df` with `.alt_df_full` in
  [`a_maxlev()`](https://johnsonandjohnson.github.io/junco/reference/a_maxlev.md).

#### Fixed

- Fixes [\#102](https://github.com/johnsonandjohnson/junco/issues/102)
  bug inappropriate warnings from `cond_rm_facets` function
- Fix bug for not selecting NA records in `h_subset_combo()`
- Consistent
  [`tt_to_tbldf()`](https://johnsonandjohnson.github.io/junco/reference/tt_to_tbldf.md)
  function behavior with invalid structures
  [\#116](https://github.com/johnsonandjohnson/junco/issues/116)

### Other changes

- Moved ‘rbmi’ from ‘Imports’ to ‘Suggests’ and made package fully
  functional without it
