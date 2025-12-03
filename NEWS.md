# junco 0.1.3

## New features

- Added validation of table structure in `tt_to_tlgrtf()` using `rtables::validate_table_struct()` with a warning if the structure is invalid
- Added standard error (SE) column for each treatment arm's least square (LS) means estimate to the `summarize_lsmeans_wide()` layout.
- Add `a_maxlev()` to be able to calculate count and percentage of the maximum level of an ordered factor per subject

## Minor improvements and bug fixes

- Remove some unused functions (jj_uc_map, postfun_cog, postfun_eq5d, column_N, non_blank_sentinel, null_fn, unicodify
- Replace {pharmaverseadam} with {pharmaverseadamjnj}
- fix bug for not selecting NA records in `h_subset_combo()`
- update `string_to_title()` to handle factors (#26)

## Other changes

* Moved 'rbmi' from 'Imports' to 'Suggests' and made package fully functional without it
