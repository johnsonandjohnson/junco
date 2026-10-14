# Reference Group Handling with get_ref_info

## Overview

Many clinical tables require statistics computed relative to a reference
(control) group — for example, a risk difference versus placebo. In
`rtables`, the reference group can be injected automatically via
`.ref_group` and `.in_ref_col`, but this breaks down when:

- the reference column is nested under a spanning header,
- a `split_cols_by_multivar` is present, or
- an `add_overall_col` is used alongside treatment splits.

[`get_ref_info()`](https://johnsonandjohnson.github.io/junco/reference/get_ref_info.md)
solves this by letting the analysis function look up the reference group
itself, using an explicit column path (`ref_path`).

``` r

library(rtables)
library(junco)
library(dplyr)
```

## The `ref_path` Convention

`ref_path` is a character vector of alternating split-variable names and
their values, following the same convention as
[`rtables::col_paths()`](https://insightsengineering.github.io/rtables/latest-tag/reference/make_col_row_df.html):

    c("var1", "level1", "var2", "level2", ...)

For a simple layout with `split_cols_by("ARM")`, the placebo reference
path is:

``` r

ref_path <- c("ARM", "B: Placebo")
```

For a layout with a spanning header variable `colspan_trt` above `ARM`,
the path must include both levels:

``` r

ref_path <- c("colspan_trt", " ", "ARM", "B: Placebo")
```

The path must exactly match the column-split hierarchy in the layout.
Use `col_paths(build_table(lyt, df))` to inspect the available paths.

## Basic Usage

[`get_ref_info()`](https://johnsonandjohnson.github.io/junco/reference/get_ref_info.md)
returns a list with two elements:

- `in_ref_col`: `TRUE` if the current column matches `ref_path`, `FALSE`
  otherwise, `NULL` if the reference split variable is not present in
  the current column hierarchy.
- `ref_group`: the subset of the row data belonging to the reference
  group (a data frame, or a vector if `.var` is supplied).

## Working Example: AE Table with Risk Difference Columns

This example demonstrates the standard pattern for a table with spanning
headers and risk difference columns — the primary use case for
[`get_ref_info()`](https://johnsonandjohnson.github.io/junco/reference/get_ref_info.md).

``` r

trtvar <- "TRT01A"
ctrl_grp <- "Placebo"

adsl <- pharmaverseadamjnj::adsl |>
  filter(SAFFL == "Y") |>
  select(STUDYID, USUBJID, all_of(trtvar), SAFFL) |>
  mutate(!!trtvar := factor(
    .data[[trtvar]],
    levels = c("Xanomeline Low Dose", "Xanomeline High Dose", "Placebo")
  ))

adae <- pharmaverseadamjnj::adae |>
  filter(TRTEMFL == "Y") |>
  select(USUBJID, TRTEMFL, AEBODSYS, AEDECOD)

# Add spanning header and risk difference variables
adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)
adsl$rrisk_header <- "Risk Difference (%) (95% CI)"
adsl$rrisk_label <- paste(adsl[[trtvar]], "vs", ctrl_grp)

ae <- adae |> right_join(adsl, by = "USUBJID")
```

The `ref_path` must trace the full column-split hierarchy down to the
reference group:

``` r

colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

ref_path <- c("colspan_trt", " ", trtvar, ctrl_grp)
```

Now build the layout. The key point: `a_freq_j` uses
[`get_ref_info()`](https://johnsonandjohnson.github.io/junco/reference/get_ref_info.md)
internally to identify the reference group and compute risk differences
in the dedicated difference columns:

``` r

extra_args <- list(
  denom = "n_altdf",
  riskdiff = TRUE,
  ref_path = ref_path,
  method = "wald",
  .stats = "count_unique_fraction",
  .formats = c(rr_ci_3d = jjcsformat_xx("xx.x (xx.x, xx.x)"))
)

lyt <- basic_table(show_colcounts = TRUE, colcount_format = "N=xx") |>
  split_cols_by("colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) |>
  split_cols_by(trtvar) |>
  split_cols_by("rrisk_header", nested = FALSE) |>
  split_cols_by(trtvar,
    labels_var = "rrisk_label",
    split_fun = remove_split_levels(ctrl_grp)
  ) |>
  analyze("TRTEMFL",
    afun = a_freq_j,
    extra_args = append(extra_args, list(val = "Y", label = "Subjects with >=1 AE"))
  ) |>
  split_rows_by("AEBODSYS",
    split_label = "System Organ Class",
    split_fun = trim_levels_in_group("AEDECOD"),
    label_pos = "topleft",
    section_div = " ",
    nested = FALSE
  ) |>
  summarize_row_groups("AEBODSYS", cfun = a_freq_j, extra_args = extra_args) |>
  analyze("AEDECOD", afun = a_freq_j, extra_args = extra_args) |>
  append_topleft("  Preferred Term, n (%)")

result <- build_table(lyt, ae, alt_counts_df = adsl)
head(result, 10)
#>                                                      Active Study Agent                                              Risk Difference (%) (95% CI)                  
#> System Organ Class                       Xanomeline Low Dose   Xanomeline High Dose    Placebo     Xanomeline Low Dose vs Placebo   Xanomeline High Dose vs Placebo
#>   Preferred Term, n (%)                         N=67                   N=51              N=64                   N=67                             N=51              
#> ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Subjects with >=1 AE                         56 (83.6%)             48 (94.1%)        49 (76.6%)          7.0 (-6.6, 20.7)                 17.6 (5.3, 29.8)        
#> Cardiac disorders                            10 (14.9%)             10 (19.6%)        9 (14.1%)          0.9 (-11.2, 12.9)                 5.5 (-8.3, 19.4)        
#>   Atrial fibrillation                         1 (1.5%)               1 (2.0%)          1 (1.6%)           -0.1 (-4.3, 4.1)                  0.4 (-4.5, 5.3)        
#>   Atrial flutter                              1 (1.5%)               1 (2.0%)             0               1.5 (-1.4, 4.4)                   2.0 (-1.8, 5.8)        
#>   Atrial hypertrophy                              0                     0              1 (1.6%)           -1.6 (-4.6, 1.5)                 -1.6 (-4.6, 1.5)        
#>   Atrioventricular block first degree         1 (1.5%)                  0                 0               1.5 (-1.4, 4.4)                   0.0 (0.0, 0.0)         
#>   Atrioventricular block second degree            0                     0              1 (1.6%)           -1.6 (-4.6, 1.5)                 -1.6 (-4.6, 1.5)        
#>   Bradycardia                                     0                     0              1 (1.6%)           -1.6 (-4.6, 1.5)                 -1.6 (-4.6, 1.5)        
#>   Bundle branch block left                        0                     0              1 (1.6%)           -1.6 (-4.6, 1.5)                 -1.6 (-4.6, 1.5)        
#>   Bundle branch block right                   1 (1.5%)                  0              1 (1.6%)           -0.1 (-4.3, 4.1)                 -1.6 (-4.6, 1.5)
```

## How `get_ref_info` Works Internally

When `a_freq_j` (or any analysis function) calls
`get_ref_info(ref_path, .spl_context)`:

1.  It checks whether the split variables in `ref_path` are present in
    the current column hierarchy (using
    [`in_column()`](https://johnsonandjohnson.github.io/junco/reference/cur_col_split_path_utils.md)
    with wildcards).
2.  If present, it subsets the row data to the reference group using the
    pre-computed column facet indices stored in `.spl_context`.
3.  It returns `in_ref_col = TRUE/FALSE` indicating whether the current
    column IS the reference column (so the function can skip computing a
    difference against itself).

When the reference split is absent from the current column hierarchy
(e.g. in an `add_overall_col("Total")` column), both `in_ref_col` and
`ref_group` are `NULL`.

## Wildcard Matching with `in_column()`

[`in_column()`](https://johnsonandjohnson.github.io/junco/reference/cur_col_split_path_utils.md)
— the underlying helper used by
[`get_ref_info()`](https://johnsonandjohnson.github.io/junco/reference/get_ref_info.md)
— supports `"*"` as a wildcard for any single split variable name or
value:

``` r

# Simulate a split context for ARM == "B: Placebo" under colspan_trt == " "
spl_ctx <- data.frame(
  cur_col_split     = I(list(c("colspan_trt", "ARM"))),
  cur_col_split_val = I(list(c(" ", "B: Placebo")))
)

# Exact match
in_column(c("colspan_trt", " ", "ARM", "B: Placebo"), spl_ctx)
#> [1] TRUE

# Wildcard on spanning header — still TRUE
in_column(c("*", "*", "ARM", "B: Placebo"), spl_ctx)
#> [1] TRUE

# Different ARM level — FALSE
in_column(c("*", "*", "ARM", "A: Drug X"), spl_ctx)
#> [1] FALSE
```

## Relationship to `h_get_cur_trt_grp`

[`get_ref_info()`](https://johnsonandjohnson.github.io/junco/reference/get_ref_info.md)
and
[`h_get_cur_trt_grp()`](https://johnsonandjohnson.github.io/junco/reference/h_get_cur_trt_grp.md)
serve different purposes:

|  | [`get_ref_info()`](https://johnsonandjohnson.github.io/junco/reference/get_ref_info.md) | [`h_get_cur_trt_grp()`](https://johnsonandjohnson.github.io/junco/reference/h_get_cur_trt_grp.md) |
|----|----|----|
| **Returns** | reference-group data + `in_ref_col` flag | current treatment group value (single string) |
| **Used for** | obtaining `.ref_group` / `.in_ref_col` for statistics | identifying which treatment group the current column belongs to |
| **Typical callers** | `a_freq_j`, `a_eair_j`, `s_ancova_j` | `a_summarize_aval_chg_diff_j`, `a_freq_j`, `a_freq_resp_var_j` |

In layouts that combine `split_cols_by_multivar` with difference
columns,
[`h_get_cur_trt_grp()`](https://johnsonandjohnson.github.io/junco/reference/h_get_cur_trt_grp.md)
resolves the current treatment group from the interleaved column path,
and
[`get_ref_info()`](https://johnsonandjohnson.github.io/junco/reference/get_ref_info.md)
obtains the reference-group data.

## Inspecting Column Paths

When constructing `ref_path`, inspect the column paths of a built table:

``` r

col_paths(result)
#> [[1]]
#> [1] "colspan_trt"         "Active Study Agent"  "TRT01A"             
#> [4] "Xanomeline Low Dose"
#> 
#> [[2]]
#> [1] "colspan_trt"          "Active Study Agent"   "TRT01A"              
#> [4] "Xanomeline High Dose"
#> 
#> [[3]]
#> [1] "colspan_trt" " "           "TRT01A"      "Placebo"    
#> 
#> [[4]]
#> [1] "rrisk_header"                 "Risk Difference (%) (95% CI)"
#> [3] "TRT01A"                       "Xanomeline Low Dose"         
#> 
#> [[5]]
#> [1] "rrisk_header"                 "Risk Difference (%) (95% CI)"
#> [3] "TRT01A"                       "Xanomeline High Dose"
```

Each path corresponds to a leaf column. The `ref_path` should match one
of these paths exactly (or a prefix thereof when the reference column
has further nested splits below it).
