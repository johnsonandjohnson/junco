# Standard All vs Some (e.g. Related AEs) column structure

Standard All vs Some (e.g. Related AEs) column structure

## Usage

``` r
some_v_all_col_struct(
  lyt,
  colspan_trt_map = NULL,
  combo_map_df = NULL,
  trtvar = names(colspan_trt_map)[2],
  subgrp_var = NULL,
  subgrp_lbl = subgrp_var,
  all_lbl,
  subgrp_lvls,
  .pre = list(),
  .post = list()
)
```

## Arguments

- lyt:

  (`PreDataTableLayouts`). The layout to modify. This should virtually
  always be the object returned by `basic_table`.

- colspan_trt_map:

  (`data.frame`). The spanning label map for the main columns, as given
  by `create_colspan_map`.

- combo_map_df:

  (`data.frame` or `NULL`). A combination data frame as defined by
  [`add_combo_levels()`](https://insightsengineering.github.io/rtables/latest-tag/reference/add_overall_level.html)
  with an additional `is_control` column indicating whether the virtual
  level will act as a reference (`TRUE`) or active (`FALSE`) group.

- trtvar:

  (`character(1)` or `NULL`)  
  the treatment variable to split by. Defaults to the treatment variable
  in `colspan_trt_map`.

- subgrp_var:

  (`character(1)` or `NULL`)  
  the name of the subgroup variable to split by within the `trtvar`
  split

- subgrp_lbl:

  (`character(1)`)  
  The label to put above the combination level representing
  `subgrp_lvls`

- all_lbl:

  (`character(1)`)  
  The label to put above the "All" column

- subgrp_lvls:

  (`characgter`)  
  All level(s) to be included in the `subgrp_lbl` column.

- .pre:

  (`list` of `function`s)  
  Passed to
  [`rtables::make_split_fun()`](https://insightsengineering.github.io/rtables/latest-tag/reference/make_split_fun.html)
  as `pre` for treatment splitting.

- .post:

  (`list` of `function`s)  
  Passed to
  [`rtables::make_split_fun()`](https://insightsengineering.github.io/rtables/latest-tag/reference/make_split_fun.html)
  as `post` for treatment splitting after the standard column-structure
  processing.

## Details

This column structure generating function is for comparing a single
portion of the data (as represented by level(s) of `subgrp_var`) against
the full data, comparison of AE counts to treatment-related AE counts
being a motivating example.

## Examples

``` r

 library(junco)
dat <- create_colspan_var(
  data.frame(
    TRT01A = factor(rep(c("Placebo", "Active 1", "Active 2"), each = 5)),
    GRADE = factor(rep(paste("Grade ", 1:5), 3))
  ),
  non_active_grp = "Placebo",
  non_active_grp_span_lbl = "Control",
  active_grp_span_lbl = "Active Treatment",
  colspan_var = "colspan_trt",
  trt_var = "TRT01A"
)
colspan_trt_map <- create_colspan_map(
  dat,
  non_active_grp = "Placebo",
  non_active_grp_span_lbl = "Control",
  active_grp_span_lbl = "Active Treatment",
  colspan_var = "colspan_trt",
  trt_var = "TRT01A"
)

lyt <- basic_table() |>
    some_v_all_col_struct(
    colspan_trt_map,
    subgrp_var = "GRADE",
    subgrp_lvls = c("Grade 4", "Grade 5"),
    subgrp_lbl = "High Grade",
    all_lbl = "All Grades")

build_table(lyt, dat)
#>                    Active Treatment                            Control        
#>           Active 1                  Active 2                   Placebo        
#>             (N=5)                     (N=5)                     (N=5)         
#>    All Grades   High Grade   All Grades   High Grade   All Grades   High Grade
#> ——————————————————————————————————————————————————————————————————————————————
```
