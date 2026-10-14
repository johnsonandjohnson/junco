# Obtain reference group information from split context.

**\[stable\]**

`get_ref_info()` identifies a reference group defined by a column-split
path and returns both the reference-group data and an indicator of
whether the current column is the reference column. It is intended for
use inside custom `rtables` analysis functions.

The reference group is specified using `ref_path`, which consists of
alternating column-split variable names and its corresponding levels.
For example, `c("SEX", "F", "ARM", "Placebo")` specifies the
column-split path where `SEX` is `"F"` and `ARM` is `"Placebo"`.

## Usage

``` r
get_ref_info(ref_path, .spl_context, .var = NULL)
```

## Arguments

- ref_path:

  (`character`)  
  Reference group specification as an `rtables` `colpath`; see Details.

- .spl_context:

  (`data.frame`)  
  Ancestor split-state information passed by `rtables`.

- .var:

  (`character(1)`)  
  The variable being analyzed; see
  [rtables::additional_fun_params](https://insightsengineering.github.io/rtables/latest-tag/reference/additional_fun_params.html).
  If supplied, the corresponding column is extracted from the
  reference-group data. If `NULL`, the complete reference-group data
  frame is returned.

## Value

A list with the following elements:

- `in_ref_col` (`logical(1)` or `NULL`) indicates whether the current
  column matches the reference path. This corresponds to `.in_ref_col`
  in
  [rtables::additional_fun_params](https://insightsengineering.github.io/rtables/latest-tag/reference/additional_fun_params.html).

- `ref_group` (`data.frame`, vector, or `NULL`) contains the
  observations belonging to the reference group. If `.var` is `NULL`,
  the complete data frame is returned; otherwise, the column specified
  by `.var` is returned. This corresponds to `.ref_group` in
  [rtables::additional_fun_params](https://insightsengineering.github.io/rtables/latest-tag/reference/additional_fun_params.html).

If the reference path is not present in the current column-split
hierarchy, both elements are `NULL`.

## Examples

``` r
dm <- DM
dm$colspan_trt <- factor(
  ifelse(dm$ARM == "B: Placebo", " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)
colspan_trt_map <- create_colspan_map(
  dm,
  non_active_grp = "B: Placebo",
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = "ARM"
)

# A standard analysis function which uses a reference group.
standard_afun <- function(x, .ref_group, .in_ref_col) {
  diff_means <- if (isFALSE(.in_ref_col)) {
    mean(x) - mean(.ref_group)
  } else {
    NULL
  }
  in_rows(
    m = rcell(mean(x), label = "Mean"),
    dm = rcell(diff_means, label = "Difference in Means vs Placebo"),
    .formats = "xx.xx"
  )
}

# The custom analysis function which can work with a global reference group.
result_afun <- function(x, ref_path, .spl_context, .var) {
  ref <- get_ref_info(ref_path, .spl_context, .var)
  standard_afun(x, .ref_group = ref$ref_group, .in_ref_col = ref$in_ref_col)
}

ref_path <- c("colspan_trt", " ", "ARM", "B: Placebo")

lyt <- basic_table() |>
  split_cols_by("colspan_trt", split_fun = trim_levels_to_map(colspan_trt_map)) |>
  split_cols_by("ARM") |>
  add_overall_col("Total") |>
  analyze("AGE", afun = result_afun, extra_args = list(ref_path = ref_path))

build_table(lyt, dm)
#>                                      Active Study Agent                         
#>                                  A: Drug X   C: Combination   B: Placebo   Total
#> ————————————————————————————————————————————————————————————————————————————————
#> Mean                               34.91         34.57          33.02      34.22
#> Difference in Means vs Placebo     1.89           1.55                          
```
