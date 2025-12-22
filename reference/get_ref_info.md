# Obtain Reference Information for a Global Reference Group

This helper function can be used in custom analysis functions, by
passing an extra argument `ref_path` which defines a global reference
group by the corresponding column split hierarchy levels.

## Usage

``` r
get_ref_info(ref_path, .spl_context, .var = NULL)
```

## Arguments

- ref_path:

  (`character`)  
  reference group specification as an `rtables` `colpath`, see details.

- .spl_context:

  (`data.frame`)  
  see
  [rtables::spl_context](https://insightsengineering.github.io/rtables/latest-tag/reference/spl_context.html).

- .var:

  (`character`)  
  the variable being analyzed, see
  [rtables::additional_fun_params](https://insightsengineering.github.io/rtables/latest-tag/reference/additional_fun_params.html).

## Value

A list with `ref_group` and `in_ref_col`, which can be used as
`.ref_group` and `.in_ref_col` as if being directly passed to an
analysis function by `rtables`, see
[rtables::additional_fun_params](https://insightsengineering.github.io/rtables/latest-tag/reference/additional_fun_params.html).

## Details

The reference group is specified in `colpath` hierarchical fashion in
`ref_path`: the first column split variable is the first element, and
the level to use is the second element. It continues until the last
column split variable with last level to use. Note that depending on
`.var`, either a `data.frame` (if `.var` is `NULL`) or a vector
(otherwise) is returned. This allows usage for analysis functions with
`df` and `x` arguments, respectively.

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

standard_afun <- function(x, .ref_group, .in_ref_col) {
  in_rows(
    "Difference of Averages" = non_ref_rcell(
      mean(x) - mean(.ref_group),
      is_ref = .in_ref_col,
      format = "xx.xx"
    )
  )
}

result_afun <- function(x, ref_path, .spl_context, .var) {
  ref <- get_ref_info(ref_path, .spl_context, .var)
  standard_afun(x, .ref_group = ref$ref_group, .in_ref_col = ref$in_ref_col)
}

ref_path <- c("colspan_trt", " ", "ARM", "B: Placebo")

lyt <- basic_table() |>
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) |>
  split_cols_by("ARM") |>
  analyze(
    "AGE",
    extra_args = list(ref_path = ref_path),
    afun = result_afun
  )

build_table(lyt, dm)
#>                              Active Study Agent                 
#>                          A: Drug X   C: Combination   B: Placebo
#> ————————————————————————————————————————————————————————————————
#> Difference of Averages     1.89           1.55                  
```
