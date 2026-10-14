# Analysis Function: Sum and Ratio of Sums

**\[stable\]**

Computes one or more of the following statistics for use with
[`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html):

- `"sum"`: `sum(.var)`

- `"sum_unique"`: `sum(.var)` after keeping unique rows by `id_var`

- `"ratio"`: `sum(.var) / sum(denom_by)`

- `"ratio_unique"`: same ratio after keeping unique rows by `id_var`

Both `.var` and `denom_by` columns must be numeric (integer, double, or
logical coerced to numeric).

## Usage

``` r
a_sum_ratio_j(
  df,
  .var,
  .stats = "sum",
  denom_by = NULL,
  id_var = NULL,
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL,
  na_str = NULL,
  ...
)
```

## Arguments

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- .var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

- .stats:

  (`character`)  
  one or more of `"sum"`, `"sum_unique"`, `"ratio"`, `"ratio_unique"`.
  Default: `"sum"`.

- denom_by:

  (`string` or `NULL`)  
  denominator column; required for `"ratio"` and `"ratio_unique"`.

- id_var:

  (`string` or `NULL`)  
  subject-id column for unique row selection; required for
  `"sum_unique"` and `"ratio_unique"`.

- .formats:

  (named `character` or `list`)  
  formats for the statistics. See Details in `analyze_vars` for more
  information on the `'auto'` setting.

- .labels:

  (named `character`)  
  labels for the statistics (without indent).

- .indent_mods:

  (named `integer` or `NULL`)  
  indent modifiers for the row labels. Defaults to `NULL` (no
  modification).

- na_str:

  (`character` or `NULL`)  
  string(s) used to replace `NA` values in the formatted output.
  Defaults to `NULL` (no replacement).

- ...:

  additional arguments for the lower level functions.

## Value

A `RowsVerticalSection` for use by rtables.

## Author

VR

## Examples

``` r
df <- data.frame(
  USUBJID = c("S01", "S01", "S02", "S03"),
  ARM     = factor(c("A", "A", "A", "B")),
  EVENTS  = c(1, 1, 0, 1),
  DAYS    = c(10, 10, 20, 15)
)

# All 4 stats in a layout (S01 duplicate tests unique logic)
lyt <- basic_table() |>
  split_cols_by("ARM") |>
  analyze(
    "EVENTS",
    afun = a_sum_ratio_j,
    extra_args = list(
      .stats   = c("sum", "sum_unique", "ratio", "ratio_unique"),
      denom_by = "DAYS",
      id_var   = "USUBJID"
    )
  )

build_table(lyt, df)
#>                     A          B    
#> ————————————————————————————————————
#> Sum                 2          1    
#> Sum (unique)        1          1    
#> Ratio            2 (5.0%)   1 (6.7%)
#> Ratio (unique)   1 (3.3%)   1 (6.7%)
```
