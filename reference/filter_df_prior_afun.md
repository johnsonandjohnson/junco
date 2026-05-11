# Filter Data Prior To Analysis Function

**\[experimental\]**

Applies row filtering to a dataset before executing a user-supplied
analysis function.

## Usage

``` r
filter_df_prior_afun(
  df,
  .var,
  afun,
  subset_expr = expression(rep(TRUE, nrow(df))),
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

- afun:

  (`function`)  
  Analysis function. Must accept `x` or `df` as its first parameter. Can
  optionally take other parameters.

- subset_expr:

  (`expression` or `NULL`)  
  Logical expression used to subset rows of `df` before analysis.
  Evaluated in the context of `df`. Defaults to
  `expression(rep(TRUE, nrow(df)))`, meaning no filtering.

- ...:

  Additional arguments passed to `afun`.

## Value

The object returned by `afun` applied to the filtered dataset.

## Details

This is a generic wrapper that:

1.  Subsets `df` using `subset_expr`.

2.  Passes data to `afun`, depending on its first argument. If it is
    named:

    - `x`, then `df[[.var]]` is passed.

    - `df`, then the `df` data frame is passed.

3.  Forwards `.var` (if it is present in the formal arguments of `afun`)
    and all additional arguments (`...`) to `afun`.

## Examples

``` r
df <- data.frame(
  USUBJID = rep(1:6, each = 2),
  AVISIT = rep(c("Baseline", "Day 1"), 6),
  AVAL = c(1, 3, 2, 9, 13, 19, 15, 23, 43, 56, 24, 32),
  ABLFL = rep(c(TRUE, FALSE), 6),
  BASE = rep(c(1, 2, 13, 15, 43, 24), each = 2),
  CHG = c(0, 2, 0, 7, 0, 6, 0, 8, 0, 13, 0, 8)
)
df
#>    USUBJID   AVISIT AVAL ABLFL BASE CHG
#> 1        1 Baseline    1  TRUE    1   0
#> 2        1    Day 1    3 FALSE    1   2
#> 3        2 Baseline    2  TRUE    2   0
#> 4        2    Day 1    9 FALSE    2   7
#> 5        3 Baseline   13  TRUE   13   0
#> 6        3    Day 1   19 FALSE   13   6
#> 7        4 Baseline   15  TRUE   15   0
#> 8        4    Day 1   23 FALSE   15   8
#> 9        5 Baseline   43  TRUE   43   0
#> 10       5    Day 1   56 FALSE   43  13
#> 11       6 Baseline   24  TRUE   24   0
#> 12       6    Day 1   32 FALSE   24   8

afun <- tern::a_summary
.stats <- c("n", "mean_sd")

# No filtering.
filter_df_prior_afun(df, "CHG", afun, .stats = .stats)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>   row_name formatted_cell indent_mod row_label
#> 1        n             12          0         n
#> 2  mean_sd      3.7 (4.5)          0 Mean (SD)

# Baseline records only.
filter_df_prior_afun(df, "CHG", afun, expression(ABLFL), .stats = .stats)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>   row_name formatted_cell indent_mod row_label
#> 1        n              6          0         n
#> 2  mean_sd      0.0 (0.0)          0 Mean (SD)
```
