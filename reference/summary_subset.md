# Wrappers Around `tern::a_summary()` with Optional Subsetting and Labels

**\[stable\]**

These wrappers around
[`tern::a_summary()`](https://pharmaverse.github.io/tern/latest-tag/reference/analyze_variables.html)
allow optional row subsetting using a logical expression evaluated in
the context of the input data frame.

`a_summary_subset()` is an
[rtables](https://insightsengineering.github.io/rtables/latest-tag/reference/rtables-package.html)
analysis function (see
[`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html)),
while `c_summary_subset_label()` is a content row function (see
[`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html))
that may optionally append a label to the resulting content row.

## Usage

``` r
a_summary_subset(df, .var, filter_expr, ...)

c_summary_subset_label(
  df,
  labelstr,
  .var,
  filter_expr,
  label = "",
  label_indent_mod = 0L,
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

- filter_expr:

  (`expression`) A single logical expression wrapped in
  `expression(...)` used to subset rows of `df` before summarization.
  Exactly one expression is allowed. The expression is evaluated using
  [`eval()`](https://rdrr.io/r/base/eval.html) in an environment
  containing the columns of `df`, meaning that all column names are
  available as variables.

  In the current implementation, variables used in the expression can
  only be column names of `df`. That is, the following is not supported:
  `min_age <- 45;`
  `a_summary_subset(df, "AVAL", expression(AGE > min_age))`, given that
  `AVAL` is a column in `df` and `min_age` is not a column in `df`.

  The expression must evaluate to a logical vector whose length is
  exactly equal to `nrow(df)`. No recycling is performed. In particular,
  scalar logical values such as `expression(TRUE)` or
  `expression(FALSE)` are not allowed when `nrow(df) > 1`.

- ...:

  Additional arguments passed to
  [`tern::a_summary()`](https://pharmaverse.github.io/tern/latest-tag/reference/analyze_variables.html),
  excluding the `x` argument, which is constructed internally from
  `df[[.var]]` after applying the filter.

- labelstr:

  (`character`)  
  label of the level of the parent split currently being summarized
  (must be present as second argument in Content Row Functions). See
  [`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html)
  for more information.

- label:

  (`character(1)`)  
  Label to insert as the first row.

- label_indent_mod:

  (`integer(1)`)  
  Indentation modifier applied to the label row.

## Value

Returns the same type of object as
[`tern::a_summary()`](https://pharmaverse.github.io/tern/latest-tag/reference/analyze_variables.html).

## Details

[`tern::a_summary()`](https://pharmaverse.github.io/tern/latest-tag/reference/analyze_variables.html)
is a formatted analysis function that computes summary statistics for a
single variable. In typical use within the
[rtables](https://insightsengineering.github.io/rtables/latest-tag/reference/rtables-package.html)
framework, the analysis variable is first extracted from a data frame
and then passed to
[`tern::a_summary()`](https://pharmaverse.github.io/tern/latest-tag/reference/analyze_variables.html)
as a standalone vector.

The `a_summary_subset()` and `c_summary_subset_label()` wrappers extend
this behavior by:

- accepting a data frame (`df`) as input,

- optionally subsetting rows using a logical expression,

- extracting the analysis variable `.var` after subsetting,

- and passing the resulting vector to
  [`tern::a_summary()`](https://pharmaverse.github.io/tern/latest-tag/reference/analyze_variables.html).

## Functions

- `a_summary_subset()`: Analysis function with optional subsetting.

- `c_summary_subset_label()`: Content row function with optional
  subsetting and an optional label prepended to the resulting content
  row using
  [`prepend_label_cell()`](https://johnsonandjohnson.github.io/junco/reference/prepend_label_cell.md).

## See also

[`tern::a_summary()`](https://pharmaverse.github.io/tern/latest-tag/reference/analyze_variables.html)

## Author

WW

## Examples

``` r

## Analysis function:

df <- data.frame(
  ID = 1:8,
  AGE = c(34, NA, 52, 45, 50, NA, 41, 29),
  AVAL = c(1.2, 3, -1.5, 0.8, 3.5, 2, -1.1, 0.3)
)
df
#>   ID AGE AVAL
#> 1  1  34  1.2
#> 2  2  NA  3.0
#> 3  3  52 -1.5
#> 4  4  45  0.8
#> 5  5  50  3.5
#> 6  6  NA  2.0
#> 7  7  41 -1.1
#> 8  8  29  0.3

stats <- c("n", "sum")

tern::a_summary(df$AVAL, .stats = stats)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>   row_name formatted_cell indent_mod row_label
#> 1        n              8          0         n
#> 2      sum            8.2          0       Sum

a_summary_subset(df, "AVAL", expression(!is.na(AGE)), .stats = stats)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>   row_name formatted_cell indent_mod row_label
#> 1        n              6          0         n
#> 2      sum            3.2          0       Sum
a_summary_subset(df, "AVAL", expression(AGE > 30 & AVAL > 0), .stats = stats)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>   row_name formatted_cell indent_mod row_label
#> 1        n              3          0         n
#> 2      sum            5.5          0       Sum


## Content row function:

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

c_summary_subset_label(
  df = df,
  .var = "BASE",
  filter_expr = expression(ABLFL),
  label = "Baseline",
  .stats = c("n", "mean_sd")
)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>   row_name formatted_cell indent_mod row_label
#> 1  __label                         0  Baseline
#> 2        n              6          0         n
#> 3  mean_sd    16.3 (15.6)          0 Mean (SD)
```
