# Utilities for the Current Column Split Path

**\[stable\]**

These helper functions are intended for use in
[rtables](https://insightsengineering.github.io/rtables/latest-tag/reference/rtables-package.html)
custom analysis functions that depend on the current column split
context.

## Usage

``` r
cur_col_split_path(.spl_context)

in_column(col_path, .spl_context)
```

## Arguments

- .spl_context:

  (`data.frame`)  
  gives information about ancestor split states that is passed by
  `rtables`.

- col_path:

  (`character` or `NULL`)  
  Column path specified as alternating split variable names and split
  values. Must have at least two elements and an even length, e.g.
  `c("ARM", "Placebo")` or `c("ARM", "*", "desc_stat", "N")`; see
  [`rtables::col_paths()`](https://insightsengineering.github.io/rtables/latest-tag/reference/make_col_row_df.html).

  The special value `"*"` can be used as a wildcard to match any split
  variable name or value.

  `NULL` can be used to indicate that no path is specified, in which
  case the function returns `FALSE`, regardless of `.spl_context`.

## Value

- `cur_col_split_path()` returns a character vector containing the
  current column split path extracted from `.spl_context`, interleaved
  as `c(var1, val1, var2, val2, ...)`.

&nbsp;

- `in_column()` returns a single logical value indicating whether the
  current column split matches the specified `col_path`.

## Functions

- `cur_col_split_path()`: Get the current column split path.

- `in_column()`: Determine whether a given column path matches the
  current column split path.

## See also

- [`rtables::col_paths()`](https://insightsengineering.github.io/rtables/latest-tag/reference/make_col_row_df.html)
  for obtaining column paths from a built table.

## Author

WW

## Examples

``` r
.spl_context_1 <- data.frame(
  cur_col_split = I(list(c("ARM"))),
  cur_col_split_val = I(list(c("Placebo")))
)

.spl_context_2 <- data.frame(
  cur_col_split = I(list(c("ARM", "desc_stat"))),
  cur_col_split_val = I(list(c("Placebo", "N")))
)

cur_col_split_path(.spl_context_1)
#> [1] "ARM"     "Placebo"
cur_col_split_path(.spl_context_2)
#> [1] "ARM"       "Placebo"   "desc_stat" "N"        

in_column(c("ARM", "Placebo"), .spl_context_1)
#> [1] TRUE
in_column(c("ARM", "*"), .spl_context_1)
#> [1] TRUE
in_column(c("ARM", "X"), .spl_context_1)
#> [1] FALSE

in_column(c("ARM", "*", "desc_stat", "N"), .spl_context_2)
#> [1] TRUE
in_column(c("ARM", "*", "desc_stat", "Mean"), .spl_context_2)
#> [1] FALSE

# Use in an rtables custom analysis function.

data <- formatters::ex_adsl
data$desc_stat <- "phantom"

# analysis function
a_N_mean <- function(x, .spl_context) {
  if (in_column(c("ARM", "*", "desc_stat", "N"), .spl_context)) {
    rcell(length(x))
  } else if (in_column(c("ARM", "*", "desc_stat", "Mean"), .spl_context)) {
    rcell(mean(x), format = "xx.x")
  } else {
    rcell(NULL)
  }
}

combodf <- data.frame(
  valname = c("N", "Mean"),
  label = c("N", "Mean"),
  levelcombo = I(list(select_all_levels, select_all_levels)),
  exargs = I(list(list(), list()))
)

lyt <- basic_table() |>
  split_cols_by("ARM") |>
  split_cols_by(
    "desc_stat",
    split_fun = add_combo_levels(combodf, keep_levels = c("N", "Mean"))
  ) |>
  analyze("AGE", afun = a_N_mean)

tbl <- build_table(lyt, data)
tbl
#>        A: Drug X      B: Placebo      C: Combination  
#>        N     Mean      N     Mean       N       Mean  
#> ——————————————————————————————————————————————————————
#> AGE   134    33.8     134    35.4      132      35.4  
```
