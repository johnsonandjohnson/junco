# Statistics within the column space

A function factory used for obtaining statistics within the columns of
your table. Used in change from baseline tables. This takes the visit
names as its row labels.

## Usage

``` r
column_stats(
  exclude_visits = c("Baseline (DB)"),
  var_names = c("AVAL", "CHG", "BASE"),
  stats = list(main = c(N = "N", mean = "Mean", SD = "SD", SE = "SE", Med = "Med", Min =
    "Min", Max = "Max"), base = c(mean = "Mean"))
)
```

## Arguments

- exclude_visits:

  Vector of visit(s) for which you do not want the statistics displayed
  in the baseline mean or change from baseline sections of the table.

- var_names:

  Vector of variable names to use instead of the default AVAL, CHG,
  BASE. The first two elements are treated as main variables with full
  statistics, and the third element is treated as the base variable. By
  default, the function expects these specific variable names in your
  data, but you can customize them to match your dataset's column names.

- stats:

  A list with two components, `main` and `base`, that define the
  statistics to be calculated for the main variables (default: AVAL,
  CHG) and the base variable (default: BASE). Default for main
  variables: c(N = "N", mean = "Mean", SD = "SD", SE = "SE", Med =
  "Med", Min = "Min", Max = "Max") Default for base variable: c(mean =
  "Mean") You can customize these statistics by providing your own named
  vectors in the list. The names are used internally for calculations,
  and the values are used as display labels in the table.

## Value

an analysis function (for use with
[rtables::analyze](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html))
implementing the specified statistics.
