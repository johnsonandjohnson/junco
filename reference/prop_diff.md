# Proportion difference estimation

The analysis function `a_proportion_diff_j()` can be used to create a
layout element to estimate the difference in proportion of responders
within a studied population. The primary analysis variable, `vars`, is a
logical variable indicating whether a response has occurred for each
record. See the `method` parameter for options of methods to use when
constructing the confidence interval of the proportion difference. A
stratification variable can be supplied via the `strata` element of the
`variables` argument.

## Usage

``` r
a_proportion_diff_j(
  df,
  .var,
  ref_path,
  .spl_context,
  ...,
  conf_level = 0.95,
  label = paste0("% Difference (", f_conf_level(conf_level), ")"),
  .stats = NULL,
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL
)
```

## Arguments

- df:

  (`data.frame`)  
  input data frame.

- .var:

  (`string`)  
  name of the response variable.

- ref_path:

  (`character`)  
  path to the reference group.

- .spl_context:

  (`environment`)  
  split context environment.

- ...:

  Additional arguments passed to the statistics function.

- conf_level:

  (`numeric`)  
  confidence level for the confidence interval.

- label:

  (`string`)  
  label for the `diff_est_ci` statistic in `a_proportion_diff_j()`.
  Defaults to `"% Difference (conf_level CI)"`. Can be overridden by the
  caller.

- .stats:

  (`character`)  
  statistics to calculate.

- .formats:

  (`list`)  
  formats for the statistics.

- .labels:

  (`list`)  
  labels for the statistics.

- .indent_mods:

  (`list`)  
  indentation modifications for the statistics.

## Value

- `a_proportion_diff_j()` returns the corresponding list with formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Functions

- `a_proportion_diff_j()`: Formatted analysis function which is used as
  `afun` in
  [`estimate_proportion_diff()`](https://pharmaverse.github.io/tern/latest-tag/reference/prop_diff.html).

## Note

The `a_proportion_diff_j()` function has the `_j` suffix to distinguish
it from
[`tern::a_proportion_diff()`](https://pharmaverse.github.io/tern/latest-tag/reference/prop_diff.html).
It is a thin wrapper around
[`tern::s_proportion_diff()`](https://pharmaverse.github.io/tern/latest-tag/reference/prop_diff.html)
with the following additions:

- The `diff_est_ci` statistic (returned natively by tern since
  tern#1523) is relabelled with a shorter junco-style label
  (`"% Difference (conf_level CI)"`).

- `ref_path` needs to be provided as extra argument to specify the
  control group column.

## Examples

``` r
nex <- 100
dta <- data.frame(
  "rsp" = sample(c(TRUE, FALSE), nex, TRUE),
  "grp" = sample(c("A", "B"), nex, TRUE),
  "f1" = sample(c("a1", "a2"), nex, TRUE),
  "f2" = sample(c("x", "y", "z"), nex, TRUE),
  stringsAsFactors = TRUE
)

l <- basic_table() |>
  split_cols_by(var = "grp") |>
  analyze(
    vars = "rsp",
    afun = a_proportion_diff_j,
    show_labels = "hidden",
    na_str = tern::default_na_str(),
    extra_args = list(
      conf_level = 0.9,
      method = "ha",
      ref_path = c("grp", "B")
    )
  )

build_table(l, df = dta)
#>                                           A           B
#> ———————————————————————————————————————————————————————
#> Difference in Response rate (%)         12.0           
#>   90% CI (Anderson-Hauck)           (-5.4, 29.4)       
#> % Difference (90% CI)             12.0 (-5.4, 29.4)    
```
