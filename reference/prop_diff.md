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
  .stats = NULL,
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL
)

s_proportion_diff_j(
  df,
  .var,
  .ref_group,
  .in_ref_col,
  variables = list(strata = NULL),
  conf_level = 0.95,
  method = c("waldcc", "wald", "cmh", "ha", "newcombe", "newcombecc", "strat_newcombe",
    "strat_newcombecc"),
  weights_method = "cmh"
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

- .ref_group:

  (`data.frame`)  
  reference group data frame.

- .in_ref_col:

  (`logical`)  
  whether the current column is the reference column.

- variables:

  (`list`)  
  list with strata variable names.

- conf_level:

  (`numeric`)  
  confidence level for the confidence interval.

- method:

  (`string`)  
  method to use for confidence interval calculation.

- weights_method:

  (`string`)  
  method to use for weights calculation in stratified analysis.

## Value

- `a_proportion_diff_j()` returns the corresponding list with formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

&nbsp;

- `s_proportion_diff_j()` returns a named list of elements `diff`,
  `diff_ci`, `diff_est_ci` and `diff_ci_3d`.

## Functions

- `a_proportion_diff_j()`: Formatted analysis function which is used as
  `afun` in
  [`estimate_proportion_diff()`](https://insightsengineering.github.io/tern/latest-tag/reference/prop_diff.html).

- `s_proportion_diff_j()`: Statistics function estimating the difference
  in terms of responder proportion.

## Note

The `a_proportion_diff_j()` function has the `_j` suffix to distinguish
it from
[`tern::a_proportion_diff()`](https://insightsengineering.github.io/tern/latest-tag/reference/prop_diff.html).
The functions here are a copy from the `tern` package with additional
features:

- Additional statistic `diff_est_ci` is returned.

- `ref_path` needs to be provided as extra argument to specify the
  control group column.

When performing an unstratified analysis, methods `'cmh'`,
`'strat_newcombe'`, and `'strat_newcombecc'` are not permitted.

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

s_proportion_diff_j(
  df = subset(dta, grp == "A"),
  .var = "rsp",
  .ref_group = subset(dta, grp == "B"),
  .in_ref_col = FALSE,
  conf_level = 0.90,
  method = "ha"
)
#> $diff
#> diff_ha 
#>      12 
#> attr(,"label")
#> [1] "Difference in Response rate (%)"
#> 
#> $diff_ci
#> diff_ci_ha_l diff_ci_ha_u 
#>    -5.374519    29.374519 
#> attr(,"label")
#> [1] "90% CI (Anderson-Hauck)"
#> 
#> $diff_est_ci
#>      diff_ha diff_ci_ha_l diff_ci_ha_u 
#>    12.000000    -5.374519    29.374519 
#> attr(,"label")
#> [1] "% Difference (90% CI)"
#> 
#> $diff_ci_3d
#>      diff_ha diff_ci_ha_l diff_ci_ha_u 
#>    12.000000    -5.374519    29.374519 
#> attr(,"label")
#> [1] "Relative Risk (90% CI)"
#> 

s_proportion_diff_j(
  df = subset(dta, grp == "A"),
  .var = "rsp",
  .ref_group = subset(dta, grp == "B"),
  .in_ref_col = FALSE,
  variables = list(strata = c("f1", "f2")),
  conf_level = 0.90,
  method = "cmh"
)
#> $diff
#> diff_cmh 
#> 12.27932 
#> attr(,"label")
#> [1] "Difference in Response rate (%)"
#> 
#> $diff_ci
#> diff_ci_cmh_l diff_ci_cmh_u 
#>     -2.657093     27.215725 
#> attr(,"label")
#> [1] "90% CI (CMH, without correction)"
#> 
#> $diff_est_ci
#>      diff_cmh diff_ci_cmh_l diff_ci_cmh_u 
#>     12.279316     -2.657093     27.215725 
#> attr(,"label")
#> [1] "% Difference (90% CI)"
#> 
#> $diff_ci_3d
#>      diff_cmh diff_ci_cmh_l diff_ci_cmh_u 
#>     12.279316     -2.657093     27.215725 
#> attr(,"label")
#> [1] "Relative Risk (90% CI)"
#> 
```
