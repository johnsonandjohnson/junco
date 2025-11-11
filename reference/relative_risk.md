# Relative risk estimation

The analysis function `a_relative_risk()` is used to create a layout
element to estimate the relative risk for response within a studied
population. Only the CMH method is available currently. The primary
analysis variable, `vars`, is a logical variable indicating whether a
response has occurred for each record. A stratification variable must be
supplied via the `strata` element of the `variables` argument.

## Usage

``` r
a_relative_risk(
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

s_relative_risk(
  df,
  .var,
  .ref_group,
  .in_ref_col,
  variables = list(strata = NULL),
  conf_level = 0.95,
  method = "cmh",
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
  method to use for relative risk calculation.

- weights_method:

  (`string`)  
  method to use for weights calculation in stratified analysis.

## Value

- `a_relative_risk()` returns the corresponding list with formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

&nbsp;

- `s_relative_risk()` returns a named list of elements `rel_risk_ci` and
  `pval`.

## Details

The variance of the CMH relative risk estimate is calculated using the
Greenland and Robins (1985) variance estimation.

## Functions

- `a_relative_risk()`: Formatted analysis function which is used as
  `afun`. Note that the junco specific `ref_path` and `.spl_context`
  arguments are used for reference column information.

- `s_relative_risk()`: Statistics function estimating the relative risk
  for response.

## Note

This has been adapted from the `odds_ratio` functions in the `tern`
package.

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
    afun = a_relative_risk,
    extra_args = list(
      conf_level = 0.90,
      variables = list(strata = "f1"),
      ref_path = c("grp", "B")
    )
  )

build_table(l, df = dta)
#>                                    A            B
#> —————————————————————————————————————————————————
#>   Relative risk (90% CI)   1.14 (0.79 - 1.65)    
#>   p-value                        0.548           
nex <- 100
dta <- data.frame(
  "rsp" = sample(c(TRUE, FALSE), nex, TRUE),
  "grp" = sample(c("A", "B"), nex, TRUE),
  "f1" = sample(c("a1", "a2"), nex, TRUE),
  "f2" = sample(c("x", "y", "z"), nex, TRUE),
  stringsAsFactors = TRUE
)

s_relative_risk(
  df = subset(dta, grp == "A"),
  .var = "rsp",
  .ref_group = subset(dta, grp == "B"),
  .in_ref_col = FALSE,
  variables = list(strata = c("f1", "f2")),
  conf_level = 0.90
)
#> $rel_risk_ci
#>       est       lcl       ucl 
#> 1.0317456 0.7006869 1.5192220 
#> attr(,"label")
#> [1] "Relative risk (90% CI)"
#> 
#> $pval
#> [1] 0.8962701
#> 
```
