# Relative risk estimation

The analysis function `a_relative_risk()` is used to create a layout
element to estimate the relative risk for response within a studied
population using stratified CMH or unstratified Wald methods. The
primary analysis variable, `vars`, is a logical variable indicating
whether a response has occurred for each record. For CMH, a
stratification variable must be supplied via the `strata` element of the
`variables` argument.

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
  .ref_group = NULL,
  .in_ref_col = NULL,
  variables = list(strata = NULL),
  conf_level = 0.95,
  complete_cases = FALSE,
  method = c("cmh", "wald", "waldcc"),
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

  (`logical(1)`)  
  whether the current column is the reference column.

- variables:

  (`list`)  
  list with strata variable names.

- conf_level:

  (`numeric`)  
  confidence level for the confidence interval.

- complete_cases:

  (`logical(1)`)  
  whether to remove incomplete rows from `df` before constructing the
  response, group, strata, and contingency table.

- method:

  (`string`)  
  relative risk method: `cmh`, `wald`, or `waldcc`.

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
Greenland and Robins (1985) variance estimation. The unstratified `wald`
and `waldcc` methods use a normal approximation for the log relative
risk. `waldcc` adds 0.5 to each response count and each group total in
both the estimate and variance. With `wald`, a zero response count gives
undefined confidence limits and p-value (`NA`).

## Functions

- `a_relative_risk()`: Formatted analysis function which is used as
  `afun`. Note that the junco specific `ref_path` and `.spl_context`
  arguments are used for reference column information.

- `s_relative_risk()`: Statistics function estimating the relative risk
  for response.

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
#> ——————————————————————————————————————————————————
#>   Relative risk (90% CI)   1.40 (1.00 - 1.97)   NA
#>   p-value                        0.109          NA
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
#> 1.1893424 0.8714401 1.6232159 
#> attr(,"label")
#> [1] "Relative risk (90% CI)"
#> 
#> $pval
#> [1] 0.3714946
#> 
```
