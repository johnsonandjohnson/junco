# Calculate results across binary-imputation scenarios

Runs
[`h_impute_analyze_resp()`](https://johnsonandjohnson.github.io/junco/reference/h_impute_analyze_resp.md)
for each paired control and treatment response probability, pools
estimates across imputations using Rubin's rules, and pools
Wilson-Hilferty z statistics to obtain a p-value. A scenario where both
probabilities are zero or one is deterministic and is therefore analyzed
once rather than repeatedly imputed.

## Usage

``` r
resp_multiple_imputation(
  dat,
  p_ctrl,
  p_trt,
  trtvar,
  ctrlab,
  trtlab,
  respvar,
  stratvar,
  n_imputations,
  pvalcat
)
```

## Arguments

- dat:

  (`data.frame`) Analysis data containing treatment, response, and
  stratification variables.

- p_ctrl:

  (`numeric`) Control-group response probabilities, one for each
  scenario.

- p_trt:

  (`numeric`) Treatment-group response probabilities, paired with
  `p_ctrl` by position.

- trtvar:

  (`string`) Name of the treatment variable in `dat`.

- ctrlab:

  (`string`) Value of `trtvar` identifying the control group.

- trtlab:

  (`string`) Value of `trtvar` identifying the treatment group.

- respvar:

  (`string`) Name of a logical binary response variable in `dat`.

- stratvar:

  (`character`) Names of one or more stratification variables in `dat`.

- n_imputations:

  (`count`) Number of imputations for non-deterministic scenarios.

- pvalcat:

  (named `list`) P-value categories passed to
  [`categorize_pval()`](https://johnsonandjohnson.github.io/junco/reference/categorize_pval.md).

## Value

A tibble with one row per scenario. `rd` and `rd_se` are the pooled
risk-difference estimate and standard error; `p_value` and `p_cat` are
the pooled p-value and its category. `marker_flag` and `effect_label`
contain `"*"` when a deterministic scenario or a single-imputation
p-value fallback was used.

## Examples

``` r
dat <- data.frame(
  arm = rep(c("Control", "Treatment"), each = 4),
  response = c(TRUE, FALSE, NA, TRUE, TRUE, FALSE, NA, FALSE),
  strata = rep(c("A", "B"), 4)
)
pvalcat <- list("<0.05" = c(0, 0.05), ">=0.05" = c(0.05, 1))

set.seed(123)
resp_multiple_imputation(
  dat,
  p_ctrl = c(0, 0.25),
  p_trt = c(1, 0.75),
  trtvar = "arm",
  ctrlab = "Control",
  trtlab = "Treatment",
  respvar = "response",
  stratvar = "strata",
  n_imputations = 5,
  pvalcat = pvalcat
)
#> Warning: Less than 5 observations in some strata.
#> Warning: <5 data points in some strata. CMH test may be incorrect.
#> Warning: Less than 5 observations in some strata.
#> Warning: <5 data points in some strata. CMH test may be incorrect.
#> Warning: Less than 5 observations in some strata.
#> Warning: <5 data points in some strata. CMH test may be incorrect.
#> Warning: Less than 5 observations in some strata.
#> Warning: <5 data points in some strata. CMH test may be incorrect.
#> Warning: Less than 5 observations in some strata.
#> Warning: <5 data points in some strata. CMH test may be incorrect.
#> Warning: Less than 5 observations in some strata.
#> Warning: <5 data points in some strata. CMH test may be incorrect.
#> # A tibble: 2 × 9
#>   p_ctrl p_trt     m    rd rd_se p_value p_cat  marker_flag effect_label
#>    <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl> <chr>  <chr>       <chr>       
#> 1   0     1        1   0    0.25   1     >=0.05 "*"         0.0%*       
#> 2   0.25  0.75     5  -0.2  0.3    0.951 >=0.05 ""          -20.0%      
```
