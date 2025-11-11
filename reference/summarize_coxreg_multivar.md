# Layout Generating Function for TEFOS03 and Related Cox Regression Layouts

Layout Generating Function for TEFOS03 and Related Cox Regression
Layouts

## Usage

``` r
summarize_coxreg_multivar(
  lyt,
  var,
  variables,
  control = control_coxreg(),
  formats = list(coef_se = jjcsformat_xx("xx.xx (xx.xx)"), hr_est =
    jjcsformat_xx("xx.xx"), hr_ci = jjcsformat_xx("(xx.xx, xx.xx)"), pval =
    jjcsformat_pval_fct(0))
)
```

## Arguments

- lyt:

  (`layout`)  
  input layout where analyses will be added to.

- var:

  (`string`)  
  any variable from the data, because this is not used.

- variables:

  (named `list` of `string`)  
  list of additional analysis variables.

- control:

  (`list`)  
  relevant list of control options.

- formats:

  (named `character` or `list`)  
  formats for the statistics. See Details in `analyze_vars` for more
  information on the `'auto'` setting.

## Value

`lyt` modified to add the desired cox regression table section.

## Examples

``` r
anl <- tern::tern_ex_adtte |>
  dplyr::mutate(EVENT = 1 - CNSR)

variables <- list(
  time = "AVAL",
  event = "EVENT",
  arm = "ARM",
  covariates = c("SEX", "AGE")
)

basic_table() |>
  summarize_coxreg_multivar(
    var = "STUDYID",
    variables = variables
  ) |>
  build_table(df = anl)
#>                                                    Model Fit               Hazard Ratio      
#>                                              Coeff. (SE)    p-value   Estimate      95% CI   
#> —————————————————————————————————————————————————————————————————————————————————————————————
#> Model Parameter                                                                              
#>   Treatment (B: Placebo vs. A: Drug X)       0.42 (0.09)    <0.001      1.52     (1.26, 1.83)
#>   Treatment (C: Combination vs. A: Drug X)   0.66 (0.10)    <0.001      1.93     (1.59, 2.34)
#>   Sex (M vs. F)                              -0.13 (0.08)    0.098      0.88     (0.76, 1.02)
#>   Age                                        0.00 (0.01)     0.645      1.00     (0.99, 1.01)
```
