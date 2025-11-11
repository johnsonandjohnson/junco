# Extract Estimates from Multivariate Cox Regression Model Fit Object

Extract Estimates from Multivariate Cox Regression Model Fit Object

## Usage

``` r
h_extract_coxreg_multivar(x)
```

## Arguments

- x:

  (`coxreg.multivar`)  
  from
  [`tern::fit_coxreg_multivar()`](https://insightsengineering.github.io/tern/latest-tag/reference/fit_coxreg.html).

## Value

A data frame containing Cox regression results with columns for term,
coef_se (coefficient and standard error), p.value, hr (hazard ratio),
hr_ci (confidence interval for hazard ratio), and labels (formatted term
labels).

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

control <- tern::control_coxreg(
  conf_level = 0.9,
  ties = "efron"
)

fit <- tern::fit_coxreg_multivar(
  data = anl,
  variables = variables,
  control = control
)

h_extract_coxreg_multivar(fit)
#> # A tibble: 4 × 6
#>   term              coef_se    p.value    hr hr_ci     labels                   
#>   <chr>             <list>       <dbl> <dbl> <list>    <chr>                    
#> 1 ARMB: Placebo     <dbl [2]> 8.89e- 6 1.52  <dbl [2]> Treatment (B: Placebo vs…
#> 2 ARMC: Combination <dbl [2]> 4.24e-11 1.92  <dbl [2]> Treatment (C: Combinatio…
#> 3 SEXM              <dbl [2]> 9.80e- 2 0.881 <dbl [2]> Sex (M vs. F)            
#> 4 AGE               <dbl [2]> 6.42e- 1 1.00  <dbl [2]> Age                      
```
