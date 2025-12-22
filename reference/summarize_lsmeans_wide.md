# Layout Generating Function for LS Means Wide Table Layouts

Layout Generating Function for LS Means Wide Table Layouts

## Usage

``` r
summarize_lsmeans_wide(
  lyt,
  variables,
  ref_level,
  treatment_levels,
  conf_level,
  pval_sided = "2",
  include_variance = TRUE,
  include_pval = TRUE,
  formats = list(lsmean = jjcsformat_xx("xx.x"), mse = jjcsformat_xx("xx.x"), df =
    jjcsformat_xx("xx."), lsmean_diff = jjcsformat_xx("xx.x"), se =
    jjcsformat_xx("xx.xx"), ci = jjcsformat_xx("(xx.xx, xx.xx)"), pval =
    jjcsformat_pval_fct(0))
)
```

## Arguments

- lyt:

  (`layout`)  
  empty layout, i.e. result of
  [`rtables::basic_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/basic_table.html)

- variables:

  (named `list` of `string`)  
  list of additional analysis variables.

- ref_level:

  (`string`)  
  the reference level of the treatment arm variable.

- treatment_levels:

  (`character`)  
  the non-reference levels of the treatment arm variable.

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- pval_sided:

  (`string`)  
  either '2' for two-sided or '1' for 1-sided with greater than control
  or '-1' for 1-sided with smaller than control alternative hypothesis.

- include_variance:

  (`flag`)  
  whether to include the variance statistics (M.S. error and d.f.).

- include_pval:

  (`flag`)  
  whether to include the p-value column.

- formats:

  (named `character` or `list`)  
  formats for the statistics. See Details in `analyze_vars` for more
  information on the `'auto'` setting.

## Value

Modified layout.

## Examples

``` r
variables <- list(
  response = "FEV1",
  covariates = c("RACE", "SEX"),
  arm = "ARMCD",
  id = "USUBJID",
  visit = "AVISIT"
)
fit <- fit_ancova(
  vars = variables,
  data = mmrm::fev_data,
  conf_level = 0.9,
  weights_emmeans = "equal"
)
anl <- broom::tidy(fit)
basic_table() |>
  summarize_lsmeans_wide(
    variables = variables,
    ref_level = fit$ref_level,
    treatment_levels = fit$treatment_levels,
    pval_sided = "2",
    conf_level = 0.8
  ) |>
  build_table(df = anl)
#>                Reference Group                    Testing Group                                                        Testing - Reference                   
#>        Treatment   N    LS Mean    SE    Treatment   N    LS Mean    SE    M. S. Error   Error DF   LS Mean    SE       80% CI      2-sided p-value~[super a]
#> —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> VIS1      PBO      68    33.2     0.82      TRT      66    36.8     0.79      41.2         129        3.7     1.13   (1.80, 5.56)             0.001          
#> 
#> VIS2      PBO      69    38.0     0.63      TRT      71    42.3     0.61      26.0         135        4.2     0.88   (2.79, 5.70)            <0.001          
#> 
#> VIS3      PBO      71    43.8     0.47      TRT      58    46.8     0.52      15.0         124        3.1     0.70   (1.89, 4.22)            <0.001          
#> 
#> VIS4      PBO      67    48.7     1.22      TRT      67    52.5     1.19      94.6         129        3.9     1.70   (1.07, 6.71)             0.024          
```
