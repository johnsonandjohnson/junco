# Formatted Analysis Function for Comparative Statistic in Response Tables (RESP01)

This function applies to a `logical` column called `.var` from `df`. The
response proportion is compared between the treatment arms identified by
column `arm`.

## Usage

``` r
resp01_a_comp_stat_logical(
  df,
  .var,
  conf_level,
  include,
  arm,
  strata,
  formats,
  methods,
  stat = c("comp_stat_ci", "pval")
)
```

## Arguments

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- .var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- include:

  (`flag`)  
  whether to include the results for this variable.

- arm:

  (`string`)  
  column name in the data frame that identifies the treatment arms.

- strata:

  (`character` or `NULL`)  
  variable names indicating stratification factors.

- formats:

  (`list`)  
  containing formats for `comp_stat_ci` and `pval`.

- methods:

  (`list`)  
  containing methods for comparative statistics. The element
  `comp_stat_ci` can be 'rr' (relative risk), 'or_cmh' (odds ratio with
  CMH estimation and p-value) or 'or_logistic' (odds ratio estimated by
  conditional or standard logistic regression). The element `pval` can
  be 'fisher' (Fisher's exact test) or 'chisq' (chi-square test), only
  used when using unstratified analyses with 'or_logistic'.

- stat:

  (`string`)  
  the statistic to return, either `comp_stat_ci` or `pval`.

## Value

The formatted result as
[`rtables::rcell()`](https://insightsengineering.github.io/rtables/latest-tag/reference/rcell.html).

## See also

[`resp01_a_comp_stat_factor()`](https://johnsonandjohnson.github.io/junco/reference/resp01_a_comp_stat_factor.md)
for the `factor` equivalent.

## Examples

``` r
dm <- droplevels(subset(formatters::DM, SEX %in% c("F", "M")))
dm$RESP <- as.logical(sample(c(TRUE, FALSE), size = nrow(DM), replace = TRUE))

resp01_a_comp_stat_logical(
  dm,
  .var = "RESP",
  conf_level = 0.9,
  include = TRUE,
  arm = "SEX",
  strata = "RACE",
  stat = "comp_stat_ci",
  method = list(comp_stat_ci = "or_cmh"),
  formats = list(
    comp_stat_ci = jjcsformat_xx("xx.xx (xx.xx - xx.xx)"),
    pval = jjcsformat_pval_fct(0.05)
  )
)
#> rcell: 0.98 (0.69 - 1.38) 
```
