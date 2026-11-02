# Relative Risk CMH Statistic

Calculates the relative risk which is defined as the ratio between the
response rates between the experimental treatment group and the control
treatment group, adjusted for stratification factors by applying
Cochran-Mantel-Haenszel (CMH) weights.

## Usage

``` r
prop_ratio_cmh(tbl, conf_level = 0.95)
```

## Arguments

- tbl:

  (`array`)  
  a 2 by 2 by number of strata contingency table, as returned in the
  `tbl` element of
  [`tern::h_prepare_rsp_table()`](https://pharmaverse.github.io/tern/latest-tag/reference/h_prepare_rsp_table.html).
  Rows (first dimension) are reference then treatment; columns (second
  dimension) are responders then nonresponders; the third dimension
  represents the strata.

- conf_level:

  (`proportion`)  
  confidence level of the interval.

## Value

A list with elements `rel_risk_ci` and `pval`.

## Examples

``` r

set.seed(2)
rsp <- factor(
  sample(c(TRUE, FALSE), 100, TRUE),
  levels = c(TRUE, FALSE)
)
grp <- sample(c("Placebo", "Treatment"), 100, TRUE)
grp <- factor(grp, levels = c("Placebo", "Treatment"))
strata_data <- data.frame(
  "f1" = sample(c("a", "b"), 100, TRUE),
  "f2" = sample(c("x", "y", "z"), 100, TRUE),
  stringsAsFactors = TRUE
)

tbl <- table(grp, rsp, interaction(strata_data))
prop_ratio_cmh(
  tbl = tbl,
  conf_level = 0.90
)
#> $rel_risk_ci
#>       est       lcl       ucl 
#> 0.7417303 0.4907583 1.1210485 
#> 
#> $pval
#> [1] 0.1892073
#> 
```
