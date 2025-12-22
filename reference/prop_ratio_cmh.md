# Relative Risk CMH Statistic

Calculates the relative risk which is defined as the ratio between the
response rates between the experimental treatment group and the control
treatment group, adjusted for stratification factors by applying
Cochran-Mantel-Haenszel (CMH) weights.

## Usage

``` r
prop_ratio_cmh(rsp, grp, strata, conf_level = 0.95)
```

## Arguments

- rsp:

  (`logical`)  
  whether each subject is a responder or not.

- grp:

  (`factor`)  
  defining the groups.

- strata:

  (`factor`)  
  variable with one level per stratum and same length as `rsp`.

- conf_level:

  (`proportion`)  
  confidence level of the interval.

## Value

A list with elements `rel_risk_ci` and `pval`.

## Examples

``` r
set.seed(2)
rsp <- sample(c(TRUE, FALSE), 100, TRUE)
grp <- sample(c("Placebo", "Treatment"), 100, TRUE)
grp <- factor(grp, levels = c("Placebo", "Treatment"))
strata_data <- data.frame(
  "f1" = sample(c("a", "b"), 100, TRUE),
  "f2" = sample(c("x", "y", "z"), 100, TRUE),
  stringsAsFactors = TRUE
)

prop_ratio_cmh(
  rsp = rsp, grp = grp, strata = interaction(strata_data),
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
