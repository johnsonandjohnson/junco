# Relative Risk Wald Statistic

Estimates the relative response risk in the second group divided by that
in the first group, with log-scale Wald confidence limits and a
two-sided Wald p-value.

## Usage

``` r
prop_ratio_wald(tbl, conf_level = 0.95, correction = FALSE)
```

## Arguments

- tbl:

  (`matrix`)  
  a 2 by 2 contingency table, as returned in the `tbl` element of
  [`tern::h_prepare_rsp_table()`](https://pharmaverse.github.io/tern/latest-tag/reference/h_prepare_rsp_table.html).
  Rows are reference then treatment; columns are responders then
  nonresponders.

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- correction:

  (`flag`)  
  whether to add 0.5 to the response counts and group totals, in order
  to support the case where there are zero responses in either group
  (Haldane-Anscombe correction).

## Value

A `list` containing two elements:

- `rel_risk_ci`: A named numeric vector (`est`, `lcl`, `ucl`) specifying
  the point estimate, lower confidence limit, and upper confidence limit
  of the relative risk.

- `pval`: A numeric scalar representing the two-sided Wald p-value.

## Examples

``` r
prop_ratio_wald(
  tbl = rbind(ref = c(1, 1), trt = c(2, 1)),
  correction = TRUE
)
#> $rel_risk_ci
#>       est       lcl       ucl 
#> 1.1904762 0.3550976 3.9911101 
#> 
#> $pval
#> [1] 0.7775718
#> 
```
