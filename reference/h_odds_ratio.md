# Helper functions for odds ratio estimation

**\[stable\]**

Functions to calculate odds ratios in
[`s_odds_ratio_j()`](https://johnsonandjohnson.github.io/junco/reference/odds_ratio.md).

## Usage

``` r
or_glm_j(data, conf_level)

or_clogit_j(data, conf_level, method = "exact")

or_cmh(data, conf_level)
```

## Arguments

- data:

  (`data.frame`)  
  data frame containing at least the variables `rsp` and `grp`, and
  optionally `strata` for `or_clogit_j()`.

- conf_level:

  (`numeric`)  
  confidence level for the confidence interval.

- method:

  (`string`)  
  whether to use the correct (`'exact'`) calculation in the conditional
  likelihood or one of the approximations, or the CMH method. See
  [`survival::clogit()`](https://rdrr.io/pkg/survival/man/clogit.html)
  for details.

## Value

A named `list` of elements `or_ci`, `n_tot` and `pval`.

## Functions

- `or_glm_j()`: Estimates the odds ratio based on
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html). Note that there
  must be exactly 2 groups in `data` as specified by the `grp` variable.

- `or_clogit_j()`: Estimates the odds ratio based on
  [`survival::clogit()`](https://rdrr.io/pkg/survival/man/clogit.html).
  This is done for the whole data set including all groups, since the
  results are not the same as when doing pairwise comparisons between
  the groups.

- `or_cmh()`: Estimates the odds ratio based on CMH. Note that there
  must be exactly 2 groups in `data` as specified by the `grp` variable.

## See also

[odds_ratio](https://johnsonandjohnson.github.io/junco/reference/odds_ratio.md)

## Examples

``` r
data <- data.frame(
  rsp = as.logical(c(1, 1, 0, 1, 0, 0, 1, 1)),
  grp = letters[c(1, 1, 1, 2, 2, 2, 1, 2)],
  strata = letters[c(1, 2, 1, 2, 2, 2, 1, 2)],
  stringsAsFactors = TRUE
)

or_glm_j(data, conf_level = 0.95)
#> $or_ci
#>        est        lcl        ucl 
#> 0.33333333 0.01669735 6.65441589 
#> 
#> $n_tot
#> n_tot 
#>     8 
#> 
#> $pval
#> [1] 0.472011
#> 

data <- data.frame(
  rsp = as.logical(c(1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0)),
  grp = letters[c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3)],
  strata = LETTERS[c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)],
  stringsAsFactors = TRUE
)

or_clogit_j(data, conf_level = 0.95)
#> $or_ci_pvals
#> $or_ci_pvals$b
#>        est        lcl        ucl       pval 
#> 0.28814553 0.02981009 2.78522598 0.28237516 
#> 
#> $or_ci_pvals$c
#>       est       lcl       ucl      pval 
#> 0.5367919 0.0673365 4.2791881 0.5569374 
#> 
#> 
#> $n_tot
#> n_tot 
#>    20 
#> 

set.seed(123)
data <- data.frame(
  rsp = as.logical(rbinom(n = 40, size = 1, prob = 0.5)),
  grp = letters[sample(1:2, size = 40, replace = TRUE)],
  strata = LETTERS[sample(1:2, size = 40, replace = TRUE)],
  stringsAsFactors = TRUE
)

or_cmh(data, conf_level = 0.95)
#> $or_ci
#>       est       lcl       ucl 
#> 0.9969199 0.2877116 3.4543244 
#> 
#> $n_tot
#> n_tot 
#>    40 
#> 
#> $pval
#> [1] 0.9960178
#> 
```
