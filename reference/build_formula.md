# Building Model Formula

This builds the model formula which is used inside
[`fit_mmrm_j()`](https://johnsonandjohnson.github.io/junco/reference/fit_mmrm_j.md)
and provided to
[`mmrm::mmrm()`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html)
internally. It can be instructive to look at the resulting formula
directly sometimes.

## Usage

``` r
build_formula(
  vars,
  cor_struct = c("unstructured", "toeplitz", "heterogeneous toeplitz", "ante-dependence",
    "heterogeneous ante-dependence", "auto-regressive", "heterogeneous auto-regressive",
    "compound symmetry", "heterogeneous compound symmetry")
)
```

## Arguments

- vars:

  (`list`)  
  variables to use in the model.

- cor_struct:

  (`string`)  
  specify the covariance structure to use.

## Value

Formula to use in
[`mmrm::mmrm()`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html).

## Examples

``` r
vars <- list(
  response = "AVAL", covariates = c("RACE", "SEX"),
  id = "USUBJID", arm = "ARMCD", visit = "AVISIT"
)
build_formula(vars, "auto-regressive")
#> AVAL ~ RACE + SEX + ARMCD * AVISIT + ar1(AVISIT | USUBJID)
#> <environment: 0x56113ae946e0>
build_formula(vars)
#> AVAL ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
#> <environment: 0x56113ae5ce18>
```
