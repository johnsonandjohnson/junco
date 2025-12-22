# `ANCOVA` Analysis

Performs the `ANCOVA` analysis, separately for each visit.

## Usage

``` r
fit_ancova(
  vars = list(response = "AVAL", covariates = c(), arm = "ARM", visit = "AVISIT", id =
    "USUBJID"),
  data,
  conf_level = 0.95,
  weights_emmeans = "proportional"
)
```

## Arguments

- vars:

  (named `list` of `string` or `character`)  
  specifying the variables in the `ANCOVA` analysis. The following
  elements need to be included as character vectors and match
  corresponding columns in `data`:

  - `response`: the response variable.

  - `covariates`: the additional covariate terms (might also include
    interactions).

  - `id`: the subject ID variable (not really needed for the
    computations but for internal logistics).

  - `arm`: the treatment group variable (factor).

  - `visit`: the visit variable (factor).

  Note that the `arm` variable is by default included in the model, thus
  should not be part of `covariates`.

- data:

  (`data.frame`)  
  with all the variables specified in `vars`. Records with missing
  values in any independent variables will be excluded.

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- weights_emmeans:

  (`string`)  
  argument from
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html),
  `'counterfactual'` by default.

## Value

A `tern_model` object which is a list with model results:

- `fit`: A list with a fitted
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html) result for each
  visit.

- `mse`: Mean squared error, i.e. variance estimate, for each visit.

- `df`: Degrees of freedom for the variance estimate for each visit.

- `lsmeans`: This is a list with data frames `estimates` and
  `contrasts`. The attribute `weights` savse the settings used
  (`weights_emmeans`).

- `vars`: The variable list.

- `labels`: Corresponding list with variable labels extracted from
  `data`.

- `ref_level`: The reference level for the arm variable, which is always
  the first level.

- `treatment_levels`: The treatment levels for the arm variable.

- `conf_level`: The confidence level which was used to construct the
  `lsmeans` confidence intervals.

## Examples

``` r
library(mmrm)

fit <- fit_ancova(
  vars = list(
    response = "FEV1",
    covariates = c("RACE", "SEX"),
    arm = "ARMCD",
    id = "USUBJID",
    visit = "AVISIT"
  ),
  data = fev_data,
  conf_level = 0.9,
  weights_emmeans = "equal"
)
```
