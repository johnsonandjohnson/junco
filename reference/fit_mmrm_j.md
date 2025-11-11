# `MMRM` Analysis

Does the `MMRM` analysis. Multiple other functions can be called on the
result to produce tables and graphs.

## Usage

``` r
fit_mmrm_j(
  vars = list(response = "AVAL", covariates = c(), id = "USUBJID", arm = "ARM", visit =
    "AVISIT"),
  data,
  conf_level = 0.95,
  cor_struct = "unstructured",
  weights_emmeans = "counterfactual",
  averages_emmeans = list(),
  ...
)
```

## Arguments

- vars:

  (named `list` of `string` or `character`)  
  specifying the variables in the `MMRM`. The following elements need to
  be included as character vectors and match corresponding columns in
  `data`:

  - `response`: the response variable.

  - `covariates`: the additional covariate terms (might also include
    interactions).

  - `id`: the subject ID variable.

  - `arm`: the treatment group variable (factor).

  - `visit`: the visit variable (factor).

  - `weights`: optional weights variable (if `NULL` or omitted then no
    weights will be used).

  Note that the main effects and interaction of `arm` and `visit` are by
  default included in the model.

- data:

  (`data.frame`)  
  with all the variables specified in `vars`. Records with missing
  values in any independent variables will be excluded.

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- cor_struct:

  (`string`)  
  specifying the covariance structure, defaults to `'unstructured'`. See
  the details.

- weights_emmeans:

  (`string`)  
  argument from
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html),
  `'counterfactual'` by default.

- averages_emmeans:

  (`list`)  
  optional named list of visit levels which should be averaged and
  reported along side the single visits.

- ...:

  additional arguments for
  [`mmrm::mmrm()`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html),
  in particular `reml` and options listed in
  [`mmrm::mmrm_control()`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm_control.html).

## Value

A `tern_model` object which is a list with model results:

- `fit`: The `mmrm` object which was fitted to the data. Note that via
  `mmrm::component(fit, 'optimizer')` the finally used optimization
  algorithm can be obtained, which can be useful for refitting the model
  later on.

- `cov_estimate`: The matrix with the covariance matrix estimate.

- `diagnostics`: A list with model diagnostic statistics (REML
  criterion, AIC, corrected AIC, BIC).

- `lsmeans`: This is a list with data frames `estimates` and
  `contrasts`. The attributes `averages` and `weights` save the settings
  used (`averages_emmeans` and `weights_emmeans`).

- `vars`: The variable list.

- `labels`: Corresponding list with variable labels extracted from
  `data`.

- `cor_struct`: input.

- `ref_level`: The reference level for the arm variable, which is always
  the first level.

- `treatment_levels`: The treatment levels for the arm variable.

- `conf_level`: The confidence level which was used to construct the
  `lsmeans` confidence intervals.

- `additional`: List with any additional inputs passed via `...`

## Details

Multiple different degree of freedom adjustments are available via the
`method` argument for
[`mmrm::mmrm()`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html).
In addition, covariance matrix adjustments are available via `vcov`.
Please see
[`mmrm::mmrm_control()`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm_control.html)
for details and additional useful options.

For the covariance structure (`cor_struct`), the user can choose among
the following options.

- `unstructured`: Unstructured covariance matrix. This is the most
  flexible choice and default. If there are `T` visits, then
  `T * (T+1) / 2` variance parameters are used.

- `toeplitz`: Homogeneous Toeplitz covariance matrix, which uses `T`
  variance parameters.

- `heterogeneous toeplitz`: Heterogeneous Toeplitz covariance matrix,
  which uses `2 * T - 1` variance parameters.

- `ante-dependence`: Homogeneous Ante-Dependence covariance matrix,
  which uses `T` variance parameters.

- `heterogeneous ante-dependence`: Heterogeneous Ante-Dependence
  covariance matrix, which uses `2 * T - 1` variance parameters.

- `auto-regressive`: Homogeneous Auto-Regressive (order 1) covariance
  matrix, which uses 2 variance parameters.

- `heterogeneous auto-regressive`: Heterogeneous Auto-Regressive
  (order 1) covariance matrix, which uses `T + 1` variance parameters.

- `compound symmetry`: Homogeneous Compound Symmetry covariance matrix,
  which uses 2 variance parameters.

- `heterogeneous compound symmetry`: Heterogeneous Compound Symmetry
  covariance matrix, which uses `T + 1` variance parameters.

## Note

This function has the `_j` suffix to distinguish it from
[`mmrm::fit_mmrm()`](https://openpharma.github.io/mmrm/latest-tag/reference/fit_mmrm.html).
It is a copy from the `tern.mmrm` package and later will be replaced by
tern.mmrm::fit_mmrm(). No new features are included in this function
here.

## Examples

``` r
mmrm_results <- fit_mmrm_j(
  vars = list(
    response = "FEV1",
    covariates = c("RACE", "SEX"),
    id = "USUBJID",
    arm = "ARMCD",
    visit = "AVISIT"
  ),
  data = mmrm::fev_data,
  cor_struct = "unstructured",
  weights_emmeans = "equal",
  averages_emmeans = list(
    "VIS1+2" = c("VIS1", "VIS2")
  )
)
```
