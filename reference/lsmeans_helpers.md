# Helpers for Processing Least Square Means

Helpers for Processing Least Square Means

## Usage

``` r
h_get_emmeans_res(fit, vars, weights)

h_get_average_visit_specs(emmeans_res, vars, averages, fit)

h_get_spec_visit_estimates(emmeans_res, specs, conf_level, tests = FALSE, ...)

h_get_single_visit_estimates(emmeans_res, conf_level)

h_get_relative_reduc_df(estimates, vars)

h_single_visit_contrast_specs(emmeans_res, vars)

h_average_visit_contrast_specs(specs, averages)
```

## Arguments

- fit:

  result of model fitting function, e.g.
  [`mmrm::mmrm()`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html)
  or [`stats::lm()`](https://rdrr.io/r/stats/lm.html).

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

- weights:

  (`string`)  
  argument from
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html),
  'counterfactual' by default.

- emmeans_res:

  (`list`)  
  initial `emmeans` result from `h_get_emmeans_res()`.

- averages:

  (`list`)  
  optional named list of visit levels which should be averaged and
  reported along side the single visits.

- specs:

  (`list`)  
  list of least square means specifications, with elements `coefs`
  (coefficient list) and `grid` (corresponding `data.frame`).

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- tests:

  (`flag`)  
  whether to add test results to the estimates.

- ...:

  additional arguments for
  [`emmeans::contrast()`](https://rvlenth.github.io/emmeans/reference/contrast.html).

- estimates:

  (`data.frame`)  
  single visit least square mean estimates.

## Functions

- `h_get_emmeans_res()`: returns a list with `object` (`emmGrid` object
  containing `emmeans` results) and `grid` (`data.frame` containing the
  potential arm and the visit variables together with the sample size
  `n` for each combination).

- `h_get_average_visit_specs()`: constructs average of visits
  specifications.

- `h_get_spec_visit_estimates()`: estimates least square means as a
  `data.frame` given specifications.

- `h_get_single_visit_estimates()`: estimates least square means for
  single visits.

- `h_get_relative_reduc_df()`: constructs `data.frame` with relative
  reduction vs. reference arm based on single visit estimates.

- `h_single_visit_contrast_specs()`: constructs single visit contrast
  specifications.

- `h_average_visit_contrast_specs()`: constructs average visits contrast
  specifications, given the `specs` for single visit contrasts and the
  averages required.

## Note

The difference here compared to the original
tern.mmrm::h_get_spec_visit_estimates() function is that additional
arguments for
[`emmeans::contrast()`](https://rvlenth.github.io/emmeans/reference/contrast.html)
can be passed via the Once this has been added to the `tern.mmrm`
package then its functions can be used instead.
