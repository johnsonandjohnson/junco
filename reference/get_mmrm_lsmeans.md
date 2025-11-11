# Extract Least Square Means from `MMRM`

Extracts the least square means from an `MMRM` fit.

## Usage

``` r
get_mmrm_lsmeans(fit, vars, conf_level, weights, averages = list())
```

## Arguments

- fit:

  (`mmrm`)  
  result of
  [`mmrm::mmrm()`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html).

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

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- weights:

  (`string`)  
  type of weights to be used for the least square means, see
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html)
  for details.

- averages:

  (`list`)  
  named list of visit levels which should be averaged and reported along
  side the single visits.

## Value

A list with data frames `estimates` and `contrasts`. The attributes
`averages` and `weights` save the settings used.
