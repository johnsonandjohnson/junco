# Helper Function to Fit the MMRM and Return LS Mean Estimates and Contrasts

Helper Function to Fit the MMRM and Return LS Mean Estimates and
Contrasts

## Usage

``` r
h_summarize_mmrm(
  .var,
  df_parent,
  variables,
  ref_arm_level,
  ref_visit_levels,
  ...
)
```

## Arguments

- .var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

- df_parent:

  (`data.frame`)  
  data set containing all analysis variables from all visits and arms.

- variables:

  (named `list` of `string`)  
  list of additional analysis variables.

- ref_arm_level:

  (`string`)  
  the reference arm which should be compared against.

- ref_visit_levels:

  (`character`)  
  the reference visits which should not be included in the model fit.

- ...:

  additional options passed to
  [`fit_mmrm_j()`](https://johnsonandjohnson.github.io/junco/reference/fit_mmrm_j.md).

## Value

The resulting estimates and contrasts LS means as returned by
[`tidy.tern_model()`](https://johnsonandjohnson.github.io/junco/reference/tabulate_lsmeans.md).
