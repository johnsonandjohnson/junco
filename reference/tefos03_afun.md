# Analysis Function for TEFOS03 and Related Table Layouts

Analysis Function for TEFOS03 and Related Table Layouts

## Usage

``` r
tefos03_afun(df, .var, .spl_context, variables, control, formats)
```

## Arguments

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- .var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

- .spl_context:

  (`data.frame`)  
  gives information about ancestor split states that is passed by
  `rtables`.

- variables:

  (`list`)  
  see
  [`tern::fit_coxreg_multivar()`](https://insightsengineering.github.io/tern/latest-tag/reference/fit_coxreg.html)
  for required variable specifications.

- control:

  (`list`)  
  from
  [`tern::control_coxreg()`](https://insightsengineering.github.io/tern/latest-tag/reference/control_coxreg.html).

- formats:

  (`list`)  
  including `coef_se`, `hr_est`, `hr_ci` and `pval` formats.
