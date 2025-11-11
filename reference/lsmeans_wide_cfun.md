# Content Row Analysis Function for LS Means Wide Table Layouts

Content Row Analysis Function for LS Means Wide Table Layouts

## Usage

``` r
lsmeans_wide_cfun(
  df,
  labelstr,
  .spl_context,
  variables,
  ref_level,
  treatment_levels,
  pval_sided = c("2", "1", "-1"),
  conf_level,
  formats
)
```

## Arguments

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- labelstr:

  (`character`)  
  label of the level of the parent split currently being summarized
  (must be present as second argument in Content Row Functions). See
  [`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html)
  for more information.

- .spl_context:

  (`data.frame`)  
  gives information about ancestor split states that is passed by
  `rtables`.

- variables:

  (`list`)  
  see
  [`fit_ancova()`](https://johnsonandjohnson.github.io/junco/reference/fit_ancova.md)
  for required variable specifications.

- ref_level:

  (`string`)  
  the reference level of the treatment arm variable.

- treatment_levels:

  (`character`)  
  the non-reference levels of the treatment arm variable.

- pval_sided:

  (`string`)  
  either '2' for two-sided or '1' for 1-sided with greater than control
  or '-1' for 1-sided with smaller than control alternative hypothesis.

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- formats:

  (`list`)  
  including `lsmean`, `mse`, `df`, `lsmean_diff`, `se`, `ci`, `pval`
  formats.

## Details

This assumes a lot of structure of the layout, and is only intended to
be used inside
[`summarize_lsmeans_wide()`](https://johnsonandjohnson.github.io/junco/reference/summarize_lsmeans_wide.md),
please see there for the layout structure that is needed.
