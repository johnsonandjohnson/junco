# s_coxph_hr

s_coxph_hr **\[deprecated\]** Use
[`tern::s_coxph_pairwise`](https://pharmaverse.github.io/tern/latest-tag/reference/survival_coxph_pairwise.html)
instead, and use
[`tern::coxph_pairwise()`](https://pharmaverse.github.io/tern/latest-tag/reference/survival_coxph_pairwise.html)
as the layout-level wrapper.

## Usage

``` r
s_coxph_hr(
  df,
  .var,
  ref_path,
  .spl_context,
  ...,
  .stats = NULL,
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL
)
```

## Arguments

- df:

  (`data.frame`)  
  input data frame.

- .var:

  (`string`)  
  name of the response variable.

- ref_path:

  (`character`)  
  path to the reference group.

- .spl_context:

  (`environment`)  
  split context environment.

- ...:

  Additional arguments passed to the statistics function.

- .stats:

  (`character`)  
  statistics to calculate.

- .formats:

  (`list`)  
  formats for the statistics.

- .labels:

  (`list`)  
  labels for the statistics.

- .indent_mods:

  (`list`)  
  indentation modifications for the statistics.
