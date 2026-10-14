# Helper function to set labels for `s_proportion_diff_mf()` statistics

**\[stable\]**

Sets descriptive labels on the statistics returned by
[`s_proportion_diff_mf()`](https://johnsonandjohnson.github.io/junco/reference/proportion_diff_mf.md).

The label includes the detailed names of the two methods that may be
selected according to the Mantel-Fleiss criterion: `mf_method` is
selected when the criterion is satisfied, while `non_mf_method` is
selected when the criterion is not satisfied or cannot be evaluated.

Specifically, the method part of the label has the form
`"(mf_method_label / non_mf_method_label)"`, where `mf_method_label` and
`non_mf_method_label` are the corresponding detailed method names.

## Usage

``` r
h_set_labels_prop_diff_mf(
  y,
  mf_method,
  non_mf_method = "uncond_exact_diff",
  conf_level
)
```

## Arguments

- y:

  (`list`)  
  The list of statistics returned by
  [`s_proportion_diff_mf()`](https://johnsonandjohnson.github.io/junco/reference/proportion_diff_mf.md)
  for which labels should be set. Must contain the named elements
  `"diff"`, `"diff_ci"`, and `"diff_est_ci"`.

- mf_method:

  (`character(1)`)  
  The method specified for the Mantel-Fleiss-based analysis. Available
  choices are those accepted by the `method` argument of
  [`tern::d_proportion_diff()`](https://pharmaverse.github.io/tern/latest-tag/reference/d_proportion_diff.html).

- non_mf_method:

  (`character(1)`)  
  The method used when the Mantel-Fleiss criterion is not satisfied or
  cannot be evaluated. Available choices are those accepted by the
  method argument of
  [`tern::d_proportion_diff()`](https://pharmaverse.github.io/tern/latest-tag/reference/d_proportion_diff.html).

- conf_level:

  (`proportion`)  
  confidence level of the interval.

## Value

The input `y` with descriptive label attributes added to the `"diff"`,
`"diff_ci"`, and `"diff_est_ci"` elements.
