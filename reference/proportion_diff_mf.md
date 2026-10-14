# Proportion Difference with Method Selection Based on the Mantel-Fleiss Criterion

**\[stable\]**

Performs an adaptive analysis of the difference in response proportions
between a non-reference group and a reference group for stratified data,
with the method selected based on the Mantel-Fleiss (MF) criterion.

When the Mantel-Fleiss criterion is satisfied, a Cochran-Mantel-Haenszel
(CMH) analysis is performed using the method specified by `mf_method`.
When the criterion is not satisfied, an unconditional exact method is
used instead.

## Usage

``` r
a_proportion_diff_mf(
  df,
  .var,
  .in_ref_col = NULL,
  .ref_group = NULL,
  val = TRUE,
  variables = list(strata = NULL),
  na.rm = FALSE,
  conf_level = 0.95,
  mf_method = c("cmh", "cmh_sato", "cmh_mn"),
  mf_threshold = NULL,
  ...,
  .stats = NULL,
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL,
  exact_footnote = rtables::RefFootnote(note = tern::d_proportion_diff(method =
    "uncond_exact_diff", method_only = TRUE), index = 1L, symbol = "+")
)

s_proportion_diff_mf(
  df,
  .var,
  .in_ref_col = NULL,
  .ref_group = NULL,
  val = TRUE,
  variables = list(strata = NULL),
  na.rm = FALSE,
  conf_level = 0.95,
  mf_method = c("cmh", "cmh_sato", "cmh_mn"),
  mf_threshold = NULL
)
```

## Arguments

- df:

  (`data.frame`)  
  The data containing all analysis variables for the non-reference
  group.

- .var:

  (`character(1)`)  
  The column name in `df` (and, if supplied, `.ref_group`) specifying
  the response variable. The response is converted to a logical vector
  by comparing its values with `val`. `df[[var]]` (and
  `.ref_group[[var]]`, if supplied) must be an atomic vector as defined
  by
  [`checkmate::check_atomic_vector()`](https://mllg.github.io/checkmate/reference/checkAtomicVector.html),
  of one of the following types: `logical`, `integer`, `numeric`, or
  `character`.

- .in_ref_col:

  (`logical(1)` or `NULL`)  
  Indicates whether the function is being evaluated for the reference
  column. Set to `TRUE` for the reference column and to `FALSE` or
  `NULL` otherwise. For the statistics function, `TRUE` or `NULL`
  results in empty statistics, while for the analysis function, the
  result is a `RowsVerticalSection` with a `NULL` value for all selected
  statistics.

- .ref_group:

  (`data.frame` or `NULL`)  
  The data corresponding to the reference group.

- val:

  (`logical(1)` or `integer(1)` or `numeric(1)` or `character(1)`)  
  The value used to identify the response of interest. An observation is
  considered a response of interest when `df[[.var]] == val` (and, if
  the the reference group is supplied, `.ref_group[[.var]] == val`). If
  `df[[.var]]` is a `factor`, `val` must be a `character` value matching
  one of its levels. Otherwise, `val` must have the same class as
  `df[[.var]]`.

- variables:

  (`list` or `NULL`)  
  Optional strata variables specified as `list(strata = <strata_vars>)`,
  where `<strata_vars>` is a character vector containing column names in
  `df` (and, if supplied, `.ref_group`). All specified strata variables
  must be factors.

- na.rm:

  (`logical(1)`)  
  Whether to remove incomplete rows from `df` (and, if supplied,
  `.ref_group`) before constructing the response, group, and strata
  vectors. Completeness is assessed only for `.var` and the strata
  columns (if supplied). If `na.rm = TRUE`, rows containing missing
  values in any of these columns are removed. If `na.rm = FALSE`, the
  function fails if any of these columns contain missing values, rather
  than returning results containing `NA` values.

  This argument is passed unchanged to the `complete_cases` argument of
  [`tern::h_prepare_rsp_table()`](https://pharmaverse.github.io/tern/latest-tag/reference/h_prepare_rsp_table.html).

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- mf_method:

  (`character(1)`)  
  The method used for estimating the difference in response proportions
  and its confidence interval when strata are provided and the
  Mantel-Fleiss criterion is satisfied. Available choices are: `"cmh"`,
  `"cmh_sato"`, and `"cmh_mn"`. See **Details** for more information.

- mf_threshold:

  (`numeric(1)` or `NULL`)  
  Threshold for the Mantel-Fleiss criterion. If `NULL`, the default from
  [`tern::mantel_fleiss_crit()`](https://pharmaverse.github.io/tern/latest-tag/reference/mantel_fleiss_crit.html)
  is used.

- ...:

  additional arguments for the lower level functions.

- .stats:

  (`character`)  
  Statistics to select. Available choices are `"diff"`, `"diff_ci"`, and
  `"diff_est_ci"`. This parameter affects only the formatted analysis
  function and has no effect on the statistics function.

- .formats:

  (named `character` or `list`)  
  Formats for the statistics.

- .labels:

  (named `character`)  
  labels for the statistics (without indent).

- .indent_mods:

  (named `integer`)  
  indent modifiers for the labels. Defaults to 0, which corresponds to
  the unmodified default behavior. Can be negative.

- exact_footnote:

  (`character(1)` or `RefFootnote`)  
  Footnote used to indicate that the unconditional exact method was
  actually used for the analysis.

## Value

- `a_proportion_diff_mf()` returns the corresponding
  `RowsVerticalSection` object with formatted results.

&nbsp;

- `s_proportion_diff_mf()` returns a list containing:

  - `diff` (`numeric(1)` or `numeric(0)`):

    Point estimate of the difference in response proportions, or
    `numeric(0)` when the function is evaluated for the reference column
    (`.in_ref_col` is NULL or `.in_ref_col` is `TRUE`).

  - `diff_ci` (`numeric(2)` or `numeric(0)`):

    Confidence interval for the difference in response proportions, or
    `numeric(0)` when the function is evaluated for the reference column
    (`.in_ref_col` is NULL or `.in_ref_col` is `TRUE`).

  - `diff_est_ci` (`numeric(3)` or `numeric(0)`):

    Point estimate and confidence interval for the difference in
    response proportions, or `numeric(0)` when the function is evaluated
    for the reference column (`.in_ref_col` is NULL or `.in_ref_col` is
    `TRUE`).

  - `executed_method` (`character(1)`):

    Name of the method actually used for the analysis. Takes the value
    `NA_character_` when the function is evaluated for the reference
    column (`.in_ref_col` is `NULL` or `.in_ref_col` is `TRUE`).
    Otherwise, identifies the method used: `"cmh"`, `"cmh_sato"`,
    `"cmh_mn"`, or `"uncond_exact_diff"`. Formatted analysis functions
    can use this value to determine whether an appropriate footnote
    should be added.

## Details

The data are prepared by
[`tern::h_prepare_rsp_table()`](https://pharmaverse.github.io/tern/latest-tag/reference/h_prepare_rsp_table.html),
which constructs the response vector, group indicator, optional strata
variable, and the corresponding 2 x 2 contingency table(s). When
multiple strata variables are supplied, the cross-combinations of their
levels define the analysis strata.

The response variable is converted to a binary response according to
`val`. The analysis compares the response proportion in the
non-reference group with that in the reference group. The difference is
calculated as the non-reference group minus the reference group.

When strata are provided and the Mantel-Fleiss criterion is satisfied,
the selected CMH method
[`tern::prop_diff_cmh()`](https://pharmaverse.github.io/tern/latest-tag/reference/h_prop_diff.html)
is used to obtain the point estimate and confidence interval for the
difference in response proportions. The available
Cochran-Mantel-Haenszel-type methods for estimating the difference in
response proportions and its confidence interval are:

- `"cmh"`: CMH method.

- `"cmh_sato"`: CMH method using the Sato variance estimator.

- `"cmh_mn"`: CMH method using the Miettinen-Nurminen method.

See
[`tern::prop_diff_cmh()`](https://pharmaverse.github.io/tern/latest-tag/reference/h_prop_diff.html)
for more details and references.

When strata are not specified, or when strata are specified but the
Mantel-Fleiss criterion is not satisfied or cannot be evaluated (e.g.,
when all strata contain no observed values), the unconditional exact
method
[`tern::prop_diff_uncond_exact()`](https://pharmaverse.github.io/tern/latest-tag/reference/h_prop_diff.html)
is used to obtain the point estimate and confidence interval.

The Mantel-Fleiss criterion is evaluated using
[`tern::mantel_fleiss_crit()`](https://pharmaverse.github.io/tern/latest-tag/reference/mantel_fleiss_crit.html).

The statistics function `s_proportion_diff_mf()` follows the usage and
output of
[`tern::s_proportion_diff()`](https://pharmaverse.github.io/tern/latest-tag/reference/prop_diff.html)
but removes the `method` argument and selects internally between a
Cochran-Mantel-Haenszel-type method and an unconditional exact method
based on the Mantel-Fleiss criterion.

## Functions

- `a_proportion_diff_mf()`: Formatted analysis function used as `afun`
  for the difference in response proportions with method selection based
  on the Mantel-Fleiss criterion.

- `s_proportion_diff_mf()`: Statistics function estimating the
  difference in response proportions and its confidence interval. When
  strata variables are provided and the Mantel-Fleiss criterion is
  satisfied, the CMH method
  [`tern::prop_diff_cmh()`](https://pharmaverse.github.io/tern/latest-tag/reference/h_prop_diff.html)
  is used, with the `diff_se` argument determined by `mf_method`.
  Otherwise, i.e., when strata variables are not provided, or when
  strata variables are provided but the Mantel-Fleiss criterion is not
  satisfied or cannot be evaluated, the unconditional exact method
  [`tern::prop_diff_uncond_exact()`](https://pharmaverse.github.io/tern/latest-tag/reference/h_prop_diff.html)
  is used.

## See also

[`cond_proportion_j()`](https://johnsonandjohnson.github.io/junco/reference/cond_proportion_j.md)

## Author

WW

## Examples

``` r
n <- 100 # Number of observations.
set.seed(123)
dta <- data.frame(
  "rsp" = sample(c("Y", "N"), n, TRUE),
  "grp" = c(
    sample(c("A", "Placebo"), round(n - 0.1 * n), TRUE),
    rep("C", round(0.1 * n))
  ),
  "f1" = sample(c("a1", "a2"), n, TRUE),
  "f2" = sample(c("x", "y", "z"), n, TRUE),
  stringsAsFactors = TRUE
)

lyt <- basic_table() |>
  split_cols_by(var = "grp", ref_group = "Placebo") |>
  analyze(
    vars = "rsp",
    afun = a_proportion_diff_mf,
    extra_args = list(
      val = "Y",
      variables = list(strata = c("f1", "f2")),
      conf_level = 0.90,
      mf_method = "cmh_mn",
      .stats = "diff_est_ci"
    )
  )

build_table(lyt, df = dta)
#>                                                                                                          A                       C              Placebo
#> ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Difference in Response rate (%) and 90% CI (CMH, Miettinen and Nurminen / Unconditional exact)   -6.5 (-24.1, 11.3)   -11.2 (-38.8, 16.8) {+}          
#> ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> 
#> {+} - Unconditional exact
#> ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> 

prop_d <- s_proportion_diff_mf(
  df = subset(dta, grp == "A"),
  .var = "rsp",
  .ref_group = subset(dta, grp == "Placebo"),
  .in_ref_col = FALSE,
  val = "Y",
  variables = list(strata = c("f1", "f2")),
  conf_level = 0.90,
  mf_method = "cmh_mn"
)
prop_d
#> $diff
#> diff_cmh_mn 
#>   -6.543243 
#> attr(,"label")
#> [1] "Difference in Response rate (%) (CMH, Miettinen and Nurminen / Unconditional exact)"
#> 
#> $diff_ci
#> diff_ci_cmh_mn_l diff_ci_cmh_mn_u 
#>        -24.07009         11.29786 
#> attr(,"label")
#> [1] "Difference in Response rate (%) 90% CI (CMH, Miettinen and Nurminen / Unconditional exact)"
#> 
#> $diff_est_ci
#>      diff_cmh_mn diff_ci_cmh_mn_l diff_ci_cmh_mn_u 
#>        -6.543243       -24.070094        11.297864 
#> attr(,"label")
#> [1] "Difference in Response rate (%) and 90% CI (CMH, Miettinen and Nurminen / Unconditional exact)"
#> 
#> $executed_method
#> [1] "cmh_mn"
#> 
```
