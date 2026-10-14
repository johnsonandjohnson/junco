# Test of Proportion Difference with Method Selection Based on the Mantel-Fleiss Criterion

**\[stable\]**

Tests the difference in response proportions between a non-reference
group and a reference group for stratified data. The test method is
selected based on the Mantel-Fleiss (MF) criterion.

When the Mantel-Fleiss criterion is satisfied, a Cochran-Mantel-Haenszel
(CMH)-type test is performed using the method specified by `mf_method`.
When the criterion is not satisfied, Fisher's exact test is used
instead.

## Usage

``` r
a_test_proportion_diff_mf(
  df,
  .var,
  .in_ref_col = NULL,
  .ref_group = NULL,
  val = TRUE,
  variables = list(strata = NULL),
  na.rm = FALSE,
  alternative = c("two.sided", "less", "greater"),
  mf_method = c("cmh", "cmh_sato", "cmh_wh"),
  mf_threshold = NULL,
  ...,
  .stats = NULL,
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL,
  exact_footnote = rtables::RefFootnote(note = tern::d_test_proportion_diff(method =
    "fisher", method_only = TRUE), index = 1L, symbol = "+")
)

s_test_proportion_diff_mf(
  df,
  .var,
  .in_ref_col = NULL,
  .ref_group = NULL,
  val = TRUE,
  variables = list(strata = NULL),
  na.rm = FALSE,
  alternative = c("two.sided", "less", "greater"),
  mf_method = c("cmh", "cmh_sato", "cmh_wh"),
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

- alternative:

  (`string`)  
  whether `two.sided`, or one-sided `less` or `greater` p-value should
  be displayed.

- mf_method:

  (`character(1)`)  
  The method used for testing the difference in response proportions
  when strata are provided and the Mantel-Fleiss criterion is satisfied.
  Available choices are `"cmh"`, `"cmh_sato"`, and `"cmh_wh"`. See
  **Details** for more information.

- mf_threshold:

  (`numeric(1)` or `NULL`)  
  Threshold for the Mantel-Fleiss criterion. If `NULL`, the default from
  [`tern::mantel_fleiss_crit()`](https://pharmaverse.github.io/tern/latest-tag/reference/mantel_fleiss_crit.html)
  is used.

- ...:

  additional arguments for the lower level functions.

- .stats:

  (character)  
  Statistics to select. The only available choice is `"pval"`. This
  parameter affects only the formatted analysis function and has no
  effect on the statistics function.

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

- `a_test_proportion_diff_mf()` returns the corresponding
  `RowsVerticalSection` object with formatted results.

&nbsp;

- `s_test_proportion_diff_mf()` returns a list containing:

  - `pval` (`numeric(1)` or `numeric(0)`):

    P-value from the test, or `numeric(0)` when the function is
    evaluated for the reference column (`.in_ref_col` is NULL or
    `.in_ref_col` is `TRUE`).

  - `executed_method` (`character(1)`):

    Name of the test actually used for the analysis. Takes the value
    `NA_character_` when the function is evaluated for the reference
    column (`.in_ref_col` is `NULL` or `.in_ref_col` is `TRUE`).
    Otherwise, identifies the method used: `"cmh"`, `"cmh_sato"`,
    `"cmh_wh"`, or `"fisher"`. Formatted analysis functions can use this
    value to determine whether an appropriate footnote should be added.

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
the selected CMH test
[`tern::prop_cmh()`](https://pharmaverse.github.io/tern/latest-tag/reference/h_prop_diff_test.html)
is used to obtain the p-value for the test of the difference in response
proportions. The available Cochran-Mantel-Haenszel tests are:

- `"cmh"`: Stratified Cochran-Mantel-Haenszel test.

- `"cmh_sato"`: Stratified CMH test using the Sato variance estimator.

- `"cmh_wh"`: Stratified CMH test using the Wilson-Hilferty
  transformation.

See
[`tern::prop_cmh()`](https://pharmaverse.github.io/tern/latest-tag/reference/h_prop_diff_test.html)
for more details and references.

When strata are not specified, or when strata are specified but the
Mantel-Fleiss criterion is not satisfied or cannot be evaluated (e.g.,
when none of the strata contain any observed values), Fisher's exact
test
[`tern::prop_fisher()`](https://pharmaverse.github.io/tern/latest-tag/reference/h_prop_diff_test.html)
is used instead.

The Mantel-Fleiss criterion is evaluated using
[`tern::mantel_fleiss_crit()`](https://pharmaverse.github.io/tern/latest-tag/reference/mantel_fleiss_crit.html).

The statistics function `s_test_proportion_diff_mf()` follows the usage
and output of
[`tern::s_test_proportion_diff()`](https://pharmaverse.github.io/tern/latest-tag/reference/prop_diff_test.html)
but removes the `method` argument and selects internally between a
Cochran-Mantel-Haenszel-type test and Fisher's exact test based on the
Mantel-Fleiss criterion.

## Functions

- `a_test_proportion_diff_mf()`: Formatted analysis function used as
  `afun` to test the difference in response proportions, with method
  selection based on the Mantel-Fleiss criterion.

- `s_test_proportion_diff_mf()`: Statistics function testing the
  difference in response proportions. When strata variables are provided
  and the Mantel-Fleiss criterion is satisfied, the CMH test
  [`tern::prop_cmh()`](https://pharmaverse.github.io/tern/latest-tag/reference/h_prop_diff_test.html)
  is used, with the values of the `diff_se` and `transform` arguments
  determined by `mf_method`. Otherwise, i.e., when strata variables are
  not provided, or when strata variables are provided but the
  Mantel-Fleiss criterion is not satisfied or cannot be evaluated,
  Fisher's exact test
  [`tern::prop_fisher()`](https://pharmaverse.github.io/tern/latest-tag/reference/h_prop_diff_test.html)
  is used.

## See also

[proportion_diff_mf](https://johnsonandjohnson.github.io/junco/reference/proportion_diff_mf.md),
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
    afun = a_test_proportion_diff_mf,
    extra_args = list(
      val = "Y",
      variables = list(strata = c("f1", "f2"))
    )
  )

build_table(lyt, df = dta)
#>                                                            A         C       Placebo
#> ————————————————————————————————————————————————————————————————————————————————————
#>   (Fisher's Exact Test / Cochran-Mantel-Haenszel Test)   0.550   0.725 {+}          
#> ————————————————————————————————————————————————————————————————————————————————————
#> 
#> {+} - Fisher's Exact Test
#> ————————————————————————————————————————————————————————————————————————————————————
#> 

test_prop_d <- s_test_proportion_diff_mf(
  df = subset(dta, grp == "A"),
  .var = "rsp",
  .ref_group = subset(dta, grp == "Placebo"),
  .in_ref_col = FALSE,
  val = "Y",
  variables = list(strata = c("f1", "f2"))
)
test_prop_d
#> $pval
#> [1] 0.5503584
#> attr(,"z_stat")
#> [1] -0.5972232
#> attr(,"label")
#> [1] "(Fisher's Exact Test / Cochran-Mantel-Haenszel Test)"
#> 
#> $executed_method
#> [1] "cmh"
#> 
```
