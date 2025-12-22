# Difference test for two proportions

**\[stable\]**

The analysis function `a_test_proportion_diff()` can be used to create a
layout element to test the difference between two proportions. The
primary analysis variable, `vars`, indicates whether a response has
occurred for each record. See the `method` parameter for options of
methods to use to calculate the p-value. Additionally, a stratification
variable can be supplied via the `strata` element of the `variables`
argument. The argument `alternative` specifies the direction of the
alternative hypothesis.

## Usage

``` r
a_test_proportion_diff(
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
  data set containing all analysis variables.

- .var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

- ref_path:

  (`character`)  
  global reference group specification, see
  [`get_ref_info()`](https://johnsonandjohnson.github.io/junco/reference/get_ref_info.md).

- .spl_context:

  (`data.frame`)  
  gives information about ancestor split states that is passed by
  `rtables`.

- ...:

  Additional arguments passed to
  [`tern::s_test_proportion_diff()`](https://insightsengineering.github.io/tern/latest-tag/reference/prop_diff_test.html),
  including:

  - `method` (`string`)  
    one of `chisq`, `cmh`, `cmh_wh`, `fisher` or `schouten`; specifies
    the test used to calculate the p-value.

- .stats:

  (`character`)  
  statistics to select for the table.

- .formats:

  (named `character` or `list`)  
  formats for the statistics. See Details in `analyze_vars` for more
  information on the `'auto'` setting.

- .labels:

  (named `character`)  
  labels for the statistics (without indent).

- .indent_mods:

  (named `integer`)  
  indent modifiers for the labels. Defaults to 0, which corresponds to
  the unmodified default behavior. Can be negative.

## Value

- `a_test_proportion_diff()` returns the corresponding list with
  formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Functions

- `a_test_proportion_diff()`: Formatted analysis function which is used
  as `afun`

## Note

This function has been forked from the `tern` package. Additional
features are:

- Additional `ref_path` argument for flexible reference column path
  specification.

## Examples

``` r
dta <- data.frame(
  rsp = sample(c(TRUE, FALSE), 100, TRUE),
  grp = factor(rep(c("A", "B"), each = 50)),
  strata = factor(rep(c("V", "W", "X", "Y", "Z"), each = 20))
)

l <- basic_table() |>
  split_cols_by(var = "grp") |>
  analyze(
    vars = "rsp",
    afun = a_test_proportion_diff,
    show_labels = "hidden",
    extra_args = list(
      method = "cmh",
      variables = list(strata = "strata"),
      ref_path = c("grp", "B")
    )
  )

build_table(l, df = dta)
#>                                              A     B
#> ————————————————————————————————————————————————————
#>   p-value (Cochran-Mantel-Haenszel Test)   0.374    
```
