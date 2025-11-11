# Odds ratio estimation

**\[stable\]**

## Usage

``` r
a_odds_ratio_j(
  df,
  .var,
  .df_row,
  ref_path,
  .spl_context,
  ...,
  .stats = NULL,
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL
)

s_odds_ratio_j(
  df,
  .var,
  .ref_group,
  .in_ref_col,
  .df_row,
  variables = list(arm = NULL, strata = NULL),
  conf_level = 0.95,
  groups_list = NULL,
  na_if_no_events = TRUE,
  method = c("exact", "approximate", "efron", "breslow", "cmh")
)
```

## Arguments

- df:

  (`data.frame`)  
  input data frame.

- .var:

  (`string`)  
  name of the response variable.

- .df_row:

  (`data.frame`)  
  data frame containing all rows.

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

- .ref_group:

  (`data.frame`)  
  reference group data frame.

- .in_ref_col:

  (`logical`)  
  whether the current column is the reference column.

- variables:

  (`list`)  
  list with arm and strata variable names.

- conf_level:

  (`numeric`)  
  confidence level for the confidence interval.

- groups_list:

  (`list`)  
  list of groups for combination.

- na_if_no_events:

  (`flag`)  
  whether the point estimate should be `NA` if there are no events in
  one arm. The p-value and confidence interval will still be computed.

- method:

  (`string`)  
  whether to use the correct (`'exact'`) calculation in the conditional
  likelihood or one of the approximations, or the CMH method. See
  [`survival::clogit()`](https://rdrr.io/pkg/survival/man/clogit.html)
  for details.

## Value

- `a_odds_ratio_j()` returns the corresponding list with formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

&nbsp;

- `s_odds_ratio_j()` returns a named list with the statistics `or_ci`
  (containing `est`, `lcl`, and `ucl`), `pval` and `n_tot`.

## Functions

- `a_odds_ratio_j()`: Formatted analysis function which is used as
  `afun`. Note that the junco specific `ref_path` and `.spl_context`
  arguments are used for reference column information.

- `s_odds_ratio_j()`: Statistics function which estimates the odds ratio
  between a treatment and a control. A `variables` list with `arm` and
  `strata` variable names must be passed if a stratified analysis is
  required.

## Note

The `a_odds_ratio_j()` and `s_odds_ratio_j()` functions have the `_j`
suffix to distinguish them from
[`tern::a_odds_ratio()`](https://insightsengineering.github.io/tern/latest-tag/reference/odds_ratio.html)
and
[`tern::s_odds_ratio()`](https://insightsengineering.github.io/tern/latest-tag/reference/odds_ratio.html),
respectively. These functions differ as follows:

- Additional `method = 'cmh'` option is provided to calculate the
  Cochran-Mantel-Haenszel estimate.

- The p-value is returned as an additional statistic.

Once these updates are contributed back to `tern`, they can later be
replaced by the `tern` versions.

## Examples

``` r
set.seed(12)
dta <- data.frame(
  rsp = sample(c(TRUE, FALSE), 100, TRUE),
  grp = factor(rep(c("A", "B"), each = 50), levels = c("A", "B")),
  strata = factor(sample(c("C", "D"), 100, TRUE))
)

a_odds_ratio_j(
  df = subset(dta, grp == "A"),
  .var = "rsp",
  ref_path = c("grp", "B"),
  .spl_context = data.frame(
    cur_col_split = I(list("grp")),
    cur_col_split_val = I(list(c(grp = "A"))),
    full_parent_df = I(list(dta))
  ),
  .df_row = dta
)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>   row_name  formatted_cell indent_mod           row_label
#> 1    n_tot              50          0             Total n
#> 2    or_ci NE (0.00 - Inf)          1 Odds Ratio (95% CI)
#> 3     pval          >0.999          1             p-value


l <- basic_table() |>
  split_cols_by(var = "grp") |>
  analyze(
    "rsp",
    afun = a_odds_ratio_j,
    show_labels = "hidden",
    extra_args = list(
      ref_path = c("grp", "B"),
      .stats = c("or_ci", "pval")
    )
  )

build_table(l, df = dta)
#>                                 A            B
#> ——————————————————————————————————————————————
#>   Odds Ratio (95% CI)   0.85 (0.38 - 1.88)    
#>   p-value                     0.685           

l2 <- basic_table() |>
  split_cols_by(var = "grp") |>
  analyze(
    "rsp",
    afun = a_odds_ratio_j,
    show_labels = "hidden",
    extra_args = list(
      variables = list(arm = "grp", strata = "strata"),
      method = "cmh",
      ref_path = c("grp", "A"),
      .stats = c("or_ci", "pval")
    )
  )

build_table(l2, df = dta)
#>                         A           B         
#> ——————————————————————————————————————————————
#>   Odds Ratio (95% CI)       1.31 (0.58 - 2.96)
#>   p-value                         0.524       
s_odds_ratio_j(
  df = subset(dta, grp == "A"),
  .var = "rsp",
  .ref_group = subset(dta, grp == "B"),
  .in_ref_col = FALSE,
  .df_row = dta
)
#> $or_ci
#>       est       lcl       ucl 
#> 0.8484848 0.3831831 1.8788053 
#> attr(,"label")
#> [1] "Odds Ratio (95% CI)"
#> 
#> $n_tot
#> n_tot 
#>   100 
#> 
#> $pval
#> [1] 0.6854057
#> 

s_odds_ratio_j(
  df = subset(dta, grp == "A"),
  .var = "rsp",
  .ref_group = subset(dta, grp == "B"),
  .in_ref_col = FALSE,
  .df_row = dta,
  variables = list(arm = "grp", strata = "strata")
)
#> $or_ci
#>       est       lcl       ucl 
#> 0.7689750 0.3424155 1.7269154 
#> attr(,"label")
#> [1] "Odds Ratio (95% CI)"
#> 
#> $n_tot
#> n_tot 
#>   100 
#> 
#> $pval
#> [1] 0.5245098
#> 

s_odds_ratio_j(
  df = subset(dta, grp == "A"),
  method = "cmh",
  .var = "rsp",
  .ref_group = subset(dta, grp == "B"),
  .in_ref_col = FALSE,
  .df_row = dta,
  variables = list(arm = "grp", strata = c("strata"))
)
#> $or_ci
#>       est       lcl       ucl 
#> 0.7647498 0.3376522 1.7320849 
#> attr(,"label")
#> [1] "Odds Ratio (95% CI)"
#> 
#> $n_tot
#> n_tot 
#>   100 
#> 
#> $pval
#> [1] 0.5241419
#> 
```
