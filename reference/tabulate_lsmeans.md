# Tabulation of Least Square Means Results

**\[stable\]**

These functions can be used to produce tables from LS means, e.g. from
[`fit_mmrm_j()`](https://johnsonandjohnson.github.io/junco/reference/fit_mmrm_j.md)
or
[`fit_ancova()`](https://johnsonandjohnson.github.io/junco/reference/fit_ancova.md).

## Usage

``` r
# S3 method for class 'tern_model'
tidy(x, ...)

s_lsmeans(
  df,
  .in_ref_col,
  alternative = c("two.sided", "less", "greater"),
  show_relative = c("reduction", "increase")
)

a_lsmeans(
  df,
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

- x:

  (`numeric`)  
  vector of numbers we want to analyze.

- ...:

  additional arguments for the lower level functions.

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- .in_ref_col:

  (`logical`)  
  `TRUE` when working with the reference level, `FALSE` otherwise.

- alternative:

  (`string`)  
  whether `two.sided`, or one-sided `less` or `greater` p-value should
  be displayed.

- show_relative:

  should the 'reduction' (`control - treatment`, default) or the
  'increase' (`treatment - control`) be shown for the relative change
  from baseline?

- ref_path:

  (`character`)  
  global reference group specification, see
  [`get_ref_info()`](https://johnsonandjohnson.github.io/junco/reference/get_ref_info.md).

- .spl_context:

  (`data.frame`)  
  gives information about ancestor split states that is passed by
  `rtables`.

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

for `s_lsmeans`, a list containing the same statistics returned by
tern.mmrm::s_mmrm_lsmeans, with the additional `diff_mean_est_ci`
three-dimensional statistic. For `a_lsmeans`, a `VertalRowsSection` as
returned by
[rtables::in_rows](https://insightsengineering.github.io/rtables/latest-tag/reference/in_rows.html).

## Functions

- `tidy(tern_model)`: Helper method (for
  [`broom::tidy()`](https://broom.tidymodels.org/reference/reexports.html))
  to prepare a `data.frame` from an `tern_model` object containing the
  least-squares means and contrasts.

- `s_lsmeans()`: Statistics function which is extracting estimates from
  a tidied least-squares means data frame.

- `a_lsmeans()`: Formatted Analysis function to be used as `afun`

## Note

These functions have been forked from the `tern.mmrm` package.
Additional features are:

- Additional `ref_path` argument for tern.mmrm::summarize_lsmeans().

- The function is more general in that it also works for LS means
  results from ANCOVA

- Additional statistic `diff_mean_est_ci` is returned

- P-value sidedness can be chosen

## Examples

``` r
result <- fit_mmrm_j(
  vars = list(
    response = "FEV1",
    covariates = c("RACE", "SEX"),
    id = "USUBJID",
    arm = "ARMCD",
    visit = "AVISIT"
  ),
  data = mmrm::fev_data,
  cor_struct = "unstructured",
  weights_emmeans = "equal"
)

df <- broom::tidy(result)

s_lsmeans(df[8, ], .in_ref_col = FALSE)
#> $n
#> [1] 67
#> 
#> $adj_mean_se
#> [1] 52.78422  1.18776
#> 
#> $adj_mean_ci
#> [1] 50.43481 55.13362
#> attr(,"label")
#> [1] "Adjusted Mean 95% CI"
#> 
#> $adj_mean_est_ci
#> [1] 52.78422 50.43481 55.13362
#> attr(,"label")
#> [1] "Adjusted Mean (95% CI)"
#> 
#> $diff_mean_se
#> [1] 4.398457 1.680545
#> 
#> $diff_mean_ci
#> [1] 1.074493 7.722422
#> attr(,"label")
#> [1] "Difference in Adjusted Means 95% CI"
#> 
#> $diff_mean_est_ci
#> [1] 4.398457 1.074493 7.722422
#> attr(,"label")
#> [1] "Difference in Adjusted Means (95% CI)"
#> 
#> $change
#> [1] -0.09090396
#> attr(,"label")
#> [1] "Relative Reduction (%)"
#> 
#> $p_value
#> [1] 0.009886854
#> attr(,"label")
#> [1] "2-sided p-value"
#> 
s_lsmeans(df[8, ], .in_ref_col = FALSE, alternative = "greater", show_relative = "increase")
#> $n
#> [1] 67
#> 
#> $adj_mean_se
#> [1] 52.78422  1.18776
#> 
#> $adj_mean_ci
#> [1] 50.43481 55.13362
#> attr(,"label")
#> [1] "Adjusted Mean 95% CI"
#> 
#> $adj_mean_est_ci
#> [1] 52.78422 50.43481 55.13362
#> attr(,"label")
#> [1] "Adjusted Mean (95% CI)"
#> 
#> $diff_mean_se
#> [1] 4.398457 1.680545
#> 
#> $diff_mean_ci
#> [1] 1.074493 7.722422
#> attr(,"label")
#> [1] "Difference in Adjusted Means 95% CI"
#> 
#> $diff_mean_est_ci
#> [1] 4.398457 1.074493 7.722422
#> attr(,"label")
#> [1] "Difference in Adjusted Means (95% CI)"
#> 
#> $change
#> [1] 0.09090396
#> attr(,"label")
#> [1] "Relative Increase (%)"
#> 
#> $p_value
#> [1] 0.004943427
#> attr(,"label")
#> [1] "1-sided p-value (greater)"
#> 

dat_adsl <- mmrm::fev_data |>
  dplyr::select(USUBJID, ARMCD) |>
  unique()

basic_table() |>
  split_cols_by("ARMCD") |>
  add_colcounts() |>
  split_rows_by("AVISIT") |>
  analyze(
    "AVISIT",
    afun = a_lsmeans,
    show_labels = "hidden",
    na_str = tern::default_na_str(),
    extra_args = list(
      .stats = c(
        "n",
        "adj_mean_se",
        "adj_mean_ci",
        "diff_mean_se",
        "diff_mean_ci"
      ),
      .labels = c(
        adj_mean_se = "Adj. LS Mean (Std. Error)",
        adj_mean_ci = "95% CI",
        diff_mean_ci = "95% CI"
      ),
      .formats = c(adj_mean_se = jjcsformat_xx("xx.x (xx.xx)")),
      alternative = "greater",
      ref_path = c("ARMCD", result$ref_level)
    )
  ) |>
  build_table(
    df = broom::tidy(result),
    alt_counts_df = dat_adsl
  )
#>                                             PBO                TRT       
#>                                           (N=105)             (N=95)     
#> —————————————————————————————————————————————————————————————————————————
#> VIS1                                                                     
#>   n                                          68                 66       
#>   Adj. LS Mean (Std. Error)             33.3 (0.76)        37.1 (0.76)   
#>     95% CI                            (31.839, 34.825)   (35.599, 38.613)
#>   Difference in Adjusted Means (SE)                       3.774 (1.074)  
#>     95% CI                                                (1.651, 5.897) 
#> VIS2                                                                     
#>   n                                          69                 71       
#>   Adj. LS Mean (Std. Error)             38.2 (0.61)        41.9 (0.60)   
#>     95% CI                            (36.963, 39.380)   (40.713, 43.094)
#>   Difference in Adjusted Means (SE)                       3.732 (0.859)  
#>     95% CI                                                (2.035, 5.430) 
#> VIS3                                                                     
#>   n                                          71                 58       
#>   Adj. LS Mean (Std. Error)             43.7 (0.46)        46.8 (0.51)   
#>     95% CI                            (42.760, 44.588)   (45.748, 47.761)
#>   Difference in Adjusted Means (SE)                       3.081 (0.690)  
#>     95% CI                                                (1.716, 4.445) 
#> VIS4                                                                     
#>   n                                          67                 67       
#>   Adj. LS Mean (Std. Error)             48.4 (1.19)        52.8 (1.19)   
#>     95% CI                            (46.035, 50.737)   (50.435, 55.134)
#>   Difference in Adjusted Means (SE)                       4.398 (1.681)  
#>     95% CI                                                (1.074, 7.722) 
```
