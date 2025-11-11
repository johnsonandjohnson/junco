# Workaround statistics function to add HR with CI

This is a workaround for
[`tern::s_coxph_pairwise()`](https://insightsengineering.github.io/tern/latest-tag/reference/survival_coxph_pairwise.html),
which adds a statistic containing the hazard ratio estimate together
with the confidence interval.

## Usage

``` r
a_coxph_hr(
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

s_coxph_hr(
  df,
  .ref_group,
  .in_ref_col,
  .var,
  is_event,
  strata = NULL,
  control = control_coxph(),
  alternative = c("two.sided", "less", "greater")
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

  additional arguments for the lower level functions.

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

- .ref_group:

  (`data.frame` or `vector`)  
  the data corresponding to the reference group.

- .in_ref_col:

  (`logical`)  
  `TRUE` when working with the reference level, `FALSE` otherwise.

- is_event:

  (`character`)  
  variable name storing Logical values: `TRUE` if event, `FALSE` if time
  to event is censored.

- strata:

  (`character` or `NULL`)  
  variable names indicating stratification factors.

- control:

  (`list`)  
  relevant list of control options.

- alternative:

  (`string`)  
  whether `two.sided`, or one-sided `less` or `greater` p-value should
  be displayed.

## Value

for `s_coxph_hr` a list containing the same statistics returned by
[tern::s_coxph_pairwise](https://insightsengineering.github.io/tern/latest-tag/reference/survival_coxph_pairwise.html)
and the additional `lr_stat_df` statistic. for `a_coxph_hr`, a
`VerticalRowsSection` object.

## Functions

- `a_coxph_hr()`: Formatted analysis function which is used as `afun`.

- `s_coxph_hr()`: Statistics function forked from
  [`tern::s_coxph_pairwise()`](https://insightsengineering.github.io/tern/latest-tag/reference/survival_coxph_pairwise.html).
  the difference is that:

  1.  It returns the additional statistic `lr_stat_df` (log rank
      statistic with degrees of freedom).

## Examples

``` r
library(dplyr)

adtte_f <- tern::tern_ex_adtte |>
  filter(PARAMCD == "OS") |>
  mutate(is_event = CNSR == 0)

df <- adtte_f |> filter(ARMCD == "ARM A")
df_ref_group <- adtte_f |> filter(ARMCD == "ARM B")

basic_table() |>
  split_cols_by(var = "ARMCD", ref_group = "ARM A") |>
  add_colcounts() |>
  analyze("AVAL",
    afun = s_coxph_hr,
    extra_args = list(is_event = "is_event"),
    var_labels = "Unstratified Analysis",
    show_labels = "visible"
  ) |>
  build_table(df = adtte_f)
#>                           ARM A                            ARM B                                                  ARM C                        
#>                           (N=69)                          (N=73)                                                  (N=58)                       
#> ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Unstratified Analysis                                                                                                                          
#>   p-value (log-rank)                                0.0904951081206286                                     0.00860286392158099                 
#>   lr_stat_df                                        2.86554400612335, 1                                    6.90346948570891, 1                 
#>   hr                                                 1.40675529802996                                        1.81062231929339                  
#>   95% CI                                    0.945772426764327, 2.09242774745061                     1.15592003553536, 2.83614184575062         
#>   Hazard Ratio (95% CI)            1.40675529802996, 0.945772426764327, 2.09242774745061   1.81062231929339, 1.15592003553536, 2.83614184575062
#>   n_tot                                                     142                                                    127                         
#>   n_tot_events                                              101                                                     84                         

basic_table() |>
  split_cols_by(var = "ARMCD", ref_group = "ARM A") |>
  add_colcounts() |>
  analyze("AVAL",
    afun = s_coxph_hr,
    extra_args = list(
      is_event = "is_event",
      strata = "SEX",
      control = tern::control_coxph(pval_method = "wald")
    ),
    var_labels = "Unstratified Analysis",
    show_labels = "visible"
  ) |>
  build_table(df = adtte_f)
#>                           ARM A                           ARM B                                                  ARM C                        
#>                           (N=69)                          (N=73)                                                 (N=58)                       
#> ——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Unstratified Analysis                                                                                                                         
#>   p-value (wald)                                    0.0783698773203908                                    0.00662292473338187                 
#>   lr_stat_df                                       3.12966155715821, 1                                    7.60136269697425, 1                 
#>   hr                                                 1.43602612933735                                       1.88581879789238                  
#>   95% CI                                    0.959767646673475, 2.1486148770351                     1.19297959598155, 2.98103383365769         
#>   Hazard Ratio (95% CI)            1.43602612933735, 0.959767646673475, 2.1486148770351   1.88581879789238, 1.19297959598155, 2.98103383365769
#>   n_tot                                                    142                                                    127                         
#>   n_tot_events                                             101                                                     84                         
adtte_f <- tern::tern_ex_adtte |>
  dplyr::filter(PARAMCD == "OS") |>
  dplyr::mutate(is_event = CNSR == 0)
df <- adtte_f |> dplyr::filter(ARMCD == "ARM A")
df_ref <- adtte_f |> dplyr::filter(ARMCD == "ARM B")

s_coxph_hr(
  df = df,
  .ref_group = df_ref,
  .in_ref_col = FALSE,
  .var = "AVAL",
  is_event = "is_event",
  strata = NULL
)
#> $pvalue
#> [1] 0.09049511
#> attr(,"label")
#> [1] "p-value (log-rank)"
#> 
#> $lr_stat_df
#> [1] 2.865544 1.000000
#> 
#> $hr
#> [1] 0.7108557
#> 
#> $hr_ci
#> [1] 0.4779138 1.0573368
#> attr(,"label")
#> [1] "95% CI"
#> 
#> $hr_ci_3d
#> [1] 0.7108557 0.4779138 1.0573368
#> attr(,"label")
#> [1] "Hazard Ratio (95% CI)"
#> 
#> $n_tot
#> [1] 142
#> 
#> $n_tot_events
#> [1] 101
#> 
```
