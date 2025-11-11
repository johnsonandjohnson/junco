# Dynamic tabulation of MMRM results with tables

**\[stable\]**

These functions can be used to produce tables for MMRM results, within
tables which are split by arms and visits. This is helpful when
higher-level row splits are needed (e.g. splits by parameter or
subgroup).

## Usage

``` r
s_summarize_mmrm(
  df,
  .var,
  variables,
  ref_levels,
  .spl_context,
  alternative = c("two.sided", "less", "greater"),
  show_relative = c("reduction", "increase"),
  ...
)

a_summarize_mmrm(
  df,
  .var,
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

- variables:

  (named `list` of `string`)  
  list of additional analysis variables.

- ref_levels:

  (`list`)  
  with `visit` and `arm` reference levels.

- .spl_context:

  (`data.frame`)  
  gives information about ancestor split states that is passed by
  `rtables`.

- alternative:

  (`string`)  
  whether `two.sided`, or one-sided `less` or `greater` p-value should
  be displayed.

- show_relative:

  should the 'reduction' (`control - treatment`, default) or the
  'increase' (`treatment - control`) be shown for the relative change
  from baseline?

- ...:

  eventually passed to
  [`fit_mmrm_j()`](https://johnsonandjohnson.github.io/junco/reference/fit_mmrm_j.md)
  via
  [`h_summarize_mmrm()`](https://johnsonandjohnson.github.io/junco/reference/h_summarize_mmrm.md).

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

- `a_summarize_mmrm()` returns the corresponding list with formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Functions

- `s_summarize_mmrm()`: Statistics function which is extracting
  estimates, not including any results when in the reference visit, and
  only showing LS mean estimates when in the reference arm and not in
  reference visit. It uses
  [`s_lsmeans()`](https://johnsonandjohnson.github.io/junco/reference/tabulate_lsmeans.md)
  for the final processing.

- `a_summarize_mmrm()`: Formatted analysis function which is used as
  `afun`.

## Examples

``` r
set.seed(123)
longdat <- data.frame(
  ID = rep(DM$ID, 5),
  AVAL = c(
    rep(0, nrow(DM)),
    rnorm(n = nrow(DM) * 4)
  ),
  VISIT = factor(rep(paste0("V", 0:4), each = nrow(DM)))
) |>
  dplyr::inner_join(DM, by = "ID")

basic_table() |>
  split_rows_by("VISIT") |>
  split_cols_by("ARM") |>
  analyze(
    vars = "AVAL",
    afun = a_summarize_mmrm,
    na_str = tern::default_na_str(),
    show_labels = "hidden",
    extra_args = list(
      variables = list(
        covariates = c("AGE"),
        id = "ID",
        arm = "ARM",
        visit = "VISIT"
      ),
      conf_level = 0.9,
      cor_struct = "toeplitz",
      ref_levels = list(VISIT = "V0", ARM = "B: Placebo")
    )
  ) |>
  build_table(longdat) |>
  prune_table(all_zero)
#>                                                   A: Drug X                B: Placebo             C: Combination    
#> ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> V1                                                                                                                  
#>   n                                                  121                      106                      129          
#>   Adjusted Mean (SE)                            0.081 (0.090)            -0.139 (0.096)           0.104 (0.087)     
#>     Adjusted Mean 90% CI                       (-0.066, 0.229)          (-0.297, 0.019)          (-0.039, 0.247)    
#>     Adjusted Mean (90% CI)                  0.081 (-0.066, 0.229)    -0.139 (-0.297, 0.019)   0.104 (-0.039, 0.247) 
#>   Difference in Adjusted Means (SE)             0.220 (0.132)                                     0.243 (0.130)     
#>     Difference in Adjusted Means 90% CI         (0.004, 0.437)                                    (0.030, 0.457)    
#>     Difference in Adjusted Means (90% CI)    0.220 (0.004, 0.437)                              0.243 (0.030, 0.457) 
#>     Relative Reduction (%)                          158.6%                                            175.1%        
#>     2-sided p-value                                 0.094                                             0.061         
#> V2                                                                                                                  
#>   n                                                  121                      106                      129          
#>   Adjusted Mean (SE)                            -0.079 (0.090)           -0.124 (0.096)           0.076 (0.087)     
#>     Adjusted Mean 90% CI                       (-0.226, 0.069)          (-0.282, 0.034)          (-0.067, 0.219)    
#>     Adjusted Mean (90% CI)                  -0.079 (-0.226, 0.069)   -0.124 (-0.282, 0.034)   0.076 (-0.067, 0.219) 
#>   Difference in Adjusted Means (SE)             0.046 (0.132)                                     0.200 (0.130)     
#>     Difference in Adjusted Means 90% CI        (-0.171, 0.262)                                   (-0.013, 0.413)    
#>     Difference in Adjusted Means (90% CI)   0.046 (-0.171, 0.262)                             0.200 (-0.013, 0.413) 
#>     Relative Reduction (%)                          36.8%                                             161.0%        
#>     2-sided p-value                                 0.728                                             0.123         
#> V3                                                                                                                  
#>   n                                                  121                      106                      129          
#>   Adjusted Mean (SE)                            0.060 (0.090)            0.101 (0.096)            0.110 (0.087)     
#>     Adjusted Mean 90% CI                       (-0.088, 0.208)          (-0.057, 0.259)          (-0.033, 0.253)    
#>     Adjusted Mean (90% CI)                  0.060 (-0.088, 0.208)    0.101 (-0.057, 0.259)    0.110 (-0.033, 0.253) 
#>   Difference in Adjusted Means (SE)             -0.041 (0.132)                                    0.009 (0.130)     
#>     Difference in Adjusted Means 90% CI        (-0.257, 0.176)                                   (-0.204, 0.223)    
#>     Difference in Adjusted Means (90% CI)   -0.041 (-0.257, 0.176)                            0.009 (-0.204, 0.223) 
#>     Relative Reduction (%)                          40.3%                                             -9.1%         
#>     2-sided p-value                                 0.757                                             0.943         
#> V4                                                                                                                  
#>   n                                                  121                      106                      129          
#>   Adjusted Mean (SE)                            0.113 (0.090)            -0.133 (0.096)           -0.022 (0.087)    
#>     Adjusted Mean 90% CI                       (-0.035, 0.260)          (-0.291, 0.025)          (-0.165, 0.121)    
#>     Adjusted Mean (90% CI)                  0.113 (-0.035, 0.260)    -0.133 (-0.291, 0.025)   -0.022 (-0.165, 0.121)
#>   Difference in Adjusted Means (SE)             0.245 (0.132)                                     0.111 (0.130)     
#>     Difference in Adjusted Means 90% CI         (0.029, 0.462)                                   (-0.103, 0.324)    
#>     Difference in Adjusted Means (90% CI)    0.245 (0.029, 0.462)                             0.111 (-0.103, 0.324) 
#>     Relative Reduction (%)                          184.9%                                            83.5%         
#>     2-sided p-value                                 0.062                                             0.393         
```
