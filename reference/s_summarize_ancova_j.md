# ANCOVA Summary Function

Combination of
[tern::s_summary](https://insightsengineering.github.io/tern/latest-tag/reference/analyze_variables.html),
and ANCOVA based estimates for mean and diff between columns, based on
ANCOVA function `s_ancova_j`.

## Usage

``` r
a_summarize_ancova_j(
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

s_summarize_ancova_j(df, .var, .df_row, .ref_group, .in_ref_col, ...)
```

## Arguments

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- .var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

- .df_row:

  (`data.frame`)  
  data set that includes all the variables that are called in `.var` and
  `variables`.

- ref_path:

  (`character`)  
  path to the reference group.

- .spl_context:

  (`environment`)  
  split context environment.

- ...:

  Additional arguments passed to `s_ancova_j`.

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

  (`data.frame` or `vector`)  
  the data corresponding to the reference group.

- .in_ref_col:

  (`flag`)  
  `TRUE` when working with the reference level, `FALSE` otherwise.

## Value

- `a_summarize_ancova_j()` returns the corresponding list with formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

returns the statistics from `tern::s_summary(x)`, appended with a new
statistics based upon ANCOVA

## Details

Combination of
[tern::s_summary](https://insightsengineering.github.io/tern/latest-tag/reference/analyze_variables.html),
and ANCOVA based estimates for mean and diff between columns, based on
ANCOVA function `s_ancova_j`

## Functions

- `a_summarize_ancova_j()`: Formatted analysis function which is used as
  `afun`. Note that the junco specific `ref_path` and `.spl_context`
  arguments are used for reference column information.

## See also

Other Inclusion of ANCOVA Functions:
[`a_summarize_aval_chg_diff_j()`](https://johnsonandjohnson.github.io/junco/reference/a_summarize_aval_chg_diff_j.md),
[`s_ancova_j()`](https://johnsonandjohnson.github.io/junco/reference/s_ancova_j.md)

## Examples

``` r
basic_table() |>
  split_cols_by("Species") |>
  add_colcounts() |>
  analyze(
    vars = "Petal.Length",
    afun = a_summarize_ancova_j,
    show_labels = "hidden",
    na_str = tern::default_na_str(),
    table_names = "unadj",
    var_labels = "Unadjusted comparison",
    extra_args = list(
      variables = list(arm = "Species", covariates = NULL),
      conf_level = 0.95,
      .labels = c(lsmean = "Mean", lsmean_diff = "Difference in Means"),
      ref_path = c("Species", "setosa")
    )
  ) |>
  analyze(
    vars = "Petal.Length",
    afun = a_summarize_ancova_j,
    show_labels = "hidden",
    na_str = tern::default_na_str(),
    table_names = "adj",
    var_labels = "Adjusted comparison (covariates: Sepal.Length and Sepal.Width)",
    extra_args = list(
      variables = list(
        arm = "Species",
        covariates = c("Sepal.Length", "Sepal.Width")
      ),
      conf_level = 0.95,
      ref_path = c("Species", "setosa")
    )
  ) |>
  build_table(iris)
#>                                              setosa            versicolor           virginica    
#>                                              (N=50)              (N=50)              (N=50)      
#> —————————————————————————————————————————————————————————————————————————————————————————————————
#> n                                              50                  50                  50        
#> Mean (SD)                                 1.46 (0.174)        4.26 (0.470)        5.55 (0.552)   
#> Median                                        1.50                4.35                5.55       
#> Min, max                                    1.0, 1.9            3.0, 5.1            4.5, 6.9     
#> 25% and 75%-ile                            1.40, 1.60          4.00, 4.60          5.10, 5.90    
#> Adjusted Mean (SE)                         1.46 (0.06)         4.26 (0.06)         5.55 (0.06)   
#> Adjusted Mean (95% CI)                  1.46 (1.34, 1.58)   4.26 (4.14, 4.38)   5.55 (5.43, 5.67)
#> Difference in Adjusted Means (95% CI)                       2.80 (2.63, 2.97)   4.09 (3.92, 4.26)
#>   p-value                                                        <0.001              <0.001      
#> n                                              50                  50                  50        
#> Mean (SD)                                 1.46 (0.174)        4.26 (0.470)        5.55 (0.552)   
#> Median                                        1.50                4.35                5.55       
#> Min, max                                    1.0, 1.9            3.0, 5.1            4.5, 6.9     
#> 25% and 75%-ile                            1.40, 1.60          4.00, 4.60          5.10, 5.90    
#> Adjusted Mean (SE)                         2.02 (0.08)         4.19 (0.05)         5.07 (0.06)   
#> Adjusted Mean (95% CI)                  2.02 (1.87, 2.17)   4.19 (4.09, 4.28)   5.07 (4.95, 5.18)
#> Difference in Adjusted Means (95% CI)                       2.17 (1.96, 2.38)   3.05 (2.81, 3.29)
#>   p-value                                                        <0.001              <0.001      

library(dplyr)
library(tern)

df <- iris |> filter(Species == "virginica")
.df_row <- iris
.var <- "Petal.Length"
variables <- list(arm = "Species", covariates = "Sepal.Length * Sepal.Width")
.ref_group <- iris |> filter(Species == "setosa")
conf_level <- 0.95
s_summarize_ancova_j(
  df,
  .var = .var,
  .df_row = .df_row,
  variables = variables,
  .ref_group = .ref_group,
  .in_ref_col = FALSE,
  conf_level = conf_level
)
#> $n
#>  n 
#> 50 
#> 
#> $sum
#>   sum 
#> 277.6 
#> 
#> $mean
#>  mean 
#> 5.552 
#> 
#> $sd
#>        sd 
#> 0.5518947 
#> 
#> $se
#>        se 
#> 0.0780497 
#> 
#> $mean_sd
#>      mean        sd 
#> 5.5520000 0.5518947 
#> 
#> $mean_se
#>      mean        se 
#> 5.5520000 0.0780497 
#> 
#> $mean_ci
#> mean_ci_lwr mean_ci_upr 
#>    5.395153    5.708847 
#> attr(,"label")
#> [1] "Mean 95% CI"
#> 
#> $mean_sei
#> mean_sei_lwr mean_sei_upr 
#>      5.47395      5.63005 
#> attr(,"label")
#> [1] "Mean -/+ 1xSE"
#> 
#> $mean_sdi
#> mean_sdi_lwr mean_sdi_upr 
#>     5.000105     6.103895 
#> attr(,"label")
#> [1] "Mean -/+ 1xSD"
#> 
#> $mean_ci_3d
#>        mean mean_ci_lwr mean_ci_upr 
#>    5.552000    5.395153    5.708847 
#> attr(,"label")
#> [1] "Mean (95% CI)"
#> 
#> $mean_pval
#>      p_value 
#> 4.093231e-51 
#> attr(,"label")
#> [1] "Mean p-value (H0: mean = 0)"
#> 
#> $median
#> median 
#>   5.55 
#> 
#> $mad
#> mad 
#>   0 
#> 
#> $median_ci
#> median_ci_lwr median_ci_upr 
#>           5.2           5.7 
#> attr(,"conf_level")
#> [1] 0.9671609
#> attr(,"label")
#> [1] "Median 95% CI"
#> 
#> $median_ci_3d
#>        median median_ci_lwr median_ci_upr 
#>          5.55          5.20          5.70 
#> attr(,"label")
#> [1] "Median (95% CI)"
#> 
#> $quantiles
#> quantile_0.25 quantile_0.75 
#>           5.1           5.9 
#> attr(,"label")
#> [1] "25% and 75%-ile"
#> 
#> $iqr
#> iqr 
#> 0.8 
#> 
#> $range
#> min max 
#> 4.5 6.9 
#> 
#> $min
#> min 
#> 4.5 
#> 
#> $max
#> max 
#> 6.9 
#> 
#> $median_range
#> median    min    max 
#>   5.55   4.50   6.90 
#> attr(,"label")
#> [1] "Median (Min - Max)"
#> 
#> $cv
#>       cv 
#> 9.940466 
#> 
#> $geom_mean
#> geom_mean 
#>  5.525789 
#> 
#> $geom_sd
#>  geom_sd 
#> 1.102724 
#> 
#> $geom_mean_sd
#> geom_mean   geom_sd 
#>  5.525789  1.102724 
#> 
#> $geom_mean_ci
#> mean_ci_lwr mean_ci_upr 
#>    5.374343    5.681502 
#> attr(,"label")
#> [1] "Geometric Mean 95% CI"
#> 
#> $geom_cv
#>  geom_cv 
#> 9.801743 
#> 
#> $geom_mean_ci_3d
#>   geom_mean mean_ci_lwr mean_ci_upr 
#>    5.525789    5.374343    5.681502 
#> attr(,"label")
#> [1] "Geometric Mean (95% CI)"
#> 
#> $n
#> [1] 50
#> attr(,"label")
#> [1] "n"
#> 
#> $lsmean
#> [1] 5.071002
#> attr(,"label")
#> [1] "Adjusted Mean"
#> 
#> $lsmean_se
#> [1] 5.07100244 0.06041213
#> attr(,"label")
#> [1] "Adjusted Mean (SE)"
#> 
#> $lsmean_ci
#> [1] 5.071002 4.951593 5.190412
#> attr(,"label")
#> [1] "Adjusted Mean (95% CI)"
#> 
#> $lsmean_diff
#> [1] 3.062603
#> attr(,"label")
#> [1] "Difference in Adjusted Means"
#> 
#> $lsmean_diff_ci
#> [1] 2.808526 3.316680
#> attr(,"label")
#> [1] "Difference in Adjusted Means 95% CI"
#> 
#> $lsmean_diffci
#> [1] 3.062603 2.808526 3.316680
#> attr(,"label")
#> [1] "Difference in Adjusted Means (95% CI)"
#> 
#> $pval
#> [1] 8.117283e-52
#> attr(,"label")
#> [1] "p-value"
#> 
```
