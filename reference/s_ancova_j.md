# Junco Extended ANCOVA Function

Extension to tern:::s_ancova, 3 extra statistics are returned

- `lsmean_se`: Marginal mean and estimated SE in the group.

- `lsmean_ci`: Marginal mean and associated confidence interval in the
  group.

- `lsmean_diffci`: Difference in mean and associated confidence level in
  one combined statistic. In addition, the LS mean weights can be
  specified. In addition, also a NULL .ref_group can be specified, the
  lsmean_diff related estimates will be returned as NA.

## Usage

``` r
s_ancova_j(
  df,
  .var,
  .df_row,
  variables,
  .ref_group,
  .in_ref_col,
  conf_level,
  interaction_y = FALSE,
  interaction_item = NULL,
  weights_emmeans = "counterfactual"
)
```

## Arguments

- df:

  : need to check on how to inherit params from tern::s_ancova

- .var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

- .df_row:

  (`data.frame`)  
  data set that includes all the variables that are called in `.var` and
  `variables`.

- variables:

  (named `list` of `string`)  
  list of additional analysis variables, with expected elements:

  - `arm` (`string`)  
    group variable, for which the covariate adjusted means of multiple
    groups will be summarized. Specifically, the first level of `arm`
    variable is taken as the reference group.

  - `covariates` (`character`)  
    a vector that can contain single variable names (such as `"X1"`),
    and/or interaction terms indicated by `"X1 * X2"`.

- .ref_group:

  (`data.frame` or `vector`)  
  the data corresponding to the reference group.

- .in_ref_col:

  (`flag`)  
  `TRUE` when working with the reference level, `FALSE` otherwise.

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- interaction_y:

  (`string` or `flag`)  
  a selected item inside of the `interaction_item` variable which will
  be used to select the specific ANCOVA results. if the interaction is
  not needed, the default option is `FALSE`.

- interaction_item:

  (`string` or `NULL`)  
  name of the variable that should have interactions with arm. if the
  interaction is not needed, the default option is `NULL`.

- weights_emmeans:

  (`string`)  
  argument from
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html),
  `"counterfactual"` by default.

## Value

returns a named list of 8 statistics (3 extra compared to
`tern:::s_ancova()`).

## See also

Other Inclusion of ANCOVA Functions:
[`a_summarize_ancova_j()`](https://johnsonandjohnson.github.io/junco/reference/s_summarize_ancova_j.md),
[`a_summarize_aval_chg_diff_j()`](https://johnsonandjohnson.github.io/junco/reference/a_summarize_aval_chg_diff_j.md)

## Examples

``` r
library(dplyr)
library(tern)

df <- iris |> filter(Species == "virginica")
.df_row <- iris
.var <- "Petal.Length"
variables <- list(arm = "Species", covariates = "Sepal.Length * Sepal.Width")
.ref_group <- iris |> filter(Species == "setosa")
conf_level <- 0.95
s_ancova_j(df, .var, .df_row, variables, .ref_group, .in_ref_col = FALSE, conf_level)
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
