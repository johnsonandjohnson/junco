# Implements an Analysis of Covariance (ANCOVA)

Performance analysis of covariance. See
[`rbmi_ancova()`](https://johnsonandjohnson.github.io/junco/reference/rbmi_ancova.md)
for full details.

## Usage

``` r
rbmi_ancova_single(
  data,
  outcome,
  group,
  covariates,
  weights = c("counterfactual", "equal", "proportional_em", "proportional")
)
```

## Arguments

- data:

  A `data.frame` containing the data to be used in the model.

- outcome:

  string, the name of the outcome variable in `data`.

- group:

  string, the name of the group variable in `data`.

- covariates:

  character vector containing the name of any additional covariates to
  be included in the model as well as any interaction terms.

- weights:

  Character, either `"counterfactual"` (default), `"equal"`,
  `"proportional_em"` or `"proportional"`. Specifies the weighting
  strategy to be used when calculating the lsmeans. See the weighting
  section for more details.

## Value

a list containing `var` with variance estimates as well as `trt_*` and
`lsm_*` entries. See
[`rbmi_ancova()`](https://johnsonandjohnson.github.io/junco/reference/rbmi_ancova.md)
for full details.

## Details

- `group` must be a factor variable with only 2 levels.

- `outcome` must be a continuous numeric variable.

## See also

[`rbmi_ancova()`](https://johnsonandjohnson.github.io/junco/reference/rbmi_ancova.md)

## Examples

``` r
if (requireNamespace("rbmi", quietly = TRUE)) {
  iris2 <- iris[iris$Species %in% c("versicolor", "virginica"), ]
  iris2$Species <- factor(iris2$Species)
  rbmi_ancova_single(iris2, "Sepal.Length", "Species", c("Petal.Length * Petal.Width"))
}
#> $var
#> $var$est
#> [1] 0.1128236
#> 
#> $var$se
#> [1] 0.01637017
#> 
#> $var$df
#> [1] 95
#> 
#> 
#> $trt_virginica
#> $trt_virginica$est
#> Speciesvirginica 
#>       -0.5010775 
#> 
#> $trt_virginica$se
#> [1] 0.1271019
#> 
#> $trt_virginica$df
#> [1] 95
#> 
#> 
#> $lsm_versicolor
#> $lsm_versicolor$est
#> [1] 6.512539
#> 
#> $lsm_versicolor$se
#> [1] 0.07188155
#> 
#> $lsm_versicolor$df
#> [1] 95
#> 
#> 
#> $lsm_virginica
#> $lsm_virginica$est
#> [1] 6.011461
#> 
#> $lsm_virginica$se
#> [1] 0.07188155
#> 
#> $lsm_virginica$df
#> [1] 95
#> 
#> 
```
