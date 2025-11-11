# Extract Single Visit Information from a Fitted MMRM for Multiple Imputation Analysis

Extracts relevant estimates from a given fitted MMRM. See
[`rbmi_mmrm()`](https://johnsonandjohnson.github.io/junco/reference/rbmi_mmrm.md)
for full details.

## Usage

``` r
rbmi_mmrm_single_info(fit, visit_level, visit, group, weights)
```

## Arguments

- fit:

  (`mmrm`)  
  the fitted MMRM.

- visit_level:

  (`string`)  
  the visit level to extract information for.

- visit:

  (`string`)  
  the name of the visit variable.

- group:

  (`string`)  
  the name of the group variable.

- weights:

  (`string`)  
  the weighting strategy to be used when calculating the least square
  means, either `'counterfactual'` or `'equal'`.

## Value

a list with `trt_*`, `var_*` and `lsm_*` elements. See
[rbmi_mmrm](https://johnsonandjohnson.github.io/junco/reference/rbmi_mmrm.md)
for full details.

## See also

[`rbmi_mmrm()`](https://johnsonandjohnson.github.io/junco/reference/rbmi_mmrm.md)
