# Wrappers Around `tern::a_summary()` with Row Split Exclusion

**\[stable\]**

These wrappers around
[`tern::a_summary()`](https://pharmaverse.github.io/tern/latest-tag/reference/analyze_variables.html)
allow selected row split levels to be excluded from producing the
analysis.

## Usage

``` r
a_summary_j_with_exclude(df, .var, exclude_levels, .spl_context, ...)
```

## Arguments

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- .var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

- exclude_levels:

  (`list`)  
  A named list where names correspond to split variables and values are
  vectors of levels to exclude.

- .spl_context:

  (`data.frame`)  
  gives information about ancestor split states that is passed by
  `rtables`.

- ...:

  Additional arguments passed to
  [`tern::a_summary()`](https://pharmaverse.github.io/tern/latest-tag/reference/analyze_variables.html),
  excluding the `x` argument, which is constructed internally from
  `df[[.var]]`.

## Value

Returns the same type of object as
[`tern::a_summary()`](https://pharmaverse.github.io/tern/latest-tag/reference/analyze_variables.html),
or `NULL` when the current row split path is excluded by
`exclude_levels`.

## Details

[`tern::a_summary()`](https://pharmaverse.github.io/tern/latest-tag/reference/analyze_variables.html)
is a formatted analysis function that computes summary statistics for a
single variable. In typical use within the
[rtables](https://insightsengineering.github.io/rtables/latest-tag/reference/rtables-package.html)
framework, the analysis variable is first extracted from a data frame
and then passed to
[`tern::a_summary()`](https://pharmaverse.github.io/tern/latest-tag/reference/analyze_variables.html)
as a standalone vector.

`a_summary_j_with_exclude()` extends this behavior by:

- accepting a data frame (`df`) as input,

- checking whether the current row split path should be excluded,

- returning `NULL` for excluded row split levels,

- extracting the analysis variable `.var` for all other row split
  levels,

- and passing the resulting vector to
  [`tern::a_summary()`](https://pharmaverse.github.io/tern/latest-tag/reference/analyze_variables.html).

## Functions

- `a_summary_j_with_exclude()`: Analysis function which can exclude row
  split levels from producing the analysis. These have to be specified
  in the `exclude_levels` argument, see
  [`?do_exclude_split`](https://johnsonandjohnson.github.io/junco/reference/do_exclude_split.md)
  for details.

## See also

[`tern::a_summary()`](https://pharmaverse.github.io/tern/latest-tag/reference/analyze_variables.html),
[`do_exclude_split()`](https://johnsonandjohnson.github.io/junco/reference/do_exclude_split.md)

## Examples

``` r
df <- data.frame(
  USUBJID = 1:8,
  AVISIT = factor(rep(c("Baseline", "Week 1"), 4)),
  ARM = factor(rep(c("A", "B"), each = 4)),
  AVAL = c(1.2, 3, -1.5, 0.8, 3.5, 2, -1.1, 0.3)
)

lyt <- basic_table() |>
  split_cols_by("ARM") |>
  split_rows_by("AVISIT") |>
  analyze(
    "AVAL",
    afun = a_summary_j_with_exclude,
    extra_args = list(exclude_levels = list(AVISIT = "Baseline"))
  )

build_table(lyt, df)
#>                                           A                        B           
#> ———————————————————————————————————————————————————————————————————————————————
#> Baseline                                                                       
#>   a_summary_j_with_exclude                                                     
#> Week 1                                                                         
#>   n                                       2                        2           
#>   Sum                                    3.8                      2.3          
#>   Mean                                   1.9                      1.1          
#>   SD                                     1.6                      1.2          
#>   SE                                     1.1                      0.8          
#>   Mean (SD)                           1.9 (1.6)                1.1 (1.2)       
#>   Mean (SE)                           1.9 (1.1)                1.1 (0.8)       
#>   Mean 95% CI                      (-12.08, 15.88)          (-9.65, 11.95)     
#>   Mean -/+ 1xSE                     (0.80, 3.00)             (0.30, 2.00)      
#>   Mean -/+ 1xSD                     (0.34, 3.46)             (-0.05, 2.35)     
#>   Mean p-value (H0: mean = 0)          0.3341                   0.4052         
#>   Median                                 1.9                      1.1          
#>   Median Absolute Deviation              0.0                      0.0          
#>   Median 95% CI                          NA                       NA           
#>   25% and 75%-ile                     0.8 - 3.0                0.3 - 2.0       
#>   IQR                                    2.2                      1.7          
#>   Min - Max                           0.8 - 3.0                0.3 - 2.0       
#>   Minimum                                0.8                      0.3          
#>   Maximum                                3.0                      2.0          
#>   Median (Min - Max)               1.9 (0.8 - 3.0)          1.1 (0.3 - 2.0)    
#>   CV (%)                                81.9                     104.5         
#>   Geometric Mean                         1.5                      0.8          
#>   Geometric SD                           2.5                      3.8          
#>   Geometric Mean (SD)                 1.5 (2.5)                0.8 (3.8)       
#>   Geometric Mean 95% CI            (0.00, 6870.45)         (0.00, 132877.73)   
#>   CV % Geometric Mean                   118.1                    224.7         
#>   Median (95% CI)                  1.90 (NA - NA)           1.15 (NA - NA)     
#>   Mean (95% CI)                 1.90 (-12.08 - 15.88)    1.15 (-9.65 - 11.95)  
#>   Geometric Mean (95% CI)       1.55 (0.00 - 6870.45)   0.77 (0.00 - 132877.73)
```
