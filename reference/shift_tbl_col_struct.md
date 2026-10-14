# Create Column Structure for a J&J-Style Shift Table

Create Column Structure for a J&J-Style Shift Table

## Usage

``` r
shift_tbl_col_struct(lyt, var, span_lbl = "Baseline", .outer_spl_var = var)
```

## Arguments

- lyt:

  (`PreDataTableLayouts`)  
  The layout, typically as returned directly from `basic_table`.

- var:

  (`character(1)`)  
  The variable defining the partition to create a shift table for.

- span_lbl:

  (`character(1)`)  
  The spanning label for the shift portion of the column structure.
  Defaults to `"Baseline"`.

- .outer_spl_var:

  (`character(1)`)  
  The variable to "split on" when creating the spanning labels. Must
  exist in the dataset(s) used in `build_table` but its values are
  irrelevant.

## Value

`lyt` updated with the specified shift table column structure.

## Details

This function creates a column structure akin to the following (with
Grade 1 through Grade 5 representing the levels of `var`):

                                  Baseline
      N   Grade 1   Grade 2   Grade 3   Grade 4   Grade 5   Total
    ——————————————————————————————————————————————————————————————

## See also

Other std_col_struct:
[`grouped_cols_w_diffs()`](https://johnsonandjohnson.github.io/junco/reference/grouped_cols_w_diffs.md),
[`make_multicomp_splfun()`](https://johnsonandjohnson.github.io/junco/reference/make_multicomp_splfun.md)

## Examples

``` r
shift_data <- data.frame(
  BASE = factor(c("Grade 1", "Grade 2", "Grade 3")),
  CHG = c("Improved", "Stable", "Worsened")
)

lyt <- basic_table() |>
  shift_tbl_col_struct("BASE") |>
  analyze("CHG", afun = function(x, ...) length(x))

build_table(lyt, shift_data)
#>                        Baseline              
#>       N   Grade 1   Grade 2   Grade 3   Total
#> —————————————————————————————————————————————
#> CHG   3      1         1         1        3  
```
