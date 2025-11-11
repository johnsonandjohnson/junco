# Helper Function to Create Logical Design Matrix from Factor Variable

Helper Function to Create Logical Design Matrix from Factor Variable

## Usage

``` r
h_get_design_mat(df, .var)
```

## Arguments

- df:

  (`data.frame`)  
  including a factor variable with name in `.var`.

- .var:

  (`string`)  
  name of the factor variable.

## Value

The logical matrix with dummy encoding of all factor levels.

## Examples

``` r
h_get_design_mat(df = data.frame(a = factor(c("a", "b", "a"))), .var = "a")
#>       a     b
#> 1  TRUE FALSE
#> 2 FALSE  TRUE
#> 3  TRUE FALSE
#> attr(,"assign")
#> [1] 1 1
#> attr(,"contrasts")
#> attr(,"contrasts")$a
#> [1] "contr.treatment"
#> 
```
