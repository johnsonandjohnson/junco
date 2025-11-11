# Extract Substring from Column Expression

Retrieves the substring from a column expression related to a variable
component.

## Usage

``` r
h_colexpr_substr(var, col_expr)
```

## Arguments

- var:

  Variable to extract from the expression.

- col_expr:

  Column expression string.

## Value

Substring corresponding to the variable.

## Details

get substring from col_expr related to var component intended usage is
on strings coming from .spl_context\$cur_col_expr these strings are of
type '!(is.na(var) & var %in% 'xxx') & !(is.na(var2) & var2 %in% 'xxx')'
