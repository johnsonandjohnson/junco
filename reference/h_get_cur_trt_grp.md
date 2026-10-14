# Get Current Treatment Group

**\[stable\]**

Retrieves the current treatment group from the current column
split-path, given the treatment variable name.

## Usage

``` r
h_get_cur_trt_grp(trt_var, .spl_context)
```

## Arguments

- trt_var:

  (`character(1)`)  
  The treatment variable name.

- .spl_context:

  (`data.frame`)  
  The current split context.

## Value

A character string containing the treatment group name.

## See also

[`cur_col_split_path()`](https://johnsonandjohnson.github.io/junco/reference/cur_col_split_path_utils.md)

## Author

WW

## Examples

``` r
if (FALSE) { # \dontrun{
.spl_context <- data.frame(
  cur_col_split = I(list(c("ARM"))),
  cur_col_split_val = I(list(c("Placebo")))
)

h_get_cur_trt_grp("ARM", .spl_context)
h_get_cur_trt_grp("TRT", .spl_context) # errors: TRT not found
} # }
```
