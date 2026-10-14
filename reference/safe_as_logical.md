# Safe Conversion to Logical Vector

Safe Conversion to Logical Vector

## Usage

``` r
safe_as_logical(x, na.rm = FALSE)
```

## Arguments

- x:

  (`vector`)  
  The input vector to be safely converted to a logical vector.

- na.rm:

  (`flag`)  
  Whether to remove missing values after conversion.

## Value

A logical vector corresponding to the input `x`. If the conversion
introduces unexpected `NA` values, an error is raised.
