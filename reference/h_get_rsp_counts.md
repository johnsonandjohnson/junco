# Helper Function to Extract Count Responders

Converts a response vector, or a response column from a data frame, to
logical values and returns its responder and observation counts.

## Usage

``` r
h_get_rsp_counts(x, .var, na.rm = FALSE)
```

## Arguments

- x:

  (`vector` or `data.frame`)  
  Response data.

- .var:

  (`string`)  
  Response column in `x` when `x` is a data frame.

- na.rm:

  (`flag`)  
  Whether to remove missing responses before counting.

## Value

A named list with `rsp` (logical response vector), `n_rsp` (responders)
and `len_rsp` (non-missing observations, when `na.rm = TRUE`).
