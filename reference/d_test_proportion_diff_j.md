# Description of the difference test between two proportions

**\[stable\]**

This is an auxiliary function that describes the analysis in
`s_test_proportion_diff`.

## Usage

``` r
d_test_proportion_diff_j(method, alternative)
```

## Arguments

- method:

  (`string`)  
  one of `chisq`, `cmh`, `fisher`; specifies the test used to calculate
  the p-value.

- alternative:

  (`string`)  
  whether `two.sided`, or one-sided `less` or `greater` p-value should
  be displayed.

## Value

A `string` describing the test from which the p-value is derived.
