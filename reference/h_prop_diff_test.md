# Helper functions to test proportion differences

Helper functions to implement various tests on the difference between
two proportions.

## Usage

``` r
prop_chisq(tbl, alternative)

prop_cmh(ary, alternative)

prop_fisher(tbl, alternative)
```

## Arguments

- tbl:

  (`matrix`)  
  matrix with two groups in rows and the binary response
  (`TRUE`/`FALSE`) in columns.

- ary:

  (`array`, 3 dimensions)  
  array with two groups in rows, the binary response (`TRUE`/`FALSE`) in
  columns, and the strata in the third dimension.

## Value

A p-value.

## Functions

- `prop_chisq()`: Performs Chi-Squared test. Internally calls
  [`stats::prop.test()`](https://rdrr.io/r/stats/prop.test.html).

- `prop_cmh()`: Performs stratified Cochran-Mantel-Haenszel test.
  Internally calls
  [`stats::mantelhaen.test()`](https://rdrr.io/r/stats/mantelhaen.test.html).

- `prop_fisher()`: Performs the Fisher's exact test. Internally calls
  [`stats::fisher.test()`](https://rdrr.io/r/stats/fisher.test.html).

## Note

strata with less than five observations will result in a warning and
possibly incorrect results; strata with less than two observations are
automatically discarded.

## See also

[`prop_diff_test()`](https://johnsonandjohnson.github.io/junco/reference/prop_diff_test.md)
for implementation of these helper functions.
