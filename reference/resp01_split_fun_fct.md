# Split Function Factory for the Response Tables (RESP01)

The main purpose here is to have a column dependent split into either
comparative statistic (relative risk or odds ratio with p-value) in the
'Overall' column, and count proportions and corresponding confidence
intervals in the other treatment arm columns.

## Usage

``` r
resp01_split_fun_fct(method = c("rr", "or_logistic", "or_cmh"), conf_level)
```

## Arguments

- method:

  (`string`)  
  which method to use for the comparative statistics.

- conf_level:

  (`proportion`)  
  confidence level of the interval.

## Value

A split function for use in the response table RESP01 and similar ones.

## See also

[`rtables::make_split_fun()`](https://insightsengineering.github.io/rtables/latest-tag/reference/make_split_fun.html)
describing the requirements for this kind of post-processing function.

## Examples

``` r
split_fun <- resp01_split_fun_fct(
  method = "or_cmh",
  conf_level = 0.95
)
```
