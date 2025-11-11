# Count denom fraction statistic

Derives the count_denom_fraction statistic (i.e., 'xx /xx (xx.x
percent)' ) Summarizes the number of unique subjects with a response =
'Y' for a given variable (e.g. TRTEMFL) within each category of another
variable (e.g., SEX). Note that the denominator is derived using input
df, in order to have these aligned with alt_source_df, it is expected
that df includes all subjects.

## Usage

``` r
response_by_var(
  df,
  labelstr = NULL,
  .var,
  .N_col,
  resp_var = NULL,
  id = "USUBJID",
  .format = jjcsformat_count_denom_fraction,
  ...
)
```

## Arguments

- df:

  Name of dataframe being analyzed.

- labelstr:

  Custom label for the variable being analyzed.

- .var:

  Name of the variable being analyzed. Records with non-missing values
  will be counted in the denominator.

- .N_col:

  numeric(1). The total for the current column.

- resp_var:

  Name of variable, for which, records with a value of 'Y' will be
  counted in the numerator.

- id:

  Name of column in df which will have patient identifiers

- .format:

  Format for the count/denominator/fraction output.

- ...:

  Additional arguments passed to the function.

## Value

a `RowsVerticalSection` for use by the internal tabulation machinery of
`rtables`

## Details

This is an analysis function for use within `analyze`. Arguments `df`,
`.var` will be populated automatically by rtables during the tabulation
process.

## Examples

``` r
library(dplyr)

ADAE <- data.frame(
  USUBJID = c(
    "XXXXX01", "XXXXX02", "XXXXX03", "XXXXX04", "XXXXX05",
    "XXXXX06", "XXXXX07", "XXXXX08", "XXXXX09", "XXXXX10"
  ),
  SEX_DECODE = c(
    "Female", "Female", "Male", "Female", "Male",
    "Female", "Male", "Female", "Male", "Female"
  ),
  TRT01A = c(
    "ARMA", "ARMB", "ARMA", "ARMB", "ARMB",
    "Placebo", "Placebo", "Placebo", "ARMA", "ARMB"
  ),
  TRTEMFL = c("Y", "Y", "N", "Y", "Y", "Y", "Y", "N", "Y", "Y")
)

ADAE <- ADAE |>
  mutate(
    TRT01A = as.factor(TRT01A),
    SEX_DECODE = as.factor(SEX_DECODE)
  )

lyt <- basic_table() |>
  split_cols_by("TRT01A") |>
  analyze(
    vars = "SEX_DECODE",
    var_labels = "Sex, n/Ns (%)",
    show_labels = "visible",
    afun = response_by_var,
    extra_args = list(resp_var = "TRTEMFL"),
    nested = FALSE
  )

result <- build_table(lyt, ADAE)

result
#>                     ARMA           ARMB         Placebo   
#> ——————————————————————————————————————————————————————————
#> Sex, n/Ns (%)                                             
#>   Female        1/1 (100.0%)   3/3 (100.0%)   1/2 (50.0%) 
#>   Male          1/2 (50.0%)    1/1 (100.0%)   1/1 (100.0%)
```
