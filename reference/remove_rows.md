# Pruning function to remove specific rows of a table regardless of counts

This function will remove all rows of a table based on the row text
provided by the user.

## Usage

``` r
remove_rows(removerowtext = NULL, reg_expr = FALSE)
```

## Arguments

- removerowtext:

  (`character`)  
  Define a text string for which any row with row text will be removed.

- reg_expr:

  (`logical`)  
  Apply removerowtext as a regular expression (grepl with fixed = TRUE)

## Value

Function that can be utilized as pruning function in prune_table.

## Examples

``` r
ADSL <- data.frame(
  USUBJID = c(
    "XXXXX01", "XXXXX02", "XXXXX03", "XXXXX04", "XXXXX05",
    "XXXXX06", "XXXXX07", "XXXXX08", "XXXXX09", "XXXXX10"
  ),
  TRT01P = c(
    "ARMA", "ARMB", "ARMA", "ARMB", "ARMB", "Placebo",
    "Placebo", "Placebo", "ARMA", "ARMB"
  ),
  Category = c(
    "Cat 1", "Cat 2", "Cat 1", "Unknown", "Cat 2",
    "Cat 1", "Unknown", "Cat 1", "Cat 2", "Cat 1"
  ),
  SAFFL = c("N", "N", "N", "N", "N", "N", "N", "N", "N", "N"),
  PKFL = c("N", "N", "N", "N", "N", "N", "N", "N", "N", "N")
)

ADSL <- ADSL |>
  dplyr::mutate(TRT01P = as.factor(TRT01P))

lyt <- basic_table() |>
  split_cols_by("TRT01P") |>
  analyze(
    "Category",
    afun = a_freq_j,
    extra_args = list(.stats = "count_unique_fraction")
  )

result <- build_table(lyt, ADSL)

result
#>             ARMA        ARMB       Placebo 
#> ———————————————————————————————————————————
#> Cat 1     2 (66.7%)   1 (25.0%)   2 (66.7%)
#> Cat 2     1 (33.3%)   2 (50.0%)       0    
#> Unknown       0       1 (25.0%)   1 (33.3%)

result <- prune_table(result, prune_func = remove_rows(removerowtext = "Unknown"))

result
#>           ARMA        ARMB       Placebo 
#> —————————————————————————————————————————
#> Cat 1   2 (66.7%)   1 (25.0%)   2 (66.7%)
#> Cat 2   1 (33.3%)   2 (50.0%)       0    
```
