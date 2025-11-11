# Count Pruner

This is a pruning constructor function which identifies records to be
pruned based on the count (assumed to be the first statistic displayed
when a compound statistic (e.g., \## / \## (XX.X percent) is presented).

## Usage

``` r
count_pruner(
  count = 0,
  cat_include = NULL,
  cat_exclude = NULL,
  cols = c("TRT01A")
)
```

## Arguments

- count:

  count threshold. Function will keep all records strictly greater than
  this threshold.

- cat_include:

  Category to be considered for pruning

- cat_exclude:

  logical Category to be excluded from pruning

- cols:

  column path (character or integer (column indices))

## Value

function that can be utilized as pruning function in prune_table

## Examples

``` r
ADSL <- data.frame(
  USUBJID = c(
    "XXXXX01", "XXXXX02", "XXXXX03", "XXXXX04", "XXXXX05",
    "XXXXX06", "XXXXX07", "XXXXX08", "XXXXX09", "XXXXX10"
  ),
  TRT01P = factor(
    c(
      "ARMA", "ARMB", "ARMA", "ARMB", "ARMB",
      "Placebo", "Placebo", "Placebo", "ARMA", "ARMB"
    )
  ),
  FASFL = c("Y", "Y", "Y", "Y", "N", "Y", "Y", "Y", "Y", "Y"),
  SAFFL = c("N", "N", "N", "N", "N", "N", "N", "N", "N", "N"),
  PKFL = c("N", "N", "N", "N", "N", "N", "N", "N", "N", "N")
)

lyt <- basic_table() |>
  split_cols_by("TRT01P") |>
  add_overall_col("Total") |>
  analyze("FASFL",
    var_labels = "Analysis set:",
    afun = a_freq_j,
    extra_args = list(label = "Full", val = "Y"),
    show_labels = "visible"
  ) |>
  analyze("SAFFL",
    var_labels = "Analysis set:",
    afun = a_freq_j,
    extra_args = list(label = "Safety", val = "Y"),
    show_labels = "visible"
  ) |>
  analyze("PKFL",
    var_labels = "Analysis set:",
    afun = a_freq_j,
    extra_args = list(label = "PK", val = "Y"),
    show_labels = "visible"
  )

result <- build_table(lyt, ADSL)

result
#>                     ARMA          ARMB         Placebo         Total    
#> ————————————————————————————————————————————————————————————————————————
#> Analysis set:                                                           
#>   Full          3/3 (100.0%)   3/4 (75.0%)   3/3 (100.0%)   9/10 (90.0%)
#> Analysis set:                                                           
#>   Safety         0/3 (0.0%)    0/4 (0.0%)     0/3 (0.0%)    0/10 (0.0%) 
#> Analysis set:                                                           
#>   PK             0/3 (0.0%)    0/4 (0.0%)     0/3 (0.0%)    0/10 (0.0%) 

result <- prune_table(
  result,
  prune_func = count_pruner(cat_exclude = c("Safety"), cols = "Total")
)

result
#>                     ARMA          ARMB         Placebo         Total    
#> ————————————————————————————————————————————————————————————————————————
#> Analysis set:                                                           
#>   Full          3/3 (100.0%)   3/4 (75.0%)   3/3 (100.0%)   9/10 (90.0%)
#> Analysis set:                                                           
#>   Safety         0/3 (0.0%)    0/4 (0.0%)     0/3 (0.0%)    0/10 (0.0%) 
```
