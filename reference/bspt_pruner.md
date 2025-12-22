# Pruning Function for pruning based on a fraction and/or a difference from the control arm

This is a pruning constructor function which identifies records to be
pruned based on the the fraction from the percentages. In addition to
just looking at a fraction within an arm, this function also allows
further flexibility to also prune based on a comparison versus the
control arm.

## Usage

``` r
bspt_pruner(
  fraction = 0.05,
  keeprowtext = "Analysis set: Safety",
  reg_expr = FALSE,
  control = NULL,
  diff_from_control = NULL,
  only_more_often = TRUE,
  cols = c("TRT01A")
)
```

## Arguments

- fraction:

  (`proportion`)  
  Fraction threshold. Function will keep all records strictly greater
  than this threshold.

- keeprowtext:

  (`character`)  
  Row to be excluded from pruning.

- reg_expr:

  (`logical`)  
  Apply keeprowtext as a regular expression (grepl with fixed = TRUE)

- control:

  (`character`)  
  Control Group

- diff_from_control:

  (`numeric`)  
  Difference from control threshold.

- only_more_often:

  (`logical`)  
  TRUE: Only consider when column pct is more often than control. FALSE:
  Also select a row where column pct is less often than control and
  abs(diff) above threshold

- cols:

  (`character`)  
  Column path.

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
    "ARMA", "ARMB", "ARMA", "ARMB", "ARMB",
    "Placebo", "Placebo", "Placebo", "ARMA", "ARMB"
  ),
  FASFL = c("Y", "Y", "Y", "Y", "N", "Y", "Y", "Y", "Y", "Y"),
  SAFFL = c("N", "N", "N", "N", "N", "N", "N", "N", "N", "N"),
  PKFL = c("N", "N", "N", "N", "N", "N", "N", "N", "N", "N")
)

ADSL <- ADSL |>
  dplyr::mutate(TRT01P = as.factor(TRT01P)) |>
  dplyr::mutate(SAFFL = factor(SAFFL, c("Y", "N"))) |>
  dplyr::mutate(PKFL = factor(PKFL, c("Y", "N")))

lyt <- basic_table() |>
  split_cols_by("TRT01P") |>
  add_overall_col("Total") |>
  split_rows_by(
    "FASFL",
    split_fun = drop_and_remove_levels("N"),
    child_labels = "hidden"
  ) |>
  analyze("FASFL",
    var_labels = "Analysis set:",
    afun = a_freq_j,
    show_labels = "visible",
    extra_args = list(label = "Full", .stats = "count_unique_fraction")
  ) |>
  split_rows_by(
    "SAFFL",
    split_fun = remove_split_levels("N"),
    child_labels = "hidden"
  ) |>
  analyze("SAFFL",
    var_labels = "Analysis set:",
    afun = a_freq_j,
    show_labels = "visible",
    extra_args = list(label = "Safety", .stats = "count_unique_fraction")
  ) |>
  split_rows_by(
    "PKFL",
    split_fun = remove_split_levels("N"),
    child_labels = "hidden"
  ) |>
  analyze("PKFL",
    var_labels = "Analysis set:",
    afun = a_freq_j,
    show_labels = "visible",
    extra_args = list(label = "PK", .stats = "count_unique_fraction")
  )

result <- build_table(lyt, ADSL)

result
#>                    ARMA        ARMB       Placebo       Total  
#> ———————————————————————————————————————————————————————————————
#> Analysis set:                                                  
#>   Full          3 (100.0%)   3 (75.0%)   3 (100.0%)   9 (90.0%)
#> Analysis set:                                                  
#>   Safety            0            0           0            0    
#> Analysis set:                                                  
#>   PK                0            0           0            0    

result <- prune_table(
  result,
  prune_func = bspt_pruner(
    fraction = 0.05,
    keeprowtext = "Safety",
    cols = c("Total")
  )
)

result
#>                    ARMA        ARMB       Placebo       Total  
#> ———————————————————————————————————————————————————————————————
#> Analysis set:                                                  
#>   Full          3 (100.0%)   3 (75.0%)   3 (100.0%)   9 (90.0%)
#> Analysis set:                                                  
#>   Safety            0            0           0            0    
```
