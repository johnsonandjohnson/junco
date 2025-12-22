# Complex Scoring Function

A function used for sorting AE tables (and others) as required.

## Usage

``` r
jj_complex_scorefun(
  spanningheadercolvar = "colspan_trt",
  usefirstcol = FALSE,
  colpath = NULL,
  firstcat = NULL,
  lastcat = NULL
)
```

## Arguments

- spanningheadercolvar:

  (`character`)  
  Name of spanning header variable that defines the active treatment
  columns. If you do not have an active treatment spanning header column
  then user can define this as NA.

- usefirstcol:

  (`logical`)  
  This allows you to just use the first column of the table to sort on.

- colpath:

  (`character`)  
  Name of column path that is needed to sort by (default=NULL). This
  overrides other arguments if specified (except firstcat and lastcat
  which will be applied if requested on this colpath).

- firstcat:

  (`logical`)  
  If you wish to put any category at the top of the list despite any
  n's, user can specify it here.

- lastcat:

  (`logical`)  
  If you wish to put any category at the bottom of the list despite any
  n's, user can specify it here.

## Value

A function which can be used as a score function (scorefun in
`sort_at_path`).

## Details

This sort function sorts as follows:

- Takes all the columns from a specified spanning column header
  (default= colspan_trt) and sorts by the last treatment column within
  this.

- If no spanning column header variable exists (e.g you have only one
  active treatment arm and have decided to remove the spanning header
  from your layout), it will sort by the first treatment column in your
  table.

This function is not really designed for tables that have sub-columns.
However, if users wish to override any default sorting behavior, they
can simply specify their own colpath to use for sorting on (default =
NULL)

## Examples

``` r
library(dplyr)
ADAE <- data.frame(
  USUBJID = c(
    "XXXXX01", "XXXXX02", "XXXXX03", "XXXXX04", "XXXXX05",
    "XXXXX06", "XXXXX07", "XXXXX08", "XXXXX09", "XXXXX10"
  ),
  AEBODSYS = c(
    "SOC 1", "SOC 2", "SOC 1", "SOC 2", "SOC 2",
    "SOC 2", "SOC 2", "SOC 1", "SOC 2", "SOC 1"
  ),
  AEDECOD = c(
    "Coded Term 2", "Coded Term 1", "Coded Term 3", "Coded Term 4",
    "Coded Term 4", "Coded Term 4", "Coded Term 5", "Coded Term 3",
    "Coded Term 1", "Coded Term 2"
  ),
  TRT01A = c(
    "ARMA", "ARMB", "ARMA", "ARMB", "ARMB",
    "Placebo", "Placebo", "Placebo", "ARMA", "ARMB"
  ),
  TRTEMFL = c("Y", "Y", "N", "Y", "Y", "Y", "Y", "N", "Y", "Y")
)

ADAE <- ADAE |>
  dplyr::mutate(TRT01A = as.factor(TRT01A))

ADAE$colspan_trt <- factor(ifelse(ADAE$TRT01A == "Placebo", " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

ADAE$rrisk_header <- "Risk Difference (%) (95% CI)"
ADAE$rrisk_label <- paste(ADAE$TRT01A, paste("vs", "Placebo"))

colspan_trt_map <- create_colspan_map(ADAE,
  non_active_grp = "Placebo",
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = "TRT01A"
)

ref_path <- c("colspan_trt", " ", "TRT01A", "Placebo")

ADSL <- unique(ADAE |> select(USUBJID, "colspan_trt", "rrisk_header", "rrisk_label", "TRT01A"))

lyt <- basic_table() |>
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) |>
  split_cols_by("TRT01A") |>
  split_cols_by("rrisk_header", nested = FALSE) |>
  split_cols_by(
    "TRT01A",
    labels_var = "rrisk_label",
    split_fun = remove_split_levels("Placebo")
  ) |>
  analyze(
    "TRTEMFL",
    a_freq_j,
    show_labels = "hidden",
    extra_args = list(
      method = "wald",
      label = "Subjects with >=1 AE",
      ref_path = ref_path,
      .stats = "count_unique_fraction"
    )
  ) |>
  split_rows_by("AEBODSYS",
    split_label = "System Organ Class",
    split_fun = trim_levels_in_group("AEDECOD"),
    label_pos = "topleft",
    section_div = c(" "),
    nested = FALSE
  ) |>
  summarize_row_groups(
    "AEBODSYS",
    cfun = a_freq_j,
    extra_args = list(
      method = "wald",
      ref_path = ref_path,
      .stats = "count_unique_fraction"
    )
  ) |>
  analyze(
    "AEDECOD",
    afun = a_freq_j,
    extra_args = list(
      method = "wald",
      ref_path = ref_path,
      .stats = "count_unique_fraction"
    )
  )

result <- build_table(lyt, ADAE, alt_counts_df = ADSL)

result
#>                          Active Study Agent                          Risk Difference (%) (95% CI)       
#> System Organ Class        ARMA         ARMB       Placebo       ARMA vs Placebo        ARMB vs Placebo  
#> ————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Subjects with >=1 AE   3 (100.0%)   4 (100.0%)   3 (100.0%)      0.0 (0.0, 0.0)        0.0 (0.0, 0.0)   
#> SOC 1                  2 (66.7%)    1 (25.0%)    1 (33.3%)    33.3 (-42.1, 100.0)    -8.3 (-76.5, 59.8) 
#>   Coded Term 2         1 (33.3%)    1 (25.0%)        0         33.3 (-20.0, 86.7)    25.0 (-17.4, 67.4) 
#>   Coded Term 3         1 (33.3%)        0        1 (33.3%)     0.0 (-75.4, 75.4)     -33.3 (-86.7, 20.0)
#>                                                                                                         
#> SOC 2                  1 (33.3%)    3 (75.0%)    2 (66.7%)    -33.3 (-100.0, 42.1)    8.3 (-59.8, 76.5) 
#>   Coded Term 1         1 (33.3%)    1 (25.0%)        0         33.3 (-20.0, 86.7)    25.0 (-17.4, 67.4) 
#>   Coded Term 4             0        2 (50.0%)    1 (33.3%)    -33.3 (-86.7, 20.0)    16.7 (-55.8, 89.1) 
#>   Coded Term 5             0            0        1 (33.3%)    -33.3 (-86.7, 20.0)    -33.3 (-86.7, 20.0)

result <- sort_at_path(
  result,
  c("root", "AEBODSYS"),
  scorefun = jj_complex_scorefun()
)

result <- sort_at_path(
  result,
  c("root", "AEBODSYS", "*", "AEDECOD"),
  scorefun = jj_complex_scorefun()
)

result
#>                          Active Study Agent                          Risk Difference (%) (95% CI)       
#> System Organ Class        ARMA         ARMB       Placebo       ARMA vs Placebo        ARMB vs Placebo  
#> ————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Subjects with >=1 AE   3 (100.0%)   4 (100.0%)   3 (100.0%)      0.0 (0.0, 0.0)        0.0 (0.0, 0.0)   
#> SOC 2                  1 (33.3%)    3 (75.0%)    2 (66.7%)    -33.3 (-100.0, 42.1)    8.3 (-59.8, 76.5) 
#>   Coded Term 4             0        2 (50.0%)    1 (33.3%)    -33.3 (-86.7, 20.0)    16.7 (-55.8, 89.1) 
#>   Coded Term 1         1 (33.3%)    1 (25.0%)        0         33.3 (-20.0, 86.7)    25.0 (-17.4, 67.4) 
#>   Coded Term 5             0            0        1 (33.3%)    -33.3 (-86.7, 20.0)    -33.3 (-86.7, 20.0)
#>                                                                                                         
#> SOC 1                  2 (66.7%)    1 (25.0%)    1 (33.3%)    33.3 (-42.1, 100.0)    -8.3 (-76.5, 59.8) 
#>   Coded Term 2         1 (33.3%)    1 (25.0%)        0         33.3 (-20.0, 86.7)    25.0 (-17.4, 67.4) 
#>   Coded Term 3         1 (33.3%)        0        1 (33.3%)     0.0 (-75.4, 75.4)     -33.3 (-86.7, 20.0)
```
