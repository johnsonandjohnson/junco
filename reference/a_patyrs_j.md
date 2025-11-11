# Patient years exposure

Statistical/Analysis Function for presenting Patient years exposure
summary data

## Usage

``` r
s_patyrs_j(
  df,
  .var,
  id = "USUBJID",
  .alt_df_full,
  source = c("alt_df", "df"),
  inriskdiffcol = FALSE
)

a_patyrs_j(
  df,
  .var,
  .df_row,
  id = "USUBJID",
  .alt_df_full = NULL,
  .formats = NULL,
  .labels = NULL,
  source = c("alt_df", "df"),
  .spl_context,
  .stats = "patyrs"
)
```

## Arguments

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- .var:

  (`string`)  
  variable name containing the patient years data.

- id:

  (`string`)  
  subject variable name.

- .alt_df_full:

  (`dataframe`)  
  alternative dataset for calculations.

- source:

  (`string`)  
  source of data, either "alt_df" or "df".

- inriskdiffcol:

  (`logical`)  
  flag indicating if the function is called within a risk difference
  column.

- .df_row:

  (`data.frame`)  
  data frame across all of the columns for the given row split.

- .formats:

  (named 'character' or 'list')  
  formats for the statistics.

- .labels:

  (named 'character')  
  labels for the statistics.

- .spl_context:

  (`data.frame`)  
  gives information about ancestor split states.

- .stats:

  (`character`)  
  statistics to select for the table.

## Value

- `s_patyrs_j()` return x a list containing the patient years
  statistics. The list of available statistics for can be viewed by
  running `junco_get_stats("a_patyrs_j")`, currently this is just a
  single statistic `patyrs`, patient years of exposure.

&nbsp;

- `a_patyrs_j` returns the corresponding list with formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Functions

- `s_patyrs_j()`: Statistical Function for Patient years exposure
  summary data

- `a_patyrs_j()`: Formatted analysis function for patient years summary
  which is used as `afun` in `analyze` or `cfun` in
  `summarize_row_groups`.

## Examples

``` r
library(tern)
library(dplyr)
trtvar <- "ARM"
ctrl_grp <- "B: Placebo"
cutoffd <- as.Date("2023-09-24")


adexsum <- ex_adsl %>%
  create_colspan_var(
    non_active_grp          = ctrl_grp,
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl     = "Active Study Agent",
    colspan_var             = "colspan_trt",
    trt_var                 = trtvar
  ) %>%
  mutate(
    rrisk_header = "Risk Difference (95% CI)",
    rrisk_label = paste(!!rlang::sym(trtvar), "vs", ctrl_grp),
    TRTDURY = case_when(
      !is.na(EOSDY) ~ EOSDY,
      TRUE ~ as.integer(cutoffd - as.Date(TRTSDTM) + 1)
    )
  ) %>%
  select(USUBJID, !!rlang::sym(trtvar), colspan_trt, rrisk_header, rrisk_label, TRTDURY)

adae <- ex_adae %>%
  group_by(USUBJID, AEDECOD) %>%
  select(USUBJID, AEDECOD, ASTDY) %>%
  mutate(rwnum = row_number()) %>%
  mutate(AOCCPFL = case_when(
    rwnum == 1 ~ "Y",
    TRUE ~ NA
  )) %>%
  filter(AOCCPFL == "Y")

aefup <- left_join(adae, adexsum, by = "USUBJID")

colspan_trt_map <- create_colspan_map(adexsum,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

ref_path <- c("colspan_trt", " ", trtvar, ctrl_grp)

lyt <- basic_table(show_colcounts = TRUE, colcount_format = "N=xx", top_level_section_div = " ") %>%
  split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map)) %>%
  split_cols_by(trtvar) %>%
  split_cols_by("rrisk_header", nested = FALSE) %>%
  split_cols_by(trtvar, labels_var = "rrisk_label", split_fun = remove_split_levels(ctrl_grp)) %>%
  analyze("TRTDURY",
    nested = FALSE,
    show_labels = "hidden",
    afun = a_patyrs_j
  )
result <- build_table(lyt, aefup, alt_counts_df = adexsum)
result
#>                     Active Study Agent                                   Risk Difference (95% CI)               
#>                 A: Drug X   C: Combination   B: Placebo   A: Drug X vs B: Placebo   C: Combination vs B: Placebo
#>                   N=134         N=132          N=134               N=134                       N=132            
#> ————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Patient years    96200.0       91753.0        91431.0                                                           
```
