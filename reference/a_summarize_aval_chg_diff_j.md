# Analysis function 3-column presentation

Analysis functions to produce a 1-row summary presented in a 3-column
layout in the columns (column 1 = N, column 2 = Value, column 3 =
Change).  
In the difference columns, only 1 column will be presented :
difference + CI  
When ancova = `TRUE`, the presented statistics will be based on ANCOVA
method (`s_summarize_ancova_j`).  
mean and ci (both for Value (column 2) and CHG (column 3)) using
statistic `lsmean_ci`  
mean and ci for the difference column are based on same ANCOVA model
using statistic `lsmean_diffci`  
When ancova = `FALSE`, descriptive statistics will be used instead.  
In the difference column, the 2-sample t-test will be used.

## Usage

``` r
a_summarize_aval_chg_diff_j(
  df,
  .df_row,
  .spl_context,
  ancova = FALSE,
  comp_btw_group = TRUE,
  ref_path = NULL,
  .N_col,
  denom = c("N", ".N_col"),
  indatavar = NULL,
  d = 0,
  id = "USUBJID",
  interaction_y = FALSE,
  interaction_item = NULL,
  conf_level = 0.95,
  variables = list(arm = "TRT01A", covariates = NULL),
  format_na_str = "",
  .stats = list(col1 = "count_denom_frac", col23 = "mean_ci_3d", coldiff =
    "meandiff_ci_3d"),
  .formats = list(col1 = NULL, col23 = "xx.dx (xx.dx, xx.dx)", coldiff =
    "xx.dx (xx.dx, xx.dx)"),
  .formats_fun = list(col1 = jjcsformat_count_denom_fraction, col23 = jjcsformat_xx,
    coldiff = jjcsformat_xx),
  multivars = c("AVAL", "AVAL", "CHG")
)
```

## Arguments

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- .df_row:

  (`data.frame`)  
  data frame across all of the columns for the given row split.

- .spl_context:

  (`data.frame`)  
  gives information about ancestor split states that is passed by
  `rtables`.

- ancova:

  (`logical`)  
  If FALSE, only descriptive methods will be used.  
  If TRUE, ANCOVA methods will be used for each of the columns : AVAL,
  CHG, DIFF.  

- comp_btw_group:

  (`logical`)  
  If TRUE, comparison between groups will be performed.  
  When ancova = FALSE, the estimate of between group difference (on CHG)
  will be based upon a two-sample t-test.  
  When ancova = TRUE, the same ANCOVA model will be used for the
  estimate of between group difference (on CHG).

- ref_path:

  (`character`)  
  global reference group specification, see
  [`get_ref_info()`](https://johnsonandjohnson.github.io/junco/reference/get_ref_info.md).

- .N_col:

  (`integer`)  
  column-wise N (column count) for the full column being analyzed that
  is typically passed by `rtables`.

- denom:

  (`string`)  
  choice of denominator for proportions. Options are:

  - `N`: number of records in this column/row split.  
    There is no check in place that the current split only has one
    record per subject. Users should be careful with this.

  - `.N_col`: number of records in this column intersection (based on
    alt_counts_df dataset)  
    (when alt_counts_df is a single record per subjects, this will match
    number of subjects)

- indatavar:

  (`string`)  
  If not null, variable name to extra subset incoming df to non-missing
  values of this variable.

- d:

  (default = 1)  
  choice of Decimal precision. Note that one extra precision will be
  added, as means are presented.  
  Options are:

  - numerical(1)

  - variable name containing information on the precision, this variable
    should be available on input dataset. The content of this variable
    should then be an integer.

- id:

  (`string`)  
  subject variable name.

- interaction_y:

  (`character`)  
  Will be passed onto the `tern` function `s_ancova`, when ancova =
  TRUE.

- interaction_item:

  (`character`)  
  Will be passed onto the `tern` function `s_ancova`, when ancova =
  TRUE.

- conf_level:

  (`proportion`)  
  Confidence level of the interval

- variables:

  (named list of strings)  
  list of additional analysis variables, with expected elements:

  - arm (string)  
    group variable, for which the covariate adjusted means of multiple
    groups will be summarized. Specifically, the first level of arm
    variable is taken as the reference group.

  - covariates (character)  
    a vector that can contain single variable names (such as 'X1'),
    and/or interaction terms indicated by 'X1 \* X2'.

- format_na_str:

  (`string`)  

- .stats:

  (named `list`)  
  column statistics to select for the table. The following column names
  are to be used: `col1`, `col23`, `coldiff`.  
  For `col1`, the following stats can be specified.  
  For `col23`, only `mean_ci_3d` is available. When ancova = `TRUE`
  these are LS Means, otherwise, arithmetic means.  
  For `coldiff`, only `meandiff_ci_3d` is available. When ancova =
  `TRUE` these are LS difference in means, otherwise, difference in
  means based upon 2-sample t-test.  

- .formats:

  (named `list`)  
  formats for the column statistics. `xx.d` style formats can be used.

- .formats_fun:

  (named `list`)  
  formatting functions for the column statistics, to be applied after
  the conversion of `xx.d` style to the appropriate precision.

- multivars:

  (`string(3)`)  
  Variables names to use in 3-col layout.

## Value

A function that can be used in an analyze function call

## Details

See Description

## See also

s_summarize_ancova_j

Other Inclusion of ANCOVA Functions:
[`a_summarize_ancova_j()`](https://johnsonandjohnson.github.io/junco/reference/s_summarize_ancova_j.md),
[`s_ancova_j()`](https://johnsonandjohnson.github.io/junco/reference/s_ancova_j.md)

## Examples

``` r
library(dplyr)

ADEG <- data.frame(
  STUDYID = c(
    "DUMMY", "DUMMY", "DUMMY", "DUMMY", "DUMMY",
    "DUMMY", "DUMMY", "DUMMY", "DUMMY", "DUMMY"
  ),
  USUBJID = c(
    "XXXXX01", "XXXXX02", "XXXXX03", "XXXXX04", "XXXXX05",
    "XXXXX06", "XXXXX07", "XXXXX08", "XXXXX09", "XXXXX10"
  ),
  TRT01A = c(
    "ARMA", "ARMA", "ARMA", "ARMA", "ARMA", "Placebo",
    "Placebo", "Placebo", "ARMA", "ARMA"
  ),
  PARAM = c("BP", "BP", "BP", "BP", "BP", "BP", "BP", "BP", "BP", "BP"),
  AVISIT = c(
    "Visit 1", "Visit 1", "Visit 1", "Visit 1", "Visit 1",
    "Visit 1", "Visit 1", "Visit 1", "Visit 1", "Visit 1"
  ),
  AVAL = c(56, 78, 67, 87, 88, 93, 39, 87, 65, 55),
  CHG = c(2, 3, -1, 9, -2, 0, 6, -2, 5, 2)
)

ADEG <- ADEG |>
  mutate(
    TRT01A = as.factor(TRT01A),
    STUDYID = as.factor(STUDYID)
  )

ADEG$colspan_trt <- factor(ifelse(ADEG$TRT01A == "Placebo", " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)
ADEG$rrisk_header <- "Risk Difference (%) (95% CI)"
ADEG$rrisk_label <- paste(ADEG$TRT01A, paste("vs", "Placebo"))

colspan_trt_map <- create_colspan_map(ADEG,
  non_active_grp = "Placebo",
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = "TRT01A"
)
ref_path <- c("colspan_trt", " ", "TRT01A", "Placebo")

lyt <- basic_table() |>
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) |>
  split_cols_by("TRT01A") |>
  split_rows_by(
    "PARAM",
    label_pos = "topleft",
    split_label = "Blood Pressure",
    section_div = " ",
    split_fun = drop_split_levels
  ) |>
  split_rows_by(
    "AVISIT",
    label_pos = "topleft",
    split_label = "Study Visit",
    split_fun = drop_split_levels,
    child_labels = "hidden"
  ) |>
  split_cols_by_multivar(
    c("AVAL", "AVAL", "CHG"),
    varlabels = c("n/N (%)", "Mean (CI)", "CFB (CI)")
  ) |>
  split_cols_by("rrisk_header", nested = FALSE) |>
  split_cols_by(
    "TRT01A",
    split_fun = remove_split_levels("Placebo"),
    labels_var = "rrisk_label"
  ) |>
  split_cols_by_multivar(c("CHG"), varlabels = c(" ")) |>
  analyze("STUDYID",
    afun = a_summarize_aval_chg_diff_j,
    extra_args = list(
      format_na_str = "-", d = 0,
      ref_path = ref_path, variables = list(arm = "TRT01A", covariates = NULL)
    )
  )

result <- build_table(lyt, ADEG)

result
#>                                  Active Study Agent                                                                          Risk Difference (%) (95% CI)
#> Blood Pressure                          ARMA                                                Placebo                                ARMA vs Placebo       
#>   Study Visit      n/N (%)          Mean (CI)          CFB (CI)         n/N (%)          Mean (CI)            CFB (CI)                                   
#> —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> BP                                                                                                                                                       
#>   Visit 1        7/7 (100.0%)   70.9 (58.2, 83.5)   2.6 (-0.8, 6.0)   3/3 (100.0%)   73.0 (-0.5, 146.5)   1.3 (-9.0, 11.7)         1.2 (-7.0, 9.5)       
```
