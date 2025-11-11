# Tabulation

## `junco` Tabulation

The `junco` R package provides functions to create analyses for clinical
trials in `R`. It is considered as an add-on/alternative to the `tern`
package, which is the first package available in the `NEST` framework
for analysis functions with main focus for clinical trials. The core
functionality for tabulation is built on the more general purpose
`rtables` package. New users should first begin by reading the
[“Introduction to
`tern`”](https://insightsengineering.github.io/tern/latest-tag/articles/tern.html)
and [“Introduction to
`rtables`”](https://insightsengineering.github.io/rtables/latest-release/articles/rtables.html)
vignettes.

The packages used in this vignette are:

``` r
library(rtables)
library(junco)
library(tern)
library(dplyr)
```

The datasets used in this vignette are:

``` r
adsl <- ex_adsl
adae <- ex_adae
advs <- ex_advs
```

Some common data manipulation on/from these datasets, such as adding
extra variables and defining some tabulation settings, like treatment
variable, control group for the table.

``` r
trtvar <- "ARM"
ctrl_grp <- "B: Placebo"

non_ctrl_grp <- setdiff(levels(adsl[[trtvar]]), ctrl_grp)

adsl$colspan_trt <- factor(ifelse(adsl[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

adsl$rrisk_header <- "Risk Difference (%) (95% CI)"
adsl$rrisk_label <- paste(adsl[[trtvar]], paste("vs", ctrl_grp))
adsl$rrisk_header_vs <- "Difference in Mean Change (95% CI)"

# define colspan_trt_map
colspan_trt_map <- create_colspan_map(adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

# define reference group specification
ref_path <- c("colspan_trt", " ", trtvar, ctrl_grp)

adae[["TRTEMFL"]] <- "Y"

# add adsl variables to adae
adae <- adae %>% left_join(., adsl)
advs <- advs %>% left_join(., adsl)

advs[advs[["ABLFL"]] == "Y", "CHG"] <- NA
```

## `junco` Analysis Functions

The `junco` analysis functions are used in combination with the
`rtables` layout functions,
[`rtables::analyze`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html)
and
[`rtables::summarize_row_groups`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html),
in the pipeline which creates the `rtables` table. They apply some
statistical logic to the layout of the `rtables` table. The table layout
is materialized with the
[`rtables::build_table`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html)
function and the data.

The `junco` analysis functions are functions that can be applied as an
`afun` in either
[`rtables::analyze`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html)
or as a `cfun` in
[`rtables::summarize_row_groups`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html)
function. This is a slightly different approach to `tern`, where the
table layout is constructed using `analyze` functions, which are
wrappers around
[`rtables::analyze`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html).

Just like `tern` analyze functions, the `junco` analysis functions offer
various methods useful from the perspective of clinical trials and other
statistical projects.

Examples of the `junco` analysis functions are `a_freq_j`,
`a_freq_subcol_j`, `a_summarize_aval_chg_diff_j` or `a_summarize_ex_j`.

A complete list of analysis functions can be found in [the junco website
functions
reference](https://johnsonandjohnson.github.io/junco/reference/index.md).

## Tabulation Examples using `a_freq_j`

We present an example usage of `a_freq_j` for the very common AE table
in clinical trials.

The standard table of adverse events is a summary by system organ class
and preferred term. For frequency counts by preferred term, if there are
multiple occurrences of the same AE in an individual we count them only
once.

With `junco` package, this table can be created with several calls to
the same `a_freq_j` function in a tabulation pipeline using
[`rtables::analyze`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html)
and
[`rtables::summarize_row_groups`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html).

In next paragraph, we’ll describe the difference versus using `tern`
package, for similar AE table. Differences in how to define the layout
as well as differences in the resulting table.

``` r
## extra args for a_freq_j controlling specification of
## reference group, denominator used to calculate percentages,
## and other details
extra_args_rr <- list(
  denom = "n_altdf",
  riskdiff = TRUE,
  ref_path = ref_path,
  method = "wald",
  .stats = c("count_unique_fraction")
)
```

Using `junco` analysis function `a_freq_j` we define the layout, using 2
`analyze` calls (for overall summary - TRTEMFL and preferred term -
AEDECOD), and 1 `summarize_row_groups` call (for system organ class -
AEBODSYS)

Note that the same specifications, `extra_args_rr` can be re-used in the
first and second `analyze` call, as well as in the in-between call to
`summarize_row_groups`.

For the overall summary, we add as extra specification to restrict to
TRTEMFL = “Y” values and the label to show in the row.

``` r
lyt <- basic_table(
  top_level_section_div = " ",
  colcount_format = "N=xx"
) %>%
  ## main columns
  split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map)) %>%
  split_cols_by(trtvar, show_colcounts = TRUE) %>%
  ## risk diff columns, note nested = FALSE
  split_cols_by("rrisk_header", nested = FALSE) %>%
  split_cols_by(trtvar,
    labels_var = "rrisk_label", split_fun = remove_split_levels(ctrl_grp),
    show_colcounts = FALSE
  ) %>%
  analyze("TRTEMFL",
    afun = a_freq_j,
    extra_args = append(extra_args_rr, list(val = "Y", label = "Number of subjects with AE"))
  ) %>%
  split_rows_by("AEBODSYS",
    split_label = "System Organ Class",
    split_fun = trim_levels_in_group("AEDECOD"),
    label_pos = "topleft",
    section_div = c(" "),
    nested = FALSE
  ) %>%
  summarize_row_groups("AEBODSYS",
    cfun = a_freq_j,
    extra_args = extra_args_rr
  ) %>%
  analyze("AEDECOD",
    afun = a_freq_j,
    extra_args = extra_args_rr
  )
```

Now we just build and view the resulting table.

``` r
tbl <- build_table(lyt, adae, alt_counts_df = adsl)
head(tbl, 10)
#>                                   Active Study Agent                                                                            
#>                               A: Drug X    C: Combination   B: Placebo                 Risk Difference (%) (95% CI)             
#> System Organ Class              N=134          N=132           N=134      A: Drug X vs B: Placebo   C: Combination vs B: Placebo
#> ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Number of subjects with AE   122 (91.0%)    120 (90.9%)     123 (91.8%)      -0.7 (-7.5, 6.0)             -0.9 (-7.6, 5.9)      
#>                                                                                                                                 
#> cl A.1                       78 (58.2%)      89 (67.4%)     75 (56.0%)       2.2 (-9.6, 14.1)            11.5 (-0.1, 23.1)      
#>   dcd A.1.1.1.1              50 (37.3%)      63 (47.7%)     45 (33.6%)       3.7 (-7.7, 15.2)             14.1 (2.5, 25.8)      
#>   dcd A.1.1.1.2              48 (35.8%)      50 (37.9%)     48 (35.8%)       0.0 (-11.5, 11.5)            2.1 (-9.5, 13.7)      
#>                                                                                                                                 
#> cl B.1                       47 (35.1%)      43 (32.6%)     49 (36.6%)      -1.5 (-13.0, 10.0)           -4.0 (-15.4, 7.4)      
#>   dcd B.1.1.1.1              47 (35.1%)      43 (32.6%)     49 (36.6%)      -1.5 (-13.0, 10.0)           -4.0 (-15.4, 7.4)      
#>                                                                                                                                 
#> cl B.2                       79 (59.0%)      85 (64.4%)     74 (55.2%)       3.7 (-8.1, 15.6)             9.2 (-2.6, 20.9)      
#>   dcd B.2.1.2.1              49 (36.6%)      52 (39.4%)     44 (32.8%)       3.7 (-7.7, 15.1)             6.6 (-5.0, 18.1)      
#>   dcd B.2.2.3.1              48 (35.8%)      51 (38.6%)     54 (40.3%)       -4.5 (-16.1, 7.1)           -1.7 (-13.4, 10.1)     
#>                                                                                                                                 
#> cl C.1                       43 (32.1%)      43 (32.6%)     46 (34.3%)       -2.2 (-13.5, 9.0)           -1.8 (-13.1, 9.6)
```

### Minor differences between current table and a similar table using `tern` analyze functions.

Here is a similar table generated with the tern functions
(`summarize_occurrences` and `count_occurrences`), as close as possible
to our target AE table we produced before in `tbl`.

``` r
lyt_tern <- basic_table(
  top_level_section_div = " ",
  colcount_format = "N=xx"
) %>%
  ## main columns
  split_cols_by(trtvar,
    show_colcounts = TRUE,
    split_fun = add_riskdiff(arm_x = ctrl_grp, arm_y = non_ctrl_grp)
  ) %>%
  analyze_num_patients(
    vars = "USUBJID",
    .stats = c("unique"),
    .labels = c(
      unique = "Total number of patients with at least one adverse event"
    ),
    riskdiff = TRUE
  ) %>%
  split_rows_by("AEBODSYS",
    split_label = "System Organ Class",
    split_fun = trim_levels_in_group("AEDECOD"),
    label_pos = "topleft",
    section_div = c(" "),
    nested = FALSE
  ) %>%
  summarize_occurrences(
    var = "AEBODSYS",
    denom = "N_col",
    riskdiff = TRUE,
    .stats = c("count_fraction")
  ) %>%
  count_occurrences(
    vars = "AEDECOD",
    denom = "N_col",
    riskdiff = TRUE,
    .stats = c("count_fraction")
  )


tbl_tern <- build_table(lyt_tern, adae, alt_counts_df = adsl)
head(tbl_tern, 10)
#>                                                                                                         Risk Difference (%) (95% CI)   Risk Difference (%) (95% CI) 
#>                                                             A: Drug X    B: Placebo    C: Combination     B: Placebo vs. A: Drug X     B: Placebo vs. C: Combination
#> System Organ Class                                            N=134         N=134          N=132                   N=268                           N=266            
#> ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Total number of patients with at least one adverse event   122 (91.0%)   123 (91.8%)    120 (90.9%)           0.7 (-6.0 - 7.5)               0.9 (-5.9 - 7.6)       
#>                                                                                                                                                                     
#> cl A.1                                                     78 (58.2%)     75 (56%)       89 (67.4%)          -2.2 (-14.1 - 9.6)             -11.5 (-23.1 - 0.1)     
#>   dcd A.1.1.1.1                                            50 (37.3%)    45 (33.6%)      63 (47.7%)          -3.7 (-15.2 - 7.7)            -14.1 (-25.8 - -2.5)     
#>   dcd A.1.1.1.2                                            48 (35.8%)    48 (35.8%)      50 (37.9%)          0.0 (-11.5 - 11.5)             -2.1 (-13.7 - 9.5)      
#>                                                                                                                                                                     
#> cl B.1                                                     47 (35.1%)    49 (36.6%)      43 (32.6%)          1.5 (-10.0 - 13.0)              4.0 (-7.4 - 15.4)      
#>   dcd B.1.1.1.1                                            47 (35.1%)    49 (36.6%)      43 (32.6%)          1.5 (-10.0 - 13.0)              4.0 (-7.4 - 15.4)      
#>                                                                                                                                                                     
#> cl B.2                                                      79 (59%)     74 (55.2%)      85 (64.4%)          -3.7 (-15.6 - 8.1)             -9.2 (-20.9 - 2.6)      
#>   dcd B.2.1.2.1                                            49 (36.6%)    44 (32.8%)      52 (39.4%)          -3.7 (-15.1 - 7.7)             -6.6 (-18.1 - 5.0)      
#>   dcd B.2.2.3.1                                            48 (35.8%)    54 (40.3%)      51 (38.6%)          4.5 (-7.1 - 16.1)              1.7 (-10.1 - 13.4)      
#>                                                                                                                                                                     
#> cl C.1                                                     43 (32.1%)    46 (34.3%)      43 (32.6%)          2.2 (-9.0 - 13.5)               1.8 (-9.6 - 13.1)
```

The main differences in the resulting table are

- the comparison against the reference group is reversed (ie B: Placebo
  vs. A: Drug X instead of A: Drug X vs. B: Placebo).  
- the extra column spanner for active treatment group is not present in
  the `tern` version, and cannot be added.

### a_freq_j supports various methods for the risk difference column.

One of the statistical options to control with the usage of `a_freq_j`
is the method for the risk difference calculation for the risk
difference columns. All methods available in
´tern::estimate_proportion_diff´ can be supported. Eg, switching to
`waldcc`, or others can be done. There is no option to switch methods
using tern `count_occurrences` layouts, only `wald` method is available.

``` r
extra_args_rr[["method"]] <- "waldcc"

tbl2 <- basic_table(
  top_level_section_div = " ",
  colcount_format = "N=xx"
) %>%
  ## main columns
  split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map)) %>%
  split_cols_by(trtvar, show_colcounts = TRUE) %>%
  ## risk diff columns, note nested = FALSE
  split_cols_by("rrisk_header", nested = FALSE) %>%
  split_cols_by(trtvar,
    labels_var = "rrisk_label", split_fun = remove_split_levels(ctrl_grp),
    show_colcounts = FALSE
  ) %>%
  split_rows_by("AEBODSYS",
    split_label = "System Organ Class",
    split_fun = trim_levels_in_group("AEDECOD"),
    label_pos = "topleft",
    section_div = c(" "),
    nested = FALSE
  ) %>%
  summarize_row_groups("AEBODSYS",
    cfun = a_freq_j,
    extra_args = extra_args_rr
  ) %>%
  analyze("AEDECOD",
    afun = a_freq_j,
    extra_args = extra_args_rr
  ) %>%
  build_table(adae, alt_counts_df = adsl)

head(tbl2, 10)
#>                          Active Study Agent                                                                           
#>                      A: Drug X    C: Combination   B: Placebo                Risk Difference (%) (95% CI)             
#> System Organ Class     N=134          N=132          N=134      A: Drug X vs B: Placebo   C: Combination vs B: Placebo
#> ——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> cl A.1               78 (58.2%)     89 (67.4%)     75 (56.0%)      2.2 (-10.4, 14.8)           11.5 (-0.9, 23.8)      
#>   dcd A.1.1.1.1      50 (37.3%)     63 (47.7%)     45 (33.6%)      3.7 (-8.5, 15.9)             14.1 (1.7, 26.6)      
#>   dcd A.1.1.1.2      48 (35.8%)     50 (37.9%)     48 (35.8%)      0.0 (-12.2, 12.2)           2.1 (-10.3, 14.4)      
#>                                                                                                                       
#> cl B.1               47 (35.1%)     43 (32.6%)     49 (36.6%)     -1.5 (-13.7, 10.7)           -4.0 (-16.2, 8.2)      
#>   dcd B.1.1.1.1      47 (35.1%)     43 (32.6%)     49 (36.6%)     -1.5 (-13.7, 10.7)           -4.0 (-16.2, 8.2)      
#>                                                                                                                       
#> cl B.2               79 (59.0%)     85 (64.4%)     74 (55.2%)      3.7 (-8.9, 16.3)             9.2 (-3.3, 21.7)      
#>   dcd B.2.1.2.1      49 (36.6%)     52 (39.4%)     44 (32.8%)      3.7 (-8.4, 15.9)             6.6 (-5.7, 18.8)      
#>   dcd B.2.2.3.1      48 (35.8%)     51 (38.6%)     54 (40.3%)      -4.5 (-16.8, 7.9)           -1.7 (-14.2, 10.8)     
#>                                                                                                                       
#> cl C.1               43 (32.1%)     43 (32.6%)     46 (34.3%)      -2.2 (-14.3, 9.8)           -1.8 (-13.8, 10.3)     
#>   dcd C.1.1.1.3      43 (32.1%)     43 (32.6%)     46 (34.3%)      -2.2 (-14.3, 9.8)           -1.8 (-13.8, 10.3)
```

### Creation of Subgroup tables with `a_freq_j`

The junco function `a_freq_j` also supports the creation of subgroup
tables, which is demonstrated in below table.

``` r
extra_args_rr_common <- list(
  denom = "n_altdf",
  denom_by = "SEX"
)

extra_args_rr <- append(
  extra_args_rr_common,
  list(
    riskdiff = FALSE,
    extrablankline = TRUE,
    .stats = c("n_altdf"),
    label_fstr = "Gender: %s"
  )
)

extra_args_rr2 <- append(
  extra_args_rr_common,
  list(
    riskdiff = TRUE,
    ref_path = ref_path,
    method = "wald",
    .stats = c("count_unique_denom_fraction"),
    na_str = rep("NA", 3)
  )
)

tbl <- basic_table(
  top_level_section_div = " ",
  colcount_format = "N=xx"
) %>%
  ## main columns
  split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map)) %>%
  split_cols_by(trtvar, show_colcounts = TRUE) %>%
  ## risk diff columns, note nested = FALSE
  split_cols_by("rrisk_header", nested = FALSE) %>%
  split_cols_by(trtvar,
    labels_var = "rrisk_label", split_fun = remove_split_levels(ctrl_grp),
    show_colcounts = FALSE
  ) %>%
  split_rows_by("SEX", split_fun = drop_split_levels) %>%
  summarize_row_groups("SEX",
    cfun = a_freq_j,
    extra_args = extra_args_rr
  ) %>%
  split_rows_by("TRTEMFL",
    split_fun = keep_split_levels("Y"),
    indent_mod = -1L,
    section_div = c(" ")
  ) %>%
  summarize_row_groups("TRTEMFL",
    cfun = a_freq_j,
    extra_args = append(extra_args_rr2, list(label = "Subjects with >=1 AE", extrablankline = TRUE))
  ) %>%
  split_rows_by("AEBODSYS",
    split_label = "System Organ Class",
    split_fun = trim_levels_in_group("AEDECOD"),
    label_pos = "topleft",
    section_div = c(" "),
    nested = TRUE
  ) %>%
  summarize_row_groups("AEBODSYS",
    cfun = a_freq_j,
    extra_args = extra_args_rr2
  ) %>%
  analyze("AEDECOD",
    afun = a_freq_j,
    extra_args = extra_args_rr2
  ) %>%
  build_table(adae, alt_counts_df = adsl)

head(tbl, 30)
#>                              Active Study Agent                                                                               
#>                          A: Drug X     C: Combination    B: Placebo                  Risk Difference (%) (95% CI)             
#> System Organ Class         N=134           N=132            N=134       A: Drug X vs B: Placebo   C: Combination vs B: Placebo
#> ——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Gender: F                   79               66              77                                                               
#>                                                                                                                               
#> Subjects with >=1 AE   72/79 (91.1%)   61/66 (92.4%)    73/77 (94.8%)      -3.7 (-11.7, 4.3)           -2.4 (-10.5, 5.7)      
#>                                                                                                                               
#>   cl A.1               53/79 (67.1%)   42/66 (63.6%)    48/77 (62.3%)      4.8 (-10.2, 19.7)           1.3 (-14.6, 17.2)      
#>     dcd A.1.1.1.1      34/79 (43.0%)   32/66 (48.5%)    30/77 (39.0%)      4.1 (-11.3, 19.5)            9.5 (-6.7, 25.8)      
#>     dcd A.1.1.1.2      32/79 (40.5%)   24/66 (36.4%)    31/77 (40.3%)      0.2 (-15.2, 15.6)           -3.9 (-19.9, 12.1)     
#>                                                                                                                               
#>   cl B.1               28/79 (35.4%)   21/66 (31.8%)    32/77 (41.6%)      -6.1 (-21.4, 9.1)           -9.7 (-25.5, 6.0)      
#>     dcd B.1.1.1.1      28/79 (35.4%)   21/66 (31.8%)    32/77 (41.6%)      -6.1 (-21.4, 9.1)           -9.7 (-25.5, 6.0)      
#>                                                                                                                               
#>   cl B.2               46/79 (58.2%)   40/66 (60.6%)    43/77 (55.8%)      2.4 (-13.1, 17.9)           4.8 (-11.4, 20.9)      
#>     dcd B.2.1.2.1      29/79 (36.7%)   19/66 (28.8%)    30/77 (39.0%)     -2.3 (-17.5, 13.0)           -10.2 (-25.6, 5.3)     
#>     dcd B.2.2.3.1      30/79 (38.0%)   24/66 (36.4%)    30/77 (39.0%)     -1.0 (-16.3, 14.3)           -2.6 (-18.5, 13.3)     
#>                                                                                                                               
#>   cl C.1               30/79 (38.0%)   25/66 (37.9%)    35/77 (45.5%)      -7.5 (-22.9, 8.0)           -7.6 (-23.7, 8.6)      
#>     dcd C.1.1.1.3      30/79 (38.0%)   25/66 (37.9%)    35/77 (45.5%)      -7.5 (-22.9, 8.0)           -7.6 (-23.7, 8.6)      
#>                                                                                                                               
#>   cl C.2               23/79 (29.1%)   28/66 (42.4%)    33/77 (42.9%)     -13.7 (-28.7, 1.2)           -0.4 (-16.7, 15.8)     
#>     dcd C.2.1.2.1      23/79 (29.1%)   28/66 (42.4%)    33/77 (42.9%)     -13.7 (-28.7, 1.2)           -0.4 (-16.7, 15.8)     
#>                                                                                                                               
#>   cl D.1               45/79 (57.0%)   39/66 (59.1%)    38/77 (49.4%)      7.6 (-8.0, 23.2)             9.7 (-6.6, 26.0)      
#>     dcd D.1.1.1.1      25/79 (31.6%)   26/66 (39.4%)    28/77 (36.4%)     -4.7 (-19.6, 10.1)           3.0 (-12.9, 19.0)      
#>     dcd D.1.1.4.2      30/79 (38.0%)   26/66 (39.4%)    21/77 (27.3%)      10.7 (-3.9, 25.3)           12.1 (-3.3, 27.5)      
#>                                                                                                                               
#>   cl D.2               26/79 (32.9%)   32/66 (48.5%)    40/77 (51.9%)     -19.0 (-34.3, -3.8)          -3.5 (-19.9, 13.0)     
#>     dcd D.2.1.5.3      26/79 (32.9%)   32/66 (48.5%)    40/77 (51.9%)     -19.0 (-34.3, -3.8)          -3.5 (-19.9, 13.0)     
#>                                                                                                                               
#> Gender: M                   51               60              55                                                               
#>                                                                                                                               
#> Subjects with >=1 AE   46/51 (90.2%)   53/60 (88.3%)    48/55 (87.3%)      2.9 (-9.1, 14.9)            1.1 (-10.9, 13.0)      
#>                                                                                                                               
#>   cl A.1               24/51 (47.1%)   41/60 (68.3%)    25/55 (45.5%)      1.6 (-17.4, 20.6)            22.9 (5.2, 40.5)      
#>     dcd A.1.1.1.1      16/51 (31.4%)   29/60 (48.3%)    15/55 (27.3%)      4.1 (-13.2, 21.4)            21.1 (3.8, 38.3)      
#>     dcd A.1.1.1.2      15/51 (29.4%)   22/60 (36.7%)    15/55 (27.3%)      2.1 (-15.0, 19.3)            9.4 (-7.6, 26.3)      
#>                                                                                                                               
#>   cl B.1               18/51 (35.3%)   21/60 (35.0%)    16/55 (29.1%)      6.2 (-11.6, 24.0)           5.9 (-11.1, 22.9)      
#>     dcd B.1.1.1.1      18/51 (35.3%)   21/60 (35.0%)    16/55 (29.1%)      6.2 (-11.6, 24.0)           5.9 (-11.1, 22.9)
```

This table cannot be generated using the tern `count_occurrences`
methods.

## Other junco features : Extra Statistics have been added to some tern statistical functions

``` r
tern::get_stats("summarize_ancova")
#> [1] "n"              "lsmean"         "lsmean_diff"    "lsmean_diff_ci"
#> [5] "pval"
tern::get_stats("analyze_vars_numeric")
#>  [1] "n"               "sum"             "mean"            "sd"             
#>  [5] "se"              "mean_sd"         "mean_se"         "mean_ci"        
#>  [9] "mean_sei"        "mean_sdi"        "mean_pval"       "median"         
#> [13] "mad"             "median_ci"       "quantiles"       "iqr"            
#> [17] "range"           "min"             "max"             "median_range"   
#> [21] "cv"              "geom_mean"       "geom_sd"         "geom_mean_sd"   
#> [25] "geom_mean_ci"    "geom_cv"         "median_ci_3d"    "mean_ci_3d"     
#> [29] "geom_mean_ci_3d"
```

In junco, we have added extra stats for

- ancova: se for lsmean, combined lsmean-CI (3d stat), combined
  lsmean_diff-CI (3d stat)
- analyze_vars_numeric: combined version of mean-CI, median-CI,
  geom_mean-CI (available in tern \>= 0.9.6)
- similar extra stats for Kaplan-Meier/survival and other methods

## Tabulation Examples using `a_summarize_aval_chg_diff_j`

``` r
multivars <- c("AVAL", "AVAL", "CHG")

extra_args_3col <- list(
  format_na_str = rep(NA, 3),
  ref_path = ref_path,
  ancova = FALSE,
  comp_btw_group = TRUE,
  multivars = multivars
)


lyt_vs_p1 <- basic_table(
  show_colcounts = FALSE,
  colcount_format = "N=xx"
) %>%
  ### first columns
  split_cols_by("colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map),
    show_colcounts = FALSE
  ) %>%
  split_cols_by(trtvar,
    show_colcounts = TRUE, colcount_format = "N=xx"
  ) %>%
  ## set up a 3 column split
  split_cols_by_multivar(multivars,
    varlabels = c("n/N (%)", "Mean (95% CI)", "Mean Change From Baseline (95% CI)")
  ) %>%
  split_rows_by("PARAM",
    label_pos = "topleft",
    split_label = "Parameter",
    section_div = " ",
    split_fun = drop_split_levels
  ) %>%
  ## note the child_labels = hidden for AVISIT, these labels will be taken care off by
  ## applying function summarize_aval_chg_diff further in the layout
  split_rows_by("AVISIT",
    label_pos = "topleft",
    split_label = "Study Visit",
    split_fun = drop_split_levels,
    child_labels = "hidden"
  )


lyt_vs <- lyt_vs_p1 %>%
  ### restart for the rrisk_header columns - note the nested = FALSE option
  ### also note the child_labels = "hidden" in both PARAM and AVISIT
  split_cols_by("rrisk_header_vs", nested = FALSE) %>%
  split_cols_by(trtvar,
    split_fun = remove_split_levels(ctrl_grp),
    labels_var = "rrisk_label",
    show_colcounts = TRUE,
    colcount_format = "N=xx"
  ) %>%
  ### difference columns : just 1 column & analysis needs to be done on change
  split_cols_by_multivar(multivars[3],
    varlabels = c(" ")
  ) %>%
  ### the variable passed here in analyze is not used (STUDYID), it is a dummy var passing,
  ### the function a_summarize_aval_chg_diff_j grabs the required vars from cols_by_multivar calls
  analyze("STUDYID",
    afun = a_summarize_aval_chg_diff_j,
    extra_args = extra_args_3col
  )

result_vs <- build_table(lyt_vs, advs, alt_counts_df = adsl)
```

This is the resulting table.

``` r
head(result_vs, 15)
#>                                                                                             Active Study Agent                                                                                                                                                           Difference in Mean Change (95% CI)          
#>                                                            A: Drug X                                                                C: Combination                                                                B: Placebo                                   A: Drug X vs B: Placebo   C: Combination vs B: Placebo
#> Parameter                                                    N=134                                                                       N=132                                                                       N=134                                              N=134                       N=132            
#>   Study Visit                  n/N (%)          Mean (95% CI)     Mean Change From Baseline (95% CI)       n/N (%)          Mean (95% CI)     Mean Change From Baseline (95% CI)       n/N (%)          Mean (95% CI)     Mean Change From Baseline (95% CI)                                                         
#> —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Diastolic Blood Pressure                                                                                                                                                                                                                                                                                             
#>   SCREENING                134/134 (100.0%)   50.0 (48.7, 51.2)                                        132/132 (100.0%)   50.2 (48.9, 51.5)                                        134/134 (100.0%)   50.8 (49.3, 52.2)                                                                                              
#>   BASELINE                 134/134 (100.0%)   48.6 (47.2, 50.0)                                        132/132 (100.0%)   51.1 (49.8, 52.4)                                        134/134 (100.0%)   50.4 (49.1, 51.8)                                                                                              
#>   WEEK 1 DAY 8             134/134 (100.0%)   50.3 (49.0, 51.5)            1.7 (-0.2, 3.5)             132/132 (100.0%)   48.9 (47.5, 50.2)           -2.3 (-4.0, -0.5)            134/134 (100.0%)   49.7 (48.4, 51.0)            -0.8 (-2.6, 1.1)                2.4 (-0.2, 5.0)             -1.5 (-4.1, 1.1)      
#>   WEEK 2 DAY 15            134/134 (100.0%)   50.8 (49.5, 52.2)             2.2 (0.2, 4.2)             132/132 (100.0%)   50.0 (48.5, 51.4)            -1.1 (-3.1, 0.9)            134/134 (100.0%)   49.7 (48.3, 51.2)            -0.7 (-2.9, 1.4)                3.0 (0.0, 5.9)              -0.4 (-3.3, 2.5)      
#>   WEEK 3 DAY 22            134/134 (100.0%)   50.7 (49.4, 52.0)             2.1 (0.3, 4.0)             132/132 (100.0%)   49.9 (48.6, 51.3)            -1.2 (-3.0, 0.7)            134/134 (100.0%)   49.1 (47.7, 50.4)            -1.3 (-3.2, 0.5)                3.5 (0.8, 6.1)              0.2 (-2.5, 2.8)       
#>   WEEK 4 DAY 29            134/134 (100.0%)   50.1 (48.7, 51.5)            1.5 (-0.4, 3.3)             132/132 (100.0%)   49.7 (48.3, 51.1)            -1.4 (-3.3, 0.5)            134/134 (100.0%)   49.6 (48.4, 50.8)            -0.8 (-2.6, 1.0)                2.3 (-0.3, 4.9)             -0.6 (-3.2, 2.0)      
#>   WEEK 5 DAY 36            134/134 (100.0%)   50.6 (49.3, 51.9)             2.0 (0.1, 3.9)             132/132 (100.0%)   49.1 (47.8, 50.4)           -2.0 (-3.9, -0.2)            134/134 (100.0%)   48.4 (47.0, 49.7)           -2.1 (-4.1, -0.1)                4.0 (1.3, 6.8)              0.1 (-2.6, 2.8)       
#>                                                                                                                                                                                                                                                                                                                      
#> Pulse Rate                                                                                                                                                                                                                                                                                                           
#>   SCREENING                134/134 (100.0%)   49.6 (48.1, 51.1)                                        132/132 (100.0%)   49.3 (47.8, 50.8)                                        134/134 (100.0%)   49.4 (48.0, 50.8)                                                                                              
#>   BASELINE                 134/134 (100.0%)   51.9 (50.5, 53.2)                                        132/132 (100.0%)   50.3 (48.6, 51.9)                                        134/134 (100.0%)   50.3 (48.8, 51.8)                                                                                              
#>   WEEK 1 DAY 8             134/134 (100.0%)   50.1 (48.6, 51.5)            -1.8 (-3.8, 0.2)            132/132 (100.0%)   49.8 (48.5, 51.1)            -0.5 (-2.6, 1.7)            134/134 (100.0%)   49.3 (48.0, 50.6)            -1.0 (-3.0, 1.0)               -0.8 (-3.6, 2.0)             0.5 (-2.4, 3.5)       
#>   WEEK 2 DAY 15            134/134 (100.0%)   49.7 (48.2, 51.2)            -2.2 (-4.3, 0.0)            132/132 (100.0%)   49.1 (47.7, 50.4)            -1.2 (-3.1, 0.8)            134/134 (100.0%)   50.8 (49.5, 52.1)            0.6 (-1.5, 2.6)                -2.7 (-5.6, 0.2)             -1.8 (-4.5, 1.0)      
#>   WEEK 3 DAY 22            134/134 (100.0%)   50.5 (49.2, 51.7)            -1.4 (-3.3, 0.5)            132/132 (100.0%)   49.8 (48.6, 51.0)            -0.5 (-2.5, 1.6)            134/134 (100.0%)   49.9 (48.6, 51.3)            -0.4 (-2.2, 1.5)               -1.1 (-3.7, 1.6)             -0.1 (-2.9, 2.7)      
#>   WEEK 4 DAY 29            134/134 (100.0%)   49.0 (47.5, 50.4)           -2.9 (-4.8, -1.0)            132/132 (100.0%)   51.0 (49.7, 52.3)            0.7 (-1.3, 2.7)             134/134 (100.0%)   50.0 (48.5, 51.4)            -0.3 (-2.4, 1.7)               -2.6 (-5.4, 0.2)             1.0 (-1.8, 3.9)
```

The columns for the comparison between reference group is optional. The
same table without these extra columns can be produced by specifying the
argument `comp_btw_group` = `FALSE` and leaving out the 3
`split_cols_by` calls

- split_cols_by(“rrisk_header_vs”)
- split_cols_by(trtvar)
- split_cols_by_multivar(multivars\[3\])

``` r
multivars <- c("AVAL", "AVAL", "CHG")

extra_args_3col <- list(
  format_na_str = rep(NA, 3),
  ancova = FALSE,
  comp_btw_group = FALSE,
  multivars = multivars
)

lyt_vs2 <- lyt_vs_p1 %>%
  ### the variable passed here in analyze is not used (STUDYID), it is a dummy var passing,
  ### the function a_summarize_aval_chg_diff_j grabs the required vars from cols_by_multivar calls
  analyze("STUDYID",
    afun = a_summarize_aval_chg_diff_j,
    extra_args = extra_args_3col
  )

result_vs2 <- build_table(lyt_vs2, advs, alt_counts_df = adsl)
```

This is the resulting table without the difference between treatment
group columns.

``` r
head(result_vs2, 15)
#>                                                                                             Active Study Agent                                                                                                                                              
#>                                                            A: Drug X                                                                C: Combination                                                                B: Placebo                                
#> Parameter                                                    N=134                                                                       N=132                                                                       N=134                                  
#>   Study Visit                  n/N (%)          Mean (95% CI)     Mean Change From Baseline (95% CI)       n/N (%)          Mean (95% CI)     Mean Change From Baseline (95% CI)       n/N (%)          Mean (95% CI)     Mean Change From Baseline (95% CI)
#> ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Diastolic Blood Pressure                                                                                                                                                                                                                                    
#>   SCREENING                134/134 (100.0%)   50.0 (48.7, 51.2)                                        132/132 (100.0%)   50.2 (48.9, 51.5)                                        134/134 (100.0%)   50.8 (49.3, 52.2)                                     
#>   BASELINE                 134/134 (100.0%)   48.6 (47.2, 50.0)                                        132/132 (100.0%)   51.1 (49.8, 52.4)                                        134/134 (100.0%)   50.4 (49.1, 51.8)                                     
#>   WEEK 1 DAY 8             134/134 (100.0%)   50.3 (49.0, 51.5)            1.7 (-0.2, 3.5)             132/132 (100.0%)   48.9 (47.5, 50.2)           -2.3 (-4.0, -0.5)            134/134 (100.0%)   49.7 (48.4, 51.0)            -0.8 (-2.6, 1.1)         
#>   WEEK 2 DAY 15            134/134 (100.0%)   50.8 (49.5, 52.2)             2.2 (0.2, 4.2)             132/132 (100.0%)   50.0 (48.5, 51.4)            -1.1 (-3.1, 0.9)            134/134 (100.0%)   49.7 (48.3, 51.2)            -0.7 (-2.9, 1.4)         
#>   WEEK 3 DAY 22            134/134 (100.0%)   50.7 (49.4, 52.0)             2.1 (0.3, 4.0)             132/132 (100.0%)   49.9 (48.6, 51.3)            -1.2 (-3.0, 0.7)            134/134 (100.0%)   49.1 (47.7, 50.4)            -1.3 (-3.2, 0.5)         
#>   WEEK 4 DAY 29            134/134 (100.0%)   50.1 (48.7, 51.5)            1.5 (-0.4, 3.3)             132/132 (100.0%)   49.7 (48.3, 51.1)            -1.4 (-3.3, 0.5)            134/134 (100.0%)   49.6 (48.4, 50.8)            -0.8 (-2.6, 1.0)         
#>   WEEK 5 DAY 36            134/134 (100.0%)   50.6 (49.3, 51.9)             2.0 (0.1, 3.9)             132/132 (100.0%)   49.1 (47.8, 50.4)           -2.0 (-3.9, -0.2)            134/134 (100.0%)   48.4 (47.0, 49.7)           -2.1 (-4.1, -0.1)         
#>                                                                                                                                                                                                                                                             
#> Pulse Rate                                                                                                                                                                                                                                                  
#>   SCREENING                134/134 (100.0%)   49.6 (48.1, 51.1)                                        132/132 (100.0%)   49.3 (47.8, 50.8)                                        134/134 (100.0%)   49.4 (48.0, 50.8)                                     
#>   BASELINE                 134/134 (100.0%)   51.9 (50.5, 53.2)                                        132/132 (100.0%)   50.3 (48.6, 51.9)                                        134/134 (100.0%)   50.3 (48.8, 51.8)                                     
#>   WEEK 1 DAY 8             134/134 (100.0%)   50.1 (48.6, 51.5)            -1.8 (-3.8, 0.2)            132/132 (100.0%)   49.8 (48.5, 51.1)            -0.5 (-2.6, 1.7)            134/134 (100.0%)   49.3 (48.0, 50.6)            -1.0 (-3.0, 1.0)         
#>   WEEK 2 DAY 15            134/134 (100.0%)   49.7 (48.2, 51.2)            -2.2 (-4.3, 0.0)            132/132 (100.0%)   49.1 (47.7, 50.4)            -1.2 (-3.1, 0.8)            134/134 (100.0%)   50.8 (49.5, 52.1)            0.6 (-1.5, 2.6)          
#>   WEEK 3 DAY 22            134/134 (100.0%)   50.5 (49.2, 51.7)            -1.4 (-3.3, 0.5)            132/132 (100.0%)   49.8 (48.6, 51.0)            -0.5 (-2.5, 1.6)            134/134 (100.0%)   49.9 (48.6, 51.3)            -0.4 (-2.2, 1.5)         
#>   WEEK 4 DAY 29            134/134 (100.0%)   49.0 (47.5, 50.4)           -2.9 (-4.8, -1.0)            132/132 (100.0%)   51.0 (49.7, 52.3)            0.7 (-1.3, 2.7)             134/134 (100.0%)   50.0 (48.5, 51.4)            -0.3 (-2.4, 1.7)
```
