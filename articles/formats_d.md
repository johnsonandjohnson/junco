# Formatting tools to simplify adjustment of decimal precision

## Overview

Many clinical tables require statistics presented on a table with
certain decimal precision. In some situations the used decimal precision
is different for different parameters and/or variables included in the
table. In the `rtables` and `nest` framework formats can be specified,
typically in the `format` or `.formats` argument of the used `afun` or
`analyze` call.

For more information on tabulation, formats and formatting functions,
see the vignettes
[“Tabulation”](https://pharmaverse.github.io/tern/latest-tag/articles/tables.html)
and [“Formatting
Functions”](https://pharmaverse.github.io/tern/latest-tag/articles/tern_formats.html).

The purpose in this vignette is to provide some examples on how to
utilize the new `junco` functions to facilitate adjustment of decimal
precision.

``` r

library(rtables)
library(junco)
library(dplyr)
```

## d-Style Based Format Specifications

The following table summarizes the 3 J&J conventions for mean, SD/SE,
median and range that may be used. In the table below, “X” indicates the
number of decimal places to which the raw values are captured or, in the
case of derived data, the number of decimal places to which the derived
variable is to be expressed.

|  | Default Conventions | Alternative 1 | Alternative 2 |
|:---|:--:|:--:|:--:|
| Individual (raw/derived) values | X | X | X |
| Mean | X+1 | X+1 | X |
| SD or SE | X+2 | X+1 | X |
| Median | X+1 | X+1 | X |
| Min, max | X, X | X, X | X, X |

These rules have been used to set up a d-style specification as a string
of type “xx.dx”. In the conversion process, the integer value `d` will
be replaced by `d` times `x`, e.g. with d = 1 `xx.dx` is converted into
the format specification string `xx.xx`.

Below is an example on how to set up a d-style based specification
manually.

``` r

junco_def_d <- c(
 "mean" = "xx.dx",
 "mean_sd" = "xx.dx (xx.dxx)",
 "mean_se" = "xx.dx (xx.dxx)",
 "range" = "(xx.d, xx.d)",
 "count_fraction" = jjcsformat_count_fraction
)
```

The 3 following d-style based format specifications are available in
junco:

- `junco_def_d_all`, used for setup according to default J&J precision
  rules
- `junco_def_d_alt1`, used for setup according to J&J precision rules
  alternative 1
- `junco_def_d_alt2`, used for setup according to J&J precision rules
  alternative 2

All 3 contain formatting defaults for all statistics defined in
`tern`/`junco` (a total of 140 statistics), also these that do not
follow d-based specification setup (such as `count_fraction`,
`ztest_pval`, …). For about a third of all statistics (53 out of 140)
the specifications in `junco_def_d_all` are defined with the d-style
setup. See further in this vignette a note on tools `get_fmt_details`
and `compare_fmt_specs` that can be used to investigate format
specifications setups.

``` r

d_style <- vapply(junco_def_d_all, FUN = function(fmt){ 
  !is.function(fmt) && is.character(fmt) && grepl("xx.d", fmt, fixed = TRUE)},
  FUN.VALUE = logical(1))

non_d_style <- junco_def_d_all[!d_style]

d_style <- junco_def_d_all[d_style]
names(d_style)
#>  [1] "adj_mean_se"         "adj_mean_ci"         "adj_mean_est_ci"    
#>  [4] "cv"                  "diff"                "diff_ci"            
#>  [7] "diff_est_ci"         "diff_mean_se"        "diff_mean_ci"       
#> [10] "diff_mean_est_ci"    "diff_means_est"      "diff_means_se"      
#> [13] "diff_means_est_se"   "diff_means_ci"       "diff_means_est_ci"  
#> [16] "geom_sd"             "geom_se"             "geom_mean_sd"       
#> [19] "geom_mean_se"        "geom_cv"             "geom_mean"          
#> [22] "geom_mean_ci_3d"     "iqr"                 "quantiles_upper"    
#> [25] "quantiles_lower"     "lsmean"              "lsmean_ci"          
#> [28] "lsmean_diff"         "lsmean_diff_with_ci" "lsmean_diff_ci"     
#> [31] "lsmean_se"           "mad"                 "mean"               
#> [34] "mean_sd"             "mean_se"             "mean_ci_3d"         
#> [37] "mean_diff_with_ci"   "median"              "median_ci"          
#> [40] "median_ci_3d"        "median_range"        "quantiles"          
#> [43] "range"               "sd"                  "se"                 
#> [46] "geom_mean_ci"        "max"                 "mean_ci"            
#> [49] "mean_sdi"            "mean_sei"            "min"                
#> [52] "range_censor"        "range_event"
d_style[["adj_mean_se"]]
#> [1] "xx.dx (xx.dxx)"
```

The default d-style based input specification is `junco_def_d_all`, and
when utilizing these specs with `d = 1` it would result in utilizing the
`junco` defaults defined in `junco_default_formats`. One exception is
the `tern` statistic `lr_stat_df` from method_group `coxph_pairwise`,
for which the default format is not a valid `formatter` format. This is
out of scope for current vignette.

``` r


fmt_1 <- fmt_spec_single_d(d = 1, fmt_d_def = junco_def_d_all)
fmt_1[["lr_stat_df"]] <- NULL

# get_fmt_details tool applied to a format specification vector
fmt_details <- get_fmt_details(fmt_1, recursive = FALSE, as_tibble = TRUE)

fmt_details[fmt_details$stat == "adj_mean_se", c("stat", "fun_fact", "str", "str_formatters")]
#> # A tibble: 1 × 4
#>   stat        fun_fact str            str_formatters
#>   <chr>       <chr>    <chr>          <lgl>         
#> 1 adj_mean_se ""       xx.xx (xx.xxx) TRUE

# to compare against junco_default_formats, except lr_stat_df
junco_default_formats_adj <- junco_default_formats
junco_default_formats_adj[["lr_stat_df"]] <- NULL

# compare_fmt_specs tool to compare 2 format specification vectors
comp <- compare_fmt_specs(fmt_1, junco_default_formats)
#> All common stats specs identical
#> More details are available in created object.
```

The tools `get_fmt_details` and `compare_fmt_specs` used in the above
code snippet are useful for understanding how the formatting
specifications are constructed. These tools are out of scope of current
vignette, users can refer to the help pages for basic information on
these.

## Overview of Most Relevant Functions

| **Function** | **Core Purpose** | **Covered in Example** |
|----|----|----|
| `fmt_spec_single_d` | Convert a d-style based format specification into `formatters` format specification with 1 specific value of `d` | [Example BDS-type 2](#Ex3) |
| `fmt_spec_df_d` | Convert a tibble with d-style based format specification into `formatters` format specification with value of `d` coming from column on input tibble | [Example BDS-type 1](#Ex2) |
| `fmt_spec_var_d` | Convert a variable based vector with d-style based format specification into `formatters` format specification | [Example demographic](#Ex1) |

## Examples

### Working Example 1: Table with Multiple Variables Analyzed with Different Precision (e.g. Demography Table)

This example demonstrates the standard pattern for a table with multiple
variables, each with a specific precision.

``` r

set.seed(101)
f2n <- function(x) as.numeric(x) - 1
df <- ex_adsl
df <- df |> 
  mutate(newvar = rnorm(nrow(df), mean = 50 + 3 * f2n(ARMCD) + 4 * AGE, sd = 20))

# a mix of categorical and continuous variables
vars_all <- c("AGE", "SEX", "BMRKR1", "newvar")

# set all variables to d = 0
vars_d <- rep(0, length(vars_all))
names(vars_d) <- vars_all
# set some variables to a different d value
vars_d["BMRKR1"] <- 2
vars_d["newvar"] <- 1

# conversion of d-style format to valid format specification
vars_fmt <- fmt_spec_var_d(vars_d,
 stats_in = NULL,
 fmt_d_def = junco_def_d,
 fmt_d_in = NULL
)

lyt <- basic_table() |>
 split_cols_by("ARMCD") |>
 analyze(
   vars = vars_all,
   afun = tern::a_summary,
   # note .formats = "default" in extra_args to afun   
   extra_args = list(
     .stats = c("n", "mean_sd", "range", "count_fraction"),
     .formats = "default"
   ),
   # note the usage of format argument to analyze call
   format = vars_fmt,
   section_div = " "
)
rslt <- build_table(lyt, df)
rslt
#>                           ARM A             ARM B             ARM C     
#> ————————————————————————————————————————————————————————————————————————
#> AGE                                                                     
#>   n                        134               134               132      
#>   Mean (SD)            33.8 (6.55)       35.4 (7.90)       35.4 (7.72)  
#>   Min - Max             (21, 50)          (21, 62)          (20, 69)    
#>                                                                         
#> SEX                                                                     
#>   n                        134               134               132      
#>   F                    79 (59.0%)        77 (57.5%)        66 (50.0%)   
#>   M                    51 (38.1%)        55 (41.0%)        60 (45.5%)   
#>   U                     3 (2.2%)          2 (1.5%)          4 (3.0%)    
#>   UNDIFFERENTIATED      1 (0.7%)              0             2 (1.5%)    
#>                                                                         
#> BMRKR1                                                                  
#>   n                        134               134               132      
#>   Mean (SD)          5.970 (3.5541)    5.699 (3.3067)    5.618 (3.4940) 
#>   Min - Max           (0.41, 17.67)     (0.65, 14.24)     (0.17, 21.39) 
#>                                                                         
#> newvar                                                                  
#>   n                        134               134               132      
#>   Mean (SD)          183.69 (33.496)   195.98 (35.029)   196.31 (34.609)
#>   Min - Max          (100.7, 277.7)    (133.3, 297.4)    (121.0, 347.5)
```

### Working Example 2: Table with Multiple Parameters Analyzed with Different Precision (e.g. Lab/Vital Signs Table)

``` r

# set precision d on a parameter by parameter basis 
df_d <- tribble(
 ~PARAMCD, ~d,
 "ALT", 2,
 "CRP", 3,
 "IGA", 1
)
# convert d-based setup into format specification
# a new column fmt_d is created with format specifications
df_d <- fmt_spec_df_d(df_d,
 d_column = "d",
 fmt_column = "fmt_d",
 stats_in = NULL,
 fmt_d_def = junco_def_d_all,
 fmt_d_in = NULL
)

df <- ex_adlb |>
 filter(AVISIT %in% c("BASELINE", "WEEK 1 DAY 8", "WEEK 2 DAY 15"))

# join with dataframe with format specs
df <- df |>
 left_join(df_d)
#> Joining with `by = join_by(PARAMCD)`

lyt <- basic_table() |>
 split_cols_by("ARMCD") |>
 split_rows_by("PARAMCD", split_fun = drop_split_levels) |>
 split_rows_by("AVISIT", split_fun = drop_split_levels) |>
 analyze(
   vars = "AVAL",
   afun = tern::a_summary,
   # note .formats = "default" in extra_args to afun
   extra_args = list(
     .stats = c("n", "mean_se", "range"),
     .formats = "default"
   ),
   # note the usage of formats_var argument to analyze call   
   formats_var = "fmt_d"
 )
 
rslt <- build_table(lyt, df, alt_counts_df = ex_adsl)
rslt
#>                         ARM A               ARM B               ARM C      
#> ———————————————————————————————————————————————————————————————————————————
#> ALT                                                                        
#>   BASELINE                                                                 
#>     n                    134                 134                 132       
#>     Mean (SE)      49.572 (0.7189)     50.306 (0.7207)     50.902 (0.6808) 
#>     Min - Max       23.97, 70.85        26.22, 79.12        27.63, 67.45   
#>   WEEK 1 DAY 8                                                             
#>     n                    134                 134                 132       
#>     Mean (SE)      48.602 (0.6878)     50.442 (0.6866)     51.107 (0.6781) 
#>     Min - Max       27.71, 64.64        21.68, 67.51        29.75, 71.40   
#>   WEEK 2 DAY 15                                                            
#>     n                    134                 134                 132       
#>     Mean (SE)      49.407 (0.7314)     50.245 (0.7369)     48.494 (0.6253) 
#>     Min - Max       24.34, 71.06        24.38, 71.07        26.22, 63.35   
#> CRP                                                                        
#>   BASELINE                                                                 
#>     n                    134                 134                 132       
#>     Mean (SE)     48.9467 (0.81520)   50.0803 (0.68029)   50.2079 (0.73192)
#>     Min - Max      28.959, 75.232      29.912, 73.354      30.455, 72.251  
#>   WEEK 1 DAY 8                                                             
#>     n                    134                 134                 132       
#>     Mean (SE)     51.8606 (0.69774)   50.2698 (0.76090)   50.2549 (0.81518)
#>     Min - Max      28.466, 72.897      26.920, 67.335      27.779, 71.177  
#>   WEEK 2 DAY 15                                                            
#>     n                    134                 134                 132       
#>     Mean (SE)     49.7480 (0.71440)   50.8726 (0.64128)   49.1180 (0.71787)
#>     Min - Max      29.015, 69.077      30.697, 67.695      20.630, 70.521  
#> IGA                                                                        
#>   BASELINE                                                                 
#>     n                    134                 134                 132       
#>     Mean (SE)       49.93 (0.643)       49.74 (0.716)       50.30 (0.788)  
#>     Min - Max        33.7, 65.9          25.8, 71.5          26.0, 70.0    
#>   WEEK 1 DAY 8                                                             
#>     n                    134                 134                 132       
#>     Mean (SE)       49.41 (0.699)       51.12 (0.707)       50.75 (0.732)  
#>     Min - Max        29.3, 69.3          31.2, 74.5          26.2, 71.0    
#>   WEEK 2 DAY 15                                                            
#>     n                    134                 134                 132       
#>     Mean (SE)       50.73 (0.679)       49.88 (0.721)       49.38 (0.652)  
#>     Min - Max        33.3, 71.9          27.9, 69.8          29.4, 65.1
```

### Working Example 3: Table with Parameters Analyzed with d = 2 Precision rather than default of d = 1 (e.g. Lab/Vital Signs Table)

``` r

fmt_2 <- fmt_spec_single_d(d = 2, fmt_d_def = junco_def_d_all)

df <- ex_adlb |>
 filter(AVISIT %in% c("BASELINE", "WEEK 1 DAY 8", "WEEK 2 DAY 15"))

 lyt <- basic_table() |>
 split_cols_by("ARMCD") |>
 split_rows_by("PARAMCD", split_fun = drop_split_levels) |>
 split_rows_by("AVISIT", split_fun = drop_split_levels) |>
 analyze(
   vars = "AVAL",
   afun = tern::a_summary,
   extra_args = list(
     .stats = c("n", "mean_se", "range"),
     .formats = fmt_2
   )
 )
 
rslt <- build_table(lyt, df, alt_counts_df = ex_adsl)
rslt
#>                        ARM A             ARM B             ARM C     
#> —————————————————————————————————————————————————————————————————————
#> ALT                                                                  
#>   BASELINE                                                           
#>     n                   134               134               132      
#>     Mean (SE)     49.572 (0.7189)   50.306 (0.7207)   50.902 (0.6808)
#>     Min - Max      23.97, 70.85      26.22, 79.12      27.63, 67.45  
#>   WEEK 1 DAY 8                                                       
#>     n                   134               134               132      
#>     Mean (SE)     48.602 (0.6878)   50.442 (0.6866)   51.107 (0.6781)
#>     Min - Max      27.71, 64.64      21.68, 67.51      29.75, 71.40  
#>   WEEK 2 DAY 15                                                      
#>     n                   134               134               132      
#>     Mean (SE)     49.407 (0.7314)   50.245 (0.7369)   48.494 (0.6253)
#>     Min - Max      24.34, 71.06      24.38, 71.07      26.22, 63.35  
#> CRP                                                                  
#>   BASELINE                                                           
#>     n                   134               134               132      
#>     Mean (SE)     48.947 (0.8152)   50.080 (0.6803)   50.208 (0.7319)
#>     Min - Max      28.96, 75.23      29.91, 73.35      30.45, 72.25  
#>   WEEK 1 DAY 8                                                       
#>     n                   134               134               132      
#>     Mean (SE)     51.861 (0.6977)   50.270 (0.7609)   50.255 (0.8152)
#>     Min - Max      28.47, 72.90      26.92, 67.34      27.78, 71.18  
#>   WEEK 2 DAY 15                                                      
#>     n                   134               134               132      
#>     Mean (SE)     49.748 (0.7144)   50.873 (0.6413)   49.118 (0.7179)
#>     Min - Max      29.02, 69.08      30.70, 67.70      20.63, 70.52  
#> IGA                                                                  
#>   BASELINE                                                           
#>     n                   134               134               132      
#>     Mean (SE)     49.927 (0.6430)   49.736 (0.7162)   50.305 (0.7883)
#>     Min - Max      33.72, 65.91      25.82, 71.47      26.04, 69.99  
#>   WEEK 1 DAY 8                                                       
#>     n                   134               134               132      
#>     Mean (SE)     49.406 (0.6986)   51.116 (0.7073)   50.754 (0.7322)
#>     Min - Max      29.33, 69.31      31.19, 74.52      26.15, 70.95  
#>   WEEK 2 DAY 15                                                      
#>     n                   134               134               132      
#>     Mean (SE)     50.726 (0.6786)   49.880 (0.7207)   49.377 (0.6518)
#>     Min - Max      33.33, 71.90      27.90, 69.82      29.39, 65.05
```

### Working Example 4: Table with Multiple Parameters Analyzed with Different Precision, with statistics presented in columns (e.g. Lab/Vital Signs Table)

Using the same data as in 2 prior examples, now with a different afun,
to present statistics in columns rather than in rows.

``` r

df <- ex_adlb |>
 filter(AVISIT %in% c("BASELINE", "WEEK 1 DAY 8", "WEEK 2 DAY 15")) |> 
mutate(AVISIT = droplevels(AVISIT))

# join with dataframe with format specs
df <- df |>
 left_join(df_d)
#> Joining with `by = join_by(PARAMCD)`

mysplitfun <- make_split_fun(
   post = list(stats_in_cols_setup())
)

lyt <- basic_table() |>
 split_cols_by_multivar(
   c("AVAL", "BASE", "CHG"),
   varlabels = c("Visit Values", " ", "Change from Baseline")
 ) |>
 split_cols_by("STUDYID", split_fun = mysplitfun) |>
 split_rows_by("PARAM", split_fun = drop_split_levels) |>
 split_rows_by("ARMCD", labels_var = "ARM") |>
 analyze("AVISIT", afun = column_stats, extra_args = list(
   exclude_visits = toupper(c("Baseline", "Screening")),
   .formats = "default",
   formats_var = "fmt_d"
 ))
rslt <- build_table(lyt, df, round_type = "sas")
rslt
#>                                                                Visit Values                                                                     Change from Baseline                      
#>                                         N     Mean       SD        SE        Med      Min      Max      Base Mean (SD)      N     Mean        SD        SE        Med       Min      Max  
#> ——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Alanine Aminotransferase Measurement                                                                                                                                                      
#>   A: Drug X                                                                                                                                                                               
#>     BASELINE                           134   49.572    8.3223    0.7189    49.571    23.97    70.85                                                                                       
#>     WEEK 1 DAY 8                       134   48.602    7.9620    0.6878    48.418    27.71    64.64     49.572 (8.3223)    134   -0.970    11.7006    1.0108    -1.022    -25.05    39.39 
#>     WEEK 2 DAY 15                      134   49.407    8.4665    0.7314    48.335    24.34    71.06     49.572 (8.3223)    134   -0.165    12.0056    1.0371     0.075    -26.31    34.35 
#>   B: Placebo                                                                                                                                                                              
#>     BASELINE                           134   50.306    8.3429    0.7207    50.155    26.22    79.12                                                                                       
#>     WEEK 1 DAY 8                       134   50.442    7.9484    0.6866    50.179    21.68    67.51     50.306 (8.3429)    134    0.135    12.0710    1.0428    -1.449    -24.58    34.81 
#>     WEEK 2 DAY 15                      134   50.245    8.5308    0.7369    49.971    24.38    71.07     50.306 (8.3429)    134   -0.061    12.5263    1.0821    -0.741    -36.75    28.78 
#>   C: Combination                                                                                                                                                                          
#>     BASELINE                           132   50.902    7.8217    0.6808    50.774    27.63    67.45                                                                                       
#>     WEEK 1 DAY 8                       132   51.107    7.7903    0.6781    50.804    29.75    71.40     50.902 (7.8217)    132    0.205    10.9217    0.9506     0.051    -27.38    31.08 
#>     WEEK 2 DAY 15                      132   48.494    7.1836    0.6253    49.217    26.22    63.35     50.902 (7.8217)    132   -2.407    10.9704    0.9549    -0.964    -28.56    31.29 
#> C-Reactive Protein Measurement                                                                                                                                                            
#>   A: Drug X                                                                                                                                                                               
#>     BASELINE                           134   48.9467   9.43665   0.81520   49.5531   28.959   75.232                                                                                      
#>     WEEK 1 DAY 8                       134   51.8606   8.07696   0.69774   51.2317   28.466   72.897   48.9467 (9.43665)   134   2.9139    12.47670   1.07782   2.9400    -28.226   41.194
#>     WEEK 2 DAY 15                      134   49.7480   8.26973   0.71440   50.4409   29.015   69.077   48.9467 (9.43665)   134   0.8013    12.55250   1.08437   0.4837    -34.076   30.432
#>   B: Placebo                                                                                                                                                                              
#>     BASELINE                           134   50.0803   7.87487   0.68029   50.1184   29.912   73.354                                                                                      
#>     WEEK 1 DAY 8                       134   50.2698   8.80810   0.76090   51.2478   26.920   67.335   50.0803 (7.87487)   134   0.1895    11.57918   1.00029   0.6008    -31.276   37.223
#>     WEEK 2 DAY 15                      134   50.8726   7.42330   0.64128   51.5064   30.697   67.695   50.0803 (7.87487)   134   0.7923    10.69335   0.92376   0.8225    -22.154   27.164
#>   C: Combination                                                                                                                                                                          
#>     BASELINE                           132   50.2079   8.40910   0.73192   49.8365   30.455   72.251                                                                                      
#>     WEEK 1 DAY 8                       132   50.2549   9.36565   0.81518   50.6980   27.779   71.177   50.2079 (8.40910)   132   0.0471    13.07231   1.13780   -0.3029   -32.046   30.125
#>     WEEK 2 DAY 15                      132   49.1180   8.24771   0.71787   48.6852   20.630   70.521   50.2079 (8.40910)   132   -1.0899   11.12421   0.96824   -1.4121   -30.236   19.810
#> Immunoglobulin A Measurement                                                                                                                                                              
#>   A: Drug X                                                                                                                                                                               
#>     BASELINE                           134    49.93     7.443     0.643     48.69     33.7     65.9                                                                                       
#>     WEEK 1 DAY 8                       134    49.41     8.087     0.699     49.78     29.3     69.3      49.93 (7.443)     134    -0.52     10.683     0.923     -0.14     -28.5     21.6 
#>     WEEK 2 DAY 15                      134    50.73     7.856     0.679     49.78     33.3     71.9      49.93 (7.443)     134    0.80      11.517     0.995     1.36      -25.8     32.3 
#>   B: Placebo                                                                                                                                                                              
#>     BASELINE                           134    49.74     8.291     0.716     49.28     25.8     71.5                                                                                       
#>     WEEK 1 DAY 8                       134    51.12     8.187     0.707     50.52     31.2     74.5      49.74 (8.291)     134    1.38      11.241     0.971     1.55      -25.4     35.7 
#>     WEEK 2 DAY 15                      134    49.88     8.342     0.721     50.67     27.9     69.8      49.74 (8.291)     134    0.14      11.656     1.007     0.94      -29.5     27.6 
#>   C: Combination                                                                                                                                                                          
#>     BASELINE                           132    50.30     9.057     0.788     49.74     26.0     70.0                                                                                       
#>     WEEK 1 DAY 8                       132    50.75     8.413     0.732     51.11     26.2     71.0      50.30 (9.057)     132    0.45      12.899     1.123     0.46      -29.7     31.0 
#>     WEEK 2 DAY 15                      132    49.38     7.489     0.652     49.57     29.4     65.1      50.30 (9.057)     132    -0.93     12.044     1.048     -0.21     -32.8     28.9
```

## Requirements for afuns

In order to utilize
[`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html)
with arguments `format` or `formats_var`, the return of the used afun
(typically either a call from `rcell` or `in_rows`) should have no
formatting specification, i.e. `rcell(x, format = NULL, ...)` or
`in_rows(.list = x, .formats = NULL, ...)`.

The `tern` afuns, such as
[`a_summary()`](https://pharmaverse.github.io/tern/latest-tag/reference/analyze_variables.html)
have the argument `.formats` to control formats, when these are set to
`NULL`, the default formats will be applied. In order to have the afun
to return an `rcell`/`in_rows` value without formats, the .formats
argument should be set to `"default"`. The function
[`tern::get_formats_from_stats`](https://pharmaverse.github.io/tern/latest-tag/reference/default_stats_formats_labels.html)
will then remove the formats prior to the call to `rcell`/`in_rows`. See
also details section of
[`?tern::get_formats_from_stats`](https://pharmaverse.github.io/tern/latest-tag/reference/default_stats_formats_labels.html).

As long as the (custom) afun adheres to the above principle of
processing a `.formats` argument, the
[`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html)
arguments `format` and `formats_var` can be utilized for this varying
decimal setup approach.
