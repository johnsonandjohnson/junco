# Formatting function for significant figures

## Overview

Many clinical tables require statistics presented on a table with
certain decimal precision. In some situations, the usage of significant
figures is preferred over a fixed decimal precision. A typical situation
is in the context of PK tables.

The purpose in this vignette is to provide some examples for
`format_sigfig_j` and the difference between
[`tern::format_sigfig()`](https://pharmaverse.github.io/tern/latest-tag/reference/format_sigfig.html).

``` r

library(rtables)
library(junco)
library(dplyr)
library(tern)
```

## Significant Figures and Company Specific Requirements

For PK tables, the requirements within our company to present numerical
values of core PK parameters with 3 significant figures, except for
values above 1000, the whole integer has to be reported. E.g. 3
significant figures for a value 1234.567 is `signif(1234.567, 3)` =
1230, while a value of 1235 is expected.

In addition, we want to adhere to possibility to adjust the type of
rounding to be `SAS`, `iec` or `iec_mod`, through the `formatters`
functions `format_value` and `round_fmt`. The base function `sigfig`
relies on `round`, so only supports `iec` rounding type.

For these 2 reasons, the formatting function factory `format_sigfig_j`
has been developed as alternative to
[`tern::format_sigfig`](https://pharmaverse.github.io/tern/latest-tag/reference/format_sigfig.html).

## Basic Examples of Formatting Function Factory `format_sigfig_j`

Below are a few examples where the difference in behavior between
`signif`,
[`tern::format_sigfig`](https://pharmaverse.github.io/tern/latest-tag/reference/format_sigfig.html)
and
[`junco::format_sigfig_j`](https://johnsonandjohnson.github.io/junco/reference/format_sigfig_j.md)
is demonstrated.

``` r

fmt <- format_sigfig_j(3)
fmt(0.001234)
#> [1] "0.00123"
fmt(1234.5)
#> [1] "1234"
fmt(1234.5, round_type = "sas")
#> [1] "1235"
signif(1234.5, 3)
#> [1] 1230
format_sigfig(3)(1234.5)
#> [1] "1230"

fmt2 <- format_sigfig_j(3, format = "xx (xx)")
fmt2(c(1234.5, 0.00567))
#> [1] "1234 (0.00567)"

format_sigfig_j(3, drop0trailing = FALSE)(0.01)
#> [1] "0.0100"
format_sigfig_j(3, drop0trailing = TRUE)(0.01)
#> [1] "0.01"
format_sigfig(3)(0.01)
#> [1] "0.0100"
```

## Formats based upon `junco::format_sigfig_j` in `rtables` Layout

### Example as in Tern TLG Catalog using `analyze_vars_in_cols`

The following example is taken from the [**tern TLG
catalog**](https://insightsengineering.github.io/tlg-catalog/stable/tables/pharmacokinetic/pkct01.html).

Here is the data that is being used.

``` r

adsl <- random.cdisc.data::cadsl |>
  filter(ACTARM == "A: Drug X")
adpc <- random.cdisc.data::cadpc |>
  filter(ACTARM == "A: Drug X", PARAM %in% c("Plasma Drug X", "Urine Drug X"))

# Setting up the data
adpc_1 <- adpc |>
  mutate(
    NFRLT = as.factor(NFRLT),
    AVALCAT1 = as.factor(AVALCAT1),
    NOMTPT = as.factor(paste(NFRLT, "/", PCTPT))
  ) |>
  select(STUDYID, NOMTPT, ACTARM, VISIT, AVAL, PARAM, AVALCAT1)

adpc_1$NOMTPT <- factor(
  adpc_1$NOMTPT,
  levels = levels(adpc_1$NOMTPT)[order(as.numeric(gsub(".*?([0-9\\.]+).*", "\\1", levels(adpc_1$NOMTPT))))]
)
```

The following row-structure setup in the layout definition is utilized.

``` r

# Row structure
lyt_rows <- basic_table() |>
  split_rows_by(
    var = "PARAM",
    split_fun = drop_split_levels,
    split_label = "Parameter",
    label_pos = "topleft"
  ) |>      
  split_rows_by(
    var = "ACTARM",
    split_fun = drop_split_levels,
    split_label = "Treatment Group",
    label_pos = "topleft"
  ) |>
  split_rows_by(
    var = "VISIT",
    split_fun = drop_split_levels,
    split_label = "Visit",
    label_pos = "topleft"
  ) |>
  split_rows_by(
    var = "NOMTPT",
    split_fun = drop_split_levels,
    split_label = "Nominal Time (hr) / Timepoint",
    label_pos = "topleft",
    child_labels = "hidden"
  )
```

The core component of the layout where the significant figures format
specifications are used is in below call to
[`tern::analyze_vars_in_cols`](https://pharmaverse.github.io/tern/latest-tag/reference/analyze_vars_in_cols.html).
The only adjustment made here compared to the example in the **tern TLG
catalog** is the usage of formatting function
`junco::format_sigfig_j(3)` versus `tern::format_sigfig(3)`.

``` r

lyt <- lyt_rows |>
  analyze_vars_in_cols(
    vars = c("AVAL", "AVALCAT1", rep("AVAL", 8)),
    .stats = c("n", "n_blq", "mean", "sd", "cv", "geom_mean", "geom_cv", "median", "min", "max"),
    .formats = c(
      n = "xx.", n_blq = "xx.", mean = format_sigfig_j(3), sd = format_sigfig_j(3), cv = "xx.x", median = format_sigfig_j(3),
      geom_mean = format_sigfig_j(3), geom_cv = "xx.x", min = format_sigfig_j(3), max = format_sigfig_j(3)
    ),
    .labels = c(
      n = "n", n_blq = "Number\nof\nLTRs/BLQs", mean = "Mean", sd = "SD", cv = "CV (%) Mean",
      geom_mean = "Geometric Mean", geom_cv = "CV % Geometric Mean", median = "Median", min = "Minimum", max = "Maximum"
    ),
    na_str = "NE",
    .aligns = "decimal"
  )

result <- build_table(lyt, df = adpc_1, alt_counts_df = adsl) 

result
#> Parameter                                                                                                                                                   
#>   Treatment Group                            Number                                                                                                         
#>     Visit                                      of                                                                                                           
#>       Nominal Time (hr) / Timepoint    n    LTRs/BLQs    Mean       SD     CV (%) Mean   Geometric Mean   CV % Geometric Mean   Median    Minimum    Maximum
#> ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Plasma Drug X                                                                                                                                               
#>   A: Drug X                                                                                                                                                 
#>     Day 1                                                                                                                                                   
#>       0 / Predose                     134      134       0        0            NE              NE                 NE             0         0          0     
#>       0.5 / 0.5H                      134        0      12.6      1.51        12.0          12.5                  12.2          12.6       9.72      15.6   
#>       1 / 1H                          134        0      16.2      1.63        10.0          16.1                  10.1          16.2      12.6       19.9   
#>       1.5 / 1.5H                      134        0      15.6      1.46         9.3          15.6                   9.3          15.5      12.3       19.0   
#>       2 / 2H                          134        0      13.4      1.35        10.1          13.4                  10.0          13.3      10.8       16.5   
#>       3 / 3H                          134        0       8.47     1.25        14.7           8.38                 15.0           8.40      5.88      10.9   
#>       4 / 4H                          134        0       4.79     1.02        21.2           4.69                 22.0           4.79      2.70       7.09  
#>       8 / 8H                          134        0       0.348    0.180       51.7           0.303                58.4           0.318     0.0760     0.866 
#>       12 / 12H                        134        0       0.0224   0.0189      84.6           0.0156              111.6           0.0170    0.00200    0.0830
#>     Day 2                                                                                                                                                   
#>       24 / 24H                        134      134       0        0            NE              NE                 NE             0         0          0     
#> Urine Drug X                                                                                                                                                
#>   A: Drug X                                                                                                                                                 
#>     Day 1                                                                                                                                                   
#>       0 / Predose                     134      134       0        0            NE              NE                 NE             0         0          0     
#>       4 / 0H - 4H                     134        0       4.79     1.02        21.2           4.69                 22.0           4.79      2.70       7.09  
#>       8 / 4H - 8H                     134        0       0.348    0.180       51.7           0.303                58.4           0.318     0.0760     0.866 
#>       12 / 8H - 12H                   134        0       0.0224   0.0189      84.6           0.0156              111.6           0.0170    0.00200    0.0830
#>     Day 2                                                                                                                                                   
#>       24 / 0H - 24H                   134      134       0        0            NE              NE                 NE             0         0          0
```

### Same Example with `junco` afun `column_stats`

An alternative, very similar layout can be generated using the `junco`
afun `column_stats`. The advantage of this afun, is that it can deal
with so called, varying decimal precision.

In case, not all parameters of the table are to be presented with the
same number of significant figures, the following example demonstrates
how this can be achieved using
[`junco::column_stats`](https://johnsonandjohnson.github.io/junco/reference/column_stats.md).

``` r

 mysplitfun <- make_split_fun(
   post = list(stats_in_cols_setup())
 )

 lyt_stats_in_cols <- basic_table() |>
   split_cols_by_multivar(
     c("AVAL"),
     varlabels = c("Visit Values")
   ) |>
   split_cols_by("STUDYID", split_fun = mysplitfun) |>
   split_rows_by("PARAM", split_fun = drop_split_levels) |>
   split_rows_by("ACTARM", labels_var = "ACTARM", split_fun = drop_split_levels) |>
   split_rows_by("VISIT", split_fun = drop_split_levels) 
 
 
 adpc_1 <- adpc_1 |> 
   filter(VISIT == "Day 1", !(NOMTPT %in% "0 / Predose")) 
```

Here 2 methods of setting up format specification based upon
`format_sigfig_j` are presented.

The first one is a manual setup for each parameter separately.

``` r

 adpc_fmt <- tribble(
   ~PARAM, ~fmt_d,
   "Plasma Drug X", list(
     mean = format_sigfig_j(3), sd = format_sigfig_j(3), se = format_sigfig_j(3), median = format_sigfig_j(3),
     min = format_sigfig_j(3), max = format_sigfig_j(3)
   ),
   "Urine Drug X", list(
     mean = format_sigfig_j(2), sd = format_sigfig_j(2), se = format_sigfig_j(2), median = format_sigfig_j(2),
     min = format_sigfig_j(2), max = format_sigfig_j(2)
   )
 )

adpc_2 <- left_join(adpc_1, adpc_fmt) 
#> Joining with `by = join_by(PARAM)`
```

The second one is a setup, using the same tools as in [vignette on
Formatting tools to simplify adjustment of decimal
precision](https://johnsonandjohnson.github.io/junco/articles/formats_d.md)
(`fmt_spec_df_d`), to convert a simple dataframe setup into a dataframe
with formatting specifications.

``` r

junco_def_d_sigfig <- list(
     mean = format_sigfig_j(3), sd = format_sigfig_j(3), se = format_sigfig_j(3), median = format_sigfig_j(3),
     min = format_sigfig_j(3), max = format_sigfig_j(3)
   )

adpc_fmt2 <- tribble(
   ~PARAM, ~d,
   "Plasma Drug X", 3,
   "Urine Drug X", 2
 )

adpc_fmt2 <- fmt_spec_df_d(adpc_fmt2,
                    d_column = "d",
                    fmt_column = "fmt_d2",
                    fmt_d_def = junco_def_d_sigfig,
                    fmt_d_in = NULL
)


fmt_details <- lapply(adpc_fmt2$PARAM,
                      FUN = function(param){
                        fmt <- adpc_fmt2[adpc_fmt2$PARAM == param, c("fmt_d2"), drop = TRUE][[1]]
                        fmt
                        get_fmt_details(fmt, recursive = FALSE, as_tibble = TRUE)
                      }
  )

names(fmt_details) <- adpc_fmt2$PARAM

# confirm 2 significant figures for Urine Drug X
# see column d in tibble
print(fmt_details[["Urine Drug X"]][, c("stat", "fun_fact", "str", "d", "drop0trailing", "whole_integer")])
#> # A tibble: 6 × 6
#>   stat   fun_fact        str       d drop0trailing whole_integer
#>   <chr>  <chr>           <chr> <dbl> <lgl>         <lgl>        
#> 1 mean   format_sigfig_j xx        2 FALSE         TRUE         
#> 2 sd     format_sigfig_j xx        2 FALSE         TRUE         
#> 3 se     format_sigfig_j xx        2 FALSE         TRUE         
#> 4 median format_sigfig_j xx        2 FALSE         TRUE         
#> 5 min    format_sigfig_j xx        2 FALSE         TRUE         
#> 6 max    format_sigfig_j xx        2 FALSE         TRUE

adpc_3 <- left_join(adpc_1, adpc_fmt2) 
#> Joining with `by = join_by(PARAM)`
```

Now, both layouts are being finalized and results are created.

``` r

 lyt2 <- lyt_stats_in_cols |> 
   analyze("NOMTPT", afun = column_stats, extra_args = list(
     .formats = "default",
     formats_var = "fmt_d"
   ))
 result2 <- build_table(lyt2, adpc_2, round_type = "sas") |> 
   prune_table()
 result2
#>                                              Visit Values                        
#>                        N     Mean      SD       SE       Med       Min      Max  
#> —————————————————————————————————————————————————————————————————————————————————
#> Plasma Drug X                                                                    
#>   A: Drug X                                                                      
#>     Day 1                                                                        
#>       0.5 / 0.5H      134    12.6     1.51     0.130     12.6     9.72      15.6 
#>       1 / 1H          134    16.2     1.63     0.140     16.2     12.6      19.9 
#>       1.5 / 1.5H      134    15.6     1.46     0.126     15.5     12.3      19.0 
#>       2 / 2H          134    13.4     1.35     0.117     13.3     10.8      16.5 
#>       3 / 3H          134    8.47     1.25     0.108     8.40     5.88      10.9 
#>       4 / 4H          134    4.79     1.02    0.0878     4.79     2.70      7.09 
#>       8 / 8H          134   0.348    0.180    0.0155    0.319    0.0760    0.866 
#>       12 / 12H        134   0.0224   0.0189   0.00164   0.0170   0.00200   0.0830
#> Urine Drug X                                                                     
#>   A: Drug X                                                                      
#>     Day 1                                                                        
#>       4 / 0H - 4H     134    4.8      1.0      0.088     4.8       2.7      7.1  
#>       8 / 4H - 8H     134    0.35     0.18     0.016     0.32     0.076     0.87 
#>       12 / 8H - 12H   134   0.022    0.019    0.0016    0.017    0.0020    0.083
```

``` r

lyt3 <- lyt_stats_in_cols |> 
 analyze("NOMTPT", afun = column_stats, extra_args = list(
   .formats = "default",
   formats_var = "fmt_d2"
 ))
result3 <- build_table(lyt3, adpc_3, round_type = "sas") |> 
 prune_table()
result3
#>                                              Visit Values                        
#>                        N     Mean      SD       SE       Med       Min      Max  
#> —————————————————————————————————————————————————————————————————————————————————
#> Plasma Drug X                                                                    
#>   A: Drug X                                                                      
#>     Day 1                                                                        
#>       0.5 / 0.5H      134    12.6     1.51     0.130     12.6     9.72      15.6 
#>       1 / 1H          134    16.2     1.63     0.140     16.2     12.6      19.9 
#>       1.5 / 1.5H      134    15.6     1.46     0.126     15.5     12.3      19.0 
#>       2 / 2H          134    13.4     1.35     0.117     13.3     10.8      16.5 
#>       3 / 3H          134    8.47     1.25     0.108     8.40     5.88      10.9 
#>       4 / 4H          134    4.79     1.02    0.0878     4.79     2.70      7.09 
#>       8 / 8H          134   0.348    0.180    0.0155    0.319    0.0760    0.866 
#>       12 / 12H        134   0.0224   0.0189   0.00164   0.0170   0.00200   0.0830
#> Urine Drug X                                                                     
#>   A: Drug X                                                                      
#>     Day 1                                                                        
#>       4 / 0H - 4H     134    4.8      1.0      0.088     4.8       2.7      7.1  
#>       8 / 4H - 8H     134    0.35     0.18     0.016     0.32     0.076     0.87 
#>       12 / 8H - 12H   134   0.022    0.019    0.0016    0.017    0.0020    0.083
```

Confirmation that both approaches yield the same results (raw +
formatted values).

``` r

identical(cell_values(result2), cell_values(result3))
#> [1] TRUE
identical(get_formatted_cells(result2), get_formatted_cells(result3))
#> [1] TRUE
```
