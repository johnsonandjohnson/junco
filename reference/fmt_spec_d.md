# Utilities for processing d-style string formatting specifications (eg "xx.dx")

**\[experimental\]**

## Usage

``` r
fmt_spec_single_d(
  d = 1,
  stats_in = NULL,
  fmt_d_def = junco_def_d_all,
  fmt_d_in = NULL
)

fmt_spec_df_d(
  df_d,
  d_column = "d",
  fmt_column = "fmt_d",
  stats_in = NULL,
  fmt_d_def = junco_def_d_all,
  fmt_d_in = NULL
)

fmt_spec_var_d(
  var_d,
  stats_in = NULL,
  fmt_d_def = junco_def_d_all,
  fmt_d_in = NULL
)

junco_def_d_all

junco_def_d_alt1

junco_def_d_alt2
```

## Format

- `junco_def_d_all` Used as default input vector for argument
  `fmt_d_def` in functions `fmt_spec_single_d()`, `fmt_spec_df_d()` and
  `fmt_spec_var_d()` (corresponds to the named vector of with default
  available formats
  [junco_default_formats](https://johnsonandjohnson.github.io/junco/reference/default_stats_formats_labels.md)
  with d = 1)

&nbsp;

- `junco_def_d_alt1` Alternative vector of d-style specification
  defaults.

&nbsp;

- `junco_def_d_alt2` Another alternative vector of d-style specification
  defaults.

## Arguments

- d:

  `integer(1)` Value of `d` for the translation of "xx.dx" into "xx.xx"
  style (example when `d` = 1)

- stats_in:

  `(character)` Set of statistics to restrict to.

- fmt_d_def:

  Named vector with default format specifications where d-style format
  is allowed.

- fmt_d_in:

  Named vector with format specifications where d-style format is
  allowed. Formats defined here will take higher precedence over formats
  in `fmt_d_def`.

- df_d:

  Dataframe in which the d-column specification is available.

- d_column:

  Name of column within dataframe `df_d` that contains required integer
  values for `d`.

- fmt_column:

  Name of column that will be added to dataframe that will contain
  resulting format specifications.

- var_d:

  Named vector of d specifications. Names are to be considered as
  variables to be analyzed in further processing.

## Details

Format specifications in d-style notation (such as "xx.dx") will be
translated to string based formats (or wrapped with
[`jjcsformat_xx()`](https://johnsonandjohnson.github.io/junco/reference/jjcsformat_xx.md)),
eg "xx.dx" will be translated into "xx.xx", when d = 1.

## Functions

- `fmt_spec_single_d()`: Function to convert a named list using d-style
  string format specification into a named list with valid `formatters`
  formatting specification

- `fmt_spec_df_d()`: Function to convert an input column with d-style
  formatting specification into a new column with valid formatters
  formatting specification.

  The new column can be passed onto argument `formats_var` in
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html)
  function.

- `fmt_spec_var_d()`: Function to convert a named list of variables with
  d-style formatting specification into a named list with valid
  `formatters` formatting specification.

  The result can be used as input for argument `format` in
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html)
  call.

- `junco_def_d_all`: Named vector of default format specification using
  d-style notation.

- `junco_def_d_alt1`: Alternative vector of d-style specification
  defaults.

- `junco_def_d_alt2`: Another alternative vector of d-style
  specification defaults.

## Examples

``` r
junco_def_d <- c(
  "mean" = "xx.dx",
  "mean_sd" = "xx.dx (xx.dxx)",
  "mean_se" = "xx.dx (xx.dxx)",
  "range" = "(xx.d, xx.d)"
)
myfmts <- fmt_spec_single_d(
  d = 1,
  stats_in = c("mean", "mean_sd"),
  fmt_d_def = junco_def_d,
  fmt_d_in = NULL
)
myfmts
#> $mean
#> [1] "xx.xx"
#> 
#> $mean_sd
#> [1] "xx.xx (xx.xxx)"
#> 

# example for fmt_spec_df_d ----
library(dplyr)
df_d <- tribble(
  ~PARAMCD, ~d,
  "DIABP", 2L,
  "PULSE", 3L,
  "RESP", 1L
)
yy <- fmt_spec_df_d(df_d,
  d_column = "d",
  fmt_column = "fmt_d",
  stats_in = NULL,
  fmt_d_def = junco_def_d,
  fmt_d_in = NULL
)


df <- ex_advs |>
  filter(PARAMCD %in% c("DIABP", "PULSE", "RESP")) |>
  filter(AVISIT %in% c("BASELINE", "WEEK 1 DAY 8", "WEEK 2 DAY 15"))
df <- df |>
  left_join(yy)
#> Joining with `by = join_by(PARAMCD)`

lyt <- basic_table() |>
  split_cols_by("ARMCD") |>
  split_rows_by("PARAMCD", split_fun = drop_split_levels) |>
  split_rows_by("AVISIT", split_fun = drop_split_levels) |>
  analyze(
    vars = "AVAL",
    afun = tern::a_summary,
    extra_args = list(
      .stats = c("n", "mean_se", "range"),
      .formats = "default"
    ),
    formats_var = "fmt_d"
  )

rslt <- build_table(lyt, df, alt_counts_df = ex_adsl)
rslt
#>                         ARM A               ARM B               ARM C      
#> ———————————————————————————————————————————————————————————————————————————
#> DIABP                                                                      
#>   BASELINE                                                                 
#>     n                    134                 134                 132       
#>     Mean (SE)      48.602 (0.6878)     50.442 (0.6866)     51.107 (0.6781) 
#>     Min - Max      (27.71, 64.64)      (21.68, 67.51)      (29.75, 71.40)  
#>   WEEK 1 DAY 8                                                             
#>     n                    134                 134                 132       
#>     Mean (SE)      50.260 (0.6491)     49.674 (0.6689)     48.855 (0.6866) 
#>     Min - Max      (33.04, 68.98)      (33.71, 66.49)      (30.39, 66.99)  
#>   WEEK 2 DAY 15                                                            
#>     n                    134                 134                 132       
#>     Mean (SE)      50.836 (0.6781)     49.718 (0.7296)     49.981 (0.7257) 
#>     Min - Max      (31.89, 70.34)      (30.64, 68.11)      (24.82, 65.57)  
#> PULSE                                                                      
#>   BASELINE                                                                 
#>     n                    134                 134                 132       
#>     Mean (SE)     51.8606 (0.69774)   50.2698 (0.76090)   50.2549 (0.81518)
#>     Min - Max     (28.466, 72.897)    (26.920, 67.335)    (27.779, 71.177) 
#>   WEEK 1 DAY 8                                                             
#>     n                    134                 134                 132       
#>     Mean (SE)     50.0645 (0.71838)   49.2673 (0.64961)   49.7958 (0.66585)
#>     Min - Max     (26.697, 69.371)    (30.697, 67.762)    (30.145, 72.893) 
#>   WEEK 2 DAY 15                                                            
#>     n                    134                 134                 132       
#>     Mean (SE)     49.7104 (0.75649)   50.8322 (0.65607)   49.0580 (0.66646)
#>     Min - Max     (25.968, 69.622)    (33.317, 70.273)    (32.199, 73.643) 
#> RESP                                                                       
#>   BASELINE                                                                 
#>     n                    134                 134                 132       
#>     Mean (SE)       49.41 (0.699)       51.12 (0.707)       50.75 (0.732)  
#>     Min - Max       (29.3, 69.3)        (31.2, 74.5)        (26.2, 71.0)   
#>   WEEK 1 DAY 8                                                             
#>     n                    134                 134                 132       
#>     Mean (SE)       49.49 (0.695)       49.98 (0.684)       50.97 (0.681)  
#>     Min - Max       (24.1, 66.1)        (25.0, 67.7)        (29.9, 69.8)   
#>   WEEK 2 DAY 15                                                            
#>     n                    134                 134                 132       
#>     Mean (SE)       49.66 (0.663)       50.35 (0.748)       49.31 (0.743)  
#>     Min - Max       (30.8, 71.3)        (28.5, 77.0)        (22.4, 67.6)   

# example for fmt_spec_var_d ----
var <- c("AGE" = 0, "BMI" = 1, "BMRKR1" = 2)
vars_fmt <- fmt_spec_var_d(var,
  stats_in = NULL,
  fmt_d_def = junco_def_d,
  fmt_d_in = NULL
)
vars_fmt
#> $AGE
#> $AGE$mean
#> [1] "xx.x"
#> 
#> $AGE$mean_sd
#> [1] "xx.x (xx.xx)"
#> 
#> $AGE$mean_se
#> [1] "xx.x (xx.xx)"
#> 
#> $AGE$range
#> [1] "(xx., xx.)"
#> 
#> 
#> $BMI
#> $BMI$mean
#> [1] "xx.xx"
#> 
#> $BMI$mean_sd
#> [1] "xx.xx (xx.xxx)"
#> 
#> $BMI$mean_se
#> [1] "xx.xx (xx.xxx)"
#> 
#> $BMI$range
#> [1] "(xx.x, xx.x)"
#> 
#> 
#> $BMRKR1
#> $BMRKR1$mean
#> [1] "xx.xxx"
#> 
#> $BMRKR1$mean_sd
#> function (x, output, round_type = valid_round_type, na_str = na_str_dflt) 
#> {
#>     if (anyNA(na_str) || (replace_na_dflt && any(na_str == "NA"))) {
#>         na_inds <- which(is.na(na_str) | (replace_na_dflt & na_str == 
#>             "NA"))
#>         na_str[na_inds] <- rep(na_str_dflt, length.out = length(na_str))[na_inds]
#>     }
#>     if (length(x) == 0 || isTRUE(all(x == ""))) {
#>         return(NULL)
#>     }
#>     else if (!length(positions[[1]]) == length(x)) {
#>         stop("Error: input str in call to jjcsformat_xx must contain same number of xx as the number of stats.")
#>     }
#>     round_type <- match.arg(round_type)
#>     values <- Map(y = x, fun = roundings, na_str = na_str, function(y, 
#>         fun, na_str, output) {
#>         fun(y, na_str = na_str, round_type = round_type)
#>     })
#>     regmatches(x = str, m = positions)[[1]] <- values
#>     return(str)
#> }
#> <bytecode: 0x558cca2309b0>
#> <environment: 0x558cc53cca20>
#> 
#> $BMRKR1$mean_se
#> function (x, output, round_type = valid_round_type, na_str = na_str_dflt) 
#> {
#>     if (anyNA(na_str) || (replace_na_dflt && any(na_str == "NA"))) {
#>         na_inds <- which(is.na(na_str) | (replace_na_dflt & na_str == 
#>             "NA"))
#>         na_str[na_inds] <- rep(na_str_dflt, length.out = length(na_str))[na_inds]
#>     }
#>     if (length(x) == 0 || isTRUE(all(x == ""))) {
#>         return(NULL)
#>     }
#>     else if (!length(positions[[1]]) == length(x)) {
#>         stop("Error: input str in call to jjcsformat_xx must contain same number of xx as the number of stats.")
#>     }
#>     round_type <- match.arg(round_type)
#>     values <- Map(y = x, fun = roundings, na_str = na_str, function(y, 
#>         fun, na_str, output) {
#>         fun(y, na_str = na_str, round_type = round_type)
#>     })
#>     regmatches(x = str, m = positions)[[1]] <- values
#>     return(str)
#> }
#> <bytecode: 0x558cca2309b0>
#> <environment: 0x558cc539f318>
#> 
#> $BMRKR1$range
#> [1] "(xx.xx, xx.xx)"
#> 
#> 
lyt <- basic_table() |>
  split_cols_by("ARMCD") |>
  analyze(
    vars = c("AGE", "BMRKR1"),
    afun = tern::a_summary,
    extra_args = list(
      .stats = c("n", "mean_sd", "range"),
      .formats = "default"
    ),
    format = vars_fmt,
    section_div = " "
  )

rslt <- build_table(lyt, ex_adsl)
rslt
#>                   ARM A            ARM B            ARM C     
#> ——————————————————————————————————————————————————————————————
#> AGE                                                           
#>   n                134              134              132      
#>   Mean (SD)    33.8 (6.55)      35.4 (7.90)      35.4 (7.72)  
#>   Min - Max      (21, 50)         (21, 62)         (20, 69)   
#>                                                               
#> BMRKR1                                                        
#>   n                134              134              132      
#>   Mean (SD)   5.970 (3.5541)   5.699 (3.3067)   5.618 (3.4940)
#>   Min - Max   (0.41, 17.67)    (0.65, 14.24)    (0.17, 21.39) 
```
