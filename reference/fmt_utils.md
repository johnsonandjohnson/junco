# Utilities for reviewing format specification objects

**\[experimental\]**

## Usage

``` r
get_fmt_details(myfmts, recursive = FALSE, as_tibble = TRUE)

compare_fmt_specs(fmt1, fmt2)
```

## Arguments

- myfmts:

  A named list of either formatting specifications (ie stats with its
  associated formats) or a list of variables with formatting
  specifications.

- recursive:

  `logical(1)` Set to `TRUE` when requesting details from named list of
  variables with specifications. Set to `FALSE` when requesting details
  from named list of statistics with format specifications.

- as_tibble:

  `logical(1)` When `TRUE`, convert the information into a tibble for
  ease of reviewing.

- fmt1:

  A named list of formatting specifications (ie stats with its
  associated format)

- fmt2:

  Another named list of formatting specifications (ie stats with its
  associated format), to compare against `fmt1`

## Details

fmt_utils general details

## Functions

- `get_fmt_details()`: A utility function to collect information on
  formatting specification in list or tibble format.

  The following information is collected:

  - var (variable name): Only applicable if the format specification is
    a named list with formattings specs (eg list("AGE" = c("mean" =
    "xx", "se" = "xx.x"), "BMI" = c("mean" = "xx.x", "se" = "xx.xx"))),
    eg processed via `fmt_spec_var_d` function.

  - stat (name of statistic)

  - fun_fact : name of function factory that has produced the format
    specification (if spec is from formatting function, blank if spec is
    string).  
    Only the following known formatting functions are supported:

    - jjcsformat_xx

    - jjcsformat_cnt_den_fract_fct,

    - jjcsformat_pval_fct,

    - jjcsformat_range_fct,

    - tern::format_xx,

    - tern::format_extreme_values,

    - tern::format_extreme_values_ci,

    - tern::format_count_fraction_fixed_dp,

    - tern::format_count_fraction,

    - tern::format_fraction_fixed_dp,

    - format_sigfig_j

  - str: string from specification, or str from formatting function
    factory (`jjcsformat_xx`, ....)

  - str_formatters: (`logical`) Is the str value a valid format string
    as in
    ([`list_valid_format_labels()`](https://pharmaverse.github.io/formatters/latest-tag/reference/list_formats.html))?

  - type: when formatting function factory
    `jjcsformat_cnt_den_fract_fct` is used

  - alpha: when formatting function factory `jjcsformat_pval_fct` is
    used

  - message: Message when formatting function is unknown, or input
    string was not valid format string as in
    ([`list_valid_format_labels()`](https://pharmaverse.github.io/formatters/latest-tag/reference/list_formats.html))

  - fmt_package: In which package was the formatting specification
    defined.

  - is_fun: (`logical`) Is the formatting specification a function?

  - round_type: (`logical`) Does the formatting support `round_type`
    argument/behavior?

  - fun: Formatting string or body of the formatting function.

  Only when one of the formats provided is based upon formatting
  function factory `format_sigfig_j` setup:

  - d: value used for `sigfig`

  - whole_integer: value used for `whole_integer`

  - drop0trailing: value used for `drop0trailing`

  - zero_threshold: value used for `zero_threshold`

- `compare_fmt_specs()`: A utility to report findings in differences
  between 2 formatting specification vectors. It returns a list object
  with the following information:

  - spec1 : For statistics with a different specification, tibble with
    specification details from `fmt1`.

  - spec2 : For statistics with a different specification, tibble with
    specification details from `fmt2`. The tibbles in `spec1` and
    `spec2` contain information as described in `get_fmt_details()`.

  - msg: Message on how many stats have a difference in specification.

  - comm_stats: Vector of stats in common between `fmt1` and `fmt2`.

  - diff_stats: Vector of stats with any difference identified.

## Examples

``` r
# Example for get_fmt_details --
junco_def_d <- c(
  "mean" = "xx.dx",
  "mean_sd" = "xx.dx (xx.dxx)",
  "range" = "(xx.d, xx.d)"
)

myfmts <- fmt_spec_single_d(
  d = 1,
  stats_in = c("mean", "mean_sd"),
  fmt_d_def = junco_def_d,
  fmt_d_in = NULL
)

fmt_details <- get_fmt_details(myfmts, as_tibble = TRUE)

myfmts2 <- fmt_spec_single_d(
  d = 3,
  stats_in = c("mean", "mean_sd"),
  fmt_d_def = junco_def_d,
  fmt_d_in = NULL
)
fmt_details2 <- get_fmt_details(myfmts2, as_tibble = TRUE)
fmt_details2
#> # A tibble: 2 × 11
#>   stat    fun_fact   str   str_formatters type  alpha message fmt_package is_fun
#>   <chr>   <chr>      <chr> <lgl>          <chr> <chr> <chr>   <chr>       <lgl> 
#> 1 mean    ""         xx.x… TRUE           ""    ""    ""      formatters  FALSE 
#> 2 mean_sd "jjcsform… xx.x… FALSE          ""    ""    ""      junco       TRUE  
#> # ℹ 2 more variables: round_type <lgl>, fun <named list>

# Example for get_fmt_details using format specs for variables --
# processed with fmt_spec_var_d
var <- c("AGE" = 0, "BMI" = 1, "BMRKR1" = 2)
vars_fmt <- fmt_spec_var_d(var,
  stats_in = NULL,
  fmt_d_def = junco_def_d,
  fmt_d_in = NULL
)
fmt_details2 <- get_fmt_details(vars_fmt, as_tibble = TRUE, recursive = TRUE)


# Example for compare_fmt_specs --
test1_target <- junco_default_formats
# junco_def_d_all has been constructed with d = 1 as reference
# compare against junco_default_formats
test1_d <- fmt_spec_single_d(
  d = 1,
  stats_in = NULL,
  fmt_d_def = junco_def_d_all,
  fmt_d_in = NULL
)

check1 <- compare_fmt_specs(test1_d, test1_target)
#> 1 common stats with differences in specs
#> More details are available in created object.
check1
#> $spec1
#> # A tibble: 1 × 11
#>   stat      fun_fact str   str_formatters type  alpha message fmt_package is_fun
#>   <chr>     <chr>    <chr> <lgl>          <chr> <chr> <chr>   <chr>       <lgl> 
#> 1 lr_stat_… jjcsfor… xx.x… FALSE          ""    ""    ""      junco       TRUE  
#> # ℹ 2 more variables: round_type <lgl>, fun <named list>
#> 
#> $spec2
#> # A tibble: 1 × 11
#>   stat      fun_fact str   str_formatters type  alpha message fmt_package is_fun
#>   <chr>     <chr>    <chr> <lgl>          <chr> <chr> <chr>   <chr>       <lgl> 
#> 1 lr_stat_… string   xx.x… FALSE          ""    ""    fmtfun… ""          FALSE 
#> # ℹ 2 more variables: round_type <lgl>, fun <named list>
#> 
#> $msg
#> [1] "1 common stats with differences in specs"
#> 
#> $comm_stats
#>   [1] "adj_mean_se"                 "adj_mean_ci"                
#>   [3] "adj_mean_est_ci"             "change"                     
#>   [5] "cv"                          "diff"                       
#>   [7] "diff_ci"                     "diff_est_ci"                
#>   [9] "diff_mean_se"                "diff_mean_ci"               
#>  [11] "diff_mean_est_ci"            "diff_means_n1"              
#>  [13] "diff_means_n2"               "diff_means_est"             
#>  [15] "diff_means_se"               "diff_means_est_se"          
#>  [17] "diff_means_ci"               "diff_means_est_ci"          
#>  [19] "event_free_ci"               "event_free_rate"            
#>  [21] "geom_sd"                     "geom_se"                    
#>  [23] "geom_mean_sd"                "geom_mean_se"               
#>  [25] "geom_cv"                     "geom_mean"                  
#>  [27] "geom_mean_ci_3d"             "hr"                         
#>  [29] "hr_ci"                       "hr_ci_3d"                   
#>  [31] "iqr"                         "quantiles_upper"            
#>  [33] "quantiles_lower"             "lsmean"                     
#>  [35] "lsmean_ci"                   "lsmean_diff"                
#>  [37] "lsmean_diff_with_ci"         "lsmean_diff_ci"             
#>  [39] "lsmean_se"                   "mad"                        
#>  [41] "mean"                        "mean_sd"                    
#>  [43] "mean_se"                     "mean_ci_3d"                 
#>  [45] "mean_diff_with_ci"           "mean_pval"                  
#>  [47] "median"                      "median_ci"                  
#>  [49] "median_ci_3d"                "median_range"               
#>  [51] "n"                           "n_tot"                      
#>  [53] "n_tot_events"                "or_ci"                      
#>  [55] "pt_at_risk"                  "pval"                       
#>  [57] "pvalue"                      "p_value"                    
#>  [59] "quantiles"                   "range"                      
#>  [61] "range_with_cens_info"        "rate_ci"                    
#>  [63] "rate_se"                     "rate_diff_ci_3d"            
#>  [65] "rel_risk_ci"                 "sd"                         
#>  [67] "se"                          "n_altdf"                    
#>  [69] "n_df"                        "n_rowdf"                    
#>  [71] "n_parentdf"                  "denom"                      
#>  [73] "count"                       "count_unique"               
#>  [75] "count_fraction"              "count_fraction_fixed_dp"    
#>  [77] "count_unique_fraction"       "count_unique_denom_fraction"
#>  [79] "count_denom_frac"            "count_frac"                 
#>  [81] "unique_fraction"             "rr_ci_3d"                   
#>  [83] "patyrs"                      "eair"                       
#>  [85] "eair_ci"                     "eair_est_ci"                
#>  [87] "eair_diff_est_ci"            "eair_diff_est"              
#>  [89] "eair_diff_ci"                "n_eair"                     
#>  [91] "eair_n"                      "eair_n_py"                  
#>  [93] "n_event"                     "person_years"               
#>  [95] "eair_strat"                  "n_event_total"              
#>  [97] "person_years_total"          "n_eair_strat"               
#>  [99] "eair_strat_ci"               "eair_strat_est_ci"          
#> [101] "eair_strat_diff_est_ci"      "total_subject_years"        
#> [103] "n_fit"                       "unique"                     
#> [105] "sum"                         "sum_unique"                 
#> [107] "ratio"                       "ratio_unique"               
#> [109] "ci"                          "fraction"                   
#> [111] "geom_mean_ci"                "lr_stat_df"                 
#> [113] "max"                         "mean_ci"                    
#> [115] "mean_sdi"                    "mean_sei"                   
#> [117] "min"                         "n_blq"                      
#> [119] "n_events"                    "n_patients"                 
#> [121] "n_prop"                      "n_rate"                     
#> [123] "n_rsp"                       "n_unique"                   
#> [125] "nonunique"                   "or"                         
#> [127] "prop"                        "prop_ci"                    
#> [129] "pval_counts"                 "range_censor"               
#> [131] "range_event"                 "rate"                       
#> [133] "rate_diff"                   "rate_diff_ci"               
#> [135] "rate_ratio"                  "rate_ratio_ci"              
#> [137] "riskdiff"                    "sum_exposure"               
#> [139] "unique_count"                "ztest_pval"                 
#> 
#> $diff_stats
#> [1] "lr_stat_df"
#> 

# more differences when using other d
test2_d <- fmt_spec_single_d(
  d = 2,
  stats_in = NULL,
  fmt_d_def = junco_def_d_all,
  fmt_d_in = NULL
)

check2 <- compare_fmt_specs(test2_d, test1_target)
#> 54 common stats with differences in specs
#> More details are available in created object.
```
