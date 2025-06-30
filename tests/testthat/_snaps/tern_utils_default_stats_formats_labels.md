# tern_get_stats works as expected for defaults

    Code
      res
    Output
      [1] "count"                   "count_fraction"         
      [3] "count_fraction_fixed_dp" "fraction"               

---

    Code
      res
    Output
      [1] "unique"       "nonunique"    "unique_count"

---

    Code
      res
    Output
      [1] "n"                       "count"                  
      [3] "count_fraction"          "count_fraction_fixed_dp"
      [5] "fraction"                "n_blq"                  

---

    Code
      res
    Output
       [1] "n"               "sum"             "mean"            "sd"             
       [5] "se"              "mean_sd"         "mean_se"         "mean_ci"        
       [9] "mean_sei"        "mean_sdi"        "mean_pval"       "median"         
      [13] "mad"             "median_ci"       "quantiles"       "iqr"            
      [17] "range"           "min"             "max"             "median_range"   
      [21] "cv"              "geom_mean"       "geom_sd"         "geom_mean_sd"   
      [25] "geom_mean_ci"    "geom_cv"         "median_ci_3d"    "mean_ci_3d"     
      [29] "geom_mean_ci_3d"

# tern_get_labels_from_stats works as expected

    Code
      res
    Output
      $count
      [1] "count"
      
      $count_fraction
      [1] "count_fraction"
      
      $count_fraction_fixed_dp
      [1] "count_fraction_fixed_dp"
      
      $fraction
      [1] "fraction"
      

# tern_get_indents_from_stats works as expected

    Code
      res
    Output
      $count
      [1] 0
      
      $count_fraction
      [1] 0
      
      $count_fraction_fixed_dp
      [1] 0
      
      $fraction
      [1] 0
      

# .split_std_from_custom_stats works as expected with a single input

    Code
      result
    Output
      $default_stats
      [1] "default"
      
      $custom_stats
      NULL
      
      $all_stats
      [1] "default"
      

