# get_stats works as expected

    Code
      res
    Output
      [1] "quantiles_lower"      "median_ci_3d"         "quantiles_upper"     
      [4] "range_with_cens_info"

# get_labels_from_stats works as expected

    Code
      res
    Output
      $quantiles_upper
      [1] "75%-ile (95% CI)"
      
      $range_with_cens_info
      [1] "Min, max"
      

# get_label_attr_from_stats works as expected

    Code
      res
    Output
      stats1 stats2 
       "bla"  "boo" 

# get_indents_from_stats works as expected

    Code
      res
    Output
      $quantiles_upper
      [1] 0
      
      $range_with_cens_info
      [1] 0
      

