# s_coxph_hr works with default arguments and no stratification factors

    Code
      res
    Output
      $pvalue
      [1] 0.09049511
      attr(,"label")
      [1] "p-value (log-rank)"
      
      $lr_stat_df
      [1] 2.865544 1.000000
      
      $hr
      [1] 0.7108557
      
      $hr_ci
      [1] 0.4779138 1.0573368
      attr(,"label")
      [1] "95% CI"
      
      $hr_ci_3d
      [1] 0.7108557 0.4779138 1.0573368
      attr(,"label")
      [1] "Hazard Ratio (95% CI)"
      
      $n_tot
      [1] 142
      
      $n_tot_events
      [1] 101
      

# a_coxph_hr works with custom arguments and stratification factors

    Code
      res
    Output
                                ARM A         ARM B               ARM C      
      ———————————————————————————————————————————————————————————————————————
      Stratified Analysis                                                    
        Hazard Ratio (85% CI)           1.50 (1.11, 2.04)   1.88 (1.33, 2.64)
        p-value (wald)                        0.052               0.008      

# a_coxph_hr works with stratification factors for Log-Rank test

    Code
      res
    Output
                                ARM A         ARM B               ARM C      
      ———————————————————————————————————————————————————————————————————————
      Stratified Analysis                                                    
        Hazard Ratio (85% CI)           1.50 (1.11, 2.04)   1.88 (1.33, 2.64)
        p-value (log-rank)                    0.051               0.007      

