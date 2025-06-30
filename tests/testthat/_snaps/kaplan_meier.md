# s_kaplan_meier works with default arguments

    Code
      result
    Output
      $quantiles_lower
      [1]  9.822926  5.628823 16.690121
      attr(,"label")
      [1] "25th percentile (95% CI)"
      
      $median_ci_3d
      [1] 23.91143 18.25878 32.85945
      attr(,"label")
      [1] "Median (95% CI)"
      
      $quantiles_upper
      [1] 41.98181 32.85945 53.41445
      attr(,"label")
      [1] "75th percentile (95% CI)"
      
      $range_with_cens_info
      [1]   0.07143141 154.08901021   0.00000000   0.00000000
      

# s_kaplan_meier works with customized arguments

    Code
      result
    Output
      $quantiles_lower
      [1]  7.310208  3.534653 12.168852
      attr(,"label")
      [1] "20th percentile (99% CI)"
      
      $median_ci_3d
      [1] 24.75622 19.83908 32.14787
      attr(,"label")
      [1] "Median (99% CI)"
      
      $quantiles_upper
      [1] 53.41445 41.98181 80.69172
      attr(,"label")
      [1] "80th percentile (99% CI)"
      
      $range_with_cens_info
      [1]   0.07143141 155.49873318   0.00000000   1.00000000
      

# a_kaplan_meier works with default arguments

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
                    row_name       formatted_cell indent_mod                row_label
      1      quantiles_lower 10.81 (6.65 - 13.43)          0 25th percentile (95% CI)
      2         median_ci_3d 24.76 (21.10, 31.35)          0          Median (95% CI)
      3      quantiles_upper 47.60 (39.27, 57.82)          0 75th percentile (95% CI)
      4 range_with_cens_info      (0.07, 155.50+)          0                 Min, max

# a_kaplan_meier works with customized arguments

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
                    row_name       formatted_cell indent_mod                row_label
      1      quantiles_lower  7.31 (3.53 - 12.17)          0 20th percentile (99% CI)
      2         median_ci_3d 24.76 (19.84, 32.15)          0          Median (99% CI)
      3      quantiles_upper 53.41 (41.98, 80.69)          0 80th percentile (99% CI)
      4 range_with_cens_info      (0.07, 155.50+)          0                 Min, max

# a_kaplan_meier works inside analyze in table

    Code
      res
    Output
                                                                ARM A                  ARM B                  ARM C        
      —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Kaplan-Meier estimate of time to event (months)                                                                      
        25th percentile (95% CI)                        17.37 (10.13 - 22.51)   9.82 (5.63 - 16.69)    7.31 (3.86 - 12.91) 
        Median (95% CI)                                 32.02 (22.51, 49.31)    23.91 (18.26, 32.86)   20.77 (12.86, 26.02)
        75th percentile (95% CI)                        65.28 (49.31, 87.21)    41.98 (32.86, 53.41)   37.10 (25.75, 47.60)
        Min, max                                           (0.34, 155.50+)         (0.07, 154.09)         (0.63, 80.69)    

# a_kaplan_meier works inside analyze in table with custom arguments

    Code
      res
    Output
                                                                  ARM A                        ARM B                        ARM C           
      ——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Kaplan-Meier estimate of time to event (months)                                                                                       
              Median (90% CI)                           32.0210 (25.5571, 49.3092)   23.9114 (18.8617, 32.1479)   20.7730 (12.9541, 26.0233)
        Min and Max                                          (0.34, 155.50+)               (0.07, 154.09)               (0.63, 80.69)       

