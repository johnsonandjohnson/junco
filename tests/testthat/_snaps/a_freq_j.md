# a_freq_j_with_exclude allows to exclude row split levels from the analysis

    Code
      result
    Output
                          A              B     
      —————————————————————————————————————————
      Baseline                                 
        Response A   6/50 (12.0%)   4/50 (8.0%)
        Response B   6/50 (12.0%)   4/50 (8.0%)

# a_freq_j in specific situation error for not passing alt_counts_df

    Code
      result
    Output
                           Active Study Agent                                  Risk Difference (%) (95% CI)             
                       A: Drug X    C: Combination   B: Placebo   A: Drug X vs B: Placebo   C: Combination vs B: Placebo
                        (N=134)        (N=132)        (N=134)             (N=134)                     (N=132)           
      ——————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      A                                                                                                                 
        COMPLETED      22 (16.4%)     22 (16.7%)     23 (17.2%)      -0.7 (-9.7, 8.2)             -0.5 (-9.5, 8.5)      
        DISCONTINUED   11 (8.2%)      13 (9.8%)      13 (9.7%)       -1.5 (-8.3, 5.3)             0.1 (-7.0, 7.3)       
        ONGOING         5 (3.7%)       5 (3.8%)       8 (6.0%)       -2.2 (-7.4, 2.9)             -2.2 (-7.3, 3.0)      
      B                                                                                                                 
        COMPLETED      23 (17.2%)     23 (17.4%)     19 (14.2%)      3.0 (-5.7, 11.7)             3.2 (-5.5, 12.0)      
        DISCONTINUED   12 (9.0%)      14 (10.6%)     18 (13.4%)      -4.5 (-12.0, 3.1)           -2.8 (-10.6, 5.0)      
        ONGOING        12 (9.0%)       6 (4.5%)       8 (6.0%)        3.0 (-3.3, 9.3)             -1.4 (-6.8, 3.9)      
      C                                                                                                                 
        COMPLETED      24 (17.9%)     27 (20.5%)     27 (20.1%)      -2.2 (-11.6, 7.2)            0.3 (-9.4, 10.0)      
        DISCONTINUED   15 (11.2%)     12 (9.1%)      12 (9.0%)        2.2 (-5.0, 9.4)             0.1 (-6.8, 7.0)       
        ONGOING        10 (7.5%)      10 (7.6%)       6 (4.5%)        3.0 (-2.7, 8.6)             3.1 (-2.6, 8.8)       

