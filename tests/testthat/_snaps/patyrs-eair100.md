# Check a_eair100_j with occ_var NULL and count_multiple_events

    Code
      tbl1
    Output
                                              Active Study Agent                                 Risk Difference (95% CI)            
                                          A: Drug X    C: Combination   B: Placebo   A: Drug X vs Placebo   C: Combination vs Placebo
      ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      dcd A.1.1.1.1 Person years           26831.00       26763.00       26606.00                                                    
      dcd A.1.1.1.1 n (eair per 100 SY)   50 (0.186)     63 (0.235)     45 (0.169)    0.02 (-0.05, 0.09)       0.07 (-0.01, 0.14)    
      dcd A.1.1.1.2 Person years           26831.00       26763.00       26606.00                                                    
      dcd A.1.1.1.2 n (eair per 100 SY)   48 (0.179)     50 (0.187)     48 (0.180)    0.00 (-0.07, 0.07)       0.01 (-0.07, 0.08)    

---

    Code
      tbl2
    Output
                                              Active Study Agent                                 Risk Difference (95% CI)            
                                          A: Drug X    C: Combination   B: Placebo   A: Drug X vs Placebo   C: Combination vs Placebo
      ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      dcd A.1.1.1.1 Person years           26831.00       26763.00       26606.00                                                    
      dcd A.1.1.1.1 n (eair per 100 SY)   64 (0.239)     88 (0.329)     62 (0.233)    0.01 (-0.08, 0.09)        0.10 (0.01, 0.19)    
      dcd A.1.1.1.2 Person years           26831.00       26763.00       26606.00                                                    
      dcd A.1.1.1.2 n (eair per 100 SY)   68 (0.253)     72 (0.269)     68 (0.256)    0.00 (-0.09, 0.08)       0.01 (-0.07, 0.10)    

# dynamic labels for a_eair100_j

    Code
      tbl1
    Output
                                                                           Active Study Agent                                Risk Difference (95% CI)            
                                                                       A: Drug X   C: Combination   B: Placebo   A: Drug X vs Placebo   C: Combination vs Placebo
      ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      dcd A.1.1.1.1                                                                                                                                              
        Person years                                                   15157.79       13727.02       17257.90                                                    
        Number of subjects with event (eair (per 1000 person years))   50 (3.3)       63 (4.6)       45 (2.6)     0.69 (-0.50, 1.88)        1.98 (0.62, 3.35)    
        eair (per 1000 person years)                                      3.3           4.6            2.6        0.69 (-0.50, 1.88)        1.98 (0.62, 3.35)    
      dcd A.1.1.1.2                                                                                                                                              
        Person years                                                   17961.38       16745.97       16991.93                                                    
        Number of subjects with event (eair (per 1000 person years))   48 (2.7)       50 (3.0)       48 (2.8)    -0.15 (-1.25, 0.95)       0.16 (-0.99, 1.31)    
        eair (per 1000 person years)                                      2.7           3.0            2.8       -0.15 (-1.25, 0.95)       0.16 (-0.99, 1.31)    
      dcd B.1.1.1.1                                                                                                                                              
        Person years                                                   16296.87       18185.52       16145.80                                                    
        Number of subjects with event (eair (per 1000 person years))   47 (2.9)       43 (2.4)       49 (3.0)    -0.15 (-1.33, 1.03)       -0.67 (-1.78, 0.43)   
        eair (per 1000 person years)                                      2.9           2.4            3.0       -0.15 (-1.33, 1.03)       -0.67 (-1.78, 0.43)   

---

    Code
      tbl2
    Output
                                                                          Active Study Agent                                Risk Difference (95% CI)            
                                                                      A: Drug X   C: Combination   B: Placebo   A: Drug X vs Placebo   C: Combination vs Placebo
      ——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      dcd A.1.1.1.1                                                                                                                                             
        Subject years of exposure                                     15157.79       13727.02       17257.90                                                    
        Number of subjects with event (eair (per 100 person years))   50 (0.3)       63 (0.5)       45 (0.3)     0.07 (-0.05, 0.19)        0.20 (0.06, 0.33)    
        eair (per 100 person years)                                      0.3           0.5            0.3        0.07 (-0.05, 0.19)        0.20 (0.06, 0.33)    
      dcd A.1.1.1.2                                                                                                                                             
        Subject years of exposure                                     17961.38       16745.97       16991.93                                                    
        Number of subjects with event (eair (per 100 person years))   48 (0.3)       50 (0.3)       48 (0.3)    -0.02 (-0.13, 0.09)       0.02 (-0.10, 0.13)    
        eair (per 100 person years)                                      0.3           0.3            0.3       -0.02 (-0.13, 0.09)       0.02 (-0.10, 0.13)    
      dcd B.1.1.1.1                                                                                                                                             
        Subject years of exposure                                     16296.87       18185.52       16145.80                                                    
        Number of subjects with event (eair (per 100 person years))   47 (0.3)       43 (0.2)       49 (0.3)    -0.02 (-0.13, 0.10)       -0.07 (-0.18, 0.04)   
        eair (per 100 person years)                                      0.3           0.2            0.3       -0.02 (-0.13, 0.10)       -0.07 (-0.18, 0.04)   

---

    Code
      tbl3
    Output
                                                                                    Active Study Agent                                Risk Difference (95% CI)            
                                                                                A: Drug X   C: Combination   B: Placebo   A: Drug X vs Placebo   C: Combination vs Placebo
      ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      dcd A.1.1.1.1                                                                                                                                                       
        Subject years of exposure                                               15157.79       13727.02       17257.90                                                    
        Number of subjects with event (incidence rate (per 100 person years))   50 (0.3)       63 (0.5)       45 (0.3)     0.07 (-0.05, 0.19)        0.20 (0.06, 0.33)    
        eair (per 100 person years)                                                0.3           0.5            0.3        0.07 (-0.05, 0.19)        0.20 (0.06, 0.33)    
      dcd A.1.1.1.2                                                                                                                                                       
        Subject years of exposure                                               17961.38       16745.97       16991.93                                                    
        Number of subjects with event (incidence rate (per 100 person years))   48 (0.3)       50 (0.3)       48 (0.3)    -0.02 (-0.13, 0.09)       0.02 (-0.10, 0.13)    
        eair (per 100 person years)                                                0.3           0.3            0.3       -0.02 (-0.13, 0.09)       0.02 (-0.10, 0.13)    
      dcd B.1.1.1.1                                                                                                                                                       
        Subject years of exposure                                               16296.87       18185.52       16145.80                                                    
        Number of subjects with event (incidence rate (per 100 person years))   47 (0.3)       43 (0.2)       49 (0.3)    -0.02 (-0.13, 0.10)       -0.07 (-0.18, 0.04)   
        eair (per 100 person years)                                                0.3           0.2            0.3       -0.02 (-0.13, 0.10)       -0.07 (-0.18, 0.04)   

