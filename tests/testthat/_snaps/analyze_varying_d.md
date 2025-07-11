# a_summary_d_j work with healthy input.

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
          row_name     formatted_cell indent_mod     row_label
      1       mean               0.13          0          Mean
      2    mean_sd       0.13 (0.781)          0     Mean (SD)
      3 mean_ci_3d 0.13 (-0.43, 0.69)          0 Mean (95% CI)

---

    Code
      result2
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
          row_name     formatted_cell indent_mod     row_label
      1       mean               0.13          0          Mean
      2    mean_sd       0.13 (0.781)          0     Mean (SD)
      3 mean_ci_3d 0.13 (-0.43, 0.69)          0 Mean (95% CI)

---

    Code
      result3
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
          row_name        formatted_cell indent_mod     row_label
      1       mean                 0.132          0          Mean
      2    mean_sd        0.132 (0.7806)          0     Mean (SD)
      3 mean_ci_3d 0.132 (-0.426, 0.691)          0 Mean (95% CI)

---

    Code
      result4
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
          row_name  formatted_cell indent_mod     row_label
      1       mean             0.1          0          Mean
      2    mean_sd           0 (1)          0     Mean (SD)
      3 mean_ci_3d 0.1 (-0.4, 0.7)          0 Mean (95% CI)

# a_summarize_ancova_d_j work with healthy input.

    Code
      res
    Output
                                                   setosa             versicolor             virginica     
      —————————————————————————————————————————————————————————————————————————————————————————————————————
      n                                              50                   50                    50         
      Mean (SD)                                 5.01 (0.352)         5.94 (0.516)          6.59 (0.636)    
      Median                                        5.00                 5.90                  6.50        
      Min, max                                    4.3 - 5.8            4.9 - 7.0             4.9 - 7.9     
      25% and 75%-ile                            4.80 - 5.20          5.60 - 6.30           6.20 - 6.90    
      Adjusted Mean (SE)                        6.15 (0.337)         5.72 (0.067)          5.41 (0.149)    
      Adjusted Mean (99% CI)                  6.15 (5.27, 7.03)    5.72 (5.54, 5.89)     5.41 (5.02, 5.79) 
      Difference in Adjusted Means (99% CI)                       -0.44 (-1.43, 0.55)   -0.75 (-1.89, 0.39)
        p-value                                                          0.250                 0.089       

---

    Code
      res2
    Output
                                                  setosa          versicolor         virginica    
      ————————————————————————————————————————————————————————————————————————————————————————————
      n                                             50                50                 50       
      Mean (SD)                                 5.0 (0.35)        5.9 (0.52)         6.6 (0.64)   
      Median                                       5.0               5.9                6.5       
      Min, max                                    4 - 6             5 - 7              5 - 8      
      25% and 75%-ile                           4.8 - 5.2         5.6 - 6.3          6.2 - 6.9    
      Adjusted Mean (SE)                        6.2 (0.34)        5.7 (0.07)         5.4 (0.15)   
      Adjusted Mean (99% CI)                  6.2 (5.3, 7.0)    5.7 (5.5, 5.9)     5.4 (5.0, 5.8) 
      Difference in Adjusted Means (99% CI)                    -0.4 (-1.4, 0.6)   -0.7 (-1.9, 0.4)
        p-value                                                     0.250              0.089      

---

    Code
      res3
    Output
                                                   setosa           versicolor         virginica    
      ——————————————————————————————————————————————————————————————————————————————————————————————
      n                                              50                 50                 50       
      Mean (SD)                                  5.0 (0.35)         5.9 (0.52)         6.6 (0.64)   
      Median                                        5.0                5.9                6.5       
      Min, max                                     4 - 6              5 - 7              5 - 8      
      25% and 75%-ile                            4.8 - 5.2          5.6 - 6.3          6.2 - 6.9    
      Adjusted Mean (SE)                         6.2 (0.34)         5.7 (0.07)         5.4 (0.15)   
      Adjusted Mean (99% CI)                  6.15 (5.27, 7.0)   5.72 (5.54, 5.9)   5.41 (5.02, 5.8)
      Difference in Adjusted Means (99% CI)                      -0.4 (-1.4, 0.6)   -0.7 (-1.9, 0.4)
        p-value                                                       0.250              0.089      

