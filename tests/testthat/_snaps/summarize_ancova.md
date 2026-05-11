# s_summarize_ancova works as expected

    list(n = c(n = 50L), sum = c(sum = 277.6), mean = c(mean = 5.552), 
        sd = c(sd = 0.551894695663983), se = c(se = 0.0780496963609777), 
        mean_sd = c(mean = 5.552, sd = 0.551894695663983), mean_se = c(mean = 5.552, 
        se = 0.0780496963609777), mean_ci = structure(c(mean_ci_lwr = 5.39515326292752, 
        mean_ci_upr = 5.70884673707248), label = "Mean 95% CI"), 
        mean_sei = structure(c(mean_sei_lwr = 5.47395030363902, mean_sei_upr = 5.63004969636098
        ), label = "Mean -/+ 1xSE"), mean_sdi = structure(c(mean_sdi_lwr = 5.00010530433602, 
        mean_sdi_upr = 6.10389469566398), label = "Mean -/+ 1xSD"), 
        mean_ci_3d = structure(c(mean = 5.552, mean_ci_lwr = 5.39515326292752, 
        mean_ci_upr = 5.70884673707248), label = "Mean (95% CI)"), 
        mean_pval = structure(c(p_value = 4.09323138278747e-51), label = "Mean p-value (H0: mean = 0)"), 
        median = c(median = 5.55), mad = c(mad = 0), median_ci = structure(c(median_ci_lwr = 5.2, 
        median_ci_upr = 5.7), conf_level = 0.967160862435731, label = "Median 95% CI"), 
        median_ci_3d = structure(c(median = 5.55, median_ci_lwr = 5.2, 
        median_ci_upr = 5.7), label = "Median (95% CI)"), quantiles = structure(c(quantile_0.25 = 5.1, 
        quantile_0.75 = 5.9), label = "25% and 75%-ile"), iqr = c(iqr = 0.800000000000001), 
        range = c(min = 4.5, max = 6.9), min = c(min = 4.5), max = c(max = 6.9), 
        median_range = structure(c(median = 5.55, min = 4.5, max = 6.9
        ), label = "Median (Min - Max)"), cv = c(cv = 9.9404664204608), 
        geom_mean = c(geom_mean = 5.52578887426088), geom_sd = c(geom_sd = 1.10272370969365), 
        geom_mean_sd = c(geom_mean = 5.52578887426088, geom_sd = 1.10272370969365
        ), geom_mean_ci = structure(c(mean_ci_lwr = 5.37434301357803, 
        mean_ci_upr = 5.6815023912247), label = "Geometric Mean 95% CI"), 
        geom_cv = c(geom_cv = 9.80174252966596), geom_mean_ci_3d = structure(c(geom_mean = 5.52578887426088, 
        mean_ci_lwr = 5.37434301357803, mean_ci_upr = 5.6815023912247
        ), label = "Geometric Mean (95% CI)"), n_fit = structure(50L, label = "n"), 
        lsmean = structure(5.07100244458199, label = "Adjusted Mean"), 
        lsmean_se = structure(c(5.07100244458199, 0.0604121292091169
        ), label = "Adjusted Mean (SE)"), lsmean_ci = structure(c(5.07100244458199, 
        4.95159333631352, 5.19041155285046), label = "Adjusted Mean (95% CI)"), 
        lsmean_diff = structure(3.06260323315316, label = "Difference in Adjusted Means"), 
        lsmean_diff_ci = structure(c(2.80852642307127, 3.31668004323504
        ), label = "Difference in Adjusted Means 95% CI"), lsmean_diffci = structure(c(3.06260323315316, 
        2.80852642307127, 3.31668004323504), label = "Difference in Adjusted Means (95% CI)"), 
        pval = structure(8.1172834382675e-52, label = "p-value"))

# a_summarize_ancova_j  works as expected in table layout

    Code
      result
    Output
                                                                          setosa         versicolor           virginica    
                                                                          (N=50)           (N=50)              (N=50)      
      —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Unadjusted comparison                                                                                                
        n                                                                   50               50                  50        
        Mean (SD)                                                      1.46 (0.174)     4.26 (0.470)        5.55 (0.552)   
        Median                                                             1.50             4.35                5.55       
        Min, max                                                         1.0, 1.9         3.0, 5.1            4.5, 6.9     
        25% and 75%-ile                                                 1.40, 1.60       4.00, 4.60          5.10, 5.90    
        Difference in Adjusted Means (95% CI)                                         2.80 (2.63, 2.97)   4.09 (3.92, 4.26)
          p-value                                                                          <0.001              <0.001      
      Adjusted comparison (covariates: Sepal.Length and Sepal.Width)                                                       
        Difference in Adjusted Means (95% CI)                                         2.17 (1.96, 2.38)   3.05 (2.81, 3.29)
          p-value                                                                          <0.001              <0.001      

# a_summarize_ancova_j (s_ancova_j) as expected in combined column for model without interaction

    Code
      result
    Output
                                                       setosa             versicolor           virginica       Combined: setosa + virginica
                                                       (N=44)               (N=29)              (N=49)                    (N=93)           
      —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Adjusted comparison (covariates Color)                                                                                               
        n                                                44                   29                  49                        93             
        Adjusted Mean (95% CI)                   5.02 (4.85, 5.19)     6.10 (5.88, 6.31)   6.61 (6.45, 6.78)        5.82 (5.69, 5.95)      
        Difference in Adjusted Means (95% CI)   -1.08 (-1.33, -0.82)                       0.52 (0.27, 0.76)       -0.28 (-0.51, -0.06)    

---

    Code
      result_b
    Output
                                                       setosa             versicolor           virginica       Combined: setosa + virginica
                                                       (N=44)               (N=29)              (N=49)                    (N=93)           
      —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Adjusted comparison (covariates Color)                                                                                               
        n                                                44                   29                  49                        93             
        Adjusted Mean (95% CI)                   5.02 (4.85, 5.19)     6.10 (5.88, 6.31)   6.61 (6.45, 6.78)        5.86 (5.73, 5.99)      
        Difference in Adjusted Means (95% CI)   -1.08 (-1.33, -0.82)                       0.52 (0.27, 0.76)       -0.24 (-0.46, -0.01)    

---

    Code
      result_d
    Output
                                                       setosa             versicolor           virginica       Combined: setosa + virginica
                                                       (N=44)               (N=29)              (N=49)                    (N=93)           
      —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Adjusted comparison (covariates Color)                                                                                               
        n                                                44                   29                  49                        93             
        Adjusted Mean (95% CI)                   5.01 (4.85, 5.17)     6.09 (5.89, 6.28)   6.60 (6.45, 6.75)        5.80 (5.69, 5.91)      
        Difference in Adjusted Means (95% CI)   -1.08 (-1.33, -0.82)                       0.52 (0.27, 0.76)       -0.28 (-0.51, -0.06)    

# a_summarize_ancova_j combined column and interaction, diff versions for weights_combo

    Code
      result
    Output
                                                             setosa             versicolor           virginica       Combined: setosa + virginica
                                                             (N=44)               (N=29)              (N=49)                    (N=93)           
      ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Adjusted comparison (covariates Color - red)                                                                                               
        n                                                      34                   25                  38                        72             
        Adjusted Mean (95% CI)                         4.97 (4.79, 5.15)     6.11 (5.90, 6.32)   6.60 (6.43, 6.77)        5.78 (5.66, 5.91)      
        Difference in Adjusted Means (95% CI)         -1.14 (-1.42, -0.87)                       0.49 (0.21, 0.76)       -0.33 (-0.58, -0.08)    
      Adjusted comparison (covariates Color - blue)                                                                                              
        n                                                      10                    4                  11                        21             
        Adjusted Mean (95% CI)                         5.15 (4.81, 5.49)     5.90 (5.37, 6.43)   6.62 (6.30, 6.94)        5.88 (5.65, 6.12)      
        Difference in Adjusted Means (95% CI)         -0.75 (-1.38, -0.12)                       0.72 (0.10, 1.34)       -0.02 (-0.59, 0.56)     

---

    Code
      result_b
    Output
                                                             setosa             versicolor           virginica       Combined: setosa + virginica
                                                             (N=44)               (N=29)              (N=49)                    (N=93)           
      ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Adjusted comparison (covariates Color - red)                                                                                               
        n                                                      34                   25                  38                        72             
        Adjusted Mean (95% CI)                         4.97 (4.79, 5.15)     6.11 (5.90, 6.32)   6.60 (6.43, 6.77)        5.83 (5.70, 5.95)      
        Difference in Adjusted Means (95% CI)         -1.14 (-1.42, -0.87)                       0.49 (0.21, 0.76)       -0.28 (-0.53, -0.04)    
      Adjusted comparison (covariates Color - blue)                                                                                              
        n                                                      10                    4                  11                        21             
        Adjusted Mean (95% CI)                         5.15 (4.81, 5.49)     5.90 (5.37, 6.43)   6.62 (6.30, 6.94)        5.92 (5.69, 6.15)      
        Difference in Adjusted Means (95% CI)         -0.75 (-1.38, -0.12)                       0.72 (0.10, 1.34)        0.02 (-0.56, 0.60)     

---

    Code
      result_c
    Output
                                                             setosa             versicolor           virginica       Combined: setosa + virginica
                                                             (N=44)               (N=29)              (N=49)                    (N=93)           
      ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Adjusted comparison (covariates Color - red)                                                                                               
        n                                                      34                   25                  38                        72             
        Adjusted Mean (95% CI)                         4.97 (4.79, 5.15)     6.11 (5.90, 6.32)   6.60 (6.43, 6.77)        5.83 (5.70, 5.95)      
        Difference in Adjusted Means (95% CI)         -1.14 (-1.42, -0.87)                       0.49 (0.21, 0.76)       -0.29 (-0.53, -0.04)    
      Adjusted comparison (covariates Color - blue)                                                                                              
        n                                                      10                    4                  11                        21             
        Adjusted Mean (95% CI)                         5.15 (4.81, 5.49)     5.90 (5.37, 6.43)   6.62 (6.30, 6.94)        5.92 (5.69, 6.15)      
        Difference in Adjusted Means (95% CI)         -0.75 (-1.38, -0.12)                       0.72 (0.10, 1.34)        0.02 (-0.55, 0.60)     

# a_summarize_ancova_j with sparse data

    Code
      result
    Output
                                                                            setosa         versicolor       virginica    
                                                                            (N=44)           (N=0)           (N=49)      
      ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Adjusted comparison (covariates: Sepal.Length and Sepal.Width)                                                     
        Adjusted Mean (95% CI)                                         2.08 (1.92, 2.23)       NA       5.00 (4.86, 5.14)
        Difference in Adjusted Means (95% CI)                                                  NA       2.92 (2.65, 3.20)
          p-value                                                                              NA            <0.001      

# a_summarize_ancova_j with no data

    Code
      result
    Output
                                                                       setosa   versicolor   virginica
                                                                       (N=0)      (N=0)        (N=0)  
      ————————————————————————————————————————————————————————————————————————————————————————————————
      Adjusted comparison (covariates: Sepal.Length and Sepal.Width)                                  
        lsmean_ci                                                                                     
        lsmean_diffci                                                                                 
          p-value                                                                                     

# a_summarize_ancova_j with no data in reference group

    Code
      result
    Output
                                                                       setosa      versicolor           virginica    
                                                                       (N=0)         (N=29)              (N=49)      
      ———————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Adjusted comparison (covariates: Sepal.Length and Sepal.Width)                                                 
        Adjusted Mean (95% CI)                                           NA     4.60 (4.49, 4.71)   5.42 (5.34, 5.51)
        Difference in Adjusted Means (95% CI)                            NA            NA                  NA        
          p-value                                                        NA            NA                  NA        

# a_summarize_ancova_j with multiple combined columns

    Code
      result
    Output
                                                      Placebo          Xanomeline Low Dose   Xanomeline Medium Dose   Xanomeline High Dose   Combined: Low + Medium   Combined: Medium + High   Combined: Low + Medium + High
                                                       (N=86)                (N=96)                  (N=86)                  (N=72)                 (N=182)                   (N=158)                      (N=254)           
      ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Adjusted comparison (covariates SEX)                                                                                                                                                                                   
        n                                                83                    94                      83                      72                     177                       155                          249             
        Adjusted Mean (95% CI)                  -2.62 (-4.43, -0.81)   -0.11 (-1.81, 1.59)    -0.75 (-2.56, 1.06)     -1.91 (-3.86, 0.04)     -0.41 (-1.65, 0.83)       -1.29 (-2.61, 0.04)          -0.84 (-1.89, 0.20)     
        Difference in Adjusted Means (95% CI)                           2.52 (0.03, 5.00)      1.87 (-0.69, 4.43)      0.71 (-1.95, 3.37)      2.21 (0.02, 4.41)        1.33 (-0.91, 3.58)           1.78 (-0.31, 3.87)      

