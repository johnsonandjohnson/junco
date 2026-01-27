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
        ), label = "Geometric Mean (95% CI)"), n = structure(50L, label = "n"), 
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
        Min, max                                                        1.00, 1.90       3.00, 5.10          4.50, 6.90    
        25% and 75%-ile                                                 1.40, 1.60       4.00, 4.60          5.10, 5.90    
        Difference in Adjusted Means (95% CI)                                         2.80 (2.63, 2.97)   4.09 (3.92, 4.26)
          p-value                                                                          <0.001              <0.001      
      Adjusted comparison (covariates: Sepal.Length and Sepal.Width)                                                       
        Difference in Adjusted Means (95% CI)                                         2.17 (1.96, 2.38)   3.05 (2.81, 3.29)
          p-value                                                                          <0.001              <0.001      

