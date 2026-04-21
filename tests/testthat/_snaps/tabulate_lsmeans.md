# tidy.tern_model works as expected

    structure(list(AVISIT = structure(c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 
    4L), levels = c("VIS1", "VIS2", "VIS3", "VIS4"), class = "factor"), 
        ARMCD = structure(c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L), levels = c("PBO", 
        "TRT"), class = "factor"), estimate_est = c(33.3318614835376, 
        37.1060915269439, 38.1714499381938, 41.9037537313572, 43.6739743667834, 
        46.7545190401784, 48.3857597462262, 52.7842168221017), se_est = c(0.755397358574953, 
        0.762587547080332, 0.611731946791914, 0.602348736962562, 
        0.461762123224088, 0.508631850290062, 1.18864953111098, 1.18775955682986
        ), df_est = c(148.14570349039, 143.176472203764, 147.032983242148, 
        143.484428761376, 129.802652878598, 130.134118216574, 134.08143506885, 
        132.624447273527), lower_cl_est = c(31.8391158733333, 35.5987065269252, 
        36.9625271395619, 40.7131299849693, 42.7604201819685, 45.7482614966202, 
        46.0348310962075, 50.4348133502877), upper_cl_est = c(34.8246070937418, 
        38.6134765269625, 39.3803727368256, 43.094377477745, 44.5875285515982, 
        47.7607765837365, 50.7366883962449, 55.1336202939156), n = c(68L, 
        66L, 69L, 71L, 71L, 58L, 67L, 67L), estimate_contr = c(NA, 
        3.77423004340628, NA, 3.73230379316339, NA, 3.080544673395, 
        NA, 4.39845707587548), se_contr = c(NA, 1.07414697568455, 
        NA, 0.858856403360872, NA, 0.689624517770755, NA, 1.68054526803966
        ), df_contr = c(NA, 145.551961966944, NA, 145.277128990238, 
        NA, 130.927988753988, NA, 133.388672141441), lower_cl_contr = c(NA, 
        1.65128972597106, NA, 2.0348360895312, NA, 1.71629587995375, 
        NA, 1.07449255230097), upper_cl_contr = c(NA, 5.8971703608415, 
        NA, 5.42977149679559, NA, 4.44479346683625, NA, 7.72242159944999
        ), t_stat = c(NA, 3.51369982771769, NA, 4.34566684088069, 
        NA, 4.46698833062521, NA, 2.61727973624075), p_value = c(NA, 
        0.000589148053067509, NA, 2.59446167719891e-05, NA, 1.6970468440069e-05, 
        NA, 0.00988685406692296), p_value_less = c(NA, 0.999705425973466, 
        NA, 0.999987027691614, NA, 0.99999151476578, NA, 0.995056572966539
        ), p_value_greater = c(NA, 0.000294574026533754, NA, 1.29723083859946e-05, 
        NA, 8.48523422003451e-06, NA, 0.00494342703346148), relative_reduc = c(NA, 
        -0.113231901112704, NA, -0.0977773649994079, NA, -0.0705350204111019, 
        NA, -0.0909039580848689), conf_level = c(0.95, 0.95, 0.95, 
        0.95, 0.95, 0.95, 0.95, 0.95), mse = c(40.5536637763126, 
        40.5536637763126, 26.571483280172, 26.571483280172, 14.8978516806773, 
        14.8978516806773, 95.556842009105, 95.556842009105), df = c(145.551961966944, 
        145.551961966944, 145.277128990238, 145.277128990238, 130.927988753988, 
        130.927988753988, 133.388672141441, 133.388672141441)), row.names = c(NA, 
    -8L), class = "data.frame")

# s_lsmeans works as expected with MMRM fit when not in reference column

    structure(0.00988685406692248, label = "2-sided p-value")

---

    structure(0.995056572966539, label = "1-sided p-value (less)")

---

    structure(0.00494342703346124, label = "1-sided p-value (greater)")

# a_lsmeans can show two- and one-sided p-values correctly

    Code
      result_two_sided
    Output
                                                            PBO                       TRT          
                                                          (N=105)                   (N=95)         
      —————————————————————————————————————————————————————————————————————————————————————————————
      VIS1                                                                                         
        n                                                   68                        66           
        Adjusted Mean (SE)                            33.332 (0.755)            37.106 (0.763)     
          Adjusted Mean 95% CI                       (31.839, 34.825)          (35.599, 38.613)    
          Adjusted Mean (95% CI)                  33.332 (31.839, 34.825)   37.106 (35.599, 38.613)
        Difference in Adjusted Means (SE)                                        3.774 (1.074)     
          Difference in Adjusted Means 95% CI                                   (1.651, 5.897)     
          Difference in Adjusted Means (95% CI)                              3.774 (1.651, 5.897)  
          Relative Reduction (%)                                                    -11.3%         
          p-value                                                                   <0.001         
      VIS2                                                                                         
        n                                                   69                        71           
        Adjusted Mean (SE)                            38.171 (0.612)            41.904 (0.602)     
          Adjusted Mean 95% CI                       (36.963, 39.380)          (40.713, 43.094)    
          Adjusted Mean (95% CI)                  38.171 (36.963, 39.380)   41.904 (40.713, 43.094)
        Difference in Adjusted Means (SE)                                        3.732 (0.859)     
          Difference in Adjusted Means 95% CI                                   (2.035, 5.430)     
          Difference in Adjusted Means (95% CI)                              3.732 (2.035, 5.430)  
          Relative Reduction (%)                                                     -9.8%         
          p-value                                                                   <0.001         
      VIS3                                                                                         
        n                                                   71                        58           
        Adjusted Mean (SE)                            43.674 (0.462)            46.755 (0.509)     
          Adjusted Mean 95% CI                       (42.760, 44.588)          (45.748, 47.761)    
          Adjusted Mean (95% CI)                  43.674 (42.760, 44.588)   46.755 (45.748, 47.761)
        Difference in Adjusted Means (SE)                                        3.081 (0.690)     
          Difference in Adjusted Means 95% CI                                   (1.716, 4.445)     
          Difference in Adjusted Means (95% CI)                              3.081 (1.716, 4.445)  
          Relative Reduction (%)                                                     -7.1%         
          p-value                                                                   <0.001         
      VIS4                                                                                         
        n                                                   67                        67           
        Adjusted Mean (SE)                            48.386 (1.189)            52.784 (1.188)     
          Adjusted Mean 95% CI                       (46.035, 50.737)          (50.435, 55.134)    
          Adjusted Mean (95% CI)                  48.386 (46.035, 50.737)   52.784 (50.435, 55.134)
        Difference in Adjusted Means (SE)                                        4.398 (1.681)     
          Difference in Adjusted Means 95% CI                                   (1.074, 7.722)     
          Difference in Adjusted Means (95% CI)                              4.398 (1.074, 7.722)  
          Relative Reduction (%)                                                     -9.1%         
          p-value                                                                    0.010         
      VIS1+2                                                                                       
        n                                                   91                        87           
        Adjusted Mean (SE)                            35.752 (0.558)            39.505 (0.561)     
          Adjusted Mean 95% CI                       (34.650, 36.853)          (38.399, 40.611)    
          Adjusted Mean (95% CI)                  35.752 (34.650, 36.853)   39.505 (38.399, 40.611)
        Difference in Adjusted Means (SE)                                        3.753 (0.792)     
          Difference in Adjusted Means 95% CI                                   (2.191, 5.316)     
          Difference in Adjusted Means (95% CI)                              3.753 (2.191, 5.316)  
          Relative Reduction (%)                                                    -10.5%         
          p-value                                                                   <0.001         

---

    Code
      result_one_sided_less
    Output
                                                            PBO                       TRT          
                                                          (N=105)                   (N=95)         
      —————————————————————————————————————————————————————————————————————————————————————————————
      VIS1                                                                                         
        n                                                   68                        66           
        Adjusted Mean (SE)                            33.332 (0.755)            37.106 (0.763)     
          Adjusted Mean 95% CI                       (31.839, 34.825)          (35.599, 38.613)    
          Adjusted Mean (95% CI)                  33.332 (31.839, 34.825)   37.106 (35.599, 38.613)
        Difference in Adjusted Means (SE)                                        3.774 (1.074)     
          Difference in Adjusted Means 95% CI                                   (1.651, 5.897)     
          Difference in Adjusted Means (95% CI)                              3.774 (1.651, 5.897)  
          Relative Reduction (%)                                                    -11.3%         
          p-value                                                                   >0.999         
      VIS2                                                                                         
        n                                                   69                        71           
        Adjusted Mean (SE)                            38.171 (0.612)            41.904 (0.602)     
          Adjusted Mean 95% CI                       (36.963, 39.380)          (40.713, 43.094)    
          Adjusted Mean (95% CI)                  38.171 (36.963, 39.380)   41.904 (40.713, 43.094)
        Difference in Adjusted Means (SE)                                        3.732 (0.859)     
          Difference in Adjusted Means 95% CI                                   (2.035, 5.430)     
          Difference in Adjusted Means (95% CI)                              3.732 (2.035, 5.430)  
          Relative Reduction (%)                                                     -9.8%         
          p-value                                                                   >0.999         
      VIS3                                                                                         
        n                                                   71                        58           
        Adjusted Mean (SE)                            43.674 (0.462)            46.755 (0.509)     
          Adjusted Mean 95% CI                       (42.760, 44.588)          (45.748, 47.761)    
          Adjusted Mean (95% CI)                  43.674 (42.760, 44.588)   46.755 (45.748, 47.761)
        Difference in Adjusted Means (SE)                                        3.081 (0.690)     
          Difference in Adjusted Means 95% CI                                   (1.716, 4.445)     
          Difference in Adjusted Means (95% CI)                              3.081 (1.716, 4.445)  
          Relative Reduction (%)                                                     -7.1%         
          p-value                                                                   >0.999         
      VIS4                                                                                         
        n                                                   67                        67           
        Adjusted Mean (SE)                            48.386 (1.189)            52.784 (1.188)     
          Adjusted Mean 95% CI                       (46.035, 50.737)          (50.435, 55.134)    
          Adjusted Mean (95% CI)                  48.386 (46.035, 50.737)   52.784 (50.435, 55.134)
        Difference in Adjusted Means (SE)                                        4.398 (1.681)     
          Difference in Adjusted Means 95% CI                                   (1.074, 7.722)     
          Difference in Adjusted Means (95% CI)                              4.398 (1.074, 7.722)  
          Relative Reduction (%)                                                     -9.1%         
          p-value                                                                    0.995         
      VIS1+2                                                                                       
        n                                                   91                        87           
        Adjusted Mean (SE)                            35.752 (0.558)            39.505 (0.561)     
          Adjusted Mean 95% CI                       (34.650, 36.853)          (38.399, 40.611)    
          Adjusted Mean (95% CI)                  35.752 (34.650, 36.853)   39.505 (38.399, 40.611)
        Difference in Adjusted Means (SE)                                        3.753 (0.792)     
          Difference in Adjusted Means 95% CI                                   (2.191, 5.316)     
          Difference in Adjusted Means (95% CI)                              3.753 (2.191, 5.316)  
          Relative Reduction (%)                                                    -10.5%         
          p-value                                                                   >0.999         

---

    Code
      result_one_sided_greater
    Output
                                                            PBO                       TRT          
                                                          (N=105)                   (N=95)         
      —————————————————————————————————————————————————————————————————————————————————————————————
      VIS1                                                                                         
        n                                                   68                        66           
        Adjusted Mean (SE)                            33.332 (0.755)            37.106 (0.763)     
          Adjusted Mean 95% CI                       (31.839, 34.825)          (35.599, 38.613)    
          Adjusted Mean (95% CI)                  33.332 (31.839, 34.825)   37.106 (35.599, 38.613)
        Difference in Adjusted Means (SE)                                        3.774 (1.074)     
          Difference in Adjusted Means 95% CI                                   (1.651, 5.897)     
          Difference in Adjusted Means (95% CI)                              3.774 (1.651, 5.897)  
          Relative Reduction (%)                                                    -11.3%         
          p-value                                                                   <0.001         
      VIS2                                                                                         
        n                                                   69                        71           
        Adjusted Mean (SE)                            38.171 (0.612)            41.904 (0.602)     
          Adjusted Mean 95% CI                       (36.963, 39.380)          (40.713, 43.094)    
          Adjusted Mean (95% CI)                  38.171 (36.963, 39.380)   41.904 (40.713, 43.094)
        Difference in Adjusted Means (SE)                                        3.732 (0.859)     
          Difference in Adjusted Means 95% CI                                   (2.035, 5.430)     
          Difference in Adjusted Means (95% CI)                              3.732 (2.035, 5.430)  
          Relative Reduction (%)                                                     -9.8%         
          p-value                                                                   <0.001         
      VIS3                                                                                         
        n                                                   71                        58           
        Adjusted Mean (SE)                            43.674 (0.462)            46.755 (0.509)     
          Adjusted Mean 95% CI                       (42.760, 44.588)          (45.748, 47.761)    
          Adjusted Mean (95% CI)                  43.674 (42.760, 44.588)   46.755 (45.748, 47.761)
        Difference in Adjusted Means (SE)                                        3.081 (0.690)     
          Difference in Adjusted Means 95% CI                                   (1.716, 4.445)     
          Difference in Adjusted Means (95% CI)                              3.081 (1.716, 4.445)  
          Relative Reduction (%)                                                     -7.1%         
          p-value                                                                   <0.001         
      VIS4                                                                                         
        n                                                   67                        67           
        Adjusted Mean (SE)                            48.386 (1.189)            52.784 (1.188)     
          Adjusted Mean 95% CI                       (46.035, 50.737)          (50.435, 55.134)    
          Adjusted Mean (95% CI)                  48.386 (46.035, 50.737)   52.784 (50.435, 55.134)
        Difference in Adjusted Means (SE)                                        4.398 (1.681)     
          Difference in Adjusted Means 95% CI                                   (1.074, 7.722)     
          Difference in Adjusted Means (95% CI)                              4.398 (1.074, 7.722)  
          Relative Reduction (%)                                                     -9.1%         
          p-value                                                                    0.005         
      VIS1+2                                                                                       
        n                                                   91                        87           
        Adjusted Mean (SE)                            35.752 (0.558)            39.505 (0.561)     
          Adjusted Mean 95% CI                       (34.650, 36.853)          (38.399, 40.611)    
          Adjusted Mean (95% CI)                  35.752 (34.650, 36.853)   39.505 (38.399, 40.611)
        Difference in Adjusted Means (SE)                                        3.753 (0.792)     
          Difference in Adjusted Means 95% CI                                   (2.191, 5.316)     
          Difference in Adjusted Means (95% CI)                              3.753 (2.191, 5.316)  
          Relative Reduction (%)                                                    -10.5%         
          p-value                                                                   <0.001         

