# s_lsmeans works as expected with MMRM fit when not in reference column

    structure(0.00988685406692248, label = "2-sided p-value")

---

    structure(0.995056572966539, label = "1-sided p-value (less)")

---

    structure(0.00494342703346124, label = "1-sided p-value (greater)")

# summarize_lsmeans can show two- and one-sided p-values correctly

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

