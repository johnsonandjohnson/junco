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

# tidy.tern_model works as expected with subgroup variable

    structure(list(SEX = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 
    1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), levels = c("Male", "Female"
    ), class = "factor"), AVISIT = structure(c(1L, 1L, 2L, 2L, 3L, 
    3L, 4L, 4L, 1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L), levels = c("VIS1", 
    "VIS2", "VIS3", "VIS4"), class = "factor"), ARMCD = structure(c(1L, 
    2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L), levels = c("PBO", 
    "TRT"), class = "factor"), estimate_est = c(31.2445208618236, 
    37.2582235045247, 38.2090543923776, 43.4665929721459, 42.8841887499597, 
    46.8607086448637, 47.5112487031101, 52.5706630938507, 35.0456010806879, 
    37.103353261737, 38.044469181297, 40.7883014210441, 44.4986948610809, 
    46.7916641949682, 49.1765918759859, 52.9646235701263), se_est = c(1.11612277532596, 
    1.08947311361935, 0.840708497195034, 0.898429318924619, 0.652313407562119, 
    0.799453033939912, 1.70712181713803, 1.75895802037051, 1.00372985682478, 
    1.04657401253981, 0.846726263659022, 0.777431927626908, 0.646632714553686, 
    0.648933936499828, 1.6796401142128, 1.63164569407512), df_est = c(146.129626104341, 
    141.208571340949, 146.628954138649, 146.674187907229, 127.24990346937, 
    126.691178691808, 131.702978301021, 130.463959070008, 143.901784587852, 
    142.772227957879, 150.370223420434, 143.177624671285, 128.473074314095, 
    126.43441550927, 131.743906080154, 130.500644327669), lower_cl_est = c(29.0386928164405, 
    35.1044373353696, 36.5475833634207, 41.6910542930513, 41.5934026184615, 
    45.278698272898, 44.1343225612834, 49.0908912672066, 33.0616421987336, 
    35.0345704124872, 36.3714517401995, 39.2515740638601, 43.2192665153779, 
    45.5074857912252, 45.8540379634053, 49.7367240346725), upper_cl_est = c(33.4503489072067, 
    39.4120096736799, 39.8705254213345, 45.2421316512405, 44.1749748814579, 
    48.4427190168293, 50.8881748449368, 56.0504349204948, 37.0295599626423, 
    39.1721361109868, 39.7174866223945, 42.3250287782281, 45.7781232067838, 
    48.0758425987113, 52.4991457885664, 56.19252310558), n = c(30L, 
    32L, 35L, 30L, 35L, 23L, 33L, 31L, 38L, 34L, 34L, 41L, 36L, 35L, 
    34L, 36L), estimate_contr = c(NA, 6.01370264270112, NA, 5.25753857976826, 
    NA, 3.97651989490401, NA, 5.05941439074067, NA, 2.0577521810491, 
    NA, 2.74383223974712, NA, 2.29296933388735, NA, 3.78803169414043
    ), se_contr = c(NA, 1.56066344526685, NA, 1.23237883979642, NA, 
    1.0363887973506, NA, 2.4510202090691, NA, 1.45105885996816, NA, 
    1.14913014494379, NA, 0.918030233171406, NA, 2.34178670449764
    ), df_contr = c(NA, 144.333433354449, NA, 147.409615042012, NA, 
    128.125625220836, NA, 130.971564726009, NA, 143.28252402549, 
    NA, 146.842045593752, NA, 127.869502937272, NA, 131.168646929236
    ), lower_cl_contr = c(NA, 2.92899457574362, NA, 2.82212663289411, 
    NA, 1.92586677909565, NA, 0.210701939314811, NA, -0.810496284375205, 
    NA, 0.472862697050763, NA, 0.476471982010903, NA, -0.844525491120697
    ), upper_cl_contr = c(NA, 9.09841070965862, NA, 7.69295052664241, 
    NA, 6.02717301071238, NA, 9.90812684216652, NA, 4.9260006464734, 
    NA, 5.01480178244347, NA, 4.1094666857638, NA, 8.42058887940156
    ), t_stat = c(NA, 3.85329883963089, NA, 4.26617076664247, NA, 
    3.83689972823858, NA, 2.06420753775108, NA, 1.4181038673333, 
    NA, 2.38774715972778, NA, 2.49770568662659, NA, 1.61758186040818
    ), p_value = c(NA, 0.000174791119628091, NA, 3.53916094554753e-05, 
    NA, 0.000194662964556881, NA, 0.0409719587023463, NA, 0.158331677349158, 
    NA, 0.0182240057732165, NA, 0.0137695684396458, NA, 0.108155064233287
    ), p_value_less = c(NA, 0.999912604440186, NA, 0.999982304195272, 
    NA, 0.999902668517722, NA, 0.979514020648827, NA, 0.920834161325421, 
    NA, 0.990887997113392, NA, 0.993115215780177, NA, 0.945922467883356
    ), p_value_greater = c(NA, 8.73955598140455e-05, NA, 1.76958047277377e-05, 
    NA, 9.73314822784405e-05, NA, 0.0204859793511732, NA, 0.0791658386745792, 
    NA, 0.00911200288660825, NA, 0.00688478421982292, NA, 0.0540775321166437
    ), relative_reduc = c(NA, -0.192472231188829, NA, -0.137599285388677, 
    NA, -0.0927269469428346, NA, -0.106488769056694, NA, -0.0587164185402723, 
    NA, -0.0721217117440033, NA, -0.0515289120511446, NA, -0.0770291626490328
    ), conf_level = c(0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 
    0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95), mse = c(39.7690337537306, 
    39.7690337537306, 25.4790250966069, 25.4790250966069, 14.7359666659521, 
    14.7359666659521, 96.7804543859424, 96.7804543859424, 39.7690337537306, 
    39.7690337537306, 25.4790250966069, 25.4790250966069, 14.7359666659521, 
    14.7359666659521, 96.7804543859424, 96.7804543859424), df = c(144.333433354449, 
    144.333433354449, 147.409615042012, 147.409615042012, 128.125625220836, 
    128.125625220836, 130.971564726009, 130.971564726009, 144.333433354449, 
    144.333433354449, 147.409615042012, 147.409615042012, 128.125625220836, 
    128.125625220836, 130.971564726009, 130.971564726009)), row.names = c(NA, 
    -16L), class = "data.frame")

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

# a_lsmeans works well together with subgroup variable

    Code
      result
    Output
                                                              PBO                       TRT          
                                                            (N=105)                   (N=95)         
      ———————————————————————————————————————————————————————————————————————————————————————————————
      Male                                                                                           
        VIS1                                                                                         
          n                                                   30                        32           
          Adjusted Mean (SE)                            31.245 (1.116)            37.258 (1.089)     
            Adjusted Mean 95% CI                       (29.039, 33.450)          (35.104, 39.412)    
            Adjusted Mean (95% CI)                  31.245 (29.039, 33.450)   37.258 (35.104, 39.412)
          Difference in Adjusted Means (SE)                                        6.014 (1.561)     
            Difference in Adjusted Means 95% CI                                   (2.929, 9.098)     
            Difference in Adjusted Means (95% CI)                              6.014 (2.929, 9.098)  
            Relative Reduction (%)                                                    -19.2%         
            p-value                                                                   <0.001         
        VIS2                                                                                         
          n                                                   35                        30           
          Adjusted Mean (SE)                            38.209 (0.841)            43.467 (0.898)     
            Adjusted Mean 95% CI                       (36.548, 39.871)          (41.691, 45.242)    
            Adjusted Mean (95% CI)                  38.209 (36.548, 39.871)   43.467 (41.691, 45.242)
          Difference in Adjusted Means (SE)                                        5.258 (1.232)     
            Difference in Adjusted Means 95% CI                                   (2.822, 7.693)     
            Difference in Adjusted Means (95% CI)                              5.258 (2.822, 7.693)  
            Relative Reduction (%)                                                    -13.8%         
            p-value                                                                   <0.001         
        VIS3                                                                                         
          n                                                   35                        23           
          Adjusted Mean (SE)                            42.884 (0.652)            46.861 (0.799)     
            Adjusted Mean 95% CI                       (41.593, 44.175)          (45.279, 48.443)    
            Adjusted Mean (95% CI)                  42.884 (41.593, 44.175)   46.861 (45.279, 48.443)
          Difference in Adjusted Means (SE)                                        3.977 (1.036)     
            Difference in Adjusted Means 95% CI                                   (1.926, 6.027)     
            Difference in Adjusted Means (95% CI)                              3.977 (1.926, 6.027)  
            Relative Reduction (%)                                                     -9.3%         
            p-value                                                                   <0.001         
        VIS4                                                                                         
          n                                                   33                        31           
          Adjusted Mean (SE)                            47.511 (1.707)            52.571 (1.759)     
            Adjusted Mean 95% CI                       (44.134, 50.888)          (49.091, 56.050)    
            Adjusted Mean (95% CI)                  47.511 (44.134, 50.888)   52.571 (49.091, 56.050)
          Difference in Adjusted Means (SE)                                        5.059 (2.451)     
            Difference in Adjusted Means 95% CI                                   (0.211, 9.908)     
            Difference in Adjusted Means (95% CI)                              5.059 (0.211, 9.908)  
            Relative Reduction (%)                                                    -10.6%         
            p-value                                                                    0.041         
      Female                                                                                         
        VIS1                                                                                         
          n                                                   38                        34           
          Adjusted Mean (SE)                            35.046 (1.004)            37.103 (1.047)     
            Adjusted Mean 95% CI                       (33.062, 37.030)          (35.035, 39.172)    
            Adjusted Mean (95% CI)                  35.046 (33.062, 37.030)   37.103 (35.035, 39.172)
          Difference in Adjusted Means (SE)                                        2.058 (1.451)     
            Difference in Adjusted Means 95% CI                                   (-0.810, 4.926)    
            Difference in Adjusted Means (95% CI)                              2.058 (-0.810, 4.926) 
            Relative Reduction (%)                                                     -5.9%         
            p-value                                                                    0.158         
        VIS2                                                                                         
          n                                                   34                        41           
          Adjusted Mean (SE)                            38.044 (0.847)            40.788 (0.777)     
            Adjusted Mean 95% CI                       (36.371, 39.717)          (39.252, 42.325)    
            Adjusted Mean (95% CI)                  38.044 (36.371, 39.717)   40.788 (39.252, 42.325)
          Difference in Adjusted Means (SE)                                        2.744 (1.149)     
            Difference in Adjusted Means 95% CI                                   (0.473, 5.015)     
            Difference in Adjusted Means (95% CI)                              2.744 (0.473, 5.015)  
            Relative Reduction (%)                                                     -7.2%         
            p-value                                                                    0.018         
        VIS3                                                                                         
          n                                                   36                        35           
          Adjusted Mean (SE)                            44.499 (0.647)            46.792 (0.649)     
            Adjusted Mean 95% CI                       (43.219, 45.778)          (45.507, 48.076)    
            Adjusted Mean (95% CI)                  44.499 (43.219, 45.778)   46.792 (45.507, 48.076)
          Difference in Adjusted Means (SE)                                        2.293 (0.918)     
            Difference in Adjusted Means 95% CI                                   (0.476, 4.109)     
            Difference in Adjusted Means (95% CI)                              2.293 (0.476, 4.109)  
            Relative Reduction (%)                                                     -5.2%         
            p-value                                                                    0.014         
        VIS4                                                                                         
          n                                                   34                        36           
          Adjusted Mean (SE)                            49.177 (1.680)            52.965 (1.632)     
            Adjusted Mean 95% CI                       (45.854, 52.499)          (49.737, 56.193)    
            Adjusted Mean (95% CI)                  49.177 (45.854, 52.499)   52.965 (49.737, 56.193)
          Difference in Adjusted Means (SE)                                        3.788 (2.342)     
            Difference in Adjusted Means 95% CI                                   (-0.845, 8.421)    
            Difference in Adjusted Means (95% CI)                              3.788 (-0.845, 8.421) 
            Relative Reduction (%)                                                     -7.7%         
            p-value                                                                    0.108         

