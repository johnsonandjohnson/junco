# h_summarize_mmrm works with healthy input

    structure(list(VISIT = structure(c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 
    3L, 3L, 4L, 4L, 4L), levels = c("V1", "V2", "V3", "V4"), class = "factor"), 
        ARM = structure(c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 
        2L, 3L), levels = c("B: Placebo", "A: Drug X", "C: Combination"
        ), class = "factor"), estimate_est = c(-0.139036878070297, 
        0.0797268945990817, 0.106033570988645, -0.124404892619797, 
        -0.0802664067059327, 0.0775406237447667, 0.100790213755066, 
        0.0584592871935487, 0.11179399615135, -0.132750725991174, 
        0.110919957336292, -0.0202004454288378), se_est = c(0.09284501616797, 
        0.0868790991965277, 0.0841122843147916, 0.0961438735630328, 
        0.0899674296973221, 0.0871043424944065, 0.100923353410177, 
        0.0944418096318721, 0.0914391242032588, 0.0940772630458459, 
        0.088032712224214, 0.0852299442383556), df_est = c(353.962422549419, 
        353.721661249561, 353.364183661074, 353.911134592213, 353.680552027837, 
        353.338218143227, 353.116502576009, 352.900523234809, 352.579904373285, 
        353.048392947938, 352.811824478238, 352.460585946236), lower_cl_est = c(-0.32163411411324, 
        -0.0911376394687429, -0.0593900615813153, -0.313490047945892, 
        -0.257204809990843, -0.0937675330167885, -0.0976962274546, 
        -0.12728026183879, -0.068040707415679, -0.317773051334539, 
        -0.0622149124641999, -0.187823656632904), upper_cl_est = c(0.0435603579726454, 
        0.250591428666906, 0.271457203558604, 0.0646802627062978, 
        0.0966719965789772, 0.248848780506322, 0.299276654964731, 
        0.244198836225888, 0.291628699718379, 0.0522715993521914, 
        0.284054827136784, 0.147422765775228), n = c(106L, 121L, 
        129L, 106L, 121L, 129L, 106L, 121L, 129L, 106L, 121L, 129L
        ), estimate_contr = c(NA, 0.218763772669379, 0.245070449058942, 
        NA, 0.0441384859138646, 0.201945516364564, NA, -0.0423309265615169, 
        0.0110037823962845, NA, 0.243670683327466, 0.112550280562336
        ), se_contr = c(NA, 0.12725090578327, 0.125332335721908, 
        NA, 0.131766463670688, 0.129784174238284, NA, 0.138309062567606, 
        0.136234201565688, NA, 0.128937612673651, 0.126995258945744
        ), df_contr = c(NA, 354.618198024671, 354.114638173846, NA, 
        354.539269943296, 354.056923512302, NA, 353.704951929229, 
        353.253068057148, NA, 353.692785789218, 353.19796157587), 
        lower_cl_contr = c(NA, -0.0314975469454298, -0.00141886490631624, 
        NA, -0.215003671192756, -0.0532993074887294, NA, -0.314342464253151, 
        -0.256928316324982, NA, -0.00991011449219409, -0.137211703696884
        ), upper_cl_contr = c(NA, 0.469025092284188, 0.4915597630242, 
        NA, 0.303280643020485, 0.457190340217857, NA, 0.229680611130117, 
        0.278935881117551, NA, 0.497251481147125, 0.362312264821556
        ), t_stat = c(NA, 1.719152970447, 1.95536489164866, NA, 0.334975111908412, 
        1.55601033446336, NA, -0.306060396735212, 0.0807710712128248, 
        NA, 1.88983399238368, 0.886255766527637), p_value = c(NA, 
        0.0864591647754602, 0.051326198015144, NA, 0.737841958895664, 
        0.120599023477513, NA, 0.759738724841716, 0.935669755375365, 
        NA, 0.0595976716216869, 0.376082735352519), p_value_less = c(NA, 
        0.95677041761227, 0.974336900992428, NA, 0.631079020552168, 
        0.939700488261243, NA, 0.379869362420858, 0.532165122312318, 
        NA, 0.970201164189157, 0.81195863232374), p_value_greater = c(NA, 
        0.0432295823877301, 0.025663099007572, NA, 0.368920979447832, 
        0.0602995117387566, NA, 0.620130637579142, 0.467834877687682, 
        NA, 0.0297988358108434, 0.18804136767626), relative_reduc = c(NA, 
        1.57342264660727, 1.76262911293962, NA, 0.354797025939803, 
        1.62329239720293, NA, 0.419990443361764, -0.109175107248262, 
        NA, 1.83555066466203, 0.847831751743633), conf_level = c(0.95, 
        0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 
        0.95), mse = c(0.911405185676512, 0.911405185676512, 0.911405185676512, 
        0.977490609702756, 0.977490609702756, 0.977490609702756, 
        1.07732976672698, 1.07732976672698, 1.07732976672698, 0.935820631543009, 
        0.935820631543009, 0.935820631543009), df = c(354.618198024671, 
        354.618198024671, 354.618198024671, 354.539269943296, 354.539269943296, 
        354.539269943296, 353.704951929229, 353.704951929229, 353.704951929229, 
        353.692785789218, 353.692785789218, 353.692785789218)), row.names = c(NA, 
    -12L), class = "data.frame")

# s_summarize_mmrm works with healthy input in non-ref cell

    list(n = 36L, adj_mean_se = c(0.172166748034815, 0.170297378076073
    ), adj_mean_ci = structure(c(-0.162551447160789, 0.506884943230418
    ), label = structure("Adjusted Mean 95% CI", class = c("glue", 
    "character"))), adj_mean_est_ci = structure(c(0.172166748034815, 
    -0.162551447160789, 0.506884943230418), label = structure("Adjusted Mean (95% CI)", class = c("glue", 
    "character"))), diff_mean_se = c(0.102633690286469, 0.245907582546143
    ), diff_mean_ci = structure(c(-0.380695268254029, 0.585962648826967
    ), label = structure("Difference in Adjusted Means 95% CI", class = c("glue", 
    "character"))), diff_mean_est_ci = structure(c(0.102633690286469, 
    -0.380695268254029, 0.585962648826967), label = structure("Difference in Adjusted Means (95% CI)", class = c("glue", 
    "character"))), change = structure(-1.4760416643537, label = "Relative Reduction (%)"), 
        p_value = structure(0.676618128757201, label = "2-sided p-value"))

# s_summarize_mmrm works with healthy input in ref col cell

    WAoAAAACAAQEAQACAwAAAAITAAAACQAAAA0AAAABAAAAIQAAAA4AAAACP7HM6yEFICg/xs4U
    jtyG+wAAAg4AAAACv9H2ITGrWKM/2tyWwi3otwAABAIAAAABAAQACQAAAAVsYWJlbAAAAxAA
    AAABAAQACQAAABRBZGp1c3RlZCBNZWFuIDk1JSBDSQAABAIAAAABAAQACQAAAAVjbGFzcwAA
    ABAAAAACAAQACQAAAARnbHVlAAQACQAAAAljaGFyYWN0ZXIAAAD+AAAA/gAAAg4AAAADP7HM
    6yEFICi/0fYhMatYoz/a3JbCLei3AAAEAgAAAf8AAAMQAAAAAQAEAAkAAAAWQWRqdXN0ZWQg
    TWVhbiAoOTUlIENJKQAABAIAAAL/AAAAEAAAAAIABAAJAAAABGdsdWUABAAJAAAACWNoYXJh
    Y3RlcgAAAP4AAAD+AAAAEAAAAAAAAAIQAAAAAAAABAIAAAH/AAADEAAAAAEABAAJAAAAI0Rp
    ZmZlcmVuY2UgaW4gQWRqdXN0ZWQgTWVhbnMgOTUlIENJAAAEAgAAAv8AAAAQAAAAAgAEAAkA
    AAAEZ2x1ZQAEAAkAAAAJY2hhcmFjdGVyAAAA/gAAAP4AAAIQAAAAAAAABAIAAAH/AAADEAAA
    AAEABAAJAAAAJURpZmZlcmVuY2UgaW4gQWRqdXN0ZWQgTWVhbnMgKDk1JSBDSSkAAAQCAAAC
    /wAAABAAAAACAAQACQAAAARnbHVlAAQACQAAAAljaGFyYWN0ZXIAAAD+AAAA/gAAAhAAAAAA
    AAAEAgAAAf8AAAAQAAAAAQAEAAkAAAAWUmVsYXRpdmUgUmVkdWN0aW9uICglKQAAAP4AAAAQ
    AAAAAAAABAIAAAABAAQACQAAAAVuYW1lcwAAABAAAAAJAAQACQAAAAFuAAQACQAAAAthZGpf
    bWVhbl9zZQAEAAkAAAALYWRqX21lYW5fY2kABAAJAAAAD2Fkal9tZWFuX2VzdF9jaQAEAAkA
    AAAMZGlmZl9tZWFuX3NlAAQACQAAAAxkaWZmX21lYW5fY2kABAAJAAAAEGRpZmZfbWVhbl9l
    c3RfY2kABAAJAAAABmNoYW5nZQAEAAkAAAAHcF92YWx1ZQAAAP4=

# s_summarize_mmrm works with healthy input in ref row cell

    Code
      result
    Output
      $n
      NULL
      
      $adj_mean_se
      NULL
      
      $adj_mean_ci
      NULL
      
      $adj_mean_est_ci
      NULL
      
      $diff_mean_se
      NULL
      
      $diff_mean_ci
      NULL
      
      $diff_mean_est_ci
      NULL
      
      $change
      NULL
      
      $p_value
      NULL
      

# a_summarize_mmrm works as expected in table layout

    Code
      res
    Output
                                                        A: Drug X                B: Placebo             C: Combination    
      ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      V0                                                                                                                  
          Adjusted Mean (CI)                                                                                              
          Difference in Adjusted Means (CI)                                                                               
          p-value                                                                                                         
      V1                                                                                                                  
          Adjusted Mean (90% CI)                  0.079 (-0.069, 0.227)    -0.139 (-0.297, 0.019)   0.106 (-0.037, 0.250) 
          Difference in Adjusted Means (90% CI)    0.218 (0.002, 0.435)                              0.245 (0.032, 0.459) 
          2-sided p-value                                 0.097                                             0.059         
      V2                                                                                                                  
          Adjusted Mean (90% CI)                  -0.081 (-0.229, 0.067)   -0.124 (-0.282, 0.034)   0.078 (-0.065, 0.221) 
          Difference in Adjusted Means (90% CI)   0.044 (-0.173, 0.260)                             0.202 (-0.011, 0.416) 
          2-sided p-value                                 0.740                                             0.119         
      V3                                                                                                                  
          Adjusted Mean (90% CI)                  0.058 (-0.090, 0.206)    0.101 (-0.057, 0.259)    0.112 (-0.031, 0.255) 
          Difference in Adjusted Means (90% CI)   -0.043 (-0.259, 0.174)                            0.011 (-0.202, 0.225) 
          2-sided p-value                                 0.745                                             0.930         
      V4                                                                                                                  
          Adjusted Mean (90% CI)                  0.110 (-0.037, 0.258)    -0.133 (-0.291, 0.025)   -0.020 (-0.163, 0.123)
          Difference in Adjusted Means (90% CI)    0.243 (0.027, 0.460)                             0.113 (-0.101, 0.326) 
          2-sided p-value                                 0.065                                             0.384         

---

    Code
      res2
    Output
                                                        A: Drug X                B: Placebo             C: Combination    
      ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      V1                                                                                                                  
          Adjusted Mean (90% CI)                  0.079 (-0.069, 0.227)    -0.139 (-0.297, 0.019)   0.106 (-0.037, 0.250) 
          Difference in Adjusted Means (90% CI)    0.218 (0.002, 0.435)                              0.245 (0.032, 0.459) 
          2-sided p-value                                 0.097                                             0.059         
      V2                                                                                                                  
          Adjusted Mean (90% CI)                  -0.081 (-0.229, 0.067)   -0.124 (-0.282, 0.034)   0.078 (-0.065, 0.221) 
          Difference in Adjusted Means (90% CI)   0.044 (-0.173, 0.260)                             0.202 (-0.011, 0.416) 
          2-sided p-value                                 0.740                                             0.119         
      V3                                                                                                                  
          Adjusted Mean (90% CI)                  0.058 (-0.090, 0.206)    0.101 (-0.057, 0.259)    0.112 (-0.031, 0.255) 
          Difference in Adjusted Means (90% CI)   -0.043 (-0.259, 0.174)                            0.011 (-0.202, 0.225) 
          2-sided p-value                                 0.745                                             0.930         
      V4                                                                                                                  
          Adjusted Mean (90% CI)                  0.110 (-0.037, 0.258)    -0.133 (-0.291, 0.025)   -0.020 (-0.163, 0.123)
          Difference in Adjusted Means (90% CI)    0.243 (0.027, 0.460)                             0.113 (-0.101, 0.326) 
          2-sided p-value                                 0.065                                             0.384         

# a_summarize_mmrm works as expected below row splits

    Code
      res
    Output
                                                          A: Drug X                B: Placebo              C: Combination    
      ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      A                                                                                                                      
        V0                                                                                                                   
            Adjusted Mean (CI)                                                                                               
            Difference in Adjusted Means (CI)                                                                                
            p-value                                                                                                          
        V1                                                                                                                   
            Adjusted Mean (90% CI)                  0.172 (-0.109, 0.453)     0.070 (-0.224, 0.363)    0.047 (-0.205, 0.300) 
            Difference in Adjusted Means (90% CI)   0.103 (-0.303, 0.508)                              -0.022 (-0.411, 0.367)
            2-sided p-value                                 0.677                                              0.925         
        V2                                                                                                                   
            Adjusted Mean (90% CI)                  -0.116 (-0.397, 0.164)   -0.209 (-0.503, 0.084)    -0.020 (-0.272, 0.233)
            Difference in Adjusted Means (90% CI)   0.093 (-0.312, 0.498)                              0.189 (-0.200, 0.579) 
            2-sided p-value                                 0.706                                              0.423         
        V3                                                                                                                   
            Adjusted Mean (90% CI)                  0.036 (-0.245, 0.317)     0.190 (-0.103, 0.484)    0.156 (-0.096, 0.409) 
            Difference in Adjusted Means (90% CI)   -0.154 (-0.560, 0.251)                             -0.034 (-0.423, 0.355)
            2-sided p-value                                 0.530                                              0.886         
        V4                                                                                                                   
            Adjusted Mean (90% CI)                  0.011 (-0.269, 0.292)    -0.457 (-0.750, -0.163)   0.060 (-0.192, 0.313) 
            Difference in Adjusted Means (90% CI)    0.468 (0.063, 0.874)                               0.517 (0.128, 0.906) 
            2-sided p-value                                 0.058                                              0.029         
      B                                                                                                                      
        V0                                                                                                                   
            Adjusted Mean (CI)                                                                                               
            Difference in Adjusted Means (CI)                                                                                
            p-value                                                                                                          
        V1                                                                                                                   
            Adjusted Mean (90% CI)                  0.006 (-0.247, 0.259)    -0.150 (-0.406, 0.106)     0.342 (0.080, 0.603) 
            Difference in Adjusted Means (90% CI)   0.156 (-0.206, 0.518)                               0.492 (0.126, 0.858) 
            2-sided p-value                                 0.478                                              0.027         
        V2                                                                                                                   
            Adjusted Mean (90% CI)                  -0.125 (-0.378, 0.128)    0.124 (-0.132, 0.380)    -0.170 (-0.431, 0.092)
            Difference in Adjusted Means (90% CI)   -0.248 (-0.610, 0.113)                             -0.294 (-0.660, 0.072)
            2-sided p-value                                 0.258                                              0.187         
        V3                                                                                                                   
            Adjusted Mean (90% CI)                  0.019 (-0.234, 0.272)     0.033 (-0.224, 0.289)    0.007 (-0.254, 0.269) 
            Difference in Adjusted Means (90% CI)   -0.014 (-0.376, 0.348)                             -0.025 (-0.392, 0.341)
            2-sided p-value                                 0.949                                              0.909         
        V4                                                                                                                   
            Adjusted Mean (90% CI)                  0.181 (-0.072, 0.434)     0.128 (-0.128, 0.384)    -0.094 (-0.356, 0.168)
            Difference in Adjusted Means (90% CI)   0.053 (-0.309, 0.415)                              -0.222 (-0.588, 0.144)
            2-sided p-value                                 0.809                                              0.318         
      C                                                                                                                      
        V0                                                                                                                   
            Adjusted Mean (CI)                                                                                               
            Difference in Adjusted Means (CI)                                                                                
            p-value                                                                                                          
        V1                                                                                                                   
            Adjusted Mean (90% CI)                  0.075 (-0.165, 0.315)    -0.297 (-0.574, -0.021)   -0.060 (-0.294, 0.174)
            Difference in Adjusted Means (90% CI)    0.373 (0.007, 0.738)                              0.237 (-0.125, 0.599) 
            2-sided p-value                                 0.094                                              0.280         
        V2                                                                                                                   
            Adjusted Mean (90% CI)                  -0.007 (-0.247, 0.233)   -0.304 (-0.580, -0.027)    0.348 (0.114, 0.582) 
            Difference in Adjusted Means (90% CI)   0.297 (-0.069, 0.662)                               0.652 (0.290, 1.014) 
            2-sided p-value                                 0.182                                              0.003         
        V3                                                                                                                   
            Adjusted Mean (90% CI)                  0.117 (-0.123, 0.356)     0.131 (-0.145, 0.407)    0.126 (-0.108, 0.360) 
            Difference in Adjusted Means (90% CI)   -0.014 (-0.380, 0.351)                             -0.005 (-0.367, 0.357)
            2-sided p-value                                 0.949                                              0.982         
        V4                                                                                                                   
            Adjusted Mean (90% CI)                  0.129 (-0.111, 0.369)    -0.088 (-0.364, 0.188)    -0.067 (-0.301, 0.168)
            Difference in Adjusted Means (90% CI)   0.217 (-0.149, 0.583)                              0.021 (-0.341, 0.383) 
            2-sided p-value                                 0.329                                              0.922         

---

    Code
      res2
    Output
                                                          A: Drug X                B: Placebo              C: Combination    
      ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      A                                                                                                                      
        V1                                                                                                                   
            Adjusted Mean (90% CI)                  0.172 (-0.109, 0.453)     0.070 (-0.224, 0.363)    0.047 (-0.205, 0.300) 
            Difference in Adjusted Means (90% CI)   0.103 (-0.303, 0.508)                              -0.022 (-0.411, 0.367)
            2-sided p-value                                 0.677                                              0.925         
        V2                                                                                                                   
            Adjusted Mean (90% CI)                  -0.116 (-0.397, 0.164)   -0.209 (-0.503, 0.084)    -0.020 (-0.272, 0.233)
            Difference in Adjusted Means (90% CI)   0.093 (-0.312, 0.498)                              0.189 (-0.200, 0.579) 
            2-sided p-value                                 0.706                                              0.423         
        V3                                                                                                                   
            Adjusted Mean (90% CI)                  0.036 (-0.245, 0.317)     0.190 (-0.103, 0.484)    0.156 (-0.096, 0.409) 
            Difference in Adjusted Means (90% CI)   -0.154 (-0.560, 0.251)                             -0.034 (-0.423, 0.355)
            2-sided p-value                                 0.530                                              0.886         
        V4                                                                                                                   
            Adjusted Mean (90% CI)                  0.011 (-0.269, 0.292)    -0.457 (-0.750, -0.163)   0.060 (-0.192, 0.313) 
            Difference in Adjusted Means (90% CI)    0.468 (0.063, 0.874)                               0.517 (0.128, 0.906) 
            2-sided p-value                                 0.058                                              0.029         
      B                                                                                                                      
        V1                                                                                                                   
            Adjusted Mean (90% CI)                  0.006 (-0.247, 0.259)    -0.150 (-0.406, 0.106)     0.342 (0.080, 0.603) 
            Difference in Adjusted Means (90% CI)   0.156 (-0.206, 0.518)                               0.492 (0.126, 0.858) 
            2-sided p-value                                 0.478                                              0.027         
        V2                                                                                                                   
            Adjusted Mean (90% CI)                  -0.125 (-0.378, 0.128)    0.124 (-0.132, 0.380)    -0.170 (-0.431, 0.092)
            Difference in Adjusted Means (90% CI)   -0.248 (-0.610, 0.113)                             -0.294 (-0.660, 0.072)
            2-sided p-value                                 0.258                                              0.187         
        V3                                                                                                                   
            Adjusted Mean (90% CI)                  0.019 (-0.234, 0.272)     0.033 (-0.224, 0.289)    0.007 (-0.254, 0.269) 
            Difference in Adjusted Means (90% CI)   -0.014 (-0.376, 0.348)                             -0.025 (-0.392, 0.341)
            2-sided p-value                                 0.949                                              0.909         
        V4                                                                                                                   
            Adjusted Mean (90% CI)                  0.181 (-0.072, 0.434)     0.128 (-0.128, 0.384)    -0.094 (-0.356, 0.168)
            Difference in Adjusted Means (90% CI)   0.053 (-0.309, 0.415)                              -0.222 (-0.588, 0.144)
            2-sided p-value                                 0.809                                              0.318         
      C                                                                                                                      
        V1                                                                                                                   
            Adjusted Mean (90% CI)                  0.075 (-0.165, 0.315)    -0.297 (-0.574, -0.021)   -0.060 (-0.294, 0.174)
            Difference in Adjusted Means (90% CI)    0.373 (0.007, 0.738)                              0.237 (-0.125, 0.599) 
            2-sided p-value                                 0.094                                              0.280         
        V2                                                                                                                   
            Adjusted Mean (90% CI)                  -0.007 (-0.247, 0.233)   -0.304 (-0.580, -0.027)    0.348 (0.114, 0.582) 
            Difference in Adjusted Means (90% CI)   0.297 (-0.069, 0.662)                               0.652 (0.290, 1.014) 
            2-sided p-value                                 0.182                                              0.003         
        V3                                                                                                                   
            Adjusted Mean (90% CI)                  0.117 (-0.123, 0.356)     0.131 (-0.145, 0.407)    0.126 (-0.108, 0.360) 
            Difference in Adjusted Means (90% CI)   -0.014 (-0.380, 0.351)                             -0.005 (-0.367, 0.357)
            2-sided p-value                                 0.949                                              0.982         
        V4                                                                                                                   
            Adjusted Mean (90% CI)                  0.129 (-0.111, 0.369)    -0.088 (-0.364, 0.188)    -0.067 (-0.301, 0.168)
            Difference in Adjusted Means (90% CI)   0.217 (-0.149, 0.583)                              0.021 (-0.341, 0.383) 
            2-sided p-value                                 0.329                                              0.922         

