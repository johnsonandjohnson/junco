# a_diff_means works in a table layout with ex_advs data

    Code
      res_matrix
    Output
            [,1]                                        [,2]                    
       [1,] ""                                          "A: Drug X"             
       [2,] "DIABP"                                     ""                      
       [3,] "BASELINE"                                  ""                      
       [4,] "Difference in Means Sample Size (Group 1)" "134"                   
       [5,] "Difference in Means Sample Size (Group 2)" "134"                   
       [6,] "Difference in Means"                       "-1.84"                 
       [7,] "Difference in Means SE"                    "0.972"                 
       [8,] "Difference in Means (SE)"                  "-1.840 (0.972)"        
       [9,] "Difference in Means 95% CI"                "(-3.753, 0.074)"       
      [10,] "Difference in Means (95% CI)"              "-1.840 (-3.753, 0.074)"
      [11,] "WEEK 1 DAY 8"                              ""                      
      [12,] "Difference in Means Sample Size (Group 1)" "134"                   
      [13,] "Difference in Means Sample Size (Group 2)" "134"                   
      [14,] "Difference in Means"                       "0.59"                  
      [15,] "Difference in Means SE"                    "0.932"                 
      [16,] "Difference in Means (SE)"                  "0.585 (0.932)"         
      [17,] "Difference in Means 95% CI"                "(-1.250, 2.421)"       
      [18,] "Difference in Means (95% CI)"              "0.585 (-1.250, 2.421)" 
      [19,] "WEEK 2 DAY 15"                             ""                      
      [20,] "Difference in Means Sample Size (Group 1)" "134"                   
      [21,] "Difference in Means Sample Size (Group 2)" "134"                   
      [22,] "Difference in Means"                       "1.12"                  
      [23,] "Difference in Means SE"                    "0.996"                 
      [24,] "Difference in Means (SE)"                  "1.118 (0.996)"         
      [25,] "Difference in Means 95% CI"                "(-0.843, 3.079)"       
      [26,] "Difference in Means (95% CI)"              "1.118 (-0.843, 3.079)" 
      [27,] "SYSBP"                                     ""                      
      [28,] "BASELINE"                                  ""                      
      [29,] "Difference in Means Sample Size (Group 1)" "134"                   
      [30,] "Difference in Means Sample Size (Group 2)" "134"                   
      [31,] "Difference in Means"                       "-0.84"                 
      [32,] "Difference in Means SE"                    "1.038"                 
      [33,] "Difference in Means (SE)"                  "-0.838 (1.038)"        
      [34,] "Difference in Means 95% CI"                "(-2.883, 1.206)"       
      [35,] "Difference in Means (95% CI)"              "-0.838 (-2.883, 1.206)"
      [36,] "WEEK 1 DAY 8"                              ""                      
      [37,] "Difference in Means Sample Size (Group 1)" "134"                   
      [38,] "Difference in Means Sample Size (Group 2)" "134"                   
      [39,] "Difference in Means"                       "1.44"                  
      [40,] "Difference in Means SE"                    "1.091"                 
      [41,] "Difference in Means (SE)"                  "1.439 (1.091)"         
      [42,] "Difference in Means 95% CI"                "(-0.710, 3.588)"       
      [43,] "Difference in Means (95% CI)"              "1.439 (-0.710, 3.588)" 
      [44,] "WEEK 2 DAY 15"                             ""                      
      [45,] "Difference in Means Sample Size (Group 1)" "134"                   
      [46,] "Difference in Means Sample Size (Group 2)" "134"                   
      [47,] "Difference in Means"                       "-0.01"                 
      [48,] "Difference in Means SE"                    "0.950"                 
      [49,] "Difference in Means (SE)"                  "-0.008 (0.950)"        
      [50,] "Difference in Means 95% CI"                "(-1.878, 1.862)"       
      [51,] "Difference in Means (95% CI)"              "-0.008 (-1.878, 1.862)"
            [,3]         [,4]                    
       [1,] "B: Placebo" "C: Combination"        
       [2,] ""           ""                      
       [3,] ""           ""                      
       [4,] ""           "132"                   
       [5,] ""           "134"                   
       [6,] ""           "0.67"                  
       [7,] ""           "0.965"                 
       [8,] ""           "0.665 (0.965)"         
       [9,] ""           "(-1.235, 2.565)"       
      [10,] ""           "0.665 (-1.235, 2.565)" 
      [11,] ""           ""                      
      [12,] ""           "132"                   
      [13,] ""           "134"                   
      [14,] ""           "-0.82"                 
      [15,] ""           "0.959"                 
      [16,] ""           "-0.819 (0.959)"        
      [17,] ""           "(-2.706, 1.068)"       
      [18,] ""           "-0.819 (-2.706, 1.068)"
      [19,] ""           ""                      
      [20,] ""           "132"                   
      [21,] ""           "134"                   
      [22,] ""           "0.26"                  
      [23,] ""           "1.029"                 
      [24,] ""           "0.263 (1.029)"         
      [25,] ""           "(-1.763, 2.289)"       
      [26,] ""           "0.263 (-1.763, 2.289)" 
      [27,] ""           ""                      
      [28,] ""           ""                      
      [29,] ""           "132"                   
      [30,] ""           "134"                   
      [31,] ""           "-1.75"                 
      [32,] ""           "0.966"                 
      [33,] ""           "-1.751 (0.966)"        
      [34,] ""           "(-3.654, 0.152)"       
      [35,] ""           "-1.751 (-3.654, 0.152)"
      [36,] ""           ""                      
      [37,] ""           "132"                   
      [38,] ""           "134"                   
      [39,] ""           "0.38"                  
      [40,] ""           "1.024"                 
      [41,] ""           "0.384 (1.024)"         
      [42,] ""           "(-1.632, 2.399)"       
      [43,] ""           "0.384 (-1.632, 2.399)" 
      [44,] ""           ""                      
      [45,] ""           "132"                   
      [46,] ""           "134"                   
      [47,] ""           "-0.12"                 
      [48,] ""           "0.902"                 
      [49,] ""           "-0.117 (0.902)"        
      [50,] ""           "(-1.893, 1.659)"       
      [51,] ""           "-0.117 (-1.893, 1.659)"

# a_diff_means works in a table layout (stacked header) with ex_advs data

    Code
      res_matrix
    Output
            [,1]                                        [,2]                    
       [1,] ""                                          "Active Study Agent"    
       [2,] ""                                          "A: Drug X"             
       [3,] "DIABP"                                     ""                      
       [4,] "BASELINE"                                  ""                      
       [5,] "Difference in Means Sample Size (Group 1)" "134"                   
       [6,] "Difference in Means Sample Size (Group 2)" "134"                   
       [7,] "Difference in Means"                       "-1.84"                 
       [8,] "Difference in Means SE"                    "0.972"                 
       [9,] "Difference in Means (SE)"                  "-1.840 (0.972)"        
      [10,] "Difference in Means 95% CI"                "(-3.753, 0.074)"       
      [11,] "Difference in Means (95% CI)"              "-1.840 (-3.753, 0.074)"
      [12,] "WEEK 1 DAY 8"                              ""                      
      [13,] "Difference in Means Sample Size (Group 1)" "134"                   
      [14,] "Difference in Means Sample Size (Group 2)" "134"                   
      [15,] "Difference in Means"                       "0.59"                  
      [16,] "Difference in Means SE"                    "0.932"                 
      [17,] "Difference in Means (SE)"                  "0.585 (0.932)"         
      [18,] "Difference in Means 95% CI"                "(-1.250, 2.421)"       
      [19,] "Difference in Means (95% CI)"              "0.585 (-1.250, 2.421)" 
      [20,] "WEEK 2 DAY 15"                             ""                      
      [21,] "Difference in Means Sample Size (Group 1)" "134"                   
      [22,] "Difference in Means Sample Size (Group 2)" "134"                   
      [23,] "Difference in Means"                       "1.12"                  
      [24,] "Difference in Means SE"                    "0.996"                 
      [25,] "Difference in Means (SE)"                  "1.118 (0.996)"         
      [26,] "Difference in Means 95% CI"                "(-0.843, 3.079)"       
      [27,] "Difference in Means (95% CI)"              "1.118 (-0.843, 3.079)" 
      [28,] "SYSBP"                                     ""                      
      [29,] "BASELINE"                                  ""                      
      [30,] "Difference in Means Sample Size (Group 1)" "134"                   
      [31,] "Difference in Means Sample Size (Group 2)" "134"                   
      [32,] "Difference in Means"                       "-0.84"                 
      [33,] "Difference in Means SE"                    "1.038"                 
      [34,] "Difference in Means (SE)"                  "-0.838 (1.038)"        
      [35,] "Difference in Means 95% CI"                "(-2.883, 1.206)"       
      [36,] "Difference in Means (95% CI)"              "-0.838 (-2.883, 1.206)"
      [37,] "WEEK 1 DAY 8"                              ""                      
      [38,] "Difference in Means Sample Size (Group 1)" "134"                   
      [39,] "Difference in Means Sample Size (Group 2)" "134"                   
      [40,] "Difference in Means"                       "1.44"                  
      [41,] "Difference in Means SE"                    "1.091"                 
      [42,] "Difference in Means (SE)"                  "1.439 (1.091)"         
      [43,] "Difference in Means 95% CI"                "(-0.710, 3.588)"       
      [44,] "Difference in Means (95% CI)"              "1.439 (-0.710, 3.588)" 
      [45,] "WEEK 2 DAY 15"                             ""                      
      [46,] "Difference in Means Sample Size (Group 1)" "134"                   
      [47,] "Difference in Means Sample Size (Group 2)" "134"                   
      [48,] "Difference in Means"                       "-0.01"                 
      [49,] "Difference in Means SE"                    "0.950"                 
      [50,] "Difference in Means (SE)"                  "-0.008 (0.950)"        
      [51,] "Difference in Means 95% CI"                "(-1.878, 1.862)"       
      [52,] "Difference in Means (95% CI)"              "-0.008 (-1.878, 1.862)"
            [,3]                     [,4]        
       [1,] "Active Study Agent"     " "         
       [2,] "C: Combination"         "B: Placebo"
       [3,] ""                       ""          
       [4,] ""                       ""          
       [5,] "132"                    ""          
       [6,] "134"                    ""          
       [7,] "0.67"                   ""          
       [8,] "0.965"                  ""          
       [9,] "0.665 (0.965)"          ""          
      [10,] "(-1.235, 2.565)"        ""          
      [11,] "0.665 (-1.235, 2.565)"  ""          
      [12,] ""                       ""          
      [13,] "132"                    ""          
      [14,] "134"                    ""          
      [15,] "-0.82"                  ""          
      [16,] "0.959"                  ""          
      [17,] "-0.819 (0.959)"         ""          
      [18,] "(-2.706, 1.068)"        ""          
      [19,] "-0.819 (-2.706, 1.068)" ""          
      [20,] ""                       ""          
      [21,] "132"                    ""          
      [22,] "134"                    ""          
      [23,] "0.26"                   ""          
      [24,] "1.029"                  ""          
      [25,] "0.263 (1.029)"          ""          
      [26,] "(-1.763, 2.289)"        ""          
      [27,] "0.263 (-1.763, 2.289)"  ""          
      [28,] ""                       ""          
      [29,] ""                       ""          
      [30,] "132"                    ""          
      [31,] "134"                    ""          
      [32,] "-1.75"                  ""          
      [33,] "0.966"                  ""          
      [34,] "-1.751 (0.966)"         ""          
      [35,] "(-3.654, 0.152)"        ""          
      [36,] "-1.751 (-3.654, 0.152)" ""          
      [37,] ""                       ""          
      [38,] "132"                    ""          
      [39,] "134"                    ""          
      [40,] "0.38"                   ""          
      [41,] "1.024"                  ""          
      [42,] "0.384 (1.024)"          ""          
      [43,] "(-1.632, 2.399)"        ""          
      [44,] "0.384 (-1.632, 2.399)"  ""          
      [45,] ""                       ""          
      [46,] "132"                    ""          
      [47,] "134"                    ""          
      [48,] "-0.12"                  ""          
      [49,] "0.902"                  ""          
      [50,] "-0.117 (0.902)"         ""          
      [51,] "(-1.893, 1.659)"        ""          
      [52,] "-0.117 (-1.893, 1.659)" ""          

# a_diff_means works in a table layout with synthetic paired data containing NA values

    Code
      res_matrix
    Output
            [,1]                                        [,2]                        
       [1,] ""                                          "Drug X"                    
       [2,] "Baseline"                                  ""                          
       [3,] "Difference in Means Sample Size (Group 1)" "3"                         
       [4,] "Difference in Means Sample Size (Group 2)" "3"                         
       [5,] "Difference in Means"                       "-11.33"                    
       [6,] "Difference in Means SE"                    "8.413"                     
       [7,] "Difference in Means (SE)"                  "-11.333 (8.413)"           
       [8,] "Difference in Means 95% CI"                "(-47.531, 24.865)"         
       [9,] "Difference in Means (95% CI)"              "-11.333 (-47.531, 24.865)" 
      [10,] "Week 12"                                   ""                          
      [11,] "Difference in Means Sample Size (Group 1)" "4"                         
      [12,] "Difference in Means Sample Size (Group 2)" "4"                         
      [13,] "Difference in Means"                       "-6.00"                     
      [14,] "Difference in Means SE"                    "3.937"                     
      [15,] "Difference in Means (SE)"                  "-6.000 (3.937)"            
      [16,] "Difference in Means 95% CI"                "(-18.529, 6.529)"          
      [17,] "Difference in Means (95% CI)"              "-6.000 (-18.529, 6.529)"   
      [18,] "Week 24"                                   ""                          
      [19,] "Difference in Means Sample Size (Group 1)" "2"                         
      [20,] "Difference in Means Sample Size (Group 2)" "2"                         
      [21,] "Difference in Means"                       "-7.50"                     
      [22,] "Difference in Means SE"                    "9.500"                     
      [23,] "Difference in Means (SE)"                  "-7.500 (9.500)"            
      [24,] "Difference in Means 95% CI"                "(-128.209, 113.209)"       
      [25,] "Difference in Means (95% CI)"              "-7.500 (-128.209, 113.209)"
            [,3]                         [,4]     
       [1,] "Drug Y"                     "Placebo"
       [2,] ""                           ""       
       [3,] "2"                          ""       
       [4,] "2"                          ""       
       [5,] "-15.00"                     ""       
       [6,] "5.000"                      ""       
       [7,] "-15.000 (5.000)"            ""       
       [8,] "(-78.531, 48.531)"          ""       
       [9,] "-15.000 (-78.531, 48.531)"  ""       
      [10,] ""                           ""       
      [11,] "4"                          ""       
      [12,] "4"                          ""       
      [13,] "-2.25"                      ""       
      [14,] "5.072"                      ""       
      [15,] "-2.250 (5.072)"             ""       
      [16,] "(-18.393, 13.893)"          ""       
      [17,] "-2.250 (-18.393, 13.893)"   ""       
      [18,] ""                           ""       
      [19,] "2"                          ""       
      [20,] "2"                          ""       
      [21,] "-5.50"                      ""       
      [22,] "16.500"                     ""       
      [23,] "-5.500 (16.500)"            ""       
      [24,] "(-215.152, 204.152)"        ""       
      [25,] "-5.500 (-215.152, 204.152)" ""       

# a_diff_means works when in ref column

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
                 row_name formatted_cell indent_mod
      1     diff_means_n1                         0
      2     diff_means_n2                         0
      3    diff_means_est                         0
      4     diff_means_se                         0
      5 diff_means_est_se                         0
      6     diff_means_ci                         0
      7 diff_means_est_ci                         0
                                        row_label
      1 Difference in Means Sample Size (Group 1)
      2 Difference in Means Sample Size (Group 2)
      3                       Difference in Means
      4                    Difference in Means SE
      5                  Difference in Means (SE)
      6                    Difference in Means CI
      7                  Difference in Means (CI)

# a_diff_means works with no observations in the column-split sample

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
                 row_name formatted_cell indent_mod
      1     diff_means_n1              0          0
      2     diff_means_n2            804          0
      3    diff_means_est             NA          0
      4     diff_means_se             NA          0
      5 diff_means_est_se             NA          0
      6     diff_means_ci             NA          0
      7 diff_means_est_ci             NA          0
                                        row_label
      1 Difference in Means Sample Size (Group 1)
      2 Difference in Means Sample Size (Group 2)
      3                       Difference in Means
      4                    Difference in Means SE
      5                  Difference in Means (SE)
      6                Difference in Means 95% CI
      7              Difference in Means (95% CI)

# a_diff_means works with no observations in the ref sample

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
                 row_name formatted_cell indent_mod
      1     diff_means_n1            804          0
      2     diff_means_n2              0          0
      3    diff_means_est             NA          0
      4     diff_means_se             NA          0
      5 diff_means_est_se             NA          0
      6     diff_means_ci             NA          0
      7 diff_means_est_ci             NA          0
                                        row_label
      1 Difference in Means Sample Size (Group 1)
      2 Difference in Means Sample Size (Group 2)
      3                       Difference in Means
      4                    Difference in Means SE
      5                  Difference in Means (SE)
      6                Difference in Means 95% CI
      7              Difference in Means (95% CI)

# a_diff_means works with no observations in both samples

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
                 row_name formatted_cell indent_mod
      1     diff_means_n1              0          0
      2     diff_means_n2              0          0
      3    diff_means_est             NA          0
      4     diff_means_se             NA          0
      5 diff_means_est_se             NA          0
      6     diff_means_ci             NA          0
      7 diff_means_est_ci             NA          0
                                        row_label
      1 Difference in Means Sample Size (Group 1)
      2 Difference in Means Sample Size (Group 2)
      3                       Difference in Means
      4                    Difference in Means SE
      5                  Difference in Means (SE)
      6                Difference in Means 95% CI
      7              Difference in Means (95% CI)

