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
       [8,] "Difference in Means (SE)"                  "-1.84 (0.972)"      
       [9,] "Difference in Means 95% CI"                "(-3.75, 0.07)"      
      [10,] "Difference in Means (95% CI)"              "-1.84 (-3.75, 0.07)"
      [11,] "WEEK 1 DAY 8"                              ""                   
      [12,] "Difference in Means Sample Size (Group 1)" "134"                
      [13,] "Difference in Means Sample Size (Group 2)" "134"                
      [14,] "Difference in Means"                       "0.59"               
      [15,] "Difference in Means SE"                    "0.932"              
      [16,] "Difference in Means (SE)"                  "0.59 (0.932)"       
      [17,] "Difference in Means 95% CI"                "(-1.25, 2.42)"      
      [18,] "Difference in Means (95% CI)"              "0.59 (-1.25, 2.42)" 
      [19,] "WEEK 2 DAY 15"                             ""                   
      [20,] "Difference in Means Sample Size (Group 1)" "134"                
      [21,] "Difference in Means Sample Size (Group 2)" "134"                
      [22,] "Difference in Means"                       "1.12"               
      [23,] "Difference in Means SE"                    "0.996"              
      [24,] "Difference in Means (SE)"                  "1.12 (0.996)"       
      [25,] "Difference in Means 95% CI"                "(-0.84, 3.08)"      
      [26,] "Difference in Means (95% CI)"              "1.12 (-0.84, 3.08)" 
      [27,] "SYSBP"                                     ""                   
      [28,] "BASELINE"                                  ""                   
      [29,] "Difference in Means Sample Size (Group 1)" "134"                
      [30,] "Difference in Means Sample Size (Group 2)" "134"                
      [31,] "Difference in Means"                       "-0.84"              
      [32,] "Difference in Means SE"                    "1.038"              
      [33,] "Difference in Means (SE)"                  "-0.84 (1.038)"      
      [34,] "Difference in Means 95% CI"                "(-2.88, 1.21)"      
      [35,] "Difference in Means (95% CI)"              "-0.84 (-2.88, 1.21)"
      [36,] "WEEK 1 DAY 8"                              ""                   
      [37,] "Difference in Means Sample Size (Group 1)" "134"                
      [38,] "Difference in Means Sample Size (Group 2)" "134"                
      [39,] "Difference in Means"                       "1.44"               
      [40,] "Difference in Means SE"                    "1.091"              
      [41,] "Difference in Means (SE)"                  "1.44 (1.091)"       
      [42,] "Difference in Means 95% CI"                "(-0.71, 3.59)"      
      [43,] "Difference in Means (95% CI)"              "1.44 (-0.71, 3.59)" 
      [44,] "WEEK 2 DAY 15"                             ""                   
      [45,] "Difference in Means Sample Size (Group 1)" "134"                
      [46,] "Difference in Means Sample Size (Group 2)" "134"                
      [47,] "Difference in Means"                       "-0.01"              
      [48,] "Difference in Means SE"                    "0.950"              
      [49,] "Difference in Means (SE)"                  "-0.01 (0.950)"      
      [50,] "Difference in Means 95% CI"                "(-1.88, 1.86)"      
      [51,] "Difference in Means (95% CI)"              "-0.01 (-1.88, 1.86)"
            [,3]         [,4]                 
       [1,] "B: Placebo" "C: Combination"     
       [2,] ""           ""                   
       [3,] ""           ""                   
       [4,] ""           "132"                
       [5,] ""           "134"                
       [6,] ""           "0.67"               
       [7,] ""           "0.965"              
       [8,] ""           "0.67 (0.965)"       
       [9,] ""           "(-1.24, 2.57)"      
      [10,] ""           "0.67 (-1.24, 2.57)" 
      [11,] ""           ""                   
      [12,] ""           "132"                
      [13,] ""           "134"                
      [14,] ""           "-0.82"              
      [15,] ""           "0.959"              
      [16,] ""           "-0.82 (0.959)"      
      [17,] ""           "(-2.71, 1.07)"      
      [18,] ""           "-0.82 (-2.71, 1.07)"
      [19,] ""           ""                   
      [20,] ""           "132"                
      [21,] ""           "134"                
      [22,] ""           "0.26"               
      [23,] ""           "1.029"              
      [24,] ""           "0.26 (1.029)"       
      [25,] ""           "(-1.76, 2.29)"      
      [26,] ""           "0.26 (-1.76, 2.29)" 
      [27,] ""           ""                   
      [28,] ""           ""                   
      [29,] ""           "132"                
      [30,] ""           "134"                
      [31,] ""           "-1.75"              
      [32,] ""           "0.966"              
      [33,] ""           "-1.75 (0.966)"      
      [34,] ""           "(-3.65, 0.15)"      
      [35,] ""           "-1.75 (-3.65, 0.15)"
      [36,] ""           ""                   
      [37,] ""           "132"                
      [38,] ""           "134"                
      [39,] ""           "0.38"               
      [40,] ""           "1.024"              
      [41,] ""           "0.38 (1.024)"       
      [42,] ""           "(-1.63, 2.40)"      
      [43,] ""           "0.38 (-1.63, 2.40)" 
      [44,] ""           ""                   
      [45,] ""           "132"                
      [46,] ""           "134"                
      [47,] ""           "-0.12"              
      [48,] ""           "0.902"              
      [49,] ""           "-0.12 (0.902)"      
      [50,] ""           "(-1.89, 1.66)"      
      [51,] ""           "-0.12 (-1.89, 1.66)"

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
       [9,] "Difference in Means (SE)"                  "-1.84 (0.972)"      
      [10,] "Difference in Means 95% CI"                "(-3.75, 0.07)"      
      [11,] "Difference in Means (95% CI)"              "-1.84 (-3.75, 0.07)"
      [12,] "WEEK 1 DAY 8"                              ""                   
      [13,] "Difference in Means Sample Size (Group 1)" "134"                
      [14,] "Difference in Means Sample Size (Group 2)" "134"                
      [15,] "Difference in Means"                       "0.59"               
      [16,] "Difference in Means SE"                    "0.932"              
      [17,] "Difference in Means (SE)"                  "0.59 (0.932)"       
      [18,] "Difference in Means 95% CI"                "(-1.25, 2.42)"      
      [19,] "Difference in Means (95% CI)"              "0.59 (-1.25, 2.42)" 
      [20,] "WEEK 2 DAY 15"                             ""                   
      [21,] "Difference in Means Sample Size (Group 1)" "134"                
      [22,] "Difference in Means Sample Size (Group 2)" "134"                
      [23,] "Difference in Means"                       "1.12"               
      [24,] "Difference in Means SE"                    "0.996"              
      [25,] "Difference in Means (SE)"                  "1.12 (0.996)"       
      [26,] "Difference in Means 95% CI"                "(-0.84, 3.08)"      
      [27,] "Difference in Means (95% CI)"              "1.12 (-0.84, 3.08)" 
      [28,] "SYSBP"                                     ""                   
      [29,] "BASELINE"                                  ""                   
      [30,] "Difference in Means Sample Size (Group 1)" "134"                
      [31,] "Difference in Means Sample Size (Group 2)" "134"                
      [32,] "Difference in Means"                       "-0.84"              
      [33,] "Difference in Means SE"                    "1.038"              
      [34,] "Difference in Means (SE)"                  "-0.84 (1.038)"      
      [35,] "Difference in Means 95% CI"                "(-2.88, 1.21)"      
      [36,] "Difference in Means (95% CI)"              "-0.84 (-2.88, 1.21)"
      [37,] "WEEK 1 DAY 8"                              ""                   
      [38,] "Difference in Means Sample Size (Group 1)" "134"                
      [39,] "Difference in Means Sample Size (Group 2)" "134"                
      [40,] "Difference in Means"                       "1.44"               
      [41,] "Difference in Means SE"                    "1.091"              
      [42,] "Difference in Means (SE)"                  "1.44 (1.091)"       
      [43,] "Difference in Means 95% CI"                "(-0.71, 3.59)"      
      [44,] "Difference in Means (95% CI)"              "1.44 (-0.71, 3.59)" 
      [45,] "WEEK 2 DAY 15"                             ""                   
      [46,] "Difference in Means Sample Size (Group 1)" "134"                
      [47,] "Difference in Means Sample Size (Group 2)" "134"                
      [48,] "Difference in Means"                       "-0.01"              
      [49,] "Difference in Means SE"                    "0.950"              
      [50,] "Difference in Means (SE)"                  "-0.01 (0.950)"      
      [51,] "Difference in Means 95% CI"                "(-1.88, 1.86)"      
      [52,] "Difference in Means (95% CI)"              "-0.01 (-1.88, 1.86)"
            [,3]                  [,4]        
       [1,] "Active Study Agent"  " "         
       [2,] "C: Combination"      "B: Placebo"
       [3,] ""                    ""          
       [4,] ""                    ""          
       [5,] "132"                 ""          
       [6,] "134"                 ""          
       [7,] "0.67"                ""          
       [8,] "0.965"               ""          
       [9,] "0.67 (0.965)"        ""          
      [10,] "(-1.24, 2.57)"       ""          
      [11,] "0.67 (-1.24, 2.57)"  ""          
      [12,] ""                    ""          
      [13,] "132"                 ""          
      [14,] "134"                 ""          
      [15,] "-0.82"               ""          
      [16,] "0.959"               ""          
      [17,] "-0.82 (0.959)"       ""          
      [18,] "(-2.71, 1.07)"       ""          
      [19,] "-0.82 (-2.71, 1.07)" ""          
      [20,] ""                    ""          
      [21,] "132"                 ""          
      [22,] "134"                 ""          
      [23,] "0.26"                ""          
      [24,] "1.029"               ""          
      [25,] "0.26 (1.029)"        ""          
      [26,] "(-1.76, 2.29)"       ""          
      [27,] "0.26 (-1.76, 2.29)"  ""          
      [28,] ""                    ""          
      [29,] ""                    ""          
      [30,] "132"                 ""          
      [31,] "134"                 ""          
      [32,] "-1.75"               ""          
      [33,] "0.966"               ""          
      [34,] "-1.75 (0.966)"       ""          
      [35,] "(-3.65, 0.15)"       ""          
      [36,] "-1.75 (-3.65, 0.15)" ""          
      [37,] ""                    ""          
      [38,] "132"                 ""          
      [39,] "134"                 ""          
      [40,] "0.38"                ""          
      [41,] "1.024"               ""          
      [42,] "0.38 (1.024)"        ""          
      [43,] "(-1.63, 2.40)"       ""          
      [44,] "0.38 (-1.63, 2.40)"  ""          
      [45,] ""                    ""          
      [46,] "132"                 ""          
      [47,] "134"                 ""          
      [48,] "-0.12"               ""          
      [49,] "0.902"               ""          
      [50,] "-0.12 (0.902)"       ""          
      [51,] "(-1.89, 1.66)"       ""          
      [52,] "-0.12 (-1.89, 1.66)" ""          

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
       [7,] "Difference in Means (SE)"                  "-11.33 (8.413)"         
       [8,] "Difference in Means 95% CI"                "(-47.53, 24.86)"        
       [9,] "Difference in Means (95% CI)"              "-11.33 (-47.53, 24.86)" 
      [10,] "Week 12"                                   ""                       
      [11,] "Difference in Means Sample Size (Group 1)" "4"                      
      [12,] "Difference in Means Sample Size (Group 2)" "4"                      
      [13,] "Difference in Means"                       "-6.00"                  
      [14,] "Difference in Means SE"                    "3.937"                  
      [15,] "Difference in Means (SE)"                  "-6.00 (3.937)"          
      [16,] "Difference in Means 95% CI"                "(-18.53, 6.53)"         
      [17,] "Difference in Means (95% CI)"              "-6.00 (-18.53, 6.53)"   
      [18,] "Week 24"                                   ""                       
      [19,] "Difference in Means Sample Size (Group 1)" "2"                      
      [20,] "Difference in Means Sample Size (Group 2)" "2"                      
      [21,] "Difference in Means"                       "-7.50"                  
      [22,] "Difference in Means SE"                    "9.500"                  
      [23,] "Difference in Means (SE)"                  "-7.50 (9.500)"          
      [24,] "Difference in Means 95% CI"                "(-128.21, 113.21)"      
      [25,] "Difference in Means (95% CI)"              "-7.50 (-128.21, 113.21)"
            [,3]                      [,4]     
       [1,] "Drug Y"                  "Placebo"
       [2,] ""                        ""       
       [3,] "2"                       ""       
       [4,] "2"                       ""       
       [5,] "-15.00"                  ""       
       [6,] "5.000"                   ""       
       [7,] "-15.00 (5.000)"          ""       
       [8,] "(-78.53, 48.53)"         ""       
       [9,] "-15.00 (-78.53, 48.53)"  ""       
      [10,] ""                        ""       
      [11,] "4"                       ""       
      [12,] "4"                       ""       
      [13,] "-2.25"                   ""       
      [14,] "5.072"                   ""       
      [15,] "-2.25 (5.072)"           ""       
      [16,] "(-18.39, 13.89)"         ""       
      [17,] "-2.25 (-18.39, 13.89)"   ""       
      [18,] ""                        ""       
      [19,] "2"                       ""       
      [20,] "2"                       ""       
      [21,] "-5.50"                   ""       
      [22,] "16.500"                  ""       
      [23,] "-5.50 (16.500)"          ""       
      [24,] "(-215.15, 204.15)"       ""       
      [25,] "-5.50 (-215.15, 204.15)" ""       

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

