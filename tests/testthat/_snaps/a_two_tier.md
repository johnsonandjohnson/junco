# a_three_tier produces the expected table layout

    Code
      matrix_form(res)$string
    Output
            [,1]                 [,2] [,3] [,4]
       [1,] ""                   "A"  "B"  "C" 
       [2,] "COMPLETED"          "3"  "2"  "3" 
       [3,] "DISCONTINUED"       "9"  "5"  "3" 
       [4,] "ADVERSE EVENT"      "3"  "2"  "0" 
       [5,] "MILD"               "1"  "2"  "0" 
       [6,] "SEVERE"             "2"  "0"  "0" 
       [7,] "LACK OF EFFICACY"   "4"  "0"  "2" 
       [8,] "MILD"               "2"  "0"  "1" 
       [9,] "MODERATE"           "1"  "0"  "1" 
      [10,] "SEVERE"             "1"  "0"  "0" 
      [11,] "PHYSICIAN DECISION" "2"  "3"  "1" 
      [12,] "MILD"               "1"  "1"  "0" 
      [13,] "MODERATE"           "0"  "2"  "1" 
      [14,] "SEVERE"             "1"  "0"  "0" 
      [15,] "ONGOING"            "3"  "5"  "3" 

# a_three_tier produces the expected layout when a level has no observations

    Code
      matrix_form(res)$string
    Output
            [,1]                 [,2] [,3] [,4]
       [1,] ""                   "A"  "B"  "C" 
       [2,] "COMPLETED"          "3"  "1"  "1" 
       [3,] "DISCONTINUED"       "6"  "3"  "3" 
       [4,] "LACK OF EFFICACY"   "4"  "0"  "2" 
       [5,] "MILD"               "2"  "0"  "1" 
       [6,] "MODERATE"           "1"  "0"  "1" 
       [7,] "SEVERE"             "1"  "0"  "0" 
       [8,] "PHYSICIAN DECISION" "2"  "3"  "1" 
       [9,] "MILD"               "1"  "1"  "0" 
      [10,] "MODERATE"           "0"  "2"  "1" 
      [11,] "SEVERE"             "1"  "0"  "0" 
      [12,] "ONGOING"            "2"  "4"  "2" 

# a_three_tier produces the expected layout when a level has no observations (use_all_levels)

    Code
      matrix_form(res)$string
    Output
            [,1]                 [,2] [,3] [,4]
       [1,] ""                   "A"  "B"  "C" 
       [2,] "COMPLETED"          "3"  "1"  "1" 
       [3,] "DISCONTINUED"       "6"  "3"  "3" 
       [4,] "LACK OF EFFICACY"   "4"  "0"  "2" 
       [5,] "MILD"               "2"  "0"  "1" 
       [6,] "MODERATE"           "1"  "0"  "1" 
       [7,] "SEVERE"             "1"  "0"  "0" 
       [8,] "PHYSICIAN DECISION" "2"  "3"  "1" 
       [9,] "MILD"               "1"  "1"  "0" 
      [10,] "MODERATE"           "0"  "2"  "1" 
      [11,] "SEVERE"             "1"  "0"  "0" 
      [12,] "ONGOING"            "2"  "4"  "2" 

