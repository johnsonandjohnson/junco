# get_ref_info works with a df analysis function

    Code
      result
    Output
                                   Active Study Agent                 
                               A: Drug X   C: Combination   B: Placebo
      ————————————————————————————————————————————————————————————————
      Difference of Averages     1.89           1.55                  

---

    Code
      std_result
    Output
                               A: Drug X   B: Placebo   C: Combination
      ————————————————————————————————————————————————————————————————
      Difference of Averages     1.89                        1.55     

# get_ref_info works with a vector analysis function

    Code
      result
    Output
                                     Active Study Agent                 
                                 A: Drug X   C: Combination   B: Placebo
      ——————————————————————————————————————————————————————————————————
      AGE                                                               
        Difference of Averages     1.89           1.55                  
      BMRKR1                                                            
        Difference of Averages     -0.32         -0.42                  

---

    Code
      std_result
    Output
                                 A: Drug X   B: Placebo   C: Combination
      ——————————————————————————————————————————————————————————————————
      AGE                                                               
        Difference of Averages     1.89                        1.55     
      BMRKR1                                                            
        Difference of Averages     -0.32                      -0.42     

