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

# get_ref_info works with a df in the presence of the overall column

    Code
      result
    Output
                                           Active Study Agent                         
                                       A: Drug X   C: Combination   B: Placebo   Total
      ————————————————————————————————————————————————————————————————————————————————
      Mean                               34.91         34.57          33.02      34.22
      Difference in Means vs Placebo     1.89           1.55                          

---

    Code
      std_result
    Output
                                       A: Drug X   B: Placebo   C: Combination   Total
      ————————————————————————————————————————————————————————————————————————————————
      Mean                               34.91       33.02          34.57        34.22
      Difference in Means vs Placebo     1.89                        1.55             

