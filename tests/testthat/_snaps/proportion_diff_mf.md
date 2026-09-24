# s_proportion_diff_mf() removes NAs from relevant columns and warns when na.rm = TRUE

    Code
      s_proportion_diff_mf(df = subset(data, grp == "X"), .var = "rsp", .ref_group = subset(
        data, grp == "Placebo"), .in_ref_col = FALSE, na.rm = TRUE, variables = list(
        strata = "strata"))
    Condition
      Warning:
      2 row(s) with missing values were omitted from the non-reference group (df).
      Warning:
      1 row(s) with missing values were omitted from the reference group (df_ref).
    Output
      $diff
      diff_uncond_exact_diff 
                          50 
      attr(,"label")
      [1] "Difference in Response rate (%) (CMH, without correction / Unconditional exact)"
      
      $diff_ci
      diff_ci_uncond_exact_diff_l diff_ci_uncond_exact_diff_u 
                        -76.97735                    98.73896 
      attr(,"label")
      [1] "Difference in Response rate (%) 95% CI (CMH, without correction / Unconditional exact)"
      
      $diff_est_ci
           diff_uncond_exact_diff diff_ci_uncond_exact_diff_l 
                         50.00000                   -76.97735 
      diff_ci_uncond_exact_diff_u 
                         98.73896 
      attr(,"label")
      [1] "Difference in Response rate (%) and 95% CI (CMH, without correction / Unconditional exact)"
      
      $executed_method
      [1] "uncond_exact_diff"
      

# a_proportion_diff_mf() works in full table build

    Code
      tbl
    Output
                                                                                                             X              Placebo
      —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Difference in Response rate (%) (CMH, without correction / Unconditional exact)                     16.7 {+}                 
        Difference in Response rate (%) 95% CI (CMH, without correction / Unconditional exact)       (-69.8, 87.6) {+}             
      Difference in Response rate (%) and 95% CI (CMH, without correction / Unconditional exact)   16.7 (-69.8, 87.6) {+}          
      —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      
      {+} - Unconditional exact
      —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      

# a_proportion_diff_mf() respects custom settings

    Code
      tbl
    Output
                                 X                Placebo
      ———————————————————————————————————————————————————
          my_label   16.67 (-61.04 - 82.13) {+}          
      ———————————————————————————————————————————————————
      
      {+} - Unconditional exact
      ———————————————————————————————————————————————————
      

# a_proportion_diff_mf() respects custom exact_footnote

    Code
      tbl
    Output
                                                                                                             X              Placebo
      —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Difference in Response rate (%) (CMH, without correction / Unconditional exact)                     16.7 {1}                 
        Difference in Response rate (%) 95% CI (CMH, without correction / Unconditional exact)       (-69.8, 87.6) {1}             
      Difference in Response rate (%) and 95% CI (CMH, without correction / Unconditional exact)   16.7 (-69.8, 87.6) {1}          
      —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      
      {1} - This was the exact method
      —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      

# a_proportion_diff_mf() removes NAs from relevant columns and warns when na.rm = TRUE

    Code
      build_table(analyze(split_cols_by(basic_table(), var = "grp", ref_group = "Placebo"),
      vars = "rsp", afun = a_proportion_diff_mf, extra_args = list(variables = list(
        strata = "strata"), na.rm = TRUE)), data)
    Condition
      Warning:
      1 row(s) with missing values were omitted from the non-reference group (df).
      Warning:
      1 row(s) with missing values were omitted from the reference group (df_ref).
    Output
                                                                                                   Placebo             X           
      —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Difference in Response rate (%) (CMH, without correction / Unconditional exact)                               50.0 {+}       
        Difference in Response rate (%) 95% CI (CMH, without correction / Unconditional exact)                 (-77.0, 98.7) {+}   
      Difference in Response rate (%) and 95% CI (CMH, without correction / Unconditional exact)             50.0 (-77.0, 98.7) {+}
      —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      
      {+} - Unconditional exact
      —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      

