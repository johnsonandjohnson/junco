# s_proportion_diff_mf() removes NAs and warns when na.rm = TRUE

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
      

