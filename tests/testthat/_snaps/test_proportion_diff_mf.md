# s_test_proportion_diff_mf() removes NAs from relevant columns and warns when na.rm = TRUE

    Code
      s_test_proportion_diff_mf(df = subset(data, grp == "X"), .var = "rsp",
      .ref_group = subset(data, grp == "Placebo"), .in_ref_col = FALSE, na.rm = TRUE,
      variables = list(strata = "strata"))
    Condition
      Warning:
      2 row(s) with missing values were omitted from the non-reference group (df).
      Warning:
      1 row(s) with missing values were omitted from the reference group (df_ref).
    Output
      $pval
      [1] 1
      attr(,"label")
      [1] "(Fisher's Exact Test / Cochran-Mantel-Haenszel Test)"
      
      $executed_method
      [1] "fisher"
      

# a_test_proportion_diff_mf() works in full table build (large sample)

    Code
      tbl
    Output
                                                                 X     Placebo
      ————————————————————————————————————————————————————————————————————————
        (Fisher's Exact Test / Cochran-Mantel-Haenszel Test)   0.739          

# a_test_proportion_diff_mf() works in full table build

    Code
      tbl
    Output
                                                                   X        Placebo
      —————————————————————————————————————————————————————————————————————————————
        (Fisher's Exact Test / Cochran-Mantel-Haenszel Test)   >0.999 {1}          
      —————————————————————————————————————————————————————————————————————————————
      
      {1} - Fisher's Exact Test
      —————————————————————————————————————————————————————————————————————————————
      

# a_test_proportion_diff_mf() respects custom settings (CMH method)

    Code
      tbl
    Output
                       X      Placebo
      ———————————————————————————————
          my_label   0.2641          

# a_test_proportion_diff_mf() respects custom settings

    Code
      tbl
    Output
                         X        Placebo
      ———————————————————————————————————
          my_label   0.7000 {1}          
      ———————————————————————————————————
      
      {1} - Fisher's Exact Test
      ———————————————————————————————————
      

# a_test_proportion_diff_mf() respects custom exact_footnote

    Code
      tbl
    Output
                                                                   X        Placebo
      —————————————————————————————————————————————————————————————————————————————
        (Fisher's Exact Test / Cochran-Mantel-Haenszel Test)   >0.999 {1}          
      —————————————————————————————————————————————————————————————————————————————
      
      {1} - This was the Fisher's exact test
      —————————————————————————————————————————————————————————————————————————————
      

# a_test_proportion_diff_mf() removes NAs from relevant columns and warns when na.rm = TRUE

    Code
      build_table(analyze(split_cols_by(basic_table(), var = "grp", ref_group = "Placebo"),
      vars = "rsp", afun = a_test_proportion_diff_mf, extra_args = list(variables = list(
        strata = "strata"), na.rm = TRUE)), data)
    Condition
      Warning:
      1 row(s) with missing values were omitted from the non-reference group (df).
      Warning:
      1 row(s) with missing values were omitted from the reference group (df_ref).
    Output
                                                               Placebo       X     
      —————————————————————————————————————————————————————————————————————————————
        (Fisher's Exact Test / Cochran-Mantel-Haenszel Test)             >0.999 {1}
      —————————————————————————————————————————————————————————————————————————————
      
      {1} - Fisher's Exact Test
      —————————————————————————————————————————————————————————————————————————————
      

