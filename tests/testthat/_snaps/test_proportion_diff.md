# s_test_proportion_diff works as expected

    Code
      result
    Output
      $pval
      [1] 0.6477165
      attr(,"label")
      [1] "p-value (Cochran-Mantel-Haenszel Test)"
      

---

    Code
      result
    Output
      $pval
      [1] 0.6761418
      attr(,"label")
      [1] "p-value (Cochran-Mantel-Haenszel Test, 1-sided, direction greater)"
      

---

    Code
      result
    Output
      $pval
      list()
      attr(,"label")
      [1] "p-value (Cochran-Mantel-Haenszel Test, 1-sided, direction greater)"
      

# a_test_proportion_diff works as expected in table layout

    Code
      result
    Output
                                                   A     B
      ————————————————————————————————————————————————————
        p-value (Cochran-Mantel-Haenszel Test)   0.648    
