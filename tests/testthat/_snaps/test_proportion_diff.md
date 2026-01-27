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

# prop_chisq returns right result

    Code
      res
    Output
      [1] 0.05653028

---

    Code
      res
    Output
      [1] 0.9717349

---

    Code
      res
    Output
      [1] 0.02826514

# prop_cmh returns right result

    Code
      res
    Output
      [1] 0.6477165

---

    Code
      res
    Output
      [1] 0.6761418

---

    Code
      res
    Output
      [1] 0.3238582

# prop_cmh also works when there are strata with just one observation

    Code
      res
    Output
      [1] 0.3325724

---

    Code
      res
    Output
      [1] 0.8337138

---

    Code
      res
    Output
      [1] 0.1662862

# prop_fisher returns right result

    Code
      res
    Output
      [1] 0.1109695

---

    Code
      res
    Output
      [1] 0.9875791

---

    Code
      res
    Output
      [1] 0.05548477

