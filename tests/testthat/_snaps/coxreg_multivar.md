# tefos03_first_split_fun works as expected

    Code
      col_info(result)
    Output
      An InstantiatedColumnInfo object
      Columns:
      model_fit (ID)
      hazard_ratio (ID)
      

---

    Code
      result
    Output
         Model Fit   Hazard Ratio
      ———————————————————————————

# tefos03_second_split_fun_fct works as expected

    Code
      col_info(result)
    Output
      An InstantiatedColumnInfo object
      Columns:
      model_fit (ID) -> coef_se (ID)
      model_fit (ID) -> pval (ID)
      hazard_ratio (ID) -> hr_est (ID)
      hazard_ratio (ID) -> hr_ci (ID)
      

---

    Code
      result
    Output
               Model Fit           Hazard Ratio   
         Coeff. (SE)   p-value   Estimate   92% CI
      ————————————————————————————————————————————

# tefos03_afun works as expected

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
                                        row_name formatted_cell indent_mod
      1     Treatment (B: Placebo vs. A: Drug X)         <0.001          0
      2 Treatment (C: Combination vs. A: Drug X)         <0.001          0
      3                            Sex (M vs. F)          0.098          0
      4                                      Age          0.642          0
                                       row_label
      1     Treatment (B: Placebo vs. A: Drug X)
      2 Treatment (C: Combination vs. A: Drug X)
      3                            Sex (M vs. F)
      4                                      Age

# summarize_coxreg_multivar works as expected with defaults

    Code
      result
    Output
                                                         Model Fit               Hazard Ratio      
                                                   Coeff. (SE)    p-value   Estimate      95% CI   
      —————————————————————————————————————————————————————————————————————————————————————————————
      Model Parameter                                                                              
        Treatment (B: Placebo vs. A: Drug X)       0.42 (0.09)    <0.001      1.52     (1.26, 1.83)
        Treatment (C: Combination vs. A: Drug X)   0.66 (0.10)    <0.001      1.93     (1.59, 2.34)
        Sex (M vs. F)                              -0.13 (0.08)    0.098      0.88     (0.76, 1.02)
        Age                                        0.00 (0.01)     0.645      1.00     (0.99, 1.01)

# summarize_coxreg_multivar works as expected with custom options

    Code
      result
    Output
                                                         Model Fit             Hazard Ratio     
                                                   Coeff. (SE)   p-value   Estimate     50% CI  
      ——————————————————————————————————————————————————————————————————————————————————————————
      Model Parameter                                                                           
        Treatment (B: Placebo vs. A: Drug X)        0.4 (0.1)    <0.001     1.5170    (1.4, 1.6)
        Treatment (C: Combination vs. A: Drug X)    0.7 (0.1)    <0.001     1.9203    (1.8, 2.1)
        Sex (M vs. F)                              -0.1 (0.1)     0.099     0.8813    (0.8, 0.9)
        Age                                         0.0 (0.0)     0.646     1.0024    (1.0, 1.0)

# summarize_coxreg_multivar works with row splits

    Code
      result
    Output
                                                           Model Fit               Hazard Ratio      
                                                     Coeff. (SE)    p-value   Estimate      95% CI   
      ———————————————————————————————————————————————————————————————————————————————————————————————
      LOW                                                                                            
        Model Parameter                                                                              
          Treatment (B: Placebo vs. A: Drug X)       0.38 (0.16)     0.016      1.46     (1.07, 2.00)
          Treatment (C: Combination vs. A: Drug X)   0.67 (0.17)    <0.001      1.94     (1.40, 2.70)
          Sex (M vs. F)                              -0.17 (0.13)    0.204      0.84     (0.65, 1.10)
          Age                                        0.01 (0.01)     0.189      1.01     (0.99, 1.04)
      MEDIUM                                                                                         
        Model Parameter                                                                              
          Treatment (B: Placebo vs. A: Drug X)       0.50 (0.16)     0.002      1.65     (1.20, 2.26)
          Treatment (C: Combination vs. A: Drug X)   0.65 (0.17)    <0.001      1.91     (1.37, 2.66)
          Sex (M vs. F)                              -0.15 (0.14)    0.280      0.86     (0.66, 1.13)
          Age                                        -0.01 (0.01)    0.236      0.99     (0.97, 1.01)
      HIGH                                                                                           
        Model Parameter                                                                              
          Treatment (B: Placebo vs. A: Drug X)       0.39 (0.18)     0.030      1.47     (1.04, 2.09)
          Treatment (C: Combination vs. A: Drug X)   0.58 (0.19)     0.003      1.78     (1.22, 2.59)
          Sex (M vs. F)                              -0.07 (0.14)    0.613      0.93     (0.70, 1.23)
          Age                                        0.00 (0.01)     0.598      1.00     (0.99, 1.02)

