# lsmeans_wide_cfun works as expected

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod   row_label
      1        1        Placebo          0 Day 15 (DB)

---

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod   row_label
      1        1            0.7          0 Day 15 (DB)

---

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod   row_label
      1        1            0.3          0 Day 15 (DB)

---

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod   row_label
      1        1            0.7          0 Day 15 (DB)

---

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod   row_label
      1        1           62.1          0 Day 15 (DB)

# lsmeans_wide_cfun works as expected with more than one treatment group

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod   row_label
      1        1        Placebo          0 Day 15 (DB)
      2        2                         0            

---

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod   row_label
      1        1            0.7          0 Day 15 (DB)
      2        2            0.6          0            

---

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod   row_label
      1        1            387          0 Day 15 (DB)
      2        2                         0            

# lsmeans_wide_cfun works as expected with more than one treatment group for MMRM case

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod   row_label
      1        1        Placebo          0 Day 15 (DB)
      2        2                         0            

---

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod   row_label
      1        1            0.7          0 Day 15 (DB)
      2        2            0.6          0            

---

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod   row_label
      1        1            387          0 Day 15 (DB)
      2        2            343          0            

---

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod   row_label
      1        1           62.1          0 Day 15 (DB)
      2        2           55.1          0            

# summarize_lsmeans_wide works as expected

    Code
      result
    Output
                 Reference Group             Testing Group                                                     Testing - Reference                   
             Treatment   N    LS Mean   Treatment   N    LS Mean   M. S. Error   Error DF   LS Mean    SE       80% CI      2-sided p-value~[super a]
      ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      VIS1      PBO      68    33.2        TRT      66    36.8        41.2         129        3.7     1.13   (1.80, 5.56)             0.001          
      
      VIS2      PBO      69    38.0        TRT      71    42.3        26.0         135        4.2     0.88   (2.79, 5.70)            <0.001          
      
      VIS3      PBO      71    43.8        TRT      58    46.8        15.0         124        3.1     0.70   (1.89, 4.22)            <0.001          
      
      VIS4      PBO      67    48.7        TRT      67    52.5        94.6         129        3.9     1.70   (1.07, 6.71)             0.024          

# summarize_lsmeans_wide works as expected with more than 1 treatment group

    Code
      result
    Output
                 Reference Group             Testing Group                                                     Testing - Reference                   
             Treatment   N    LS Mean   Treatment   N    LS Mean   M. S. Error   Error DF   LS Mean    SE       80% CI      2-sided p-value~[super a]
      ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      VIS1      PBO      68    33.0        TRT      66    36.5        74.3         194        3.5     1.51   (1.41, 5.53)             0.044          
                                          TRT2      66    37.5                                4.4     1.51   (2.36, 6.48)             0.008          
      
      VIS2      PBO      69    37.6        TRT      71    41.9        51.7         205        4.3     1.23   (2.58, 5.94)             0.001          
                                          TRT2      71    42.4                                4.8     1.23   (3.12, 6.47)            <0.001          
      
      VIS3      PBO      71    43.6        TRT      58    46.6        47.6         181        3.0     1.25   (1.25, 4.65)             0.036          
                                          TRT2      58    49.2                                5.5     1.25   (3.85, 7.24)            <0.001          
      
      VIS4      PBO      67    48.5        TRT      67    52.2        129.6        195        3.7     1.98   (1.04, 6.45)             0.111          
                                          TRT2      67    52.9                                4.4     1.98   (1.68, 7.08)             0.054          

# summarize_lsmeans_wide can omit variance and p-value columns

    Code
      result
    Output
                 Reference Group             Testing Group              Testing - Reference     
             Treatment   N    LS Mean   Treatment   N    LS Mean   LS Mean    SE       80% CI   
      ——————————————————————————————————————————————————————————————————————————————————————————
      VIS1      PBO      68    33.2        TRT      66    36.8       3.7     1.13   (1.80, 5.56)
      
      VIS2      PBO      69    38.0        TRT      71    42.3       4.2     0.88   (2.79, 5.70)
      
      VIS3      PBO      71    43.8        TRT      58    46.8       3.1     0.70   (1.89, 4.22)
      
      VIS4      PBO      67    48.7        TRT      67    52.5       3.9     1.70   (1.07, 6.71)

