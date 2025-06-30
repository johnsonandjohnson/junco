# resp01_split_fun_fct works as expected

    Code
      result
    Output
                              Overall                             A: Drug X              B: Placebo           C: Combination   
         Odds Ratio (95% CI)~[super a]   p-value~[super b]   n (%)   95% CI for %   n (%)   95% CI for %   n (%)   95% CI for %
      —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————

---

    Code
      col_info(result)
    Output
      An InstantiatedColumnInfo object
      Columns:
      character(0) -> comp_stat_ci (ID)
      character(0) -> pval (ID)
      A: Drug X (ARM) -> count_prop (ID)
      A: Drug X (ARM) -> prop_ci (ID)
      B: Placebo (ARM) -> count_prop (ID)
      B: Placebo (ARM) -> prop_ci (ID)
      C: Combination (ARM) -> count_prop (ID)
      C: Combination (ARM) -> prop_ci (ID)
      

# resp01_counts_cfun works as expected

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
           row_name formatted_cell indent_mod   row_label
      1 Color: Blue              2          0 Color: Blue

# resp01_a_comp_stat_logical works as expected

    Code
      result
    Output
      rcell: 0.87 (0.61 - 1.24) 

---

    Code
      result
    Output
      rcell: 0.524 

# resp01_a_comp_stat_factor works as expected

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name     formatted_cell indent_mod row_label
      1      CHN 1.00 (0.71 - 1.42)          0       CHN
      2      USA 1.09 (0.64 - 1.86)          0       USA
      3      BRA                             0       BRA
      4      PAK                             0       PAK
      5      NGA                             0       NGA
      6      RUS                             0       RUS
      7      JPN                             0       JPN
      8      GBR                             0       GBR
      9      CAN                             0       CAN

---

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod row_label
      1      CHN         >0.999          0       CHN
      2      USA          0.872          0       USA
      3      BRA                         0       BRA
      4      PAK                         0       PAK
      5      NGA                         0       NGA
      6      RUS                         0       RUS
      7      JPN                         0       JPN
      8      GBR                         0       GBR
      9      CAN                         0       CAN

# resp01_acfun works as expected

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod row_label
      1      CHN    179 (50.3%)          0       CHN
      2      USA     44 (12.4%)          0       USA
      3      BRA      29 (8.1%)          0       BRA
      4      PAK      28 (7.9%)          0       PAK
      5      NGA      24 (6.7%)          0       NGA
      6      RUS      20 (5.6%)          0       RUS
      7      JPN      18 (5.1%)          0       JPN
      8      GBR       7 (2.0%)          0       GBR
      9      CAN       7 (2.0%)          0       CAN

---

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name     formatted_cell indent_mod row_label
      1      CHN 1.00 (0.71 - 1.42)          0       CHN
      2      USA 1.09 (0.64 - 1.86)          0       USA
      3      BRA                             0       BRA
      4      PAK                             0       PAK
      5      NGA                             0       NGA
      6      RUS                             0       RUS
      7      JPN                             0       JPN
      8      GBR                             0       GBR
      9      CAN                             0       CAN

---

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name     formatted_cell indent_mod row_label
      1      bla 1.08 (0.40 - 2.91)          0       bla

