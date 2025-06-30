# cmp_split_fun works as expected

    Code
      result
    Output
         Expected, N   Received, n (%)   Missing, n (%)
      —————————————————————————————————————————————————

# cmp_cfun works correctly for expected column

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod row_label
      1  Visit 1              2          0   Visit 1

# cmp_cfun works correctly for received column

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod row_label
      1  Visit 1      1 (50.0%)          0   Visit 1

# cmp_cfun works correctly for missing column

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod row_label
      1  Visit 1      1 (50.0%)          0   Visit 1

