# s_proportion_factor works as expected

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod row_label
      1        a      2 (40.0%)          0         a
      2        b      1 (20.0%)          0         b

# s_proportion_factor shows optional total row on top

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod row_label
      1    Total              5          0     Total
      2        a      2 (40.0%)          0         a
      3        b      1 (20.0%)          0         b

# s_proportion_factor shows optional total row in bottom

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod row_label
      1        a      2 (40.0%)          0         a
      2        b      1 (20.0%)          0         b
      3      bla              5          0       bla

# s_proportion_factor optionally uses number of non-missing levels as total

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod row_label
      1        a      2 (66.7%)          0         a
      2        b      1 (33.3%)          0         b
      3      foo              3          0       foo

# s_proportion_logical works as expected

    Code
      result
    Output
      $n_prop
      rcell: 2 (40.0%) 
      

# c_proportion_logical works as expected

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod row_label
      1 est_prop      2 (40.0%)          0   Group A

# a_proportion_ci_logical works as expected

    Code
      result
    Output
      rcell: 47.34% - 57.72% 

# a_proportion_ci_factor works as expected

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
                row_name formatted_cell indent_mod        row_label
      1                F   47.2%, 57.8%          0                F
      2                M   42.2%, 52.8%          0                M
      3                U     0.0%, 1.0%          0                U
      4 UNDIFFERENTIATED     0.0%, 1.0%          0 UNDIFFERENTIATED

# prop_split_fun works as expected

    Code
      result
    Output
         n   %   Cum %
      ————————————————

# prop_table_afun works as expected with total row

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod row_label
      1        b          33.33          0         b
      2        a          66.67          0         a
      3                                  0     Total

---

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod row_label
      1        b              1          0         b
      2        a              2          0         a
      3                       3          0     Total

---

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod row_label
      1        b        33.3333          0         b
      2        a       100.0000          0         a
      3                                  0     Total

# prop_table_afun works as expected, by default without total row

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod row_label
      1        b          33.33          0         b
      2        a          66.67          0         a

---

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod row_label
      1        b              1          0         b
      2        a              2          0         a

---

    Code
      result
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod row_label
      1        b        33.3333          0         b
      2        a       100.0000          0         a

