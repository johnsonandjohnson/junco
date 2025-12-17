# jjjcs formats work

    Code
      format_value(values[1], format = jjcsformat_xx("xx"))
    Output
      [1] "5.123456"
    Code
      format_value(values[1], format = jjcsformat_xx("xx."))
    Output
      [1] "5"
    Code
      format_value(values[1], format = jjcsformat_xx("xx.x"))
    Output
      [1] "5.1"
    Code
      format_value(values[1], format = jjcsformat_xx("xx.xx"))
    Output
      [1] "5.12"
    Code
      format_value(values[1], format = jjcsformat_xx("xx.xxx"))
    Output
      [1] "5.123"
    Code
      format_value(values[1], format = jjcsformat_xx("xx.xxxx"))
    Output
      [1] "5.1235"
    Code
      format_value(values, format = jjcsformat_xx("(xx, xx)"))
    Output
      [1] "(5.123456, 7.891112)"
    Code
      format_value(values, format = jjcsformat_xx("(xx., xx.)"))
    Output
      [1] "(5, 8)"
    Code
      format_value(values, format = jjcsformat_xx("(xx.x, xx.x)"))
    Output
      [1] "(5.1, 7.9)"
    Code
      format_value(values, format = jjcsformat_xx("(xx.xx, xx.xx)"))
    Output
      [1] "(5.12, 7.89)"
    Code
      format_value(values, format = jjcsformat_xx("(xx.xxx, xx.xxx)"))
    Output
      [1] "(5.123, 7.891)"
    Code
      format_value(values, format = jjcsformat_xx("(xx.xxxx, xx.xxxx)"))
    Output
      [1] "(5.1235, 7.8911)"
    Code
      format_value(values, format = jjcsformat_xx("xx - xx"))
    Output
      [1] "5.123456 - 7.891112"
    Code
      format_value(values, format = jjcsformat_xx("xx.x - xx.x"))
    Output
      [1] "5.1 - 7.9"
    Code
      format_value(values, format = jjcsformat_xx("xx.xx - xx.xx"))
    Output
      [1] "5.12 - 7.89"
    Code
      format_value(values, format = jjcsformat_xx("xx (xx)"))
    Output
      [1] "5.123456 (7.891112)"
    Code
      format_value(values, format = jjcsformat_xx("xx (xx.)"))
    Output
      [1] "5.123456 (8)"
    Code
      format_value(values, format = jjcsformat_xx("xx (xx.x)"))
    Output
      [1] "5.123456 (7.9)"
    Code
      format_value(values, format = jjcsformat_xx("xx (xx.xx)"))
    Output
      [1] "5.123456 (7.89)"
    Code
      format_value(values, format = jjcsformat_xx("xx. (xx.)"))
    Output
      [1] "5 (8)"
    Code
      format_value(values, format = jjcsformat_xx("xx.x (xx.x)"))
    Output
      [1] "5.1 (7.9)"
    Code
      format_value(values, format = jjcsformat_xx("xx.xx (xx.xx)"))
    Output
      [1] "5.12 (7.89)"
    Code
      format_value(values, format = jjcsformat_xx("xx.x, xx.x"))
    Output
      [1] "5.1, 7.9"
    Code
      format_value(values, format = jjcsformat_xx("xx.x to xx.x"))
    Output
      [1] "5.1 to 7.9"
    Code
      format_value(c(values, 10.1235), format = jjcsformat_xx("xx. (xx. - xx.)"))
    Output
      [1] "5 (8 - 10)"
    Code
      format_value(c(values, 10.1235), format = jjcsformat_xx("xx.x (xx.x - xx.x)"))
    Output
      [1] "5.1 (7.9 - 10.1)"
    Code
      format_value(c(values, 10.1235), format = jjcsformat_xx("xx.xx (xx.xx - xx.xx)"))
    Output
      [1] "5.12 (7.89 - 10.12)"
    Code
      format_value(c(values, 10.1235), format = jjcsformat_xx(
        "xx.xxx (xx.xxx - xx.xxx)"))
    Output
      [1] "5.123 (7.891 - 10.124)"

---

    Code
      format_value(NA, "xx.", na_str = "-")
    Output
      [1] "-"
    Code
      format_value(NA, "xx", na_str = "-")
    Output
      [1] "-"
    Code
      format_value(c(1, NA), "xx")
    Output
      [1] "1"  "NA"

---

    Code
      format_value(0, "xx.")
    Output
      [1] "0"
    Code
      format_value(0, "xx.x")
    Output
      [1] "0.0"
    Code
      format_value(0, "xx.xx")
    Output
      [1] "0.00"
    Code
      format_value(0, "xx.xxx")
    Output
      [1] "0.000"
    Code
      format_value(0, "xx.xxxx")
    Output
      [1] "0.0000"

---

    Code
      format_value(c(NA, NA), format = jjcsformat_xx("xx.x - xx.x"), na_str = c("hi",
        "lo"))
    Output
      [1] "hi - lo"
    Code
      format_value(c(NA, 5.2), format = jjcsformat_xx("xx.x - xx.x"), na_str = "what")
    Output
      [1] "what - 5.2"
    Code
      format_value(c(NA, 5.2), format = jjcsformat_xx("xx.x - xx.x"), na_str = c("hi",
        "lo"))
    Output
      [1] "hi - 5.2"
    Code
      format_value(c(NA, NA), format = jjcsformat_xx("xx.x - xx.x"), na_str = "what")
    Output
      [1] "what"
    Code
      format_value(NA, format = jjcsformat_xx("xx.x"), na_str = character())
    Output
      [1] "NA"
    Code
      format_value(NA, format = jjcsformat_xx("xx.x"), na_str = NA_character_)
    Output
      [1] "NA"

# jjcsformat_range_fct is formatting ranges as expected

    Code
      my_range_format(c(0.35235, 99.2342, 1, 0))
    Output
      [1] "(0.35+, 99.23)"
    Code
      my_range_format(c(0.35235, 99.2342, 0, 1))
    Output
      [1] "(0.35, 99.23+)"
    Code
      my_range_format(c(0.35235, 99.2342, 0, 0))
    Output
      [1] "(0.35, 99.23)"
    Code
      my_range_format(c(0.35235, 99.2342, 1, 1))
    Output
      [1] "(0.35+, 99.23+)"
    Code
      my_range_format2(c(0.35235, 99.2342, 0, 1))
    Output
      [1] "(0.35, 99.23*)"

# jjcsformat_pval_fct works

    Code
      jjcsformat_pval_fct(0.005)(0.0048)
    Output
      [1] "0.0048"
    Code
      jjcsformat_pval_fct(0.005)(0.00499)
    Output
      [1] "0.00499"
    Code
      jjcsformat_pval_fct(0.005)(0.000499)
    Output
      [1] "<0.001"
    Code
      jjcsformat_pval_fct(0)(0.0048)
    Output
      [1] "0.005"
    Code
      jjcsformat_pval_fct(0.05)(0.0048)
    Output
      [1] "0.005"
    Code
      jjcsformat_pval_fct(0.005)(0.0051)
    Output
      [1] "0.005"
    Code
      jjcsformat_pval_fct(0)(1e-05)
    Output
      [1] "<0.001"
    Code
      jjcsformat_pval_fct(0)(0.0009999999)
    Output
      [1] "<0.001"
    Code
      jjcsformat_pval_fct(0)(0.001)
    Output
      [1] "0.001"
    Code
      jjcsformat_pval_fct(0)(0.9999)
    Output
      [1] ">0.999"
    Code
      jjcsformat_pval_fct(0)(0.999)
    Output
      [1] "0.999"
    Code
      jjcsformat_pval_fct(0)(0.9990000001)
    Output
      [1] ">0.999"
    Code
      jjcsformat_pval_fct(0)(NA_real_, na_str = "ne")
    Output
      [1] "ne"
    Code
      jjcsformat_pval_fct(5e-04)(NA_real_, na_str = "ne")
    Output
      [1] "ne"
    Code
      jjcsformat_pval_fct(0.005)(0.004999999)
    Output
      [1] "0.004999999"
    Code
      jjcsformat_pval_fct(0.005)(0.0049999999)
    Output
      [1] "0.0049999999"
    Code
      jjcsformat_pval_fct(0.005)(0.00499999999)
    Output
      [1] "0.0050000000"

