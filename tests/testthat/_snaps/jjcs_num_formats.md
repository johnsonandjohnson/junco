# jjcs_num_formats works

    Code
      jjcs_num_formats(d = 0)$spec
    Output
               range        mean_sd             sd         median 
          "xx., xx." "xx.x (xx.xx)"        "xx.xx"         "xx.x" 
    Code
      jjcs_num_formats(d = 1)$spec
    Output
                 range          mean_sd               sd           median 
          "xx.x, xx.x" "xx.xx (xx.xxx)"         "xx.xxx"          "xx.xx" 
    Code
      jjcs_num_formats(d = 8, cap = 2)$spec
    Output
                   range            mean_sd                 sd             median 
          "xx.xx, xx.xx" "xx.xxx (xx.xxxx)"          "xx.xxxx"           "xx.xxx" 
    Code
      jjcs_num_formats(NA)$spec
    Output
          range   mean_sd        sd    median 
       "xx, xx" "xx (xx)"      "xx"      "xx" 

---

    Code
      format_value(values, format = jjcsformat_xx_SAS("xx.x (xx.xx)"))
    Output
      [1] "5.1 (7.89)"
    Code
      format_value(values, format = jjcsformat_xx_R("xx.x (xx.xx)"))
    Output
      [1] "5.1 (7.89)"
    Code
      format_value(c(5.05, values[2]), format = jjcsformat_xx_SAS("xx.x (xx.xx)"))
    Output
      [1] "5.1 (7.89)"
    Code
      format_value(c(5.05, values[2]), format = jjcsformat_xx_R("xx.x (xx.xx)"))
    Output
      [1] "5.0 (7.89)"
    Code
      format_value(c(5.15, values[2]), format = jjcsformat_xx_R("xx.x (xx.xx)"))
    Output
      [1] "5.2 (7.89)"
    Code
      format_value(c(5.15, values[2]), format = "xx.x (xx.x)")
    Output
      [1] "5.2 (7.9)"
    Code
      format_value(c(4.15, values[2]), format = jjcsformat_xx_SAS("xx.x (xx.xx)"))
    Output
      [1] "4.2 (7.89)"
    Code
      format_value(c(4.15, values[2]), format = jjcsformat_xx_R("xx.x (xx.xx)"))
    Output
      [1] "4.2 (7.89)"
    Code
      format_value(c(4.15, values[2]), format = "xx.x (xx.x)")
    Output
      [1] "4.2 (7.9)"
    Code
      format_value(c(4.15, values[2]), format = jjcsformat_xx_SAS("xx.x (xx.x)"))
    Output
      [1] "4.2 (7.9)"
    Code
      format_value(c(3.15, values[2]), format = "xx.x (xx.x)")
    Output
      [1] "3.1 (7.9)"
    Code
      format_value(c(3.15, values[2]), format = jjcsformat_xx_SAS("xx.x (xx.x)"))
    Output
      [1] "3.2 (7.9)"
    Code
      format_value(c(3.15, values[2]), format = jjcsformat_xx_R("xx.x (xx.x)"))
    Output
      [1] "3.1 (7.9)"

---

    Code
      format_value(values, format = jjcsformat_xx_SAS("xx / xx"))
    Output
      [1] "5.123456 / 7.891112"
    Code
      format_value(values, format = jjcsformat_xx_SAS("xx. / xx."))
    Output
      [1] "5 / 8"
    Code
      format_value(values, format = jjcsformat_xx_SAS("xx.x / xx.x"))
    Output
      [1] "5.1 / 7.9"
    Code
      format_value(values, format = jjcsformat_xx_SAS("xx.xx / xx.xx"))
    Output
      [1] "5.12 / 7.89"
    Code
      format_value(values, format = jjcsformat_xx_SAS("xx.xxx / xx.xxx"))
    Output
      [1] "5.123 / 7.891"
    Code
      format_value(values, format = jjcsformat_xx_SAS("(xx, xx)"))
    Output
      [1] "(5.123456, 7.891112)"
    Code
      format_value(values, format = jjcsformat_xx_SAS("(xx., xx.)"))
    Output
      [1] "(5, 8)"
    Code
      format_value(values, format = jjcsformat_xx_SAS("(xx.x, xx.x)"))
    Output
      [1] "(5.1, 7.9)"
    Code
      format_value(values, format = jjcsformat_xx_SAS("(xx.xx, xx.xx)"))
    Output
      [1] "(5.12, 7.89)"
    Code
      format_value(values, format = jjcsformat_xx_SAS("(xx.xxx, xx.xxx)"))
    Output
      [1] "(5.123, 7.891)"
    Code
      format_value(values, format = jjcsformat_xx_SAS("(xx.xxxx, xx.xxxx)"))
    Output
      [1] "(5.1235, 7.8911)"
    Code
      format_value(values, format = jjcsformat_xx_SAS("xx - xx"))
    Output
      [1] "5.123456 - 7.891112"
    Code
      format_value(values, format = jjcsformat_xx_SAS("xx.x - xx.x"))
    Output
      [1] "5.1 - 7.9"
    Code
      format_value(values, format = jjcsformat_xx_SAS("xx.xx - xx.xx"))
    Output
      [1] "5.12 - 7.89"
    Code
      format_value(values, format = jjcsformat_xx_SAS("xx (xx)"))
    Output
      [1] "5.123456 (7.891112)"
    Code
      format_value(values, format = jjcsformat_xx_SAS("xx (xx.)"))
    Output
      [1] "5.123456 (8)"
    Code
      format_value(values, format = jjcsformat_xx_SAS("xx (xx.x)"))
    Output
      [1] "5.123456 (7.9)"
    Code
      format_value(values, format = jjcsformat_xx_SAS("xx (xx.xx)"))
    Output
      [1] "5.123456 (7.89)"
    Code
      format_value(values, format = jjcsformat_xx_SAS("xx. (xx.)"))
    Output
      [1] "5 (8)"
    Code
      format_value(values, format = jjcsformat_xx_SAS("xx.x (xx.x)"))
    Output
      [1] "5.1 (7.9)"
    Code
      format_value(values, format = jjcsformat_xx_SAS("xx.xx (xx.xx)"))
    Output
      [1] "5.12 (7.89)"
    Code
      format_value(values, format = jjcsformat_xx_SAS("xx.x, xx.x"))
    Output
      [1] "5.1, 7.9"
    Code
      format_value(values, format = jjcsformat_xx_SAS("xx.x to xx.x"))
    Output
      [1] "5.1 to 7.9"
    Code
      format_value(c(values, 10.1235), format = jjcsformat_xx_SAS("xx. (xx. - xx.)"))
    Output
      [1] "5 (8 - 10)"
    Code
      format_value(c(values, 10.1235), format = jjcsformat_xx_SAS(
        "xx.x (xx.x - xx.x)"))
    Output
      [1] "5.1 (7.9 - 10.1)"
    Code
      format_value(c(values, 10.1235), format = jjcsformat_xx_SAS(
        "xx.xx (xx.xx - xx.xx)"))
    Output
      [1] "5.12 (7.89 - 10.12)"
    Code
      format_value(c(values, 10.1235), format = jjcsformat_xx_SAS(
        "xx.xxx (xx.xxx - xx.xxx)"))
    Output
      [1] "5.123 (7.891 - 10.124)"
    Code
      format_value(NULL, jjcsformat_xx_SAS("xx"))
    Output
      [1] ""
    Code
      format_value(c(500), jjcsformat_xx_SAS("N=xx"))
    Output
      [1] "N=500"
    Code
      format_value(c(500), jjcsformat_xx_SAS("(N=xx)"))
    Output
      [1] "(N=500)"

---

    Code
      format_value(0, jjcsformat_xx_SAS("xx."))
    Output
      [1] "0"
    Code
      format_value(0, jjcsformat_xx_SAS("xx.x"))
    Output
      [1] "0.0"
    Code
      format_value(0, jjcsformat_xx_SAS("xx.xx"))
    Output
      [1] "0.00"
    Code
      format_value(0, jjcsformat_xx_SAS("xx.xxx"))
    Output
      [1] "0.000"
    Code
      format_value(0, jjcsformat_xx_SAS("xx.xxxx"))
    Output
      [1] "0.0000"

# jjcsformats NA works

    Code
      format_value(NA, jjcsformat_xx_SAS("xx."), na_str = "-")
    Output
      [1] "-"
    Code
      format_value(NA, jjcsformat_xx_SAS("xx"), na_str = "-")
    Output
      [1] "-"

---

    Code
      format_value(c(1.2, NA, NA), jjcsformat_xx_SAS("xx.x (xx.x - xx.x)"), na_str = "NA")
    Output
      [1] "1.2 (NE - NE)"
    Code
      format_value(c(1.2, NA, NA), jjcsformat_xx_SAS("xx.x (xx.x - xx.x)"), na_str = "x")
    Output
      [1] "1.2 (x - x)"
    Code
      format_value(c(NA, NA, NA), jjcsformat_xx_SAS("xx.x (xx.x - xx.x)"), na_str = "x")
    Output
      [1] "x"

---

    Code
      format_value(c(NA, NA), format = jjcsformat_xx_SAS("xx.x - xx.x"), na_str = c(
        "hi", "lo"))
    Output
      [1] "hi - lo"
    Code
      format_value(c(NA, 5.2), format = jjcsformat_xx_SAS("xx.x - xx.x"), na_str = "what")
    Output
      [1] "what - 5.2"
    Code
      format_value(c(NA, 5.2), format = jjcsformat_xx_SAS("xx.x - xx.x"), na_str = c(
        "hi", "lo"))
    Output
      [1] "hi - 5.2"
    Code
      format_value(c(NA, NA), format = jjcsformat_xx_SAS("xx.x - xx.x"), na_str = "what")
    Output
      [1] "what"

---

    Code
      format_value(NA, format = jjcsformat_xx_SAS("xx.x"), na_str = character())
    Output
      [1] "NA"
    Code
      format_value(NA, format = jjcsformat_xx_SAS("xx.x"), na_str = NA_character_)
    Output
      [1] "NA"

---

    Code
      format_value(c(6.23, NA, NA), format = jjcsformat_xx_SAS("xx.x (xx.xx, xx.xx)"),
      na_str = "-")
    Output
      [1] "6.2 (-, -)"
    Code
      format_value(c(NA, NA, NA), format = jjcsformat_xx_SAS("xx.x (xx.xx, xx.xx)"),
      na_str = "-")
    Output
      [1] "-"
    Code
      format_value(c(6.23, NA, NA), format = jjcsformat_xx_SAS("xx.x (xx.xx, xx.xx)"),
      na_str = c("-", "x", "x"))
    Output
      [1] "6.2 (-, x)"
    Code
      format_value(c(6.23, NA, NA), format = jjcsformat_xx_SAS("xx.x (xx.xx, xx.xx)"),
      na_str = c("-", "x", "y"))
    Output
      [1] "6.2 (-, x)"

# jjcsformats count_fraction works

    Code
      format_value(cdf, format = jjcsformat_count_denom_fraction)
    Output
      [1] "5/2000 (0.3%)"
    Code
      format_value(cf, format = jjcsformat_count_fraction)
    Output
      [1] "5 (0.3%)"
    Code
      format_value(cf, format = "xx (xx.x%)")
    Output
      [1] "5 (0.2%)"
    Code
      format_value(c(2000, 2001, 2000 / 2001), format = jjcsformat_count_denom_fraction)
    Output
      [1] "2000/2001 (>99.9%)"
    Code
      format_value(c(2000, 2000 / 2001), format = "xx (xx.x%)")
    Output
      [1] "2000 (100.0%)"
    Code
      format_value(c(1, 2001, 1 / 2001), format = jjcsformat_count_denom_fraction)
    Output
      [1] "1/2001 (<0.1%)"
    Code
      format_value(c(1, 1 / 2001), format = "xx (xx.x%)")
    Output
      [1] "1 (0.0%)"
    Code
      format_value(c(3, 3, 3 / 3), format = jjcsformat_count_denom_fraction)
    Output
      [1] "3/3 (100.0%)"
    Code
      format_value(c(3, 3 / 3), format = "xx (xx.x%)")
    Output
      [1] "3 (100.0%)"
    Code
      format_value(rep(NA, 3), format = jjcsformat_xx("xx.x (xx.x, xx.x)"), na_str = rep(
        "NA", 10))
    Output
      [1] "NE (NE, NE)"
    Code
      format_value(rep(NA, 3), format = jjcsformat_xx("xx.x (xx.x, xx.x)"), na_str = rep(
        "NA", 1))
    Output
      [1] "NA"
    Code
      format_value(rep(NA, 3), format = jjcsformat_xx("xx.x (xx.x, xx.x)"))
    Output
      [1] "NA"
    Code
      format_value(c(1, rep(NA, 2)), format = jjcsformat_xx("xx.x (xx.x, xx.x)"))
    Output
      [1] "1.0 (NE, NE)"
    Code
      format_value(c(1, rep(NA, 2)), format = jjcsformat_xx("xx.x (xx.x, xx.x)"),
      na_str = c("ne1", "ne2", "ne3"))
    Output
      [1] "1.0 (ne1, ne2)"

