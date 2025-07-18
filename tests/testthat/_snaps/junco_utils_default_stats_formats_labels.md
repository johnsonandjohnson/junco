# get_stats works as expected

    Code
      res
    Output
      [1] "quantiles_lower"      "median_ci_3d"         "quantiles_upper"     
      [4] "range_with_cens_info"

# get_formats_from_stats works as expected

    Code
      res
    Output
      $quantiles_upper
      function(x, output, na_str = na_str_dflt) {
            if (anyNA(na_str) || (replace_na_dflt && any(na_str == "NA"))) {
              na_inds <- which(is.na(na_str) | (replace_na_dflt & na_str == "NA"))
              na_str[na_inds] <- rep(na_str_dflt, length.out = length(na_str))[na_inds]
            }
            if (length(x) == 0 || isTRUE(all(x == ""))) {
              return(NULL)
            } else if (!length(positions[[1]]) == length(x)) {
              stop(
                "Error: input str in call to jjcs_format_xx must contain same number of xx as the number of stats."
              )
            }
      
            values <- Map(y = x, fun = roundings, na_str = na_str, function(y, fun, na_str) fun(y, na_str = na_str))
            regmatches(x = str, m = positions)[[1]] <- values
            return(str)
          }
      <environment: base>
      
      $range_with_cens_info
      function(x, ...) {
          checkmate::assert_numeric(
            x,
            len = 4L,
            finite = TRUE,
            any.missing = FALSE
          )
          checkmate::assert_true(all(x[c(3, 4)] %in% c(0, 1)))
      
          res <- vapply(x[c(1, 2)], format_xx, character(1))
          if (x[3] == 1) res[1] <- paste0(res[1], "+")
          if (x[4] == 1) res[2] <- paste0(res[2], "+")
          paste0("(", res[1], ", ", res[2], ")")
        }
      <environment: base>
      

# get_labels_from_stats works as expected

    Code
      res
    Output
      $quantiles_upper
      [1] "75%-ile (95% CI)"
      
      $range_with_cens_info
      [1] "Min, max"
      

# get_label_attr_from_stats works as expected

    Code
      res
    Output
      stats1 stats2 
       "bla"  "boo" 

# get_indents_from_stats works as expected

    Code
      res
    Output
      $quantiles_upper
      [1] 0
      
      $range_with_cens_info
      [1] 0
      

