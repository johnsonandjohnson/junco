#' @name jjcsformat_xx
#' @title Utility for specifying custom formats
#'
#' @description
#'
#' Utility for specifying custom formats that can be used as a format in `formatters::format_value`
#'
#' @param str The formatting that is required specified as a text string, eg "xx.xx"
#' @param na_str_dflt Character to represent NA value
#' @param replace_na_dflt logical(1). Should an `na_string` of "NA" within
#'    the formatters framework be overridden by `na_str_default`? Defaults to
#'    `TRUE`, as a way to have a different default na string behavior from the
#'    base `formatters` framework.
#' @param na_str String for NA values.
#' @return Either a supported format string, or a formatting function that can be
#' used as format in `formatters::format_value`
#' @family JJCS formatting functions
#' @rdname jjcsformat_xx
#' @export
#' @examples
#' value <- c(1.65, 8.645)
#' fmt <- jjcsformat_xx("xx.x")
#' is.function(fmt)
#' fmt
#' format_value(value[1], fmt, round_type = "sas")
#' format_value(value[1], fmt, round_type = "iec")
#' if (is.function(fmt)) fmt(value[1])
#'
#' fmt2 <- jjcsformat_xx("xx.x (xx.xxx)")
#' is.function(fmt2)
#' value <- c(1.65, 8.645)
#' format_value(value, fmt2, round_type = "sas")
#' format_value(value, fmt2, round_type = "iec")
#' # only possible when resulting format is a function
#' if (is.function(fmt2)) fmt2(value, round_type = "sas")
#'
#' value <- c(1.65, NA)
#' format_value(value, fmt2, round_type = "iec", na_str = c("ne1", "ne2"))
#' if (is.function(fmt2)) fmt2(value, round_type = "iec", na_str = c("ne1", "ne2"))
jjcsformat_xx <- function(
  str,
  na_str = na_str_dflt,
  na_str_dflt = "NE",
  replace_na_dflt = TRUE
) {
  if (grepl("xxx.", str, fixed = TRUE)) {
    stop("Error: jjcsformat_xx do not use xxx. in input str, replace by xx. instead.")
  }

  if (identical(str, "default"))
    return(str)

  if (is_valid_format(str)) {
    rtable_format <- str
  } else {
    if (!grepl("xx", str, fixed = TRUE)) {
      stop("Error: jjcsformat_xx input str must contain xx.")
    }
    positions <- gregexpr(
      pattern = "xx\\.?x*",
      text = str,
      perl = TRUE
    )
    x_positions <- regmatches(x = str, m = positions)[[1]]

    single_rounding <- function(fmt) {
      function(x,
               na_str,
               round_type) {
        if (fmt %in% list_valid_format_labels()$`1d`) {
          res <- format_value(x,
            fmt,
            na_str = na_str[1],
            round_type = round_type
          )
        } else if (fmt %in% paste0("xx.", strrep("x", times = 5:12))) {
          # p-value fmt sometimes might need more digits
          d <- nchar(sub(".*\\.", "", fmt))
          res <- round_fmt(x, digits = d, round_type = round_type, na_str = na_str[1])
        }
        res
      }
    }

    roundings <- lapply(X = x_positions, function(fmt) {
      single_rounding(fmt)
    })

    rtable_format <-
      function(x,
               output,
               round_type = valid_round_type,
               na_str = na_str_dflt) {
        if (anyNA(na_str) || (replace_na_dflt && any(na_str == "NA"))) {
          na_inds <- which(is.na(na_str) | (replace_na_dflt & na_str == "NA"))
          na_str[na_inds] <- rep(na_str_dflt, length.out = length(na_str))[na_inds]
        }
        if (length(x) == 0 || isTRUE(all(x == ""))) {
          return(NULL)
        } else if (!length(positions[[1]]) == length(x)) {
          stop(
            "Error: input str in call to jjcsformat_xx must contain same number of xx as the number of stats."
          )
        }

        round_type <- match.arg(round_type)

        values <- Map(y = x, fun = roundings, na_str = na_str, function(y, fun, na_str, output) {
          fun(y, na_str = na_str, round_type = round_type)
        })

        regmatches(x = str, m = positions)[[1]] <- values
        return(str)
      }

    return(rtable_format)
  }
}


#' @name count and fraction related formatting functions
#' @title Formatting functions for count and fraction, and for count denominator and fraction values
#'
#' @description
#'
#' Formats a count together with fraction (and/or denominator) with special
#' consideration when count is 0, or fraction is 1.
#' \cr See also: [tern::format_count_fraction_fixed_dp()]
#'
#' @param x (`numeric vector`)\cr Vector with elements `num` and `fraction` or `num`, `denom` and `fraction`.
#' @param d (`numeric(1)`)\cr Number of digits to round fraction to (default = 1)
#' @param ... Additional arguments passed to other methods.
#' @param type (`character(1`)\cr One of `count_fraction`, `count_denom_fraction`, `fraction_count_denom`,
#' to specify the type of format the function will represent.
#' @param verbose (`logical`)\cr Whether to print verbose output
#' @param round_type (`character(1)`)\cr the type of rounding to perform.
#' See [formatters::format_value()] for more details.
#' @param output (`string`)\cr output type.
#' See [formatters::format_value()] for more details.
#' @return A formatting function to format input into string in the format `count / denom (ratio percent)`. If `count`
#' is 0, the format is `0`. If fraction is >0.99, the format is
#' `count / denom (>99.9 percent)`
#' @family JJCS formatting functions
#' @rdname count_fraction
#' @export

jjcsformat_cnt_den_fract_fct <- function(d = 1,
                                         type = c("count_fraction", "count_denom_fraction", "fraction_count_denom"),
                                         verbose = FALSE) {
  type <- match.arg(type)

  function(x,
           round_type = valid_round_type,
           output,
           ...) {
    obj_label(x) <- NULL
    if (any(is.na(x))) {
      return("-")
    }

    round_type <- match.arg(round_type)

    checkmate::assert_vector(x)
    count <- x[1]
    checkmate::assert_integerish(count)

    fraction <- switch(type,
      "count_fraction" = x[2],
      "count_denom_fraction" = x[3],
      "fraction_count_denom" = x[3]
    )


    assert_proportion_value(
      fraction,
      include_boundaries = TRUE
    )

    if (isTRUE(all.equal(fraction, 1))) fraction <- 1

    if (type == "count_fraction") fmt_cd <- format_value(x = count, format = "xx")
    if (type %in% c("count_denom_fraction", "fraction_count_denom")) {
      denom <- x[2]
      checkmate::assert_integerish(denom)
      fmt_cd <- paste0(count, "/", denom)
    }

    if (verbose) message(paste0("round_type used: ", round_type))

    fmtpct <- format_value(100 * fraction,
      format = paste0("xx.", strrep("x", times = d)),
      output = "ascii",
      round_type = round_type
    )

    fmtpct_p2 <- fmtpct
    # deal with special cases
    if (fraction == 1) {
      fmtpct_p2 <- "100.0"
    } else if (fmtpct == format(100, nsmall = d)) {
      fmtpct_p2 <- paste0(">", 100 - 10**(-d))
    } else if (count != 0 && fmtpct == format(0, nsmall = d)) {
      fmtpct_p2 <- paste0("<", 10**(-d))
    }

    fmtpct_p2 <- paste0(fmtpct_p2, "%")
    fmtpct_p <- paste0(" (", fmtpct_p2, ")")

    result <- if (type == "fraction_count_denom") {
      paste0(fmtpct_p2, " (", fmt_cd, ")")
    } else if (count == 0 && type == "count_fraction") {
      0
    } else {
      paste0(fmt_cd, fmtpct_p)
    }

    return(result)
  }
}

#' @rdname count_fraction
#' @export
#' @examples
#'
#' jjcsformat_count_fraction(c(7, 0.7))
#' jjcsformat_count_fraction(c(70000, 70000 / 70001))
#' jjcsformat_count_fraction(c(235, 235 / 235))
#' fmt <- jjcsformat_cnt_den_fract_fct(type = "count_fraction", d = 2)
#' fmt(c(23, 23 / 235))
jjcsformat_count_fraction <- jjcsformat_cnt_den_fract_fct(type = "count_fraction")

#' @rdname count_fraction
#' @export
#' @examples
#'
#' jjcsformat_count_denom_fraction(c(7, 10, 0.7))
#' jjcsformat_count_denom_fraction(c(70000, 70001, 70000 / 70001))
#' jjcsformat_count_denom_fraction(c(235, 235, 235 / 235))
#' fmt <- jjcsformat_cnt_den_fract_fct(type = "count_denom_fraction", d = 2)
#' fmt(c(23, 235, 23 / 235))
jjcsformat_count_denom_fraction <- jjcsformat_cnt_den_fract_fct(type = "count_denom_fraction")

#' @rdname count_fraction
#' @export
#' @examples
#'
#' jjcsformat_fraction_count_denom(c(7, 10, 0.7))
#' jjcsformat_fraction_count_denom(c(70000, 70001, 70000 / 70001))
#' jjcsformat_fraction_count_denom(c(235, 235, 235 / 235))
#' fmt <- jjcsformat_cnt_den_fract_fct(type = "fraction_count_denom", d = 2)
#' fmt(c(23, 235, 23 / 235))
jjcsformat_fraction_count_denom <- jjcsformat_cnt_den_fract_fct(type = "fraction_count_denom")


#' @title Function factory for p-value formatting
#'
#' @description A function factory to generate formatting functions for p-value
#' formatting that support rounding close to the significance level specified.
#'
#' @param alpha (`numeric`)\cr the significance level to account for during rounding.
#' @return The p-value in the standard format. If `count` is 0, the format is `0`.
#'   If it is smaller than 0.001, then `<0.001`, if it is larger than 0.999, then
#'   `>0.999` is returned. Otherwise, 3 digits are used. In the special case that
#'   rounding from below would make the string equal to the specified `alpha`,
#'   then a higher number of digits is used to be able to still see the difference.
#'   For example, 0.0048 is not rounded to 0.005 but stays at 0.0048 if `alpha = 0.005`
#'   is set.
#'
#' @rdname jjcsformat_xx
#' @export
#'
#' @examples
#' my_pval_format <- jjcsformat_pval_fct(0.005)
#' my_pval_format(0.2802359)
#' my_pval_format(0.0048)
#' my_pval_format(0.00499)
#' my_pval_format(0.004999999)
#' my_pval_format(0.0051)
#' my_pval_format(0.0009)
#' my_pval_format(0.9991)
#'
jjcsformat_pval_fct <- function(alpha = 0.05) {
  checkmate::assert_number(alpha, lower = 0, upper = 1)

  function(x, round_type = valid_round_type, na_str, ...) {
    round_type <- match.arg(round_type)
    checkmate::assert_number(
      x,
      lower = 0,
      upper = 1 + .Machine$double.eps, # Be a bit tolerant here.
      na.ok = TRUE
    )
    xx_format <- "xx.xxx"
    if (!is.na(x) && alpha < 0.001 && alpha > 0) {
      stop("jjcsformat_pval_fct: argument alpha should be 0 or at least 0.001.")
    }

    if (is.na(x)) {
      format_value(x, jjcsformat_xx(xx_format), na_str = na_str)
    } else if (x < 0.001) {
      "<0.001"
    } else if (x > 0.999) {
      ">0.999"
    } else {
      res <- format_value(x, jjcsformat_xx(xx_format), round_type = round_type) # nolint start
      while (as.numeric(res) == alpha && x < alpha &&
        xx_format != paste0("xx.", strrep("x", times = 10))) {
        # Increase precision by 1 digit until the result
        # is different from threshold alpha.
        xx_format <- paste0(xx_format, "x") # nolint end
        res <- format_value(x, jjcsformat_xx(xx_format), round_type = round_type)
      }
      if (xx_format == paste0("xx.", strrep("x", times = 10))) {
        # produce message eg "stopped increasing precision for p-value"?
      }
      res
    }
  }
}

#' @title Function factory for range with censoring information formatting
#' @description A function factory to generate formatting functions for range formatting
#'   that includes information about the censoring of survival times.
#'
#' @param str (`string`)\cr the format specifying the number of digits to be used,
#'   for the range values, e.g. `"xx.xx"`.
#'
#' @param censor_char (`string`)\cr the character (of length 1) to be appended to `min` or `max`
#' @return A function that formats a numeric vector with 4 elements:
#'   - minimum
#'   - maximum
#'   - censored minimum? (1 if censored, 0 if event)
#'   - censored maximum? (1 if censored, 0 if event)
#'   The range along with the censoring information is returned as a string
#'   with the specified numeric format as `(min, max)`, and the `censor_char` is appended
#'   to `min` or `max` if these have been censored.
#'
#' @export
#' @rdname jjcsformat_xx
#' @examples
#' my_range_format <- jjcsformat_range_fct("xx.xx")
#' my_range_format(c(0.35235, 99.2342, 1, 0))
#' my_range_format(c(0.35235, 99.2342, 0, 1))
#' my_range_format(c(0.35235, 99.2342, 0, 0))
#' my_range_format(c(0.35235, 99.2342, 1, 1))
#' my_range_format <- jjcsformat_range_fct("xx.xx", censor_char = "*")
#' my_range_format(c(0.35235, 99.2342, 1, 1))
jjcsformat_range_fct <- function(str, censor_char = "+") {
  format_xx <- jjcsformat_xx(str)
  checkmate::assert_string(censor_char, na.ok = FALSE, n.chars = 1)

  function(x, output, round_type = valid_round_type, ...) {
    round_type <- match.arg(round_type)
    checkmate::assert_numeric(
      x,
      len = 4L,
      finite = TRUE,
      any.missing = FALSE
    )
    checkmate::assert_true(all(x[c(3, 4)] %in% c(0, 1)))

    res <- vapply(x[c(1, 2)], FUN = function(x) {
      format_value(x, format_xx, round_type = round_type)
    }, character(1))
    if (x[3] == 1) res[1] <- paste0(res[1], censor_char)
    if (x[4] == 1) res[2] <- paste0(res[2], censor_char)
    paste0("(", res[1], ", ", res[2], ")")
  }
}
