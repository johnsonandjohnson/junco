#' @title Function factory for xx style formatting
#' @description A function factory to generate formatting functions for value
#' formatting that support the xx style format and control the rounding method
#'
#' @param roundmethod (`string`)\cr choice of rounding methods. Options are:
#'   * `sas`: the underlying rounding method is `tidytlg::roundSAS`, where \cr
#'   roundSAS comes from this Stack Overflow post https://stackoverflow.com/questions/12688717/round-up-from-5
#'   * `iec`: the underlying rounding method is `round`
#'
#' @param na_str_dflt Character to represent NA value
#' @param replace_na_dflt logical(1). Should an `na_string` of "NA" within
#'    the formatters framework be overridden by `na_str_default`? Defaults to
#'    `TRUE`, as a way to have a different default na string behavior from the
#'    base `formatters` framework.
#' @return `format_xx_fct()` format function that can be used in rtables formatting calls
#' @export
#'
#' @family JJCS formats
#' @examples
#' jjcsformat_xx_SAS <- format_xx_fct(roundmethod = "sas")
#' jjcsformat_xx <- jjcsformat_xx_SAS
#' rcell(c(1.453), jjcsformat_xx("xx.xx"))
#' rcell(c(), jjcsformat_xx("xx.xx"))
#' rcell(c(1.453, 2.45638), jjcsformat_xx("xx.xx (xx.xxx)"))
#'
format_xx_fct <- function(roundmethod = c("sas", "iec"), na_str_dflt = "NE",
                          replace_na_dflt = TRUE) {
  roundmethod <- match.arg(roundmethod)

  if (roundmethod == "sas") {
    roundfunc <- tidytlg::roundSAS
  } else {
    roundfunc <- round
  }

  fnct <- function(str, na_str = na_str_dflt) {
    if (grepl("xxx.", str, fixed = TRUE)) {
      stop(
        "Error: jjcs_format_xx: do not use xxx. in input str, replace by xx. instead."
      )
    }
    if (!grepl("xx", str, fixed = TRUE)) {
      stop("Error: jjcs_format_xx: input str must contain xx.")
    }
    positions <- gregexpr(
      pattern = "xx\\.?x*",
      text = str,
      perl = TRUE
    )
    x_positions <- regmatches(x = str, m = positions)[[1]]
    ### str is splitted into pieces as xx. xx xx.xxx
    ### xx is no rounding
    ### xx. rounding to integer
    ### xx.x rounding to 1 decimal, etc

    no_round <- function(x, na_str = na_str_dflt) {
      if (is.na(x)) {
        return(na_str)
      } else {
        return(x)
      }
    }

    roundings <- lapply(X = x_positions, function(x) {
      y <- strsplit(split = "\\.", x = x)[[1]]
      ### "xx.x" will result in c("xx","x")
      ### "xx." will result in "xx"
      ### "xx" will remain "xx"

      if (x == "xx") {
        rounding <- no_round
      } else {
        rounding <- function(x, na_str = na_str_dflt) {
          if (is.na(x)) {
            return(na_str)
          } else {
            format(
              roundfunc(x, digits = ifelse(length(y) > 1, nchar(y[2]), 0)),
              nsmall = ifelse(length(y) > 1, nchar(y[2]), 0)
            )
          }
        }
      }
      return(rounding)
    })
    rtable_format <- function(x, output, na_str = na_str_dflt) {
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
    return(rtable_format)
  }
  return(fnct)
}


jjcsformat_xx_SAS <- format_xx_fct(roundmethod = "sas")
jjcsformat_xx_R <- format_xx_fct(roundmethod = "iec")


### if we ever decide to switch rounding method, we just have to update jjcsformat_xx here

#' @title Formatting of values
#' @name jjcsformat_xx
#' @description jjcs formatting function
#' @param str The formatting that is required specified as a text string, eg "xx.xx"
#' @param na_str character. Na string that will be passed from `formatters` into
#'    our formatting functions.
#' @return a formatting function with `"sas"`-style rounding.
#' @export
jjcsformat_xx <- jjcsformat_xx_SAS

#' @name count_fraction
#' @title Formatting count and fraction values
#'
#' @description
#'
#' Formats a count together with fraction (and/or denominator) with special
#' consideration when count is 0, or fraction is 1.
#' \cr See also: tern::format_count_fraction_fixed_dp()
#'
#' @inheritParams format_xx_fct
#' @param x `numeric`\cr with elements `num` and `fraction` or `num`, `denom` and `fraction`.
#' @param d numeric(1). Number of digits to round fraction to (default=1)
#' @param ... Additional arguments passed to other methods.
#' @return A string in the format `count / denom (ratio percent)`. If `count`
#' is 0, the format is `0`. If fraction is >0.99, the format is
#' `count / denom (>99.9 percent)`
#' @family JJCS formats
#' @rdname count_fraction
#' @export
#' @examples
#' jjcsformat_count_fraction(c(7, 0.7))
#' jjcsformat_count_fraction(c(70000, 0.9999999))
#' jjcsformat_count_fraction(c(70000, 1))
#'
jjcsformat_count_fraction <- function(
    x,
    d = 1,
    roundmethod = c("sas", "iec"),
    ...) {
  roundmethod <- match.arg(roundmethod)
  attr(x, "label") <- NULL
  if (any(is.na(x))) {
    return("-")
  }

  checkmate::assert_vector(x)
  checkmate::assert_integerish(x[1])
  assert_proportion_value(
    x[2],
    include_boundaries = TRUE
  )

  fraction <- x[2]

  if (isTRUE(all.equal(fraction, 1))) fraction <- 1

  if (roundmethod == "sas") {
    fmtpct <- format(tidytlg::roundSAS(fraction * 100, d), nsmall = d)
  } else {
    fmtpct <- format(round(fraction * 100, d), nsmall = d)
  }

  result <- if (x[1] == 0) {
    "0"
  } else if (fraction == 1) {
    ## per conventions still report as 100.0%
    paste0(x[1], " (100.0%)")
  } else if (fmtpct == format(0, nsmall = d)) {
    # else if (100*x[2] < 10**(-d)) {
    ### example pct = 0.09999 ### <0.1% (even if fmtpct == 0.1,
    # but the actual value of pct <0.1)
    paste0(x[1], " (<", 10**(-d), "%)")
  } else if (fmtpct == format(100, nsmall = d)) {
    # else if (100*x[2] > 100-10**(-d)) {
    ### example pct = 99.90001 ### >99.9% (even if fmtpct == 99.9,
    # but the actual value of pct >99.9)
    paste0(x[1], " (>", 100 - 10**(-d), "%)")
  } else {
    paste0(x[1], " (", fmtpct, "%)")
  }
  return(result)
}

#' @title Formatting count, denominator and fraction values
#'
#' @inheritParams count_fraction
#' @param ... Additional arguments passed to other methods.
#' @export
#' @rdname count_denom_fraction
#' @return `x`, formatted into a string with the appropriate
#' format and `d` digits of precision.
#' @examples
#' jjcsformat_count_denom_fraction(c(7, 10, 0.7))
#' jjcsformat_count_denom_fraction(c(70000, 70001, 70000 / 70001))
#' jjcsformat_count_denom_fraction(c(235, 235, 235 / 235))
jjcsformat_count_denom_fraction <- function(
    x,
    d = 1,
    roundmethod = c("sas", "iec"),
    ...) {
  roundmethod <- match.arg(roundmethod)
  attr(x, "label") <- NULL
  if (any(is.na(x))) {
    return("-")
  }
  checkmate::assert_vector(x)
  checkmate::assert_integerish(x[1])
  assert_proportion_value(
    x[3],
    include_boundaries = TRUE
  )

  fraction <- x[3]
  if (x[2] == x[1]) fraction <- 1

  fmt_x12 <- paste0(x[1], "/", x[2])

  if (roundmethod == "sas") {
    fmtpct <- format(tidytlg::roundSAS(fraction * 100, d), nsmall = d)
  } else {
    fmtpct <- format(round(fraction * 100, d), nsmall = d)
  }

  result <- if (x[1] == 0) {
    # "0"
    # same as in general situation
    paste0(fmt_x12, " (", fmtpct, "%)")
  } else if (100 * fraction == 100) {
    paste0(fmt_x12, " (100.0%)")
  } else if (100 * fraction < 10**(-d)) {
    ### example pct = 0.09999 ### <0.1% (even if fmtpct == 0.1, but the actual value of pct <0.1)
    paste0(fmt_x12, " (<", 10**(-d), "%)")
  } else if (100 * fraction > 100 - 10**(-d)) {
    ### example pct = 99.90001 ### >99.9% (even if fmtpct == 99.9, but the actual value of pct >99.9)
    paste0(fmt_x12, " (>", 100 - 10**(-d), "%)")
  } else {
    paste0(fmt_x12, " (", fmtpct, "%)")
  }
  return(result)
}

#' @title Formatting fraction, count and denominator values
#'
#' @details
#' Formats a 3-dimensional value such that percent values
#' near 0 or 100% are formatted as .e.g, `"<0.1%"` and
#' `">99.9%"`, where the cutoff is controlled by `d`, and
#' formatted as `"xx.x% (xx/xx)"` otherwise, with the
#' precision of the percent also controlled by `d`.
#'
#' @inheritParams count_fraction
#' @param ... Additional arguments passed to other methods.
#' @export
#' @rdname fraction_count_denom
#' @return `x` formatted as a string with `d` digits of precision,
#' with special cased values as described in Details above.
#' @examples
#' jjcsformat_fraction_count_denom(c(7, 10, 0.7))
#' jjcsformat_fraction_count_denom(c(70000, 70001, 70000 / 70001))
#' jjcsformat_fraction_count_denom(c(235, 235, 235 / 235))
jjcsformat_fraction_count_denom <- function(
    x,
    d = 1,
    roundmethod = c("sas", "iec"),
    ...) {
  roundmethod <- match.arg(roundmethod)
  attr(x, "label") <- NULL
  if (any(is.na(x))) {
    return("-")
  }
  checkmate::assert_vector(x)
  checkmate::assert_integerish(x[1])
  assert_proportion_value(
    x[3],
    include_boundaries = TRUE
  )

  fraction <- x[3]
  if (x[2] == x[1]) fraction <- 1

  fmt_x12 <- paste0(x[1], "/", x[2])

  if (roundmethod == "sas") {
    fmtpct <- format(tidytlg::roundSAS(fraction * 100, d), nsmall = d)
  } else {
    fmtpct <- format(round(fraction * 100, d), nsmall = d)
  }

  result <- if (x[1] == 0) {
    # "0"
    # same as in general situation
    paste0("(", fmt_x12, ")")
  } else if (100 * fraction == 100) {
    paste0("100.0%", " (", fmt_x12, ")")
  } else if (100 * fraction < 10**(-d)) {
    ### example pct = 0.09999 ### <0.1% (even if fmtpct == 0.1, but the actual value of pct <0.1)
    paste0("<", 10**(-d), "%", " (", fmt_x12, ")")
  } else if (100 * fraction > 100 - 10**(-d)) {
    ### example pct = 99.90001 ### >99.9% (even if fmtpct == 99.9, but the actual value of pct >99.9)
    paste0(">", 100 - 10**(-d), "%", " (", fmt_x12, ")")
  } else {
    paste0(fmtpct, "%", " (", fmt_x12, ")")
  }
  return(result)
}

#' @title Function factory for p-value formatting
#'
#' @description A function factory to generate formatting functions for p-value
#' formatting that support rounding close to the significance level specified
#'
#' @param alpha `number`\cr the significance level to account for during rounding.
#' @return The p-value in the standard format. If `count` is 0, the format is `0`.
#'   If it is smaller than 0.001, then `<0.001`, if it is larger than 0.999, then
#'   `>0.999` is returned. Otherwise, 3 digits are used. In the special case that
#'   rounding from below would make the string equal to the specified `alpha`,
#'   then a higher number of digits is used to be able to still see the difference.
#'   For example, 0.0048 is not rounded to 0.005 but stays at 0.0048 if `alpha = 0.005`
#'   is set.
#'
#' @family JJCS formats
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

  function(x, ...) {
    checkmate::assert_number(
      x,
      lower = 0,
      upper = 1 + .Machine$double.eps, # Be a bit tolerant here.
      na.ok = TRUE
    )
    if (is.na(x)) {
      "NE"
    } else if (x < 0.001) {
      "<0.001"
    } else if (x > 0.999) {
      ">0.999"
    } else {
      xx_format <- "xx.xxx"
      res <- jjcsformat_xx(xx_format)(x)
      while (as.numeric(res) == alpha && x < alpha) {
        # Increase precision by 1 digit until the result
        # is different from threshold alpha.
        xx_format <- paste0(xx_format, "x")
        res <- jjcsformat_xx(xx_format)(x)
      }
      res
    }
  }
}

#' @title Function factory for range with censoring information formatting
#' @description A function factory to generate formatting functions for range formatting
#'   that includes information about the censoring of survival times.
#'
#' @param str `string`\cr the format specifying the number of digits to be used,
#'   for the range values, e.g. `"xx.xx"`.
#' @return A function that formats a numeric vector with 4 elements:
#'   - minimum
#'   - maximum
#'   - censored minimum? (1 if censored, 0 if event)
#'   - censored maximum? (1 if censored, 0 if event)
#'   The range along with the censoring information is returned as a string
#'   with the specified numeric format as `(min, max)`, and the `+` is appended
#'   to `min` or `max` if these have been censored.
#'
#' @family JJCS formats
#' @export
#'
#' @examples
#' my_range_format <- jjcsformat_range_fct("xx.xx")
#' my_range_format(c(0.35235, 99.2342, 1, 0))
#' my_range_format(c(0.35235, 99.2342, 0, 1))
#' my_range_format(c(0.35235, 99.2342, 0, 0))
#' my_range_format(c(0.35235, 99.2342, 1, 1))
jjcsformat_range_fct <- function(str) {
  format_xx <- jjcsformat_xx(str)

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
}
