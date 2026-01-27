#' @name jjcs_num_formats
#'
#' @title Numeric Formatting Function
#'
#' @description
#' Formatting setter for selected numerical statistics
#'
#' @param d precision of individual values
#' @param cap cap to numerical precision (d > cap -- will use precision as if cap was specified as precision)
#'
#' @return list:
#'  - fmt : named vector with formatting function (jjcsformat_xx) for numerical stats: range, median, mean_sd, sd
#'  - spec : named vector with formatting specifications for numerical stats: range, median, mean_sd, sd
#' @export
#' @examples
#' P1_precision <- jjcs_num_formats(d=0)$fmt
#' jjcs_num_formats(2)$fmt
#' jjcs_num_formats(2)$spec
jjcs_num_formats <- function(d, cap = 4) {
  fmt_xx <- function(d, cap) {
    prec <- NULL
    ### report as is
    if (is.na(d)) {
      return("xx")
    }
    checkmate::assertCount(d)

    ### set a cap to number of decimal precision
    d <- min(d, cap)
    if (d > 0) prec <- paste0(prec, paste0(rep("x", d), collapse = ""))
    if (d >= 0) prec <- paste0("xx.", prec)

    return(prec)
  }

  fmt_rng <- function(d, cap) {
    prec <- fmt_xx(d, cap = cap)
    fmt <- list()
    fmt$spec <- paste0(prec, ", ", prec)
    fmt$fmt <- jjcsformat_xx(fmt$spec)

    return(fmt)
  }

  fmt_median <- function(d, cap) {
    prec <- fmt_xx(d, cap = cap)
    fmt <- list()
    fmt$spec <- prec
    fmt$fmt <- jjcsformat_xx(fmt$spec)
    return(fmt)
  }

  fmt_meansd <- function(d, cap) {
    prec1 <- fmt_xx(d, cap = cap)
    prec2 <- fmt_xx(d + 1, cap = cap + 1)
    fmt <- list()
    fmt$spec <- paste0(prec1, " (", prec2, ")")
    fmt$fmt <- jjcsformat_xx(fmt$spec)
    return(fmt)
  }
  fmt_sd <- function(d, cap) {
    prec <- fmt_xx(d, cap)
    fmt <- list()
    fmt$spec <- prec
    fmt$fmt <- jjcsformat_xx(fmt$spec)
    return(fmt)
  }

  ## apply formats for each of the stats with appropriate d and cap
  xfmt_rng <- fmt_rng(d, cap = cap)
  xfmt_meansd <- fmt_meansd(d + 1, cap = cap + 1)
  xfmt_sd <- fmt_sd(d + 2, cap = cap + 2)
  xfmt_median <- fmt_median(d + 1, cap = cap + 1)

  fmts <- list()

  fmts$fmt <- c(range = xfmt_rng$fmt, mean_sd = xfmt_meansd$fmt, sd = xfmt_sd$fmt, median = xfmt_median$fmt)

  fmts$spec <- c(range = xfmt_rng$spec, mean_sd = xfmt_meansd$spec, sd = xfmt_sd$spec, median = xfmt_median$spec)

  return(fmts)
}
