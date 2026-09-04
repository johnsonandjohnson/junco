#' @keywords internal
xxd_to_xx <- function(str, d = 0) {
  checkmate::assert_integerish(d, null.ok = TRUE)
  if (checkmate::test_list(str, null.ok = FALSE)) {
    checkmate::assert_list(str, null.ok = FALSE)
    # Or it may be a vector of characters
  } else {
    checkmate::assert_character(str, null.ok = FALSE)
  }

  nmstr <- names(str)

  if (any(grepl("xx.d", str, fixed = TRUE))) {
    checkmate::assert_integerish(d)
    str <- gsub("xx.d", paste0("xx.", strrep("x", times = d)), str, fixed = TRUE)
  }
  str <- stats::setNames(str, nmstr)
  return(str)
}

#' @keywords internal
format_xxd <- function(str, d = 0, .df_row = NULL, formatting_fun = NULL) {
  if (is.function(str)) {
    return(str)
  }

  # Handling of data precision
  if (!is.numeric(d)) {
    if (is.character(d) && length(d) == 1) {
      # check if d is a variable name available in .df_row
      if (d %in% names(.df_row)) {
        d <- max(.df_row[[d]], na.rm = TRUE)
      } else {
        message(paste("precision has been reset to d = 0, as variable", d, "not present on input"))
        d <- 0
      }
    }
  }
  # convert xxd type of string to xx
  fmt <- xxd_to_xx(str = str, d = d)

  if (!is.null(formatting_fun)) {
    fmt <- formatting_fun(fmt)
  }

  return(fmt)
}

#' @name fmt_spec_d
#'
#' @title Utilities for processing d-style string formatting specifications (eg "xx.dx")
#'
#' @details Format specifications in d-style notation (such as "xx.dx")
#' will be translated to string based formats
#' (or wrapped with [jjcsformat_xx()]), eg "xx.dx" will be translated into "xx.xx", when d = 1.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param d `integer(1)` Value of `d` for the translation of "xx.dx" into "xx.xx" style (example when `d` = 1)
#'
#' @param stats_in `(character)` Set of statistics to restrict to.
#' @param fmt_d_def Named vector with default format specifications where d-style format is allowed.
#' @param fmt_d_in Named vector with format specifications where d-style format is allowed.
#' Formats defined here will take higher precedence over formats in `fmt_d_def`.
#'
#' @param var_d Named vector of d specifations.
#' Names are to be considered as variables to be analyzed in further processing.
#'
#' @param df_d Dataframe in which the d-column specification is available.
#' @param d_column Name of column within dataframe `df_d` that contains required integer values for `d`.
#' @param fmt_column Name of column that will be added to dataframe that will contain resulting format specifications.
#'
NULL

#' @describeIn fmt_spec_d Function to convert a named list using d-style string format
#'  specification into a named list with valid `formatters` formatting specification
#'
#'
#' @examples
#' junco_def_d <- c(
#'   "mean" = "xx.dx",
#'   "mean_sd" = "xx.dx (xx.dxx)",
#'   "range" = "(xx.d, xx.d)"
#' )
#' myfmts <- fmt_spec_single_d(
#'   d = 1,
#'   stats_in = c("mean", "mean_sd"),
#'   fmt_d_def = junco_def_d,
#'   fmt_d_in = NULL
#' )
#' myfmts
#'
fmt_spec_single_d <- function(d = 1,
                              stats_in = NULL,
                              fmt_d_def = junco_def_d_all,
                              fmt_d_in = NULL) {
  checkmate::assert_int(d)

  if (is.null(stats_in)) stats_in <- names(fmt_d_def)

  fmt_d <- c(fmt_d_in, fmt_d_def[setdiff(names(fmt_d_def), names(fmt_d_in))])
  fmt_d <- fmt_d[stats_in]

  formats <- lapply(fmt_d, FUN = format_xxd, d = d, formatting_fun = jjcsformat_xx)

  formats
}


#' @describeIn fmt_spec_d Function to convert an input column with d-style formatting
#' specification into a new column with valid formatters formatting specification.
#'
#' The new column can be passed onto argument `formats_var` in [rtables::analyze()] function.
#'
#' @examples
#' # example for fmt_spec_df_d ----
#' df_d <- tribble(
#'   ~PARAMCD, ~d,
#'   "DIABP", 2L,
#'   "PULSE", 3L,
#'   "RESP", 1L
#' )
#' yy <- fmt_spec_df_d(df_d,
#'   d_column = "d",
#'   fmt_column = "fmt_d",
#'   stats_in = NULL,
#'   fmt_d_def = junco_def_d,
#'   fmt_d_in = NULL
#' )
#'
#'
#' df <- ex_advs |>
#'   dplyr::filter(PARAMCD %in% c("DIABP", "PULSE", "RESP")) |>
#'   dplyr::filter(AVISIT %in% c("BASELINE", "WEEK 1 DAY 8", "WEEK 2 DAY 15"))
#' df <- df |>
#'   dplyr::left_join(yy)
#'
#' lyt <- basic_table() |>
#'   split_cols_by("ARMCD") |>
#'   split_rows_by("PARAMCD", split_fun = drop_split_levels) |>
#'   split_rows_by("AVISIT", split_fun = drop_split_levels) |>
#'   analyze(
#'     vars = "AVAL",
#'     afun = a_summary,
#'     extra_args = list(
#'       .stats = c("n", "mean_se", "range"),
#'       .formats = "default"
#'     ),
#'     formats_var = "fmt_d"
#'   )
#'
#' rslt <- build_table(lyt, df2, alt_counts_df = ex_adsl)
#' rslt
#'
#' @export
#'
fmt_spec_df_d <- function(df_d,
                          d_column = "d",
                          fmt_column = "fmt_d",
                          stats_in = NULL,
                          fmt_d_def = junco_def_d_all,
                          fmt_d_in = NULL) {
  df_d[[fmt_column]] <- lapply(df_d[[d_column]],
    fmt_spec_single_d,
    stats_in = stats_in,
    fmt_d_def = fmt_d_def,
    fmt_d_in = fmt_d_in
  )

  df_d
}

#' @describeIn fmt_spec_d Function to convert a named list of variables with d-style formatting
#'  specification into a named list with valid `formatters` formatting specification.
#'
#'  The result can be used as input for argument `format` in [rtables::analyze()] call.
#'
#'
#' @examples
#' # example for fmt_spec_var_d ----
#' var <- c("AGE" = 0, "BMI" = 1, "BMRKR1" = 2)
#' vars_fmt <- fmt_spec_var_d(var,
#'   stats_in = NULL,
#'   fmt_d_def = junco_def_d,
#'   fmt_d_in = NULL
#' )
#' vars_fmt
#' lyt <- basic_table() |>
#'   split_cols_by("ARMCD") |>
#'   analyze(
#'     vars = c("AGE", "BMRKR1"),
#'     afun = a_summary,
#'     extra_args = list(
#'       .stats = c("n", "mean_sd", "range"),
#'       .formats = "default"
#'     ),
#'     format = vars_fmt,
#'     section_div = " "
#'   )
#'
#' rslt <- build_table(lyt, ex_adsl)
#' rslt
#' @export
#'
fmt_spec_var_d <- function(var_d,
                           stats_in = NULL,
                           fmt_d_def = junco_def_d_all,
                           fmt_d_in = NULL) {
  var_fmt <- lapply(var_d,
    fmt_spec_single_d,
    stats_in = stats_in,
    fmt_d_def = fmt_d_def,
    fmt_d_in = fmt_d_in
  )

  var_fmt
}
