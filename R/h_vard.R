#' Utilities and analysis functions (`afuns`) for varying decimal precision
#'
#' @param d (`numeric()` or `character()`)\cr When `numeric()`, either an named vector, with `integer` values for each element, of which `.var` is supposed to be one of the names.
#' \cr or an unnamed vector of length 1 (`.var` will then not be used)
#' \cr When `character()`, an unnamed vector of length 1.
#' @param .var (`string`)\cr Variable name to get the decimal precision from (only used when `d` is a named numeric vector.)
#' @param d_unspecified (`integer(1)`)\cr Value to set when no misspecification is detected, but value remained unassigned.
#' @param msg_pre (`string`)\cr String to add to message when misspecification is detected.
#' @param str (named `list`)\cr Named list with strings with xx.d or xx notations, or formatting functions.
#'
#' @name junco_varying_decimal_precision
#' @order 1
NULL

#' @describeIn junco_varying_decimal_precision Check specifications for varying decimal precision
#' 
#' @return 
#' * h_check_d(): `logical(1)` TRUE if no issues are detected.
#' 
#' @keywords internal
#' @order 8
h_check_d <- function(d, .var, d_unspecified = 0, msg_pre = NULL) {
  
  notok <- FALSE
  
  if (!notok && !(is.numeric(d) || is.character(d))) {notok <- TRUE}
  
  # non-named numeric vector has to be of length 1 and value needs to be integerish
  if (!notok && is.numeric(d) && is.null(names(d)) && (length(d) != 1 || !checkmate::testIntegerish(d))) {notok <- TRUE}
  # named numeric vector, if .var in names(d) the value needs to be integerish
  if (!notok && is.numeric(d) && !is.null(names(d)) && .var %in% names(d) && !checkmate::testIntegerish(d[.var])) {notok <- TRUE}
  # character has to be an unnamed vector of length 1, this will serve as variable name
  # the actual value and variable name will be checked later
  if (!notok && is.character(d) && (!is.null(names(d)) || length(d) != 1)) {notok <- TRUE}
  
  if (notok){
    if (!is.null(msg_pre)){
      msg_pre <- paste0(msg_pre,": ")
    }
    stop(msg_pre,
         "decimal precision (d) specification issue \n",
         "d should be either an unnamed integer of length 1 \n",
         "or a named integer() length >= 1 - .var is supposed to occur in names(d) \n",
         "or an unnamed character of length 1 - will be used as variable name in further processing "
    )
  }
  return(!notok)
}

#' @describeIn junco_varying_decimal_precision
#' Get decimal precision from specification
#' 
#' @return 
#' * `h_get_d()`: Returns the value for the decimal precision to use (`integer(1)`).
#' 
#' @keywords internal
#' @order 7
h_get_d <- function(d, .df_row, .var, d_unspecified = 0) {
  
  # will stop if a check fails
  h_check_d(d, .var, d_unspecified, msg_pre = NULL)
  
  d_act <- NULL
  
  if (is.numeric(d) && length(d) == 1 && is.null(names(d))) {
    d_act <- d
  } else if (is.numeric(d) && length(d) >= 1 && .var %in% names(d)) {
    d_act <- d[.var]
  } else if (is.numeric(d) && length(d) >= 1 && !.var %in% names(d)) {
    if (is.numeric(.df_row[[.var]])) {
      message(paste("variable", .var, "precision has been set to d =", d_unspecified))
    }
    d_act <- d_unspecified
  } else if (is.character(d) && length(d) == 1 && d %in% names(.df_row) && nrow(.df_row[!is.na(.df_row[[d]]), ]) > 0) {
    d_act <- max(.df_row[[d]], na.rm = TRUE)
  } 
  
  if (is.null(d_act)) {
    message(paste("precision has been set to d =", d_unspecified))
    d_act <- d_unspecified
  }
  
  return(d_act)
}

#' @describeIn junco_varying_decimal_precision
#' Convert xx.d notation to xx.x notation

#' @param d (`integer(1)`)\cr Value of the decimal precision to use.
#' 
#' @return 
#' * `xxd_to_xx()`: A named `list` with xx.d strings converted to xx.x strings. Formatting functions remain unchanged.
#' 
#' @keywords internal
#' @order 6
xxd_to_xx <- function(str, d = 0){
  if (checkmate::test_list(str, null.ok = FALSE)) {
    checkmate::assert_list(str, null.ok = FALSE)
    # Or it may be a vector of characters
  } else {
    checkmate::assert_character(str, null.ok = FALSE)
  }
  
  str <- lapply(str, 
                FUN = function(fmt_str) {
                  if (!is.function(fmt_str) && any(grepl("xx.d", fmt_str, fixed = TRUE))) {
                    checkmate::assert_integerish(d, null.ok = TRUE)
                    fmt_new <- gsub("xx.d", paste0("xx.", strrep("x", times = d)), fmt_str, fixed = TRUE)
                  } else {fmt_new <- fmt_str}
                })  
  
  return(str)  
}

#' @describeIn junco_varying_decimal_precision Convert xx.d notation to xx.x notation and apply formatting function
#' @inheritParams xxd_to_xx
#' @param formatting_fun (NULL or `function(1)`)\cr A formatting function to apply after the xx.d to xx.x conversion.
#' 
#' @return 
#' * `format_xxd()`: A named `list` with xx.d strings converted to xx.x strings, and if not null, the formatting function `formatting_fun` applied to the string. Formatting functions remain unchanged.
#' 
#' @keywords internal
#' @order 5
format_xxd <- function(str, d = 0, formatting_fun = NULL){
  # convert xxd type of string to xx
  fmt <- xxd_to_xx(str = str, d = d)
  
  if (!is.null(formatting_fun)){
    fmt <- lapply(fmt, 
                  FUN = function(fmt_str) {
                    if (!is.function(fmt_str) ) {
                      fmt_new <- formatting_fun(fmt_str)
                    } else {fmt_new <- fmt_str}
                  })  
    
  }
  
  char_fmt <- sapply(fmt,FUN = function(fmt_str) !is.function(fmt_str))  
  char_fmt <- fmt[char_fmt]
  char_fmt <- unique(unname(unlist(char_fmt)))
  
  valid_char <- unname(unlist(formatters::list_valid_format_labels()))
  
  invalid_char <- char_fmt[!(char_fmt %in% valid_char)]
  if (length(invalid_char) > 0) {
    stop(
      "Unknown character format label(s): '", paste(invalid_char, sep = "'", collapse = "' ; '"),
      "'. Run `list_valid_format_labels()` to get a list of all available character formats.",
      "Or add a formatting function via argument `formatting_fun`."
    )
  }
  return(fmt)
}

#' @describeIn junco_varying_decimal_precision Get precision from data
#'
#' @return 
#' * `junco_get_precision_df()`: when precisionby is not null : a dataframe with precision by variables and data precision values, the variable "decimal" will contain the precision
#' otherwise `numeric(1)`
#'
#' @param df Input dataframe
#' @param decimal Cap to the precision derivation
#' @param precisionby Grouping variable in the precision derivation process.
#' @param precisionon Variable for which the precision has to be calculated.
#' 
#' @keywords internal
#' @export 
#' 
#' @examples
#' junco_get_precision_df(formatters::ex_adsl, precisionon = "BMRKR1", decimal = 1)
#' junco_get_precision_df(formatters::ex_adsl, precisionon = "BMRKR1", decimal = 7)
#' junco_get_precision_df(formatters::ex_adsl, precisionon = "AGE", decimal = 7)
#' prec_df <- junco_get_precision_df(formatters::ex_advs, precisionon = "AVAL", precisionby = c("PARAMCD"), decimal = 3) 
#' prec_df
#' 
#' @order 4
junco_get_precision_df <- function(df, decimal = 3, precisionby = NULL, precisionon){
  
  res <- make_precision_data(
    df = df,
    decimal = decimal,
    precisionby = precisionby,
    precisionon = precisionon
  ) 
  
  if (is.null(precisionby)) {
    res <- res[["decimal"]]
  }
  
  return(res)
}
