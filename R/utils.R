#' Extract the left-hand side of a formula
#'
#' @keywords internal
leftside <- function(x) {
  checkmate::assert_formula(x)
  res <- x[[2L]]
  checkmate::assert_string(res)
  res
}

#' Custom unlist function
#'
#' Unlist a list, but retain `NULL` as `'NULL'` or `NA`.
#'
#' @keywords internal
.unlist_keep_nulls <- function(lst, null_placeholder = "NULL", recursive = FALSE) {
  lapply(lst, function(x) if (is.null(x)) null_placeholder else x) |>
    unlist(recursive = recursive)
}


#' Title Case Conversion
#'
#' @param x Input string
#' @return String converted to title case (first letter of each word capitalized)
#' @export
#' @keywords internal
string_to_title <- function(x) {
  checkmate::assert_character(x, null.ok = TRUE)
  x_lower <- tolower(x)
  gsub("(^|\\s)(\\w)", "\\1\\U\\2", x_lower, perl = TRUE)
}


#' Check If `.alt_df_full` Is `NULL`
#'
#' For example, in `a_patyrs_j()`, if `source` is `"alt_df"`, we need to
#' check if `.alt_df_full` is `NULL`.
#'
#' @noRd
check_alt_df_full <- function(argument, values, .alt_df_full) {
  if (!argument %in% values || !is.null(.alt_df_full)) return(invisible())

  name <- deparse(substitute(argument))

  stop(sprintf(
    '`.alt_df_full` cannot be `NULL` when `%s` is `"%s"`',
    name, argument
  ))
}
