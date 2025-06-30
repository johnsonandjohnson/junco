#' @name remove_col_count
#'
#' @title Removal of Unwanted Column Counts
#'
#' @description
#' Remove the N=xx column headers for specified span_label_var columns - default is 'rrisk_header
#' @details This works for only the lowest level of column splitting (since colcounts is used)
#' @param obj table tree object
#' @param span_label_var the spanning header text variable value for which column headers will be removed from
#'
#' @return table tree object with column counts in specified columns removed
#' @export
#'
remove_col_count <- function(obj, span_label_var = "rrisk_header") {
  ## programatically figure out which ones we want
  unwanted_count <- function(pth) pth[1] == span_label_var
  to_blank <- sapply(col_paths(obj), unwanted_count)
  col_counts(obj)[to_blank] <- NA_integer_
  return(obj)
}
