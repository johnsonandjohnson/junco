#' Unicode Mapping Table
#'
#' A tibble that maps special characters to their Unicode equivalents.
#'
#' @format A tibble with columns 'pattern' and 'unicode', where 'pattern' contains
#' the string to be replaced and 'unicode' contains the Unicode code point in hexadecimal.
#'
#' @export
jj_uc_map <- tibble::tribble(~pattern, ~unicode, ">=", "2265", "<=", "2264")

unicodify <- function(strs, uc_map) {
  out <- strs
  for (i in seq_len(nrow(uc_map))) {
    out <- gsub(uc_map$pattern[i], intToUtf8(strtoi(uc_map$unicode[i], base = 16)), out)
  }
  out
}
