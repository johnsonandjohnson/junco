#' Extract the left-hand side of a formula
#'
#' @param x (`formula`)\cr A two-sided formula, e.g., `y ~ x1 + x2`.
#'
#' @return (`character(1)`) The name of the left-hand side of the formula.
#'
#' @examples
#' leftside(y ~ x)
#'
#' @export
leftside <- function(x) {
  checkmate::assert_formula(x)
  res <- x[[2L]]
  res <- as.character(res)
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
#' @param x (`character` or `factor`)\cr Input string
#' @return x converted to title case (first letter of each word capitalized)
#' @export
#' @keywords internal
#' @examples
#' x <- c("THIS IS an eXaMple", "statement TO CAPItaliZe")
#' string_to_title(x)
#'
#' x <- factor(
#'   c("OPTIMAL DOSE", "UNDERDOSE"),
#'   levels = c("OPTIMAL DOSE", "UNDERDOSE", "OVERDOSE")
#' )
#' string_to_title(x)
#'
string_to_title <- function(x) {
  checkmate::assert(
    checkmate::check_character(x, null.ok = TRUE),
    checkmate::check_factor(x, null.ok = TRUE)
  )

  pattern <- "(^|\\s)(\\w)"
  replacement <- "\\1\\U\\2"

  if (is.factor(x)) {
    y <- levels(x)
    y_title <- gsub(pattern, replacement, tolower(y), perl = TRUE)
    levels(x) <- y_title
    x
  } else {
    gsub(pattern, replacement, tolower(x), perl = TRUE)
  }
}

#' Check If `.alt_df_full` Is `NULL`
#'
#' For example, in `a_patyrs_j()`, if `source` is `"alt_df"`, we need to
#' check if `.alt_df_full` is `NULL`.
#'
#' @noRd
check_alt_df_full <- function(argument, values, .alt_df_full) {
  if (!argument %in% values || !is.null(.alt_df_full)) {
    return(invisible())
  }

  name <- deparse(substitute(argument))

  stop(sprintf(
    '`.alt_df_full` cannot be `NULL` when `%s` is `"%s"`',
    name, argument
  ))
}

#' @title Extract Vectors for Two-Sample Analysis
#'
#' @noRd
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Extracts numeric vectors from two data frames for two-sample statistical
#' analysis. The function supports both unpaired and paired settings.
#'
#' For unpaired data, values are extracted directly from each dataset with
#' missing values removed independently.
#'
#' For paired data, observations are first matched using the key variable(s)
#' specified in `paired_by`, and only complete pairs are retained.
#'
#' @details
#' The function performs the following steps depending on the `paired` flag:
#'
#' \strong{Unpaired case:}
#' \enumerate{
#'   \item Extract `.var` from each dataset.
#'   \item Remove `NA` values independently from each vector.
#' }
#'
#' \strong{Paired case:}
#' \enumerate{
#'   \item Checks that `paired_by` uniquely identifies rows in both datasets.
#'   \item Merges `df1` and `df2` by `paired_by`.
#'   \item Removes rows with missing values in either matched variable.
#'   \item Extracts aligned numeric vectors.
#' }
#'
#' Missing values (`NA`, `NaN`) in pairing variables are treated as
#' incomparable and excluded during matching.
#'
#' This function is intended for internal use in two-sample statistical
#' procedures such as paired and unpaired t-tests.
#'
#' @param df1 (`data.frame`)\cr First dataset.
#' @param df2 (`data.frame`)\cr Second dataset.
#' @param .var (`character(1)`)\cr Name of the numeric variable to extract from
#'   both datasets.
#' @param paired (`logical(1)`)\cr Whether the values in `df1[[.var]]` and
#'   `df2[[.var]]` should be treated as paired (matched) samples.
#' @param paired_by (`character`)\cr Column name(s) used to match observations
#'   between `df1` and `df2`. Required only if `paired = TRUE`.
#'
#' @return
#' A named `list` with:
#' \describe{
#'   \item{x1}{Numeric vector from `df1`.}
#'   \item{x2}{Numeric vector from `df2`.}
#' }
#'
#' @keywords internal
#'
#' @importFrom stats complete.cases
#'
#' @examples
#' df1 <- data.frame(id = c("A", "B", "C", "D"), value = 1:4)
#' df2 <- data.frame(
#'   id = c("A", "C", "D", "E", "F"),
#'   value = c(11, 13:14, NA, 16)
#' )
#' df1
#' df2
#'
#' # Unpaired
#' extract_vectors(df1, df2, "value")
#'
#' # Paired
#' extract_vectors(df1, df2, "value", paired = TRUE, paired_by = "id")
#'
extract_vectors <- function(df1, df2, .var, paired = FALSE, paired_by) {
  checkmate::assert_data_frame(df1)
  checkmate::assert_data_frame(df2)
  checkmate::assert_string(.var)
  checkmate::assert_names(colnames(df1), must.include = .var)
  checkmate::assert_names(colnames(df2), must.include = .var)
  checkmate::assert_flag(paired)

  if (paired) {
    checkmate::assert_character(paired_by)
    checkmate::assert_names(colnames(df1), must.include = paired_by)
    checkmate::assert_names(colnames(df2), must.include = paired_by)

    if (any(duplicated(df1[, paired_by]))) {
      stop("df1: 'paired_by' must uniquely identify rows.")
    }
    if (any(duplicated(df2[, paired_by]))) {
      stop("df2: 'paired_by' must uniquely identify rows.")
    }

    pvcols <- c(paired_by, .var)
    suffixes <- c("_df1", "_df2")

    df <- merge(
      df1[, pvcols, drop = FALSE],
      df2[, pvcols, drop = FALSE],
      by = paired_by,
      suffixes = suffixes,
      incomparables = c(NA, NaN)
    )

    cols_var <- paste0(.var, suffixes)
    df <- df[complete.cases(df[, cols_var]), ]
    x1 <- df[[cols_var[1]]]
    x2 <- df[[cols_var[2]]]
  } else {
    x1 <- df1[[.var]]
    x2 <- df2[[.var]]

    x1 <- x1[!is.na(x1)]
    x2 <- x2[!is.na(x2)]
  }

  list(x1 = x1, x2 = x2)
}
