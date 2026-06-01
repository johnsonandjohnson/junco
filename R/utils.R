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
#' @description `r lifecycle::badge("stable")`
#'
#' Extract (aligned) vectors from two data frames for two-sample statistical
#' analysis using complete-case (non-missing) observations.
#'
#' For unpaired data, values are extracted directly from each dataset with
#' missing values (`NA`) removed independently.
#'
#' For paired data, observations are first matched using the key variable(s)
#' specified in `paired_by`, and only complete pairs are retained.
#'
#' The function validates that `paired_by` uniquely identifies rows in each
#' dataset (after removing rows with incomplete values in `paired_by` columns)
#' and raises an error if duplicates are detected.
#'
#' @details
#' The function performs the following steps depending on the `paired` flag:
#'
#' \strong{Unpaired case (`paired = FALSE`):}
#' \enumerate{
#'   \item Extract `.var` from each dataset.
#'   \item Remove `NA` values independently from each vector.
#' }
#'
#' \strong{Paired case (`paired = TRUE`):}
#' \enumerate{
#'   \item Check that `paired_by` uniquely identifies rows in each dataset,
#'     considering only rows that are complete cases for the `paired_by` columns.
#'
#'   \item Merge `df1` and `df2` by the columns specified in `paired_by`.
#'   The merged data contains only the `paired_by` columns and the `.var` column
#'   from each dataset.
#'
#'   \item Remove rows containing any missing values (`NA`) in the merged data.
#'
#'   \item Extract aligned vectors corresponding to `.var`.
#' }
#'
#' This function is intended for internal use in two-sample statistical
#' procedures such as paired and unpaired t-tests.
#'
#' @param df1 (`data.frame`)\cr First dataset.
#' @param df2 (`data.frame`)\cr Second dataset.
#' @param .var (`character(1)`)\cr Name of the variable to extract from both
#'   datasets.
#' @param paired (`logical(1)`)\cr Whether the values in `df1[[.var]]` and
#'   `df2[[.var]]` should be treated as paired (matched) samples.
#' @param paired_by (`character`)\cr Column name(s) used to match observations
#'   between `df1` and `df2`. Required only if `paired = TRUE`.
#'
#' @return
#' A named `list` with:
#' \describe{
#'   \item{x1}{Non-missing values from `df1[[.var]]` after optional pairing.}
#'   \item{x2}{Non-missing values from `df2[[.var]]` after optional pairing.}
#' }
#'
#' Returned vectors may be shorter than the original inputs due to removal of
#' unmatched observations, missing values (`NA`).
#'
#' @keywords internal
#'
#' @importFrom stats complete.cases
#'
#' @examples
#' df1 <- data.frame(id = c("A", "B", "C", "D"), value = 1:4)
#' df2 <- data.frame(id = c("A", "C", "D", "E", "F"), value = c(11, 13:14, NA, 16))
#' df1
#' df2
#'
#' # Unpaired
#' extract_vectors(df1, df2, "value")
#'
#' # Paired
#' extract_vectors(df1, df2, "value", paired = TRUE, paired_by = "id")
#'
extract_vectors <- function(df1,
                            df2,
                            .var,
                            paired = FALSE,
                            paired_by) {
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

    df1_keys <- df1[complete.cases(df1[, paired_by]), paired_by]
    df2_keys <- df2[complete.cases(df2[, paired_by]), paired_by]

    if (any(duplicated(df1_keys))) {
      stop("Duplicate values in 'paired_by' columns in df1 (complete cases only).")
    }

    if (any(duplicated(df2_keys))) {
      stop("Duplicate values in 'paired_by' columns in df2 (complete cases only).")
    }

    suffixes <- c("_df1", "_df2")

    df <- merge(
      df1[, c(paired_by, .var), drop = FALSE],
      df2[, c(paired_by, .var), drop = FALSE],
      by = paired_by,
      suffixes = suffixes
    )
    df <- df[complete.cases(df), , drop = FALSE]

    varsfx <- paste0(.var, suffixes)
    x1 <- df[[varsfx[1]]]
    x2 <- df[[varsfx[2]]]
  } else {
    x1 <- df1[[.var]]
    x2 <- df2[[.var]]

    x1 <- x1[!is.na(x1)]
    x2 <- x2[!is.na(x2)]
  }

  list(x1 = x1, x2 = x2)
}


#' Helper for Finding AVISIT after which CHG are all Missing
#'
#' @description
#' Helper for Finding AVISIT after which CHG are all Missing.
#'
#' @param df (`data.frame`)\cr with `CHG` and `AVISIT` variables.
#'
#' @return A string with either the factor level after which `AVISIT` is all missing,
#'   or `NA`.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   AVISIT = factor(c(1, 2, 3, 4, 5)),
#'   CHG = c(5, NA, NA, NA, 3)
#' )
#' find_missing_chg_after_avisit(df)
#'
#' df2 <- data.frame(
#'   AVISIT = factor(c(1, 2, 3, 4, 5)),
#'   CHG = c(5, NA, 3, NA, NA)
#' )
#' find_missing_chg_after_avisit(df2)
#'
#' df3 <- data.frame(
#'   AVISIT = factor(c(1, 2, 3, 4, 5)),
#'   CHG = c(NA, NA, NA, NA, NA)
#' )
#' find_missing_chg_after_avisit(df3)
find_missing_chg_after_avisit <- function(df) {
  checkmate::assert_data_frame(df)
  checkmate::assert_factor(df$AVISIT, unique = TRUE, any.missing = FALSE)
  checkmate::assert_numeric(df$CHG)

  # Ensure the dataframe is sorted by AVISIT
  df <- df[order(df$AVISIT), ]

  # Last visit with available data.
  visit_levels_available <- as.integer(df[!is.na(df$CHG), ]$AVISIT)

  if (!length(visit_levels_available)) {
    return(levels(df$AVISIT)[1])
  }
  visit_levels_max_available <- max(visit_levels_available)

  # Visits with missing data.
  visit_levels_missing <- as.integer(df[is.na(df$CHG), ]$AVISIT)

  # Missing visits at the end.
  visit_levels_missing_end <- visit_levels_missing[
    visit_levels_missing > visit_levels_max_available
  ]

  # Return first one if there is any.
  if (length(visit_levels_missing_end)) {
    levels(df$AVISIT)[min(visit_levels_missing_end)]
  } else {
    NA_character_
  }
}