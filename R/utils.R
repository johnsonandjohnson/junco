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

#' Extract the right-hand side of a formula
#'
#' @param x (`formula`)\cr A two-sided formula, e.g., `y ~ x1 + x2`.
#'
#' @return (`character(1)`) The right-hand side of the formula.
#'
#' @examples
#' rightside(y ~ x1 + x2)
#'
#' @export
rightside <- function(x) {
  checkmate::assert_formula(x)
  res <- x[[3L]]
  if (is.character(res) && length(res) == 1L) {
    res <- as.character(res)
  } else {
    res <- paste(deparse(res), collapse = "")
  }
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
#' @author WW
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


#'
#' @description
#' Helper for transposing a named list of named lists, where inner lists have same names
#'
#' @param x (`list`)\cr with depth of 2.
#'
#' @return Transposed version of list where inner elements now are outer elements.
#' @noRd
#' @keywords internal
transpose_named_list <- function(x) {
  # x: named list of named lists
  keys <- unique(lapply(x, names))
  if (length(keys) != 1) {
    stop("Input list must have same names on all sublists")
  } else {
    keys <- keys[[1]]
  }
  # rebuild structure
  setNames(
    lapply(keys, function(k) {
      setNames(
        lapply(x, function(inner_list) {
          inner_list[[k]]
        }),
        names(x)
      )
    }),
    keys
  )
}

#' @title Copy missing attributes to an object
#'
#' @noRd
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Copies attributes from `source` to `target` that are not already present
#' on `target`. Existing attributes of `target` are preserved.
#'
#' @param source An object providing the attributes to copy.
#' @param target An object to which missing attributes are copied.
#'
#' @return The `target` object with any missing attributes copied from `source`.
#'
#' @author WW
#' @keywords internal
#' @seealso [factor_by_order()]
#'
#' @examples
#'
#' x <- factor(c("Placebo", "Placebo", "Drug X"))
#'
#' x_labeled <- formatters::with_label(x, label = "Treatment Group")
#' attributes(x_labeled)
#'
#' # Copy the `label` attribute from `x_labeled` to `x`.
#' x_copy <- copy_attributes(x_labeled, x)
#' attributes(x_copy)
#'
copy_attributes <- function(source, target) {
  source_attr <- attributes(source)
  if (is.null(source_attr)) {
    return(target)
  }

  target_attr <- attributes(target)

  to_copy <- setdiff(names(source_attr), names(target_attr))
  attributes(target) <- c(target_attr, source_attr[to_copy])
  target
}

#' @title Check whether two vectors define a one-to-one correspondence
#'
#' @noRd
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Tests whether `x` and `y` define a bijection between their unique non-missing
#' values.
#'
#' Missing values are treated specially: an `NA` in `x` must correspond to an
#' `NA` in `y` at the same position for the function to return `TRUE`.
#' Otherwise, the function returns `FALSE`.
#'
#' @note Factors are not supported because they may contain unused levels that
#'   do not appear in the observed data. For such levels, the corresponding
#'   values in the other vector are not available, and therefore a bijection
#'   cannot be determined.
#'
#' @param x (`character` or `numeric`)\cr A vector defining one side of the
#'   mapping.
#' @param y (`character` or `numeric`)\cr A vector defining the other side of
#'   the mapping. Must have the same length as `x`.
#'
#' @return A single logical value indicating whether `x` and `y` define a
#'   bijection.
#'
#' @seealso [factor_by_order()]
#' @keywords internal
#' @author WW
#'
#' @examples
#' is_bijection(c("A", "A", "B"), c(1, 1, 2))
#'
#' is_bijection(c("A", "B"), c(1, 1))
#'
#' is_bijection(c("A", NA), c(1, NA))
#'
#' is_bijection(c("A", NA), c(1, 2))
is_bijection <- function(x, y) {
  checkmate::assert(
    checkmate::test_character(x) || checkmate::test_numeric(x)
  )
  checkmate::assert(
    checkmate::test_character(y, len = length(x)) ||
      checkmate::test_numeric(y, len = length(x))
  )

  # Missingness must align perfectly at the vector level.
  if (!all(is.na(x) == is.na(y))) {
    return(FALSE)
  }

  # Check bijection.
  all(y == y[match(x, x)], na.rm = TRUE)


  n_unique_x <- length(unique(x))
  n_unique_y <- length(unique(y))

  bijection <- if (n_unique_x != n_unique_y) {
    FALSE
  } else {
    x_first_positions <- match(x, x)
    all(y == y[x_first_positions], na.rm = TRUE)
  }
  return(bijection)
}

#' @title Create a factor with levels ordered by a separate ordering vector
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Converts a character vector or factor into a factor where the level order is
#' determined by a second integer-like vector containing the corresponding order
#' values.
#'
#' @details The values in `x` and `y` must define a bijection between the unique
#'   non-missing values: each unique value in `x` must correspond to exactly one
#'   unique value in `y`, and vice versa.
#'   Missing values are handled separately: `NA` values in `x` and `y` must
#'   occur at the same positions. Missing values are not included as factor
#'   levels.
#'
#' @param x (`character` or `factor`)\cr A vector to be converted to a factor.
#' @param y (`integerish`)\cr A vector defining the order of levels in `x`.
#'   Must have the same length as `x`.
#' @param ordered (`logical(1)`)\cr Indicates whether the result should be
#'   an ordered factor. Defaults to `FALSE`.
#'
#' @return A factor created from `x`, with levels ordered according to `y`.
#'   Attributes of `x` other than `class` and `levels` are preserved. If
#'   `ordered = TRUE`, an ordered factor is returned.
#'
#' @author WW
#'
#' @export
#' @examples
#' factor_by_order(c("A", "A", "B"), c(1, 1, 2))
#'
#' factor_by_order(c("A", "A", "B"), c(1, 1, 2), ordered = TRUE)
#'
#' factor_by_order(c("A", "A", "B"), c(2, 2, 1))
#'
#' factor_by_order(c("A", "A", "B", NA), c(1, 1, 2, NA))
#'
#' \dontrun{
#' factor_by_order(c("A", "A", "B", NA), c(1, 2, 2, 4))
#' # Error: `x` and `y` must define a bijection between their unique non-NA values; NA values must correspond.
#' }
#'
factor_by_order <- function(x, y, ordered = FALSE) {
  checkmate::assert_multi_class(x, classes = c("character", "factor"))
  checkmate::assert_integerish(y, len = length(x))
  checkmate::assert_flag(ordered)

  x_char <- as.character(x)

  if (!is_bijection(x_char, y)) {
    stop("`x` and `y` must define a bijection between their unique non-NA values; NA values must correspond.")
  }

  factor_levels <- unique(x_char[order(y)])
  factor_levels <- factor_levels[!is.na(factor_levels)]
  f <- factor(x_char, levels = factor_levels, ordered = ordered)

  # Preserve non-factor attributes of `x`.
  copy_attributes(source = x, target = f)
}
