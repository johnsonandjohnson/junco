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
  # Accept either character or factor
  if (is.factor(x)) {
    x <- as.character(x)
  }
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
  if (!argument %in% values || !is.null(.alt_df_full)) {
    return(invisible())
  }

  name <- deparse(substitute(argument))

  stop(sprintf(
    '`.alt_df_full` cannot be `NULL` when `%s` is `"%s"`',
    name, argument
  ))
}

#' @name restore_labels
#'
#' @title Restore Labels
#'
#' @description
#' function for retrieving labels from one dataframe to restore onto another.
#'
#' @param df Input dataframe, onto which to retrieve the labels from the original dataframe
#' @param orig_df Dataframe from which the labels will be retrieved
#' @param verbose logical if TRUE : Variables that have no label defined will be shown during execution. These will end up with label being the variable name.
#' @seealso Used in shell script tsfecg01
#' @return The dataframe df is returned where the labels are retrieved from the orig_df dataframe.
#' @export
#'
restore_labels <- function(df, orig_df, verbose = TRUE) {
  labels_df <- formatters::var_labels(df, fill = TRUE)
  labels_orig_df <- formatters::var_labels(orig_df, fill = TRUE)

  ## get vars that don't have a label
  miss_label <- labels_df[names(labels_df) == labels_df]
  ### vars with label
  non_miss_label <- labels_df[names(labels_df) != labels_df]

  ### retrieve the ones that are defined in the original label
  miss_label2 <- labels_orig_df[intersect(names(miss_label), names(labels_orig_df))]

  ### ones that will remain missing
  no_label <- setdiff(names(labels_df), names(c(miss_label2, non_miss_label)))
  names(no_label) <- no_label

  if (verbose & !(identical(no_label, character(0)))) {
    message(paste("Variables without a label on ", toupper(deparse(substitute(df))), ":", paste(no_label, collapse = ", ")))
  }

  ### this should include all vars again --- needed for applying var_labels <- step
  all_labels <- c(miss_label2, non_miss_label, no_label)
  ## match with the order of names on df
  all_labels <- all_labels[names(df)]

  all_labels[no_label] <- NA_character_

  ## apply the updated labels to the dataframe
  formatters::var_labels(df) <- all_labels


  attr(df, "label") <- attr(orig_df, "label")

  return(df)
}