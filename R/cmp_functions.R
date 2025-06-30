#' Split Function for Compliance Columns (TEFSCNCMP01 e.g.)
#'
#' Here we just split into 3 columns for expected, received and missing visits.
#'
#' @inheritParams proposal_argument_convention
#' @param ret (`list`)\cr result from previous split function steps.
#' @param spl (`split`)\cr split object.
#' @param fulldf (`data.frame`)\cr full data frame.
#' @param vals (`character`)\cr values to use for the split.
#' @param trim (`logical`)\cr whether to trim the values.
#'
#' @note This split function is used in the proportion table TEFSCNCMP01 and similar ones.
#' @seealso [rtables::make_split_fun()] describing the requirements for this kind of
#'   post-processing function.
cmp_post_fun <- function(ret, spl, fulldf, .spl_context) {
  short_split_result(
    expected = "Expected, N",
    received = "Received, n (%)",
    missing = "Missing, n (%)",
    fulldf = fulldf
  )
}
#' @rdname cmp_post_fun
#' @export
cmp_split_fun <- make_split_fun(post = list(cmp_post_fun))

#' @title Summary Analysis Function for Compliance Columns (TEFSCNCMP01 e.g.)
#' @description A simple statistics function which prepares the numbers with percentages
#'   in the required format, for use in a split content row. The denominator here is
#'   from the expected visits column.
#' @inherit proposal_argument_convention
#' @param variables (`list`)\cr with variable names of logical columns for
#'   `expected`, `received` and `missing` visits.
#' @param formats (`list`)\cr with the `count_percent` format to use for the received
#'   and missing visits columns.
#' @return The [rtables::in_rows()] result with the counts and proportion statistics.
#' @seealso [cmp_post_fun()] for the corresponding split function.
#' @export
cmp_cfun <- function(df, labelstr, .spl_context, variables, formats) {
  this_col_split <- .spl_context[nrow(.spl_context), "cur_col_split_val"][[1]]
  this_afun_col <- this_col_split[2]

  exp <- df[[variables$expected]]
  checkmate::assert_logical(exp)
  n_exp <- sum(exp)
  rec <- df[[variables$received]]
  mis <- df[[variables$missing]]

  if (this_afun_col == "expected") {
    in_rows(n_exp, .labels = labelstr, .formats = "xx.")
  } else if (this_afun_col == "received") {
    checkmate::assert_logical(rec)
    checkmate::assert_true(all(exp[rec]))

    # Check if count_percent format is available
    if (!is.null(formats) && !("count_percent" %in% names(formats))) {
      stop("'count_percent' format must be provided in the formats argument")
    }

    in_rows(sum(rec) * c(1, 1 / n_exp), .formats = formats$count_percent, .labels = labelstr)
  } else if (this_afun_col == "missing") {
    checkmate::assert_logical(mis)
    checkmate::assert_true(all(exp[mis]))
    checkmate::assert_true(!any(rec[mis]))

    # Check if count_percent format is available
    if (!is.null(formats) && !("count_percent" %in% names(formats))) {
      stop("'count_percent' format must be provided in the formats argument")
    }

    in_rows(sum(mis) * c(1, 1 / n_exp), .formats = formats$count_percent, .labels = labelstr)
  }
}
