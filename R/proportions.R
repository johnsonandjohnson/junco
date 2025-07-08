#' @title s_function for proportion of factor levels
#' @description A simple statistics function which prepares the numbers with percentages
#'   in the required format. The denominator here is from the alternative counts data set
#'   in the given row and column split.
#'
#'   If a total row is shown, then here just the total number is shown (without 100%).
#' @param x (`factor`)\cr categorical variable we want to analyze.
#' @param .alt_df (`data.frame`)\cr alternative data frame used for denominator calculation.
#' @param use_alt_counts (`flag`)\cr whether the `.alt_df` should be used for the total, i.e. the denominator.
#'   If not, then the number of non-missing values in `x` is used.
#' @param show_total (`string`)\cr show the total level optionally on the top or in the bottom
#'   of the factor levels.
#' @param total_label (`string`)\cr which label to use for the optional total level.
#' @return The [rtables::in_rows()] result with the proportion statistics.
#' @seealso [s_proportion_logical()] for tabulating logical `x`.
#' @export
s_proportion_factor <- function(
    x,
    .alt_df,
    use_alt_counts = TRUE,
    show_total = c("none", "top", "bottom"),
    total_label = "Total") {
  checkmate::assert_factor(x)
  checkmate::assert_flag(use_alt_counts)
  show_total <- match.arg(show_total)

  N <- if (use_alt_counts) nrow(.alt_df) else sum(!is.na(x))
  tab <- lapply(as.list(table(x)), function(xi) rcell(xi * c(1, 1 / N), format = jjcsformat_count_fraction))
  if (show_total != "none") {
    checkmate::assert_string(total_label)
    tab_total <- stats::setNames(list(rcell(N, format = "xx")), total_label)

    tab <- if (show_total == "top") {
      c(tab_total, tab)
    } else {
      c(tab, tab_total)
    }
  }
  in_rows(.list = tab)
}

#' @title s_function for proportion of `TRUE` in logical vector
#' @description A simple statistics function which prepares the numbers with percentages
#'   in the required format. The denominator here is from the alternative counts data set
#'   in the given row and column split.
#' @param x (`logical`)\cr binary variable we want to analyze.
#' @param label (`string`)\cr label to use.
#' @param .alt_df (`data.frame`)\cr alternative data frame used for denominator calculation.
#' @return The [rtables::in_rows()] result with the proportion statistics.
#' @seealso [s_proportion_factor()] for tabulating factor `x`.
#' @export
s_proportion_logical <- function(x, label = "Responders", .alt_df) {
  n <- sum(x)
  N <- nrow(.alt_df)
  p_hat <- n / N
  list(n_prop = with_label(rcell(c(n, p_hat), format = jjcsformat_count_fraction), label))
}

#' @title c_function for proportion of `TRUE` in logical vector
#' @description A simple statistics function which prepares the numbers with percentages
#'   in the required format, for use in a split content row. The denominator here is
#'   from the column N. Note that we don't use here .alt_df because that might not
#'   have required row split variables available.
#' @param x (`logical`)\cr binary variable we want to analyze.
#' @param labelstr (`string`)\cr label string.
#' @param labelstr (`string`)\cr label string.
#' @param label_fstr (`string`)\cr format string for the label.
#' @param format (`character` or `list`)\cr format for the statistics.
#' @param .N_col (`numeric`)\cr number of columns.
#' @param .N_col (`numeric`)\cr number of columns.
#' @return The [rtables::in_rows()] result with the proportion statistics.
#' @seealso [s_proportion_logical()] for the related statistics function.
#' @export
c_proportion_logical <- function(x, labelstr, label_fstr, format, .N_col) {
  checkmate::assert_logical(x)
  num <- sum(x)
  denom <- .N_col
  in_rows(est_prop = c(num, num / denom), .formats = format, .labels = sprintf(label_fstr, labelstr))
}

#' Helper Function to Create Logical Design Matrix from Factor Variable
#'
#' @param df (`data.frame`)\cr including a factor variable with name in `.var`.
#' @param .var (`string`)\cr name of the factor variable.
#'
#' @return The logical matrix with dummy encoding of all factor levels.
#' @keywords internal
#' @export
#' @examples
#' h_get_design_mat(df = data.frame(a = factor(c("a", "b", "a"))), .var = "a")
h_get_design_mat <- function(df, .var) {
  checkmate::assert_data_frame(df)
  checkmate::assert_string(.var)
  checkmate::assert_factor(df[[.var]])

  model_formula <- stats::as.formula(paste("~ -1 +", .var))
  design_mat <- stats::model.matrix(model_formula, df)
  mode(design_mat) <- "logical"
  colnames(design_mat) <- levels(df[[.var]])
  design_mat
}

#' Formatted Analysis Function For Proportion Confidence Interval for Logical
#'
#' @param x (`logical`)\cr including binary response values.
#' @param .alt_df (`data.frame`)\cr alternative data frame used for denominator calculation.
#' @param formats (`list`)\cr including element `prop_ci` with the
#'   required format. Note that the value is in percent already.
#' @param method (`string`)\cr please see [tern::s_proportion()] for possible
#'   methods.
#' @param conf_level (`numeric`)\cr confidence level for the confidence interval.
#' @param conf_level (`numeric`)\cr confidence level for the confidence interval.
#'
#' @return The [rtables::rcell()] result.
#' @export
#'
#' @examples
#' a_proportion_ci_logical(
#'   x = DM$SEX == "F",
#'   .alt_df = DM,
#'   conf_level = 0.95,
#'   formats = list(prop_ci = jjcsformat_xx("xx.xx% - xx.xx%")),
#'   method = "wald"
#' )
a_proportion_ci_logical <- function(x, .alt_df, conf_level, method, formats) {
  checkmate::assert_logical(x)
  checkmate::assert_data_frame(.alt_df)
  checkmate::assert_list(formats)

  diff_n <- nrow(.alt_df) - length(x)
  if (diff_n > 0) {
    x <- c(x, rep(FALSE, length = diff_n))
  }
  est <- s_proportion(x, conf_level = conf_level, method = method)
  rcell(est$prop_ci, format = formats$prop_ci)
}

#' Formatted Analysis Function For Proportion Confidence Interval for Factor
#'
#' @param df (`data.frame`)\cr including factor `.var`.
#' @param .var (`string`)\cr name of the factor variable.
#' @param .var (`string`)\cr name of the factor variable.
#' @param \dots see [a_proportion_ci_logical()] for additionally required
#'   arguments.
#'
#' @return The [rtables::rcell()] result.
#' @export
#'
#' @examples
#' a_proportion_ci_factor(
#'   df = DM,
#'   .var = "SEX",
#'   .alt_df = DM,
#'   conf_level = 0.95,
#'   formats = list(prop_ci = jjcsformat_xx("xx.x%, xx.x%")),
#'   method = "clopper-pearson"
#' )
a_proportion_ci_factor <- function(df, .var, ...) {
  checkmate::assert_factor(df[[.var]])

  design_mat <- h_get_design_mat(df, .var)
  design_df <- as.data.frame(design_mat)
  res <- lapply(design_df, a_proportion_ci_logical, ...)
  in_rows(.list = res, .labels = colnames(design_mat))
}

#' Split Function for Proportion Analysis Columns (TEFCGIS08 e.g.)
#'
#' Here we just split into 3 columns `n`, `%` and `Cum %`.
#'
#' @param ret (`list`)\cr return value from the previous split function.
#' @param spl (`list`)\cr split information.
#' @param fulldf (`data.frame`)\cr full data frame.
#' @param .spl_context (`environment`)\cr split context environment.
#'
#' @note This split function is used in the proportion table TEFCGIS08 and similar ones.
#' @seealso [rtables::make_split_fun()] describing the requirements for this kind of
#'   post-processing function.
prop_post_fun <- function(ret, spl, fulldf, .spl_context) {
  short_split_result(n = "n", percent = "%", cum_percent = "Cum %", fulldf = fulldf)
}

#' @rdname prop_post_fun
#'
#' @param df A data frame that contains all analysis variables.
#' @param vals A character vector that contains values to use for the split.
#' @param labels A character vector that contains labels for the statistics (without indent).
#' @param trim A single logical that indicates whether to trim the values.
#' @return a split function for use in [rtables::split_rows_by].
#' @export
prop_split_fun <- make_split_fun(post = list(prop_post_fun))

#' Formatted Analysis Function for Proportion Analysis (TEFCGIS08 e.g.)
#'
#' This function applies to a factor `x` when a column split was prepared with
#' [prop_split_fun()] before.
#'
#' @details In the column named `n`, the counts of the categories as well as an
#' optional `Total` count will be shown. In the column named `percent`, the
#' percentages of the categories will be shown, with an optional blank entry for
#' `Total`. In the column named `cum_percent`, the cumulative percentages will
#' be shown instead.
#'
#' @param x (`factor`)\cr factor variable to analyze.
#' @param .spl_context (`environment`)\cr split context environment.
#' @param formats (`list`)\cr formats for the statistics.
#' @param add_total_level (`flag`)\cr whether to add a total level.
#' @param x (`factor`)\cr factor variable to analyze.
#' @param .spl_context (`environment`)\cr split context environment.
#' @param formats (`list`)\cr formats for the statistics.
#' @param add_total_level (`flag`)\cr whether to add a total level.
#'
#' @return A `VerticalRowsSection` as returned by [rtables::in_rows].
#' @export
prop_table_afun <- function(x, .spl_context, formats, add_total_level = FALSE) {
  checkmate::assert_list(formats, len = 3, names = "unique")
  checkmate::assert_names(names(formats), permutation.of = c("n", "percent", "cum_percent"))
  checkmate::assert_flag(add_total_level)

  stat <- utils::tail(.spl_context[nrow(.spl_context), "cur_col_split_val"][[1]], 1)
  optional_total <- function(x) if (add_total_level) x else NULL
  ns <- table(x)
  res <- if (stat == "n") {
    c(as.list(ns), optional_total(length(x)))
  } else if (stat == "percent") {
    c(as.list(ns / length(x) * 100), optional_total(list(NULL)))
  } else if (stat == "cum_percent") {
    c(as.list(cumsum(ns / length(x) * 100)), optional_total(list(NULL)))
  } else {
    stop("unexpected proportion statistic in split")
  }
  in_rows(.list = res, .labels = c(levels(x), optional_total("Total")), .formats = formats[[stat]])
}
