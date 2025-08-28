#' @name  response_by_var
#' @title Count denom fraction statistic
#'
#' @description Derives the count_denom_fraction statistic (i.e., 'xx /xx (xx.x percent)' )\cr
#'              Summarizes the number of unique subjects with a response = 'Y' for a given variable
#'              (e.g. TRTEMFL) within each category of another variable (e.g., SEX).
#'              Note that the denominator is derived using input df,
#'              in order to have these aligned with alt_source_df, it is expected that df includes all subjects.
#'
#' @details This is an analysis function for use within `analyze`. Arguments
#'          `df`, `.var` will be populated automatically by rtables during
#'          the tabulation process.
#'
#' @param df           (`data.frame`)\cr Name of dataframe being analyzed.
#' @param labelstr     (`character vector`)\cr Custom label for the variable being analyzed.
#' @param .var         (`character`)\cr Name of the variable being analyzed. Records with non-missing
#'                     values will be counted in the denominator.
#' @param .N_col       (`numeric`)\cr The total for the current column.
#' @param resp_var     (`character`)\cr Name of variable, for which, records with a value of 'Y'
#'                     will be counted in the numerator.
#' @param id           (`character`)\cr Name of column in df which will have patient identifiers
#' @param .format      (`character`)\cr Format for the count/denominator/fraction output.
#' @param ...          Additional arguments passed to the function.
#' @examples
#' if (require("pharmaverseadamjnj")) {
#'   library(dplyr)
#'
#'   ADAE <- pharmaverseadamjnj::adae |>
#'     select(USUBJID, SEX, TRT01A, TRTEMFL)
#'
#'   lyt <- basic_table() |>
#'     split_cols_by("TRT01A") |>
#'     analyze(
#'       vars = "SEX",
#'       var_labels = "Sex, n/Ns (%)",
#'       show_labels = "visible",
#'       afun = response_by_var,
#'       extra_args = list(resp_var = "TRTEMFL"),
#'       nested = FALSE
#'     )
#'
#'   result <- build_table(lyt, ADAE)
#'
#'   result
#' }
#' @return a `RowsVerticalSection` for use by the internal tabulation machinery of `rtables`
#' @export
response_by_var <- function(
    df,
    labelstr = NULL,
    .var,
    .N_col,
    resp_var = NULL,
    id = "USUBJID",
    .format = jjcsformat_count_denom_fraction,
    ...) {
  # Derive statistics: xx / xx (xx.x%)

  if (is.null(resp_var)) {
    stop("afun response_by_var: resp_var cannot be NULL.")
  }

  resp_var_values <- unique(df[[resp_var]][!is.na(df[[resp_var]])])
  if (is.character(df[[resp_var]]) && any(is.na(df[[resp_var]])) && all(resp_var_values == "Y")) {
    stop(paste(
      "afun response_by_var: not clear if missing resp_var should be considered",
      "non-response. Please make it a factor with appropriate Y(/N) levels."
    ))
  }

  if (length(setdiff(resp_var_values, c("Y", "N"))) > 0) {
    stop("afun response_by_var: resp_var must contain only Y/N values.")
  }

  df <- df[!is.na(df[[.var]]), ]

  if ( # nolint start
    (is.factor(df[[resp_var]]) &&
      (identical(levels(df[[resp_var]]), c("Y", "N")) || identical(levels(df[[resp_var]]), c("N", "Y")))) ||
      is.character(df[[resp_var]])
  ) { # nolint end
    # missing values in resp_var should be excluded, not considered as not met response subject will then not
    # contribute to denominator
    df <- df[!is.na(df[[resp_var]]), ]
  }

  ## in other cases, missing values in resp_var will be considered not met response and subject will contribute to
  ## denominator
  varvec <- df[[.var]]

  # For group summaries, varvec will be a singular value, equal to the current split, whereas for afun summaries,
  # varvec will consist of all possible values of var.
  if (!is.null(labelstr)) {
    levs <- labelstr
  } else {
    levs <- if (is.factor(varvec)) levels(varvec) else unique(varvec)
  }

  fn <- function(levii) {
    dfii <- df[df[[.var]] == levii, ]

    den <- NROW(unique(dfii[, id]))
    num <- NROW(unique(dfii[dfii[[resp_var]] == "Y" & !is.na(dfii[[resp_var]]), id]))

    rcell(c(num * 1, den * 1, num * 1 / den), format = .format)
  }

  cls <- lapply(levs, fn)
  names(cls) <- levs

  # Hand off results to analyze

  in_rows(.list = cls)
}
