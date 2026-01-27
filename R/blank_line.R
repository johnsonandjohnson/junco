#' Analysis and Content Summary Function Producing Blank Line
#'
#' @inheritParams proposal_argument_convention
#'
#' @export
ac_blank_line <- function(df, labelstr = "") {
  in_rows(.list = NA_real_, .labels = labelstr, .formats = "xx", .format_na_strs = "")
}

#' Insertion of Blank Lines in a Layout
#'
#' @inheritParams proposal_argument_convention
#'
#' @description This is a hack for `rtables` in order to be able to add row gaps,
#' i.e. blank lines.
#' In particular, by default this function needs to maintain a global state for avoiding
#' duplicate table names. The global state variable is hidden by using
#' a dot in front of its name. However, this likely won't work with parallelisation across
#' multiple threads and also causes non-reproducibility of the resulting `rtables`
#' object. Therefore also a custom table name can be used.
#'
#' @return The modified layout now including a blank line after the current
#'   row content.
#' @export
#'
#' @examples
#' ADSL <- ex_adsl
#'
#' lyt <- basic_table() |>
#'   split_cols_by("ARM") |>
#'   split_rows_by("STRATA1") |>
#'   analyze(vars = "AGE", afun = function(x) {
#'     in_rows(
#'       "Mean (sd)" = rcell(c(mean(x), sd(x)), format = "xx.xx (xx.xx)")
#'     )
#'   }) |>
#'   insert_blank_line() |>
#'   analyze(vars = "AGE", table_names = "AGE_Range", afun = function(x) {
#'     in_rows(
#'       "Range" = rcell(range(x), format = "xx.xx - xx.xx")
#'     )
#'   })
#' build_table(lyt, ADSL)
insert_blank_line <- function(lyt, table_names = NULL) {
  varnames_rows <- vars_in_layout(lyt@row_layout)
  checkmate::assert_character(varnames_rows, min.len = 1L)
  last_varname_rows <- utils::tail(varnames_rows, 1L)

  this_table_name <- if (is.null(table_names)) {
    default_table_name <- paste0(".post_", last_varname_rows, "_blank")

    # A named list for tracking table names and counts
    table_count <- getOption("junco.insert_blank_line")

    new_count <- if (default_table_name %in% names(table_count)) {
      table_count[[default_table_name]] + 1
    } else {
      1
    }

    table_count[[default_table_name]] <- new_count
    options(junco.insert_blank_line = table_count)

    paste(default_table_name, new_count, sep = "_")
  } else {
    checkmate::assert_string(table_names, min.chars = 1L)
    table_names
  }

  analyze(
    lyt,
    vars = last_varname_rows,
    afun = ac_blank_line,
    show_labels = "hidden",
    table_names = this_table_name
  )
}
