#' Analysis and Content Summary Function Producing Blank Line
#'
#' @inheritParams proposal_argument_convention
#'
#' @export
ac_blank_line <- function(df, labelstr = "") {
  in_rows(.list = NA_real_, .labels = labelstr, .formats = "xx", .format_na_strs = "")
}

#' @title Insert a Single Line into a Layout
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Inserts a single-row "gap" or labeled line into an `rtables` layout by
#' appending an additional [rtables::analyze()] step for the most recently used
#' row-analysis variable. This is useful for adding visual separators or
#' headings between blocks of rows.
#'
#' @details
#' This helper appends another [rtables::analyze()] call targeting the last
#' variable present in the row layout (as returned by [rtables::vars_in_layout()]),
#' so it should be called after at least one prior analysis step has introduced
#' a row-analysis variable.
#'
#' If `table_names` is not provided, the function generates a unique internal
#' table name for the injected line. To avoid duplicate table names across
#' multiple invocations, it maintains a simple counter in the R option
#' `"junco.insert_line"`. This stateful approach:
#' - is not thread-safe and may not work with parallelisation across multiple
#'   threads/processes, and
#' - can make the resulting `rtables` object non-reproducible unless the option
#'   is controlled/reset between runs.
#'
#' For reproducible or parallel workflows, pass an explicit, globally unique
#' `table_names` value.
#'
#' @inheritParams proposal_argument_convention
#' @param afun (`function`) `rtables` analysis function that produces exactly one
#'   row of output (for example, a single `CellValue`). Defaults to `ac_blank_line`
#'   for inserting a blank or static labeled line.
#' @param ... Additional arguments passed to `afun` via
#'   `analyze(extra_args = list(...))`.
#'
#' @return A modified `PreDataTableLayouts` object containing the inserted line
#'   after the current row content.
#'
#' @seealso [rtables::analyze()], [rtables::vars_in_layout()]
#'
#' @export
#'
#' @examples
#' afun_N <- function(x) rcell(length(x), label = "N")
#' afun_mean <- function(x) rcell(mean(x), label = "Mean")
#'
#' # Blank line example
#' lyt <- basic_table() |>
#'   split_rows_by("STRATA1") |>
#'   analyze(vars = "AGE", afun = afun_N) |>
#'   insert_line() |>
#'   analyze(vars = "AGE", afun = afun_mean, table_names = "AGE_mean")
#'
#' build_table(lyt, ex_adsl)
#'
#' # Labelled line example
#' lyt2 <- basic_table() |>
#'   split_rows_by("STRATA1") |>
#'   analyze(vars = "AGE", afun = afun_N) |>
#'   insert_line(labelstr = "Point Estimates:") |>
#'   analyze(vars = "AGE", afun = afun_mean, table_names = "AGE_mean")
#'
#' build_table(lyt2, ex_adsl)
#'
insert_line <- function(lyt, afun = ac_blank_line, table_names = NULL, indent_mod = 0L, ...) {
  checkmate::assert_class(lyt, "PreDataTableLayouts")
  checkmate::assert_function(afun)
  checkmate::assert_string(table_names, min.chars = 1L, null.ok = TRUE)

  last_varname_rows <- utils::tail(vars_in_layout(lyt@row_layout), 1L)
  checkmate::assert_string(last_varname_rows, min.chars = 1L)

  if (is.null(table_names)) {
    tn <- paste0(".post_", last_varname_rows, "_line")

    # A named list for tracking table names and counts
    table_count <- getOption("junco.insert_line")
    table_count[[tn]] <- ifelse(tn %in% names(table_count), table_count[[tn]] + 1, 1)
    options(junco.insert_line = table_count)
    table_names <- paste(tn, table_count[[tn]], sep = "_")
  }

  analyze(
    lyt,
    vars = last_varname_rows,
    afun = afun,
    extra_args = list(...),
    show_labels = "hidden",
    table_names = table_names,
    indent_mod = indent_mod
  )
}

add_blank_line_rcells <- function(ret) {
  # check that ret is expected structure and not NULL
  if (is.null(ret)) {
    stop("add_blank_line_rcells: ret cannot be NULL.")
  }
  if (!(inherits(ret, "RowsVerticalSection") || inherits(ret, "CellValue"))) {
    stop("add_blank_line_rcells:  ret must be of class RowsVerticalSection or CellValue.")
  }

  if (inherits(ret, "RowsVerticalSection")) {
    xlabel <- attr(ret, "row_labels")
    indent_mods <- lapply(ret, function(obj) {
      attr(obj, "indent_mod")
    })
  } else {
    xlabel <- attr(ret, "label")
    indent_mods <- attr(ret, "indent_mod")
  }

  fmts <- lapply(ret, obj_format)
  na_strs <- lapply(ret, obj_na_str)
  # ret <- append(ret,rcell(NA_real_,format = 'xx')) use a character version for the new line, rather than NA - to
  # allow NA processing for other stuff
  ret <- append(ret, rcell(NA, format = "xx"))
  fmts <- append(fmts, "xx")
  na_strs <- append(na_strs, " ")
  indent_mods <- append(indent_mods, 0L)
  xlabel <- append(xlabel, " ")
  ret <- stats::setNames(ret, xlabel)

  # perform the update to add the extra line
  fret <- in_rows(
    .list = ret,
    .labels = xlabel,
    .formats = fmts,
    .format_na_strs = na_strs,
    .indent_mods = indent_mods
  )

  fret
}

#' @title Prepend Label Row to Analysis Output
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Adds a label row at the beginning of analysis output objects, such as
#' `CellValue`, `list` of `CellValue`s, or `RowsVerticalSection` objects.
#' These objects are returned by analysis functions used within the **rtables**
#' framework and are typically created via [rtables::rcell()] or
#' [rtables::in_rows()] functions.
#'
#' This is typically used to introduce section headers
#' (e.g., "Descriptive Statistics") in tabular or reporting outputs.
#'
#' @note If `x` is of class `RowsVerticalSection`, the attributes
#' `row_formats`, `row_na_strs`, and `row_footnotes` are not preserved.
#'
#' @param x (`list` or `CellValue` or `RowsVerticalSection`)\cr
#'   Analysis result object.
#' @param label (`character(1)`)\cr Label to be inserted as the first row.
#' @param label_indent_mod (`integer(1)`)\cr Indentation level applied to the
#'   label row.
#'
#' @returns A `RowsVerticalSection` object with the label row prepended.
#'
#' @importFrom rtables rcell in_rows
#'
#' @export
#'
#' @examples
#' rvs <- rtables::in_rows(Mean = rtables::rcell(5), Range = rtables::rcell(c(1, 8)))
#' prepend_label_cell(rvs, "Descriptive Statistics", label_indent_mod = 1L)
#'
prepend_label_cell <- function(x, label = "", label_indent_mod = 0L) {
  checkmate::check_multi_class(x, c("list", "CellValue", "RowsVerticalSection"))
  if (is.list(x)) {
    checkmate::assert_list(x, types = "CellValue", any.missing = FALSE)
  }
  checkmate::assert_string(label, na.ok = TRUE)
  checkmate::assert_int(label_indent_mod)

  if (is(x, "CellValue")) {
    label_rcell <- rtables::rcell(NULL, label = label, indent_mod = label_indent_mod)
    list(label_rcell, x)
  } else if (is(x, "list")) {
    label_rcell <- rtables::rcell(NULL, label = label, indent_mod = label_indent_mod)
    c(list(label_rcell), x)
  } else if (is(x, "RowsVerticalSection")) {
    ret <- rtables::in_rows(.list = c(list(NULL), x))
    if (is.null(attr(x, "indent_mods"))) {
      attr(ret[[1]], "indent_mod") <- label_indent_mod
    } else {
      attr(ret, "indent_mods") <- c(label_indent_mod, attr(x, "indent_mods"))
    }
    attr(ret, "row_labels") <- c(label, attr(x, "row_labels"))
    ret
  }
}
