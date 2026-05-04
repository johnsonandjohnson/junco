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

add_blank_line_rcells <- function(ret) {
  # check that ret is expected structure and not NULL
  if (is.null(ret)) {
    stop("add_blank_line_rcells: ret cannot be NULL.")
  }
  if (!(class(ret) %in% c("RowsVerticalSection", "CellValue"))) {
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
  if (class(x) == "list") {
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
