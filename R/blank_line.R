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

#' @title Pre-pend Row with Label to the Results of the Analysis Function.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @note If `x` is of class `RowsVerticalSection`, attributes `row_formats`,
#'   `row_na_strs`, `row_footnotes` are not preserved (if present).
#'
#' @param x (`list` or `CellValue` or `RowsVerticalSection`)\cr
#'   Results of the Analysis Function.
#' @param label (`string`)\cr A label to be appended.
#' @param label_indent (`integer(1)`)\cr An indent for the row with the
#'   prepended label.
#'
#' @returns `RowsVerticalSection` class object.
#' @export
#'
#' @examples
#' rvs <- in_rows(Mean = rcell(5), SD = rcell(1))
#' prepend_label_cell(rvs, "Descriptive Statistics", label_indent = 1L)
#'
prepend_label_cell <- function(x, label = "", label_indent = 0L) {
  checkmate::check_multi_class(x, c("list", "CellValue", "RowsVerticalSection"))
  if (class(x) == "list") {
    checkmate::assert_list(x, types = "CellValue", any.missing = FALSE)
  }
  checkmate::assert_string(label, na.ok = TRUE)
  checkmate::assert_int(label_indent)

  if (class(x) == "CellValue") {
    label_rcell <- rcell(NULL, label = label, indent_mod = label_indent)
    list(label_rcell, x)
  } else if (class(x) == "list") {
    label_rcell <- rcell(NULL, label = label, indent_mod = label_indent)
    c(list(label_rcell), x)
  } else if (class(x) == "RowsVerticalSection") {
    ret <- in_rows(.list = c(list(NULL), x))
    if (is.null(attr(x, "indent_mods"))) {
      attr(ret[[1]], "indent_mod") <- label_indent
    } else {
      attr(ret, "indent_mods") <- c(label_indent, attr(x, "indent_mods"))
    }
    attr(ret, "row_labels") <- c(label, attr(x, "row_labels"))
    # rvs_attr <- c("row_formats", "row_na_strs", "row_footnotes")
    # rvs_attr_gr1 <- sapply(attributes(x)[rvs_attr], length) > 1
    # rvs_attr_gr1 <- names(rvs_attr_gr1)[rvs_attr_gr1]
    # for (a in rvs_attr_gr1) {
    #   attr(ret, a) <- c(NA, attr(x, a))
    # }
    ret
  }
}
