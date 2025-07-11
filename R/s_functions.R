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
