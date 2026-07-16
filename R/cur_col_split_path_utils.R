#' @name cur_col_split_path_utils
#'
#' @title Utilities for the Current Column Split Path
#'
#' @description `r lifecycle::badge("stable")`
#'
#' These helper functions are intended for use in [rtables] custom analysis
#' functions that depend on the current column split context.
#'
#' @inheritParams proposal_argument_convention
#'
#' @author WW
#'
#' @seealso
#' - [rtables::col_paths()] for obtaining column paths from a built table.
#'
#' @examples
#' .spl_context_1 <- data.frame(
#'   cur_col_split = I(list(c("ARM"))),
#'   cur_col_split_val = I(list(c("Placebo")))
#' )
#'
#' .spl_context_2 <- data.frame(
#'   cur_col_split = I(list(c("ARM", "desc_stat"))),
#'   cur_col_split_val = I(list(c("Placebo", "N")))
#' )
#'
NULL

#' @describeIn cur_col_split_path_utils
#' Get the current column split path.
#'
#' @return
#' * `cur_col_split_path()` returns a character vector containing the current
#'   column split path extracted from `.spl_context`, interleaved as
#'   `c(var1, val1, var2, val2, ...)`.
#'
#' @export
#'
#' @examples
#' cur_col_split_path(.spl_context_1)
#' cur_col_split_path(.spl_context_2)
#'
cur_col_split_path <- function(.spl_context) {
  checkmate::assert_data_frame(.spl_context, min.rows = 1L)
  checkmate::assert_subset(c("cur_col_split", "cur_col_split_val"), choices = colnames(.spl_context))
  checkmate::assert_list(.spl_context[nrow(.spl_context), "cur_col_split"], min.len = 1L)
  checkmate::assert_list(.spl_context[nrow(.spl_context), "cur_col_split_val"], min.len = 1L)
  checkmate::assert_character(.spl_context[nrow(.spl_context), "cur_col_split"][[1]], names = "unnamed")
  checkmate::assert_character(.spl_context[nrow(.spl_context), "cur_col_split_val"][[1]], names = "unnamed")
  checkmate::assert_true(
    length(.spl_context[nrow(.spl_context), "cur_col_split"][[1]]) ==
      length(.spl_context[nrow(.spl_context), "cur_col_split_val"][[1]])
  )

  leaf_splc <- .spl_context[nrow(.spl_context), ]
  c(rbind(leaf_splc$cur_col_split[[1]], leaf_splc$cur_col_split_val[[1]]))
}

#' @describeIn cur_col_split_path_utils
#' Determine whether the current column split path is a reference path.
#'
#' @param ref_path (`character` or `NULL`)\cr
#'   Reference column path specified as alternating split variable names and
#'   split values. Must have at least two elements and an even length, e.g.
#'   `c("ARM", "Placebo")` or
#'   `c("ARM", "*", "desc_stat", "N")`; see [rtables::col_paths()].
#'
#'   The special value `"*"` can be used as a wildcard to match any split
#'   variable name or value.
#'
#'   `NULL` can be used to indicate that no reference path is specified,
#'   in which case the function returns `FALSE`, regardless of `.spl_context`.
#'
#' @return
#' * `in_ref_col()` returns a single logical value indicating whether the
#' current column split matches the specified `ref_path`.
#'
#' @export
#'
#' @examples
#' in_ref_col(c("ARM", "Placebo"), .spl_context_1)
#' in_ref_col(c("ARM", "*"), .spl_context_1)
#' in_ref_col(c("ARM", "X"), .spl_context_1)
#'
#' in_ref_col(c("ARM", "*", "desc_stat", "N"), .spl_context_2)
#' in_ref_col(c("ARM", "*", "desc_stat", "Mean"), .spl_context_2)
#'
#' # Use in an rtables custom analysis function.
#'
#' data <- formatters::ex_adsl
#' data$desc_stat <- "phantom"
#'
#' # analysis function
#' a_N_mean <- function(x, .spl_context) {
#'   if (in_ref_col(c("ARM", "*", "desc_stat", "N"), .spl_context)) {
#'     rcell(length(x))
#'   } else if (in_ref_col(c("ARM", "*", "desc_stat", "Mean"), .spl_context)) {
#'     rcell(mean(x), format = "xx.x")
#'   } else {
#'     rcell(NULL)
#'   }
#' }
#'
#' combodf <- data.frame(
#'   valname = c("N", "Mean"),
#'   label = c("N", "Mean"),
#'   levelcombo = I(list(select_all_levels, select_all_levels)),
#'   exargs = I(list(list(), list()))
#' )
#'
#' lyt <- basic_table() |>
#'   split_cols_by("ARM") |>
#'   split_cols_by(
#'     "desc_stat",
#'     split_fun = add_combo_levels(combodf, keep_levels = c("N", "Mean"))
#'   ) |>
#'   analyze("AGE", afun = a_N_mean)
#'
#' tbl <- build_table(lyt, data)
#' tbl
#'
in_ref_col <- function(ref_path, .spl_context) {
  if (is.null(ref_path)) {
    return(FALSE)
  }

  checkmate::assert_character(ref_path, min.len = 2L, names = "unnamed")
  checkmate::assert_true(length(ref_path) %% 2 == 0)
  checkmate::assert_data_frame(.spl_context, min.rows = 1L)

  cur_colpath <- cur_col_split_path(.spl_context)

  if (length(ref_path) == length(cur_colpath)) {
    fixed <- which(ref_path != "*")
    identical(ref_path[fixed], cur_colpath[fixed])
  } else {
    FALSE
  }
}
