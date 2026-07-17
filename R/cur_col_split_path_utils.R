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
#' Determine whether a given column path matches the current column split path.
#'
#' @param col_path (`character` or `NULL`)\cr
#'   Column path specified as alternating split variable names and split values.
#'   Must have at least two elements and an even length, e.g.
#'   `c("ARM", "Placebo")` or `c("ARM", "*", "desc_stat", "N")`;
#'   see [rtables::col_paths()].
#'
#'   The special value `"*"` can be used as a wildcard to match any split
#'   variable name or value.
#'
#'   `NULL` can be used to indicate that no path is specified, in which case
#'   the function returns `FALSE`, regardless of `.spl_context`.
#'
#' @return
#' * `in_column()` returns a single logical value indicating whether the
#' current column split matches the specified `col_path`.
#'
#' @export
#'
#' @examples
#' in_column(c("ARM", "Placebo"), .spl_context_1)
#' in_column(c("ARM", "*"), .spl_context_1)
#' in_column(c("ARM", "X"), .spl_context_1)
#'
#' in_column(c("ARM", "*", "desc_stat", "N"), .spl_context_2)
#' in_column(c("ARM", "*", "desc_stat", "Mean"), .spl_context_2)
#'
#' # Use in an rtables custom analysis function.
#'
#' data <- formatters::ex_adsl
#' data$desc_stat <- "phantom"
#'
#' # analysis function
#' a_N_mean <- function(x, .spl_context) {
#'   if (in_column(c("ARM", "*", "desc_stat", "N"), .spl_context)) {
#'     rcell(length(x))
#'   } else if (in_column(c("ARM", "*", "desc_stat", "Mean"), .spl_context)) {
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
in_column <- function(col_path, .spl_context) {
  if (is.null(col_path)) {
    return(FALSE)
  }

  checkmate::assert_character(col_path, min.len = 2L, names = "unnamed")
  checkmate::assert_true(length(col_path) %% 2 == 0)
  checkmate::assert_data_frame(.spl_context, min.rows = 1L)

  cur_colpath <- cur_col_split_path(.spl_context)

  if (length(col_path) == length(cur_colpath)) {
    fixed <- which(col_path != "*")
    identical(col_path[fixed], cur_colpath[fixed])
  } else {
    FALSE
  }
}

#' @describeIn cur_col_split_path_utils
#' Obtain reference information for a global reference group.
#'
#' This helper function can be used in custom analysis functions, by passing
#' an extra argument `ref_path` which defines a global reference group by
#' the corresponding column split hierarchy levels.
#'
#' @param ref_path (`character`)
#'   Reference group specification as an `rtables` `colpath`; see Details.
#' @param .var (`character`)
#'   The variable being analyzed; see [rtables::additional_fun_params].
#'
#' @return
#' * `get_ref_info()` returns a list with:
#'   * `ref_group`: the reference group data (a `data.frame` or vector depending
#'     on `.var`), equivalent to `.ref_group` from [rtables::additional_fun_params].
#'   * `in_ref_col`: logical, whether the current column is the reference column,
#'     equivalent to `.in_ref_col` from [rtables::additional_fun_params].
#'   * `trt_var`: the treatment variable name (last variable in `ref_path`).
#'   * `ctrl_grp`: the reference group level (last level in `ref_path`).
#'   * `cur_col_val`: the current column's value for `trt_var`.
#'
#' @details
#' The reference group is specified in `colpath` hierarchical fashion in
#' `ref_path`: the first column split variable is the first element, and the
#' level to use is the second element. It continues until the last column split
#' variable with last level to use.
#' Note that depending on `.var`, either a `data.frame` (if `.var` is `NULL`)
#' or a vector (otherwise) is returned. This allows usage for analysis
#' functions with `df` and `x` arguments, respectively.
#'
#' @export
#'
#' @examples
#' dm <- DM
#' dm$colspan_trt <- factor(
#'   ifelse(dm$ARM == "B: Placebo", " ", "Active Study Agent"),
#'   levels = c("Active Study Agent", " ")
#' )
#' colspan_trt_map <- create_colspan_map(
#'   dm,
#'   non_active_grp = "B: Placebo",
#'   non_active_grp_span_lbl = " ",
#'   active_grp_span_lbl = "Active Study Agent",
#'   colspan_var = "colspan_trt",
#'   trt_var = "ARM"
#' )
#'
#' # A standard analysis function which uses a reference group.
#' standard_afun <- function(x, .ref_group, .in_ref_col) {
#'   diff_means <- if (isFALSE(.in_ref_col)) {
#'     mean(x) - mean(.ref_group)
#'   } else {
#'     NULL
#'   }
#'   in_rows(
#'     m = rcell(mean(x), label = "Mean"),
#'     dm = rcell(diff_means, label = "Difference in Means vs Placebo"),
#'     .formats = "xx.xx"
#'   )
#' }
#'
#' # The custom analysis function which can work with a global reference group.
#' result_afun <- function(x, ref_path, .spl_context, .var) {
#'   ref <- get_ref_info(ref_path, .spl_context, .var)
#'   standard_afun(x, .ref_group = ref$ref_group, .in_ref_col = ref$in_ref_col)
#' }
#'
#' ref_path <- c("colspan_trt", " ", "ARM", "B: Placebo")
#'
#' lyt <- basic_table() |>
#'   split_cols_by("colspan_trt", split_fun = trim_levels_to_map(colspan_trt_map)) |>
#'   split_cols_by("ARM") |>
#'   add_overall_col("Total") |>
#'   analyze("AGE", afun = result_afun, extra_args = list(ref_path = ref_path))
#'
#' build_table(lyt, dm)
get_ref_info <- function(ref_path, .spl_context, .var = NULL) {
  if (is.null(ref_path)) {
    return(list(ref_group = NULL, in_ref_col = NULL, trt_var = NULL, ctrl_grp = NULL, cur_col_val = NULL))
  }

  checkmate::assert_character(ref_path, min.len = 2L, names = "unnamed")
  checkmate::assert_true(length(ref_path) %% 2 == 0)
  checkmate::assert_data_frame(.spl_context)

  vars_indices <- seq(from = 1L, to = length(ref_path) - 1L, by = 2L)
  level_indices <- seq(from = 2L, to = length(ref_path), by = 2L)
  ref_path_levels <- paste(ref_path[level_indices], collapse = ".")

  trt_var <- ref_path[utils::tail(vars_indices, 1L)]
  ctrl_grp <- ref_path[utils::tail(level_indices, 1L)]

  cur_colpath <- cur_col_split_path(.spl_context)
  cur_col_vars <- cur_colpath[seq(from = 1L, to = length(cur_colpath), by = 2L)]
  cur_col_vals <- cur_colpath[seq(from = 2L, to = length(cur_colpath), by = 2L)]
  trt_var_pos <- match(trt_var, cur_col_vars)
  cur_col_val <- if (!is.na(trt_var_pos)) cur_col_vals[trt_var_pos] else NULL

  # If ref_path variables are outside of the current column split variable.
  ref_var_path <- ref_path
  ref_var_path[level_indices] <- "*"
  if (!in_column(ref_var_path, .spl_context)) {
    return(list(ref_group = NULL, in_ref_col = NULL, trt_var = trt_var, ctrl_grp = ctrl_grp, cur_col_val = cur_col_val))
  }

  leaf_sc <- .spl_context[nrow(.spl_context), ]
  full_df <- leaf_sc$full_parent_df[[1]]
  row_in_ref_group <- leaf_sc[[ref_path_levels]][[1]]
  ref_group <- full_df[row_in_ref_group, ]
  if (!is.null(.var)) {
    ref_group <- ref_group[[.var]]
  }

  list(
    ref_group = ref_group,
    in_ref_col = in_column(ref_path, .spl_context),
    trt_var = trt_var,
    ctrl_grp = ctrl_grp,
    cur_col_val = cur_col_val
  )
}
