#' Obtain reference information for a global reference group.
#'
#' This helper function can be used in custom analysis functions, by passing
#' an extra argument `ref_path` which defines a global reference group by
#' the corresponding column split hierarchy levels.
#'
#' @param ref_path (`character`)
#'   Reference group specification as an `rtables` `colpath`; see Details.
#' @param .spl_context (`data.frame`)
#'   Ancestor split-state information passed by `rtables`.
#' @param .var (`character`)
#'   The variable being analyzed; see [rtables::additional_fun_params].
#'
#' @return
#' * `get_ref_info()` returns a list with:
#'   * `ref_group`: the reference group data (a `data.frame` or vector depending
#'     on `.var`), equivalent to `.ref_group` from [rtables::additional_fun_params].
#'   * `in_ref_col`: logical, whether the current column is the reference column,
#'     equivalent to `.in_ref_col` from [rtables::additional_fun_params].
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
    return(NULL)
  }

  checkmate::assert_character(ref_path, min.len = 2L, names = "unnamed")
  checkmate::assert_true(length(ref_path) %% 2L == 0L)
  checkmate::assert_data_frame(.spl_context)
  checkmate::assert_subset("full_parent_df", colnames(.spl_context))
  checkmate::assert_string(.var, min.chars = 1L, null.ok = TRUE)

  # Compare column split names while ignoring split values.
  ref_path_val_pos <- seq(2L, length(ref_path), by = 2L)
  ref_path_any_val <- replace(ref_path, ref_path_val_pos, "*")
  if (!in_column(ref_path_any_val, .spl_context)) {
    return(list(in_ref_col = NULL, ref_group = NULL))
  }

  leaf_sc <- .spl_context[nrow(.spl_context), ]
  full_df <- leaf_sc$full_parent_df[[1L]]
  ref_path_vals <- paste(ref_path[ref_path_val_pos], collapse = ".")
  ref_group_rows <- leaf_sc[[ref_path_vals]][[1L]]
  ref_group <- full_df[ref_group_rows, ]
  if (!is.null(.var)) {
    ref_group <- ref_group[[.var]]
  }

  list(
    in_ref_col = in_column(ref_path, .spl_context),
    ref_group = ref_group
  )
}
