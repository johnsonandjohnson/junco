#' @title Obtain Reference Information for a Global Reference Group
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This helper function can be used in custom analysis functions, by passing
#' an extra argument `ref_path` which defines a global reference group by
#' the corresponding column split hierarchy levels.
#'
#' @param ref_path (`character`)\cr reference group specification as an `rtables`
#'   `colpath`, see details.
#' @param .spl_context (`data.frame`)\cr see [rtables::spl_context].
#' @param .var (`character`)\cr the variable being analyzed,
#'   see [rtables::additional_fun_params].
#'
#' @return A list with `ref_group` and `in_ref_col`, which can be used as
#'   `.ref_group` and `.in_ref_col` as if being directly passed to an analysis
#'   function by `rtables`, see [rtables::additional_fun_params].
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
    return(list(ref_group = NULL, in_ref_col = NULL))
  }

  checkmate::assert_character(ref_path, min.len = 2L, names = "unnamed")
  checkmate::assert_true(length(ref_path) %% 2 == 0)
  checkmate::assert_data_frame(.spl_context)

  leaf_sc <- .spl_context[nrow(.spl_context), ]
  vars_indices <- seq(from = 1L, to = length(ref_path) - 1L, by = 2L)
  level_indices <- seq(from = 2L, to = length(ref_path), by = 2L)
  ref_path_levels <- paste(ref_path[level_indices], collapse = ".")

  # If ref_path variables are outside of the current column split variable.
  is_ref_in_colvars <- identical(leaf_sc$cur_col_split[[1]], ref_path[vars_indices])
  if (!is_ref_in_colvars) {
    return(list(ref_group = NULL, in_ref_col = NULL))
  }

  # Prepare in_ref_col.
  in_ref_col <- identical(leaf_sc$cur_col_split_val[[1]], ref_path[level_indices])

  # Prepare ref_group.
  full_df <- leaf_sc$full_parent_df[[1]]
  row_in_ref_group <- leaf_sc[[ref_path_levels]][[1]]
  ref_group <- full_df[row_in_ref_group, ]
  if (!is.null(.var)) {
    ref_group <- ref_group[[.var]]
  }

  list(ref_group = ref_group, in_ref_col = in_ref_col)
}
