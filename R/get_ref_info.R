#' Obtain Reference Information for a Global Reference Group
#'
#' This helper function can be used in custom analysis functions, by passing
#' an extra argument `ref_path` which defines a global reference group by
#' the corresponding column split hierarchy levels.
#'
#' @param ref_path (`character`)\cr reference group specification as an `rtables`
#'   `colpath`, see details.
#' @param .spl_context see [rtables::spl_context].
#' @param .var the variable being analyzed, see [rtables::additional_fun_params].
#'
#' @return A list with `ref_group` and `in_ref_col`, which can be used as
#'   `.ref_group` and `.in_ref_col` as if being directly passed to an analysis
#'   function by `rtables`, see [rtables::additional_fun_params].
#'
#' @details
#' The reference group is specified in `colpath` hierarchical fashion in `ref_path`:
#' The first column split variable is the first element, and the level to use is the
#' second element. It continues until the last column split variable with last
#' level to use.
#' Note that depending on `.var`, either a `data.frame` (if `.var` is `NULL`) or
#' a vector (otherwise) is returned. This allows usage for analysis functions with
#' `df` and `x` arguments, respectively.
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
#' standard_afun <- function(x, .ref_group, .in_ref_col) {
#'   in_rows(
#'     "Difference of Averages" = non_ref_rcell(
#'       mean(x) - mean(.ref_group),
#'       is_ref = .in_ref_col,
#'       format = "xx.xx"
#'     )
#'   )
#' }
#'
#' result_afun <- function(x, ref_path, .spl_context, .var) {
#'   ref <- get_ref_info(ref_path, .spl_context, .var)
#'   standard_afun(x, .ref_group = ref$ref_group, .in_ref_col = ref$in_ref_col)
#' }
#'
#' ref_path <- c("colspan_trt", " ", "ARM", "B: Placebo")
#'
#' lyt <- basic_table() |>
#'   split_cols_by(
#'     "colspan_trt",
#'     split_fun = trim_levels_to_map(map = colspan_trt_map)
#'   ) |>
#'   split_cols_by("ARM") |>
#'   analyze(
#'     "AGE",
#'     extra_args = list(ref_path = ref_path),
#'     afun = result_afun
#'   )
#'
#' build_table(lyt, dm)
get_ref_info <- function(ref_path, .spl_context, .var = NULL) {
  checkmate::check_character(ref_path, min.len = 2L, names = "unnamed")
  checkmate::assert_true(length(ref_path) %% 2 == 0) # Even number of elements in ref_path.
  leaf_spl_context <- .spl_context[nrow(.spl_context), ]
  full_df <- leaf_spl_context$full_parent_df[[1]]
  level_indices <- seq(from = 2L, to = length(ref_path), by = 2L)
  ref_group_string <- paste(ref_path[level_indices], collapse = ".")
  row_in_ref_group <- leaf_spl_context[[ref_group_string]][[1]]
  ref_group <- full_df[row_in_ref_group, ]
  if (!is.null(.var)) {
    ref_group <- ref_group[[.var]]
  }
  colvars_indices <- seq(from = 1L, to = length(ref_path) - 1L, by = 2L)
  checkmate::assert_true(identical(leaf_spl_context$cur_col_split[[1]], ref_path[colvars_indices]))
  in_ref_col <- identical(leaf_spl_context$cur_col_split_val[[1]], ref_path[level_indices])
  list(ref_group = ref_group, in_ref_col = in_ref_col)
}
