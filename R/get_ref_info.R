#' @title Obtain reference group information from split context.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' `get_ref_info()` identifies a reference group defined by a column-split
#' path and returns both the reference-group data and an indicator of whether
#' the current column is the reference column. It is intended for use inside
#' custom `rtables` analysis functions.
#'
#' The reference group is specified using `ref_path`, which consists of
#' alternating column-split variable names and its corresponding levels.
#' For example, `c("SEX", "F", "ARM", "Placebo")` specifies the column-split
#' path where `SEX` is `"F"` and `ARM` is `"Placebo"`.
#'
#' @param ref_path (`character`) \cr
#'   Reference group specification as an `rtables` `colpath`; see Details.
#' @param .spl_context (`data.frame`) \cr
#'   Ancestor split-state information passed by `rtables`.
#' @param .var (`character(1)`) \cr
#'   The variable being analyzed; see [rtables::additional_fun_params].
#'   If supplied, the corresponding column is extracted from the reference-group
#'   data. If `NULL`, the complete reference-group data frame is returned.
#'
#' @return
#'   A list with the following elements:
#'   \itemize{
#'     \item `in_ref_col` (`logical(1)` or `NULL`) indicates whether the
#'       current column matches the reference path.
#'       This corresponds to `.in_ref_col` in [rtables::additional_fun_params].
#'     \item `ref_group` (`data.frame`, vector, or `NULL`) contains the
#'       observations belonging to the reference group. If `.var` is `NULL`,
#'       the complete data frame is returned; otherwise, the column specified
#'       by `.var` is returned.
#'       This corresponds to `.ref_group` in [rtables::additional_fun_params].
#'   }
#'
#'   If the reference path is not present in the current column-split
#'   hierarchy, both elements are `NULL`.
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
