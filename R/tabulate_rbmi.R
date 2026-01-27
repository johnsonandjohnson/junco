#' Tabulation of RBMI Results
#'
#' @description `r lifecycle::badge('stable')`
#'
#' These functions can be used to produce tables from RBMI.
#'
#' @name tabulate_rbmi
#'
#' @note These functions have been forked from `tern.rbmi`. Additional features are:
#'
#' * Additional `ref_path` argument.
#' * Extraction of variance statistics in the `tidy()` method.
#' * Adapted to `rbmi` forked functions update with more than two treatment groups.
NULL

#' @describeIn tabulate_rbmi Helper function to produce data frame with results
#'   of pool for a single visit.
#'
#' @param x (`list`)\cr is a list of pooled object from `rbmi` analysis results.
#'   This list includes analysis results, confidence level, hypothesis testing type.
#' @param visit_name (`string`)\cr single visit level.
#' @param group_names (`character`)\cr group levels.
#' @return
#' * `h_tidy_pool()` returns a `data.frame` with results of pooled analysis for a single visit.
#'
#' @export
h_tidy_pool <- function(x, visit_name, group_names) {
  checkmate::assert_list(x)
  checkmate::assert_string(visit_name)
  checkmate::assert_character(group_names)

  ref_name <- paste0("lsm_", group_names[1], "_", visit_name)
  ref <- x[[ref_name]]

  list2df <- \(l) with(l, data.frame(est, ci_l = ci[1], ci_u = ci[2], se, pvalue, df))

  var_name <- paste0("var_", visit_name)
  var <- if (var_name %in% names(x)) {
    # This is the case for ANCOVA.
    list2df(x[[var_name]])
  } else {
    # This is the case for MMRM.
    var_names <- paste0("var_", group_names[-1], "_", visit_name)
    checkmate::assert_subset(var_names, names(x))
    var_dfs <- lapply(x[var_names], list2df)
    var <- do.call(rbind, var_dfs)
  }

  contr_names <- paste0("trt_", group_names[-1], "_", visit_name)
  contr_dfs <- lapply(x[contr_names], list2df)
  contr <- do.call(rbind, contr_dfs)

  alt_names <- paste0("lsm_", group_names[-1], "_", visit_name)
  alt_dfs <- lapply(x[alt_names], list2df)
  alt <- do.call(rbind, alt_dfs)

  df_ref <- data.frame(
    visit = visit_name,
    group = group_names[1],
    est = ref$est,
    se_est = ref$se,
    lower_cl_est = ref$ci[1],
    upper_cl_est = ref$ci[2],
    est_contr = NA_real_,
    se_contr = NA_real_,
    lower_cl_contr = NA_real_,
    upper_cl_contr = NA_real_,
    p_value = NA_real_,
    relative_reduc = NA_real_,
    mse = NA_real_,
    df = NA_real_,
    stringsAsFactors = FALSE
  )
  df_alt <- data.frame(
    visit = visit_name,
    group = group_names[-1],
    est = alt$est,
    se_est = alt$se,
    lower_cl_est = alt$ci_l,
    upper_cl_est = alt$ci_u,
    est_contr = contr$est,
    se_contr = contr$se,
    lower_cl_contr = contr$ci_l,
    upper_cl_contr = contr$ci_u,
    p_value = contr$pvalue,
    relative_reduc = contr$est / df_ref$est,
    mse = var$est,
    df = var$df,
    stringsAsFactors = FALSE
  )
  rbind(df_ref, df_alt)
}

#' Helper method (for [`broom::tidy()`]) to prepare a data frame from an
#'   `pool` `rbmi` object containing the LS means and contrasts and multiple visits
#'
#' @method tidy pool
#' @param x (`pool`) is a list of pooled object from `rbmi` analysis results. This list includes
#' analysis results, confidence level, hypothesis testing type.
#' @param visits (`character`)\cr all visit levels. Otherwise too hard to guess this.
#' @param ... Additional arguments. Not used. Needed to match generic signature only.
#' @importFrom generics tidy
#' @export
#' @keywords internal
#' @return A `data.frame`.
tidy.pool <- function(x, visits, ...) {
  ls_raw <- x$pars

  has_lsm <- grepl("^lsm_", names(ls_raw))
  has_first_visit <- grepl(paste0("_", visits[1]), names(ls_raw), fixed = TRUE)
  is_lsm_first_visit <- has_lsm & has_first_visit

  group_names <- names(ls_raw)[is_lsm_first_visit]
  group_names <- gsub(pattern = "^lsm_", replacement = "", x = group_names)
  group_names <- gsub(pattern = paste0("_", visits[1]), replacement = "", x = group_names, fixed = TRUE)

  spl <- rep(visits, each = length(ls_raw) / length(visits))

  ls_split <- split(ls_raw, spl)

  ls_df <- mapply(
    FUN = h_tidy_pool,
    x = ls_split,
    visit_name = visits,
    MoreArgs = list(group_names = group_names),
    SIMPLIFY = FALSE
  )

  result <- do.call(rbind, unname(ls_df))

  result$visit <- factor(result$visit, levels = visits)
  result$group <- factor(result$group, levels = group_names)
  result$conf_level <- x$conf.level

  result
}

#' @describeIn tabulate_rbmi Statistics function which is extracting estimates
#'   from a tidied RBMI results data frame.
#'
#' @param df (`data.frame`)\cr input with LS means results.
#' @param .in_ref_col (`flag`)\cr whether reference column is specified.
#' @param show_relative (`string`)\cr 'reduction' if (`control - treatment`, default)
#'   or 'increase' (`treatment - control`) of relative change from baseline?
#' @return
#' * `s_rbmi_lsmeans()` returns a list of statistics extracted from a tidied LS means data frame.
#' @export
s_rbmi_lsmeans <- function(df, .in_ref_col, show_relative = c("reduction", "increase")) {
  checkmate::assert_flag(.in_ref_col)

  show_relative <- match.arg(show_relative)
  if_not_ref <- function(x) if (.in_ref_col) character() else x
  list(
    adj_mean_se = c(df$est, df$se_est),
    adj_mean_ci = with_label(c(df$lower_cl_est, df$upper_cl_est), f_conf_level(df$conf_level)),
    diff_mean_se = if_not_ref(c(df$est_contr, df$se_contr)),
    diff_mean_ci = with_label(
      if_not_ref(c(df$lower_cl_contr, df$upper_cl_contr)),
      f_conf_level(df$conf_level)
    ),
    change = switch(show_relative,
      reduction = with_label(if_not_ref(df$relative_reduc), "Relative Reduction (%)"),
      increase = with_label(if_not_ref(-df$relative_reduc), "Relative Increase (%)")
    ),
    p_value = if_not_ref(df$p_value),
    additional_title_row = NULL
  )
}

#' @describeIn tabulate_rbmi Formatted Analysis function which is used as `afun`.
#'
#' @inheritParams proposal_argument_convention
#' @export
a_rbmi_lsmeans <- function(
    df,
    ref_path,
    .spl_context,
    ...,
    .stats = NULL,
    .formats = NULL,
    .labels = NULL,
    .indent_mods = NULL) {
  # Check for additional parameters to the statistics function
  dots_extra_args <- list(...)

  # Only support default stats, not custom stats
  .stats <- .split_std_from_custom_stats(.stats)$default_stats

  # Obtain reference column information
  ref <- get_ref_info(ref_path, .spl_context)

  # Apply statistics function
  x_stats <- .apply_stat_functions(
    default_stat_fnc = s_rbmi_lsmeans,
    custom_stat_fnc_list = NULL,
    args_list = c(df = list(df), .in_ref_col = ref$in_ref_col, dots_extra_args)
  )

  # Format according to specifications
  format_stats(
    x_stats,
    method_groups = "tabulate_rbmi",
    stats_in = .stats,
    formats_in = .formats,
    labels_in = .labels,
    indents_in = .indent_mods
  )
}
