#' @keywords internal
memoised_fit_mmrm <- memoise::memoise(fit_mmrm_j)

#' Helper Function to Fit the MMRM and Return LS Mean Estimates and Contrasts
#'
#' @inheritParams proposal_argument_convention
#' @param df_parent (`data.frame`)\cr data set containing all analysis variables
#'   from all visits and arms.
#' @param ref_arm_level (`string`)\cr the reference arm which should be compared
#'   against.
#' @param ref_visit_levels (`character`)\cr the reference visits which should not
#'   be included in the model fit.
#' @param ... additional options passed to [fit_mmrm_j()].
#'
#' @return The resulting estimates and contrasts LS means as returned by
#'   [tidy.tern_model()].
#' @keywords internal
h_summarize_mmrm <- function(.var, df_parent, variables, ref_arm_level, ref_visit_levels, ...) {
  checkmate::assert_string(.var)
  variables$response <- .var

  checkmate::assert_string(ref_arm_level)
  arm_levels <- levels(df_parent[[variables$arm]])
  if (arm_levels[1L] != ref_arm_level) {
    checkmate::assert_subset(ref_arm_level, arm_levels[-1L])
    df_parent[[variables$arm]] <- stats::relevel(df_parent[[variables$arm]], ref = ref_arm_level)
  }
  checkmate::assert_character(ref_visit_levels)
  in_ref_visits <- df_parent[[variables$visit]] %in% ref_visit_levels
  df_parent <- df_parent[!in_ref_visits, , drop = FALSE]
  checkmate::assert_true(nrow(df_parent) > 1)
  new_levels <- setdiff(levels(df_parent[[variables$visit]]), ref_visit_levels)
  df_parent[[variables$visit]] <- factor(df_parent[[variables$visit]], levels = new_levels)
  mod_fit <- memoised_fit_mmrm(vars = variables, data = df_parent, ...)
  tidy(mod_fit)
}

#' Dynamic tabulation of MMRM results with tables
#'
#' @description `r lifecycle::badge('stable')`
#'
#' These functions can be used to produce tables for MMRM results, within
#' tables which are split by arms and visits. This is helpful when higher-level
#' row splits are needed (e.g. splits by parameter or subgroup).
#'
#' @name summarize_mmrm
#' @examples
#' set.seed(123)
#' longdat <- data.frame(
#'   ID = rep(DM$ID, 5),
#'   AVAL = c(
#'     rep(0, nrow(DM)),
#'     rnorm(n = nrow(DM) * 4)
#'   ),
#'   VISIT = factor(rep(paste0("V", 0:4), each = nrow(DM)))
#' ) |>
#'   dplyr::inner_join(DM, by = "ID")
#'
NULL

#' @describeIn summarize_mmrm Statistics function which is extracting estimates,
#'   not including any results when in the reference visit, and only showing LS mean
#'   estimates when in the reference arm and not in reference visit. It uses
#'   [s_lsmeans()] for the final processing.
#'
#' @inheritParams proposal_argument_convention
#' @param ref_levels (`list`)\cr with `visit` and `arm` reference levels.
#' @param ... eventually passed to [fit_mmrm_j()] via [h_summarize_mmrm()].
#' @export
s_summarize_mmrm <- function(
    df,
    .var,
    variables,
    ref_levels,
    .spl_context,
    alternative = c("two.sided", "less", "greater"),
    show_relative = c("reduction", "increase"),
    ...) {
  alternative <- match.arg(alternative)

  checkmate::assert_list(variables, names = "unique")
  visit_var <- variables$visit
  arm_var <- variables$arm

  checkmate::assert_list(ref_levels, names = "unique")
  checkmate::assert_subset(c(visit_var, arm_var), names(ref_levels))
  ref_visits <- ref_levels[[visit_var]]
  ref_arm <- ref_levels[[arm_var]]
  checkmate::assert_string(ref_arm)

  current_visit <- as.character(unique(df[[visit_var]]))
  current_arm <- as.character(unique(df[[arm_var]]))
  checkmate::assert_string(current_visit)
  checkmate::assert_string(current_arm)

  in_ref_visits <- current_visit %in% ref_visits
  in_ref_arm <- current_arm == ref_arm

  if (in_ref_visits) {
    ## this is returned
    list(
      n = NULL,
      adj_mean_se = NULL,
      adj_mean_ci = NULL,
      adj_mean_est_ci = NULL,
      diff_mean_se = NULL,
      diff_mean_ci = NULL,
      diff_mean_est_ci = NULL,
      change = NULL,
      p_value = NULL
    )
  } else { # non ref visit
    n_splits <- nrow(.spl_context)

    # Check that the current row split is by the visit variable.
    current_split_var <- .spl_context[n_splits, "split"]
    checkmate::assert_true(identical(current_split_var, visit_var))

    # Then take the data frame with all visits and fit the MMRM on it.
    df_parent <- .spl_context[n_splits - 1, "full_parent_df"][[1]]
    lsm_df <- h_summarize_mmrm(
      .var = .var,
      df_parent = df_parent,
      variables = variables,
      ref_arm_level = ref_arm,
      ref_visit_levels = ref_visits,
      ...
    )

    # Subset to the current table cell we are looking at.
    matches_visit <- lsm_df[[visit_var]] == current_visit
    matches_arm <- lsm_df[[arm_var]] == current_arm
    matches_both <- which(matches_visit & matches_arm)
    checkmate::assert_int(matches_both)

    s_lsmeans(
      lsm_df[matches_both, ],
      .in_ref_col = in_ref_arm,
      alternative = alternative,
      show_relative = show_relative
    )
  } # end ref visits if/else
}

#' @describeIn summarize_mmrm Formatted analysis function which is used as `afun`.
#'
#' @return
#' * `a_summarize_mmrm()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @examples
#' basic_table() |>
#'   split_rows_by("VISIT") |>
#'   split_cols_by("ARM") |>
#'   analyze(
#'     vars = "AVAL",
#'     afun = a_summarize_mmrm,
#'     na_str = tern::default_na_str(),
#'     show_labels = "hidden",
#'     extra_args = list(
#'       variables = list(
#'         covariates = c("AGE"),
#'         id = "ID",
#'         arm = "ARM",
#'         visit = "VISIT"
#'       ),
#'       conf_level = 0.9,
#'       cor_struct = "toeplitz",
#'       ref_levels = list(VISIT = "V0", ARM = "B: Placebo")
#'     )
#'   ) |>
#'   build_table(longdat) |>
#'   prune_table(all_zero)
#' @export
a_summarize_mmrm <- function(
    df,
    .var,
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

  # Apply statistics function
  x_stats <- .apply_stat_functions(
    default_stat_fnc = s_summarize_mmrm,
    custom_stat_fnc_list = NULL,
    args_list = c(df = list(df), .var = .var, .spl_context = list(.spl_context), dots_extra_args)
  )

  # Format according to specifications
  format_stats(
    x_stats,
    method_groups = "summarize_mmrm",
    stats_in = .stats,
    formats_in = .formats,
    labels_in = .labels,
    indents_in = .indent_mods
  )
}
