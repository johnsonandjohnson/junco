#' Workaround statistics function to add HR with CI
#'
#' This is a workaround for [tern::s_coxph_pairwise()], which adds a statistic
#' containing the hazard ratio estimate together with the confidence interval.
#'
#' @inheritParams proposal_argument_convention
#'
#'
#' @name coxph_hr
#' @order 1
NULL

#' @describeIn coxph_hr Statistics function forked from [tern::s_coxph_pairwise()].
#'   the difference is that:
#'   1) It returns the additional statistic `lr_stat_df` (log rank statistic with degrees of freedom).
#' @export
#' @order 3
#'
#' @importFrom survival Surv coxph survdiff
#'
#' @examples
#' adtte_f <- tern::tern_ex_adtte |>
#'   dplyr::filter(PARAMCD == "OS") |>
#'   dplyr::mutate(is_event = CNSR == 0)
#' df <- adtte_f |> dplyr::filter(ARMCD == 'ARM A')
#' df_ref <- adtte_f |> dplyr::filter(ARMCD == 'ARM B')
#'
#' s_coxph_hr(
#'   df = df,
#'   .ref_group = df_ref,
#'   .in_ref_col = FALSE,
#'   .var = 'AVAL',
#'   is_event = 'is_event',
#'   strata = NULL
#' )
s_coxph_hr <- function(
  df,
  .ref_group,
  .in_ref_col,
  .var,
  is_event,
  strata = NULL,
  control = control_coxph(),
  alternative = c("two.sided", "less", "greater")
) {
  checkmate::assert_string(.var)
  checkmate::assert_numeric(df[[.var]])
  checkmate::assert_logical(df[[is_event]])
  assert_df_with_variables(df, list(tte = .var, is_event = is_event))
  alternative <- match.arg(alternative)
  pval_method <- control$pval_method
  ties <- control$ties
  conf_level <- control$conf_level

  if (.in_ref_col) {
    return(list(
      pvalue = with_label(list(), paste0("p-value (", pval_method, ")")),
      lr_stat_df = list(),
      hr = list(),
      hr_ci = with_label(list(), f_conf_level(conf_level)),
      hr_ci_3d = with_label(list(), paste0("Hazard Ratio (", f_conf_level(conf_level), ")")),
      n_tot = list(),
      n_tot_events = list()
    ))
  }
  data <- rbind(.ref_group, df)
  group <- factor(rep(c("ref", "x"), c(nrow(.ref_group), nrow(df))), levels = c("ref", "x"))

  df_cox <- data.frame(tte = data[[.var]], is_event = data[[is_event]], arm = group)
  if (is.null(strata)) {
    formula_cox <- survival::Surv(tte, is_event) ~ arm
  } else {
    formula_cox <- stats::as.formula(paste0(
      "survival::Surv(tte, is_event) ~ arm + strata(",
      paste(strata, collapse = ","),
      ")"
    ))
    df_cox <- cbind(df_cox, data[strata])
  }
  cox_fit <- survival::coxph(formula = formula_cox, data = df_cox, ties = ties)
  sum_cox <- summary(cox_fit, conf.int = conf_level, extend = TRUE)
  original_survdiff <- survival::survdiff(formula_cox, data = df_cox)
  log_rank_stat <- original_survdiff$chisq

  # See survival::survdiff for the d.f. calculation.
  etmp <- if (is.matrix(original_survdiff$exp)) {
    apply(original_survdiff$exp, 1, sum)
  } else {
    original_survdiff$exp
  }
  log_rank_df <- (sum(1 * (etmp > 0))) - 1
  # Check the consistency of the d.f. with the p-value returned by survival::survdiff.
  log_rank_pvalue <- stats::pchisq(log_rank_stat, log_rank_df, lower.tail = FALSE)
  checkmate::assert_true(all.equal(log_rank_pvalue, original_survdiff$pvalue))

  pval <- switch(
    pval_method,
    wald = sum_cox$waldtest["pvalue"],
    `log-rank` = log_rank_pvalue,
    likelihood = sum_cox$logtest["pvalue"]
  )

  # Handle one-sided alternatives.
  if (alternative != "two.sided") {
    right_direction <- if (alternative == "less") {
      sum_cox$conf.int[1, 1] < 1
    } else {
      sum_cox$conf.int[1, 1] >= 1
    }
    pval <- if (right_direction) {
      pval / 2
    } else {
      1 - pval / 2
    }
  }

  list(
    pvalue = with_label(unname(pval), paste0("p-value (", pval_method, ")")),
    lr_stat_df = unname(c(log_rank_stat, log_rank_df)),
    hr = sum_cox$conf.int[1, 1],
    hr_ci = with_label(unname(sum_cox$conf.int[1, 3:4]), f_conf_level(conf_level)),
    hr_ci_3d = with_label(
      c(sum_cox$conf.int[1, 1], unname(sum_cox$conf.int[1, 3:4])),
      paste0("Hazard Ratio (", f_conf_level(conf_level), ")")
    ),
    n_tot = sum_cox$n,
    n_tot_events = sum_cox$nevent
  )
}

#' @describeIn coxph_hr Formatted analysis function which is used as `afun`.
#'
#' @examples
#' library(dplyr)
#'
#' adtte_f <- tern::tern_ex_adtte |>
#'   filter(PARAMCD == "OS") |>
#'   mutate(is_event = CNSR == 0)
#'
#' df <- adtte_f |> filter(ARMCD == 'ARM A')
#' df_ref_group <- adtte_f |> filter(ARMCD == 'ARM B')
#'
#' basic_table() |>
#'   split_cols_by(var = 'ARMCD', ref_group = 'ARM A') |>
#'   add_colcounts() |>
#'   analyze("AVAL",
#'     afun = s_coxph_hr,
#'     extra_args = list(is_event = 'is_event'),
#'     var_labels = 'Unstratified Analysis',
#'     show_labels = 'visible'
#'   ) |>
#'   build_table(df = adtte_f)
#'
#'   basic_table() |>
#'   split_cols_by(var = 'ARMCD', ref_group = 'ARM A') |>
#'   add_colcounts() |>
#'   analyze("AVAL",
#'           afun = s_coxph_hr,
#'           extra_args = list(is_event = 'is_event',
#'                             strata = 'SEX',
#'                             control = tern::control_coxph(pval_method = 'wald')),
#'           var_labels = 'Unstratified Analysis',
#'           show_labels = 'visible'
#'   ) |>
#'   build_table(df = adtte_f)
#' @export
#' @order 2
a_coxph_hr <- function(
  df,
  .var,
  ref_path,
  .spl_context,
  ...,
  .stats = NULL,
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL
) {
  # Check for additional parameters to the statistics function
  dots_extra_args <- list(...)

  # Only support default stats, not custom stats
  .stats <- .split_std_from_custom_stats(.stats)$default_stats

  # Obtain reference column information
  ref <- get_ref_info(ref_path, .spl_context)

  # Apply statistics function
  x_stats <- .apply_stat_functions(
    default_stat_fnc = s_coxph_hr,
    custom_stat_fnc_list = NULL,
    args_list = c(
      df = list(df),
      .var = .var,
      .ref_group = list(ref$ref_group),
      .in_ref_col = ref$in_ref_col,
      dots_extra_args
    )
  )

  # Format according to specifications
  format_stats(
    x_stats,
    method_groups = "coxph_hr",
    stats_in = .stats,
    formats_in = .formats,
    labels_in = .labels,
    indents_in = .indent_mods
  )
}
