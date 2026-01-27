#' Odds ratio estimation
#'
#' @description `r lifecycle::badge('stable')`
#'
#' @param method (`string`)\cr whether to use the correct (`'exact'`) calculation in the conditional likelihood or one
#'   of the approximations, or the CMH method. See [survival::clogit()] for details.
#'
#' @param df (`data.frame`)\cr input data frame.
#' @param .var (`string`)\cr name of the response variable.
#' @param .df_row (`data.frame`)\cr data frame containing all rows.
#' @param ref_path (`character`)\cr path to the reference group.
#' @param .spl_context (`environment`)\cr split context environment.
#' @param ... Additional arguments passed to the statistics function.
#' @param .stats (`character`)\cr statistics to calculate.
#' @param .formats (`list`)\cr formats for the statistics.
#' @param .labels (`list`)\cr labels for the statistics.
#' @param .indent_mods (`list`)\cr indentation modifications for the statistics.
#' @param .ref_group (`data.frame`)\cr reference group data frame.
#' @param .in_ref_col (`logical`)\cr whether the current column is the reference column.
#' @param variables (`list`)\cr list with arm and strata variable names.
#' @param conf_level (`numeric`)\cr confidence level for the confidence interval.
#' @param groups_list (`list`)\cr list of groups for combination.
#'
#' @note
#' The `a_odds_ratio_j()` and `s_odds_ratio_j()` functions have the `_j` suffix to distinguish them
#' from [tern::a_odds_ratio()] and [tern::s_odds_ratio()], respectively.
#' These functions differ as follows:
#'
#' * Additional `method = 'cmh'` option is provided to calculate the Cochran-Mantel-Haenszel estimate.
#' * The p-value is returned as an additional statistic.
#'
#' Once these updates are contributed back to `tern`, they can later be replaced by the `tern` versions.
#'
#' @name odds_ratio
#' @order 1
NULL

#' @describeIn odds_ratio Statistics function which estimates the odds ratio
#'   between a treatment and a control. A `variables` list with `arm` and `strata`
#'   variable names must be passed if a stratified analysis is required.
#'
#' @param na_if_no_events (`flag`)\cr whether the point estimate should be `NA` if there
#'   are no events in one arm. The p-value and confidence interval will still be computed.
#'
#' @return
#' * `s_odds_ratio_j()` returns a named list with the statistics `or_ci`
#'   (containing `est`, `lcl`, and `ucl`), `pval` and `n_tot`.
#'
#' @examples
#' s_odds_ratio_j(
#'   df = subset(dta, grp == "A"),
#'   .var = "rsp",
#'   .ref_group = subset(dta, grp == "B"),
#'   .in_ref_col = FALSE,
#'   .df_row = dta
#' )
#'
#' s_odds_ratio_j(
#'   df = subset(dta, grp == "A"),
#'   .var = "rsp",
#'   .ref_group = subset(dta, grp == "B"),
#'   .in_ref_col = FALSE,
#'   .df_row = dta,
#'   variables = list(arm = "grp", strata = "strata")
#' )
#'
#' s_odds_ratio_j(
#'   df = subset(dta, grp == "A"),
#'   method = "cmh",
#'   .var = "rsp",
#'   .ref_group = subset(dta, grp == "B"),
#'   .in_ref_col = FALSE,
#'   .df_row = dta,
#'   variables = list(arm = "grp", strata = c("strata"))
#' )
#' @export
s_odds_ratio_j <- function(
    df,
    .var,
    .ref_group,
    .in_ref_col,
    .df_row,
    variables = list(arm = NULL, strata = NULL),
    conf_level = 0.95,
    groups_list = NULL,
    na_if_no_events = TRUE,
    method = c("exact", "approximate", "efron", "breslow", "cmh")) {
  checkmate::assert_flag(na_if_no_events)
  # New: pval here
  y <- list(or_ci = list(), n_tot = list(), pval = list())
  method <- match.arg(method)
  one_group_empty <- nrow(df) == 0 || nrow(.ref_group) == 0

  if (!.in_ref_col) {
    (assert_proportion_value)(conf_level)
    assert_df_with_variables(df, list(rsp = .var))
    assert_df_with_variables(.ref_group, list(rsp = .var))

    if (one_group_empty) {
      y <- list(or_ci = c(est = NA_real_, lcl = 0, ucl = Inf), n_tot = c(n_tot = nrow(df) + nrow(.ref_group)), pval = 1)
    } else if (is.null(variables$strata)) {
      data <- data.frame(
        rsp = c(.ref_group[[.var]], df[[.var]]),
        grp = factor(rep(c("ref", "Not-ref"), c(nrow(.ref_group), nrow(df))), levels = c("ref", "Not-ref"))
      )
      y <- or_glm_j(data, conf_level = conf_level)
    } else {
      assert_df_with_variables(.df_row, c(list(rsp = .var), variables))
      checkmate::assert_subset(method, c("exact", "approximate", "efron", "breslow", "cmh"), empty.ok = FALSE)
      # The group variable prepared for stratified analysis must be synchronized with the combination groups
      # definition.
      if (is.null(groups_list)) {
        ref_grp <- as.character(unique(.ref_group[[variables$arm]]))
        trt_grp <- as.character(unique(df[[variables$arm]]))
        grp <- stats::relevel(factor(.df_row[[variables$arm]]), ref = ref_grp)
      } else {
        # If more than one level in reference col.
        reference <- as.character(unique(.ref_group[[variables$arm]]))
        grp_ref_flag <- vapply(X = groups_list, FUN.VALUE = TRUE, FUN = function(x) all(reference %in% x))
        ref_grp <- names(groups_list)[grp_ref_flag]

        # If more than one level in treatment col.
        treatment <- as.character(unique(df[[variables$arm]]))
        grp_trt_flag <- vapply(X = groups_list, FUN.VALUE = TRUE, FUN = function(x) all(treatment %in% x))
        trt_grp <- names(groups_list)[grp_trt_flag]

        grp <- combine_levels(.df_row[[variables$arm]], levels = reference, new_level = ref_grp)
        grp <- combine_levels(grp, levels = treatment, new_level = trt_grp)
      }

      # New: CMH method
      if (method == "cmh") {
        data <- data.frame(
          rsp = c(.ref_group[[.var]], df[[.var]]),
          grp = factor(rep(c("ref", "Not-ref"), c(nrow(.ref_group), nrow(df))), levels = c("ref", "Not-ref")),
          strata = interaction(rbind(.ref_group[variables$strata], df[variables$strata]))
        )

        y <- or_cmh(data, conf_level = conf_level)
      } else {
        # The reference level in `grp` must be the same as in the `rtables` column split.
        data <- data.frame(rsp = .df_row[[.var]], grp = grp, strata = interaction(.df_row[variables$strata]))

        y_all <- or_clogit_j(data, conf_level = conf_level, method = method)
        checkmate::assert_string(trt_grp)
        # New: pval here
        checkmate::assert_subset(trt_grp, names(y_all$or_ci_pvals))
        y_or_ci_pval <- y_all$or_ci_pvals[[trt_grp]]
        y$or_ci <- y_or_ci_pval[c("est", "lcl", "ucl")]
        y$n_tot <- y_all$n_tot
        y$pval <- y_or_ci_pval["pval"]
      }
    }

    one_group_no_events <- (sum(.ref_group[[.var]]) == 0) || (sum(df[[.var]]) == 0)
    if (na_if_no_events && one_group_no_events) {
      y$or_ci[["est"]] <- NA_real_
    }

    na_because_sparse <- one_group_empty || (na_if_no_events && one_group_no_events)
    if ("est" %in% names(y$or_ci) && is.na(y$or_ci[["est"]]) && !na_because_sparse && method != "approximate") {
      warning(
        "Unable to compute the odds ratio estimate. Please try re-running the function with ",
        "parameter `method` set to \"approximate\"."
      )
    }
  }

  y$or_ci <- with_label(x = y$or_ci, label = paste0("Odds Ratio (", 100 * conf_level, "% CI)"))

  # New: pval here
  y$pval <- unname(y$pval)

  y
}

#' @describeIn odds_ratio Formatted analysis function which is used as `afun`. Note that the
#'   junco specific `ref_path` and `.spl_context` arguments are used for reference column information.
#'
#' @return
#' * `a_odds_ratio_j()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @examples
#' set.seed(12)
#' dta <- data.frame(
#'   rsp = sample(c(TRUE, FALSE), 100, TRUE),
#'   grp = factor(rep(c("A", "B"), each = 50), levels = c("A", "B")),
#'   strata = factor(sample(c("C", "D"), 100, TRUE))
#' )
#'
#' a_odds_ratio_j(
#'   df = subset(dta, grp == "A"),
#'   .var = "rsp",
#'   ref_path = c("grp", "B"),
#'   .spl_context = data.frame(
#'     cur_col_split = I(list("grp")),
#'     cur_col_split_val = I(list(c(grp = "A"))),
#'     full_parent_df = I(list(dta))
#'   ),
#'   .df_row = dta
#' )
#'
#'
#' l <- basic_table() |>
#'   split_cols_by(var = "grp") |>
#'   analyze(
#'     "rsp",
#'     afun = a_odds_ratio_j,
#'     show_labels = "hidden",
#'     extra_args = list(
#'       ref_path = c("grp", "B"),
#'       .stats = c("or_ci", "pval")
#'     )
#'   )
#'
#' build_table(l, df = dta)
#'
#' l2 <- basic_table() |>
#'   split_cols_by(var = "grp") |>
#'   analyze(
#'     "rsp",
#'     afun = a_odds_ratio_j,
#'     show_labels = "hidden",
#'     extra_args = list(
#'       variables = list(arm = "grp", strata = "strata"),
#'       method = "cmh",
#'       ref_path = c("grp", "A"),
#'       .stats = c("or_ci", "pval")
#'     )
#'   )
#'
#' build_table(l2, df = dta)
#' @export
#' @order 2
a_odds_ratio_j <- function(
    df,
    .var,
    .df_row,
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
    default_stat_fnc = s_odds_ratio_j,
    custom_stat_fnc_list = NULL,
    args_list = c(
      df = list(df),
      .var = .var,
      .df_row = list(.df_row),
      .ref_group = list(ref$ref_group),
      .in_ref_col = ref$in_ref_col,
      dots_extra_args
    )
  )

  # Format according to specifications
  format_stats(
    x_stats,
    method_groups = "odds_ratio",
    stats_in = .stats,
    formats_in = .formats,
    labels_in = .labels,
    indents_in = .indent_mods
  )
}

#' Helper functions for odds ratio estimation
#'
#' @description `r lifecycle::badge('stable')`
#'
#' Functions to calculate odds ratios in [s_odds_ratio_j()].
#'
#' @inheritParams odds_ratio
#' @inheritParams proposal_argument_convention
#' @param data (`data.frame`)\cr data frame containing at least the variables `rsp` and `grp`, and optionally
#'   `strata` for [or_clogit_j()].
#' @return A named `list` of elements `or_ci`, `n_tot` and `pval`.
#'
#' @seealso [odds_ratio]
#'
#' @name h_odds_ratio
NULL

#' @describeIn h_odds_ratio Estimates the odds ratio based on [stats::glm()]. Note that there must be
#'   exactly 2 groups in `data` as specified by the `grp` variable.
#'
#' @examples
#' data <- data.frame(
#'   rsp = as.logical(c(1, 1, 0, 1, 0, 0, 1, 1)),
#'   grp = letters[c(1, 1, 1, 2, 2, 2, 1, 2)],
#'   strata = letters[c(1, 2, 1, 2, 2, 2, 1, 2)],
#'   stringsAsFactors = TRUE
#' )
#'
#' or_glm_j(data, conf_level = 0.95)
#'
#' @export
or_glm_j <- function(data, conf_level) {
  checkmate::assert_logical(data$rsp)
  (assert_proportion_value)(conf_level)
  assert_df_with_variables(data, list(rsp = "rsp", grp = "grp"))
  checkmate::assert_multi_class(data$grp, classes = c("factor", "character"))

  data$grp <- as_factor_keep_attributes(data$grp)
  assert_df_with_factors(data, list(val = "grp"), min.levels = 2, max.levels = 2)
  formula <- stats::as.formula("rsp ~ grp")
  model_fit <- stats::glm(formula = formula, data = data, family = stats::binomial(link = "logit"))

  # Note that here we need to discard the intercept.
  or <- exp(stats::coef(model_fit)[-1])
  or_ci <- exp(stats::confint.default(model_fit, level = conf_level)[-1, , drop = FALSE])

  values <- stats::setNames(c(or, or_ci), c("est", "lcl", "ucl"))
  n_tot <- stats::setNames(nrow(model_fit$model), "n_tot")

  # New: pval here
  pval <- summary(model_fit)$coef[-1, "Pr(>|z|)"]

  list(or_ci = values, n_tot = n_tot, pval = pval)
}

#' @describeIn h_odds_ratio Estimates the odds ratio based on [survival::clogit()]. This is done for
#'   the whole data set including all groups, since the results are not the same as when doing
#'   pairwise comparisons between the groups.
#'
#' @examples
#' data <- data.frame(
#'   rsp = as.logical(c(1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0)),
#'   grp = letters[c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3)],
#'   strata = LETTERS[c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)],
#'   stringsAsFactors = TRUE
#' )
#'
#' or_clogit_j(data, conf_level = 0.95)
#'
#' @export
or_clogit_j <- function(data, conf_level, method = "exact") {
  checkmate::assert_logical(data$rsp)
  (assert_proportion_value)(conf_level)
  assert_df_with_variables(data, list(rsp = "rsp", grp = "grp", strata = "strata"))
  checkmate::assert_multi_class(data$grp, classes = c("factor", "character"))
  checkmate::assert_multi_class(data$strata, classes = c("factor", "character"))
  checkmate::assert_subset(method, c("exact", "approximate", "efron", "breslow"), empty.ok = FALSE)

  data$grp <- as_factor_keep_attributes(data$grp)
  data$strata <- as_factor_keep_attributes(data$strata)

  # Deviation from convention: `survival::strata` must be simply `strata`.
  strata <- survival::strata
  formula <- stats::as.formula("rsp ~ grp + strata(strata)")
  model_fit <- clogit_with_tryCatch(
    formula = formula,
    data = data,
    method = method
  )

  # New: pval here

  # Create a list with one set of OR estimates, CI and p-value per coefficient, i.e.  comparison of one group vs. the
  # reference group.
  coef_est <- stats::coef(model_fit)
  ci_est <- stats::confint(model_fit, level = conf_level)
  pvals <- summary(model_fit)$coef[, "Pr(>|z|)", drop = FALSE]
  or_ci_pvals <- list()
  for (coef_name in names(coef_est)) {
    grp_name <- gsub("^grp", "", x = coef_name)
    or_ci_pvals[[grp_name]] <- stats::setNames(
      object = c(exp(c(coef_est[coef_name], ci_est[coef_name, , drop = TRUE])), pvals[coef_name, 1]),
      nm = c("est", "lcl", "ucl", "pval")
    )
  }
  list(or_ci_pvals = or_ci_pvals, n_tot = c(n_tot = model_fit$n))
}

#' @describeIn h_odds_ratio Estimates the odds ratio based on CMH. Note that there must be
#'   exactly 2 groups in `data` as specified by the `grp` variable.
#'
#' @examples
#' set.seed(123)
#' data <- data.frame(
#'   rsp = as.logical(rbinom(n = 40, size = 1, prob = 0.5)),
#'   grp = letters[sample(1:2, size = 40, replace = TRUE)],
#'   strata = LETTERS[sample(1:2, size = 40, replace = TRUE)],
#'   stringsAsFactors = TRUE
#' )
#'
#' or_cmh(data, conf_level = 0.95)
#'
#' @export
or_cmh <- function(data, conf_level) {
  checkmate::assert_logical(data$rsp)
  assert_proportion_value(conf_level)
  assert_df_with_variables(data, list(rsp = "rsp", grp = "grp"))
  checkmate::assert_multi_class(data$grp, classes = c("factor", "character"))

  data$grp <- as_factor_keep_attributes(data$grp)
  assert_df_with_factors(data, list(val = "grp"), min.levels = 2, max.levels = 2)

  # first dimension: treatment, control - therefore we reverse the levels order 2nd dimension: TRUE, FALSE 3rd
  # dimension: levels of strata rsp as factor rsp to handle edge case of no FALSE (or TRUE) rsp records
  t_tbl <- table(
    factor(data$grp, levels = rev(levels(data$grp))),
    factor(data$rsp, levels = c("TRUE", "FALSE")),
    data$strata
  )

  trt_ind <- 1
  ctrl_ind <- 2

  resp_ind <- 1
  nonresp_ind <- 2

  n1resp <- t_tbl[trt_ind, resp_ind, ]
  n2resp <- t_tbl[ctrl_ind, resp_ind, ]
  n1non <- t_tbl[trt_ind, nonresp_ind, ]
  n2non <- t_tbl[ctrl_ind, nonresp_ind, ]
  # Example: n2non are the control patient non-responders per stratum.

  # CMH statistic for odds ratio, Treatment over Control
  use_stratum <- (n1resp + n2resp + n1non + n2non) > 0
  n1resp <- n1resp[use_stratum]
  n2resp <- n2resp[use_stratum]
  n1non <- n1non[use_stratum]
  n2non <- n2non[use_stratum]
  n <- n1resp + n2resp + n1non + n2non

  or_num <- sum(n1resp * n2non / n)
  or_denom <- sum(n1non * n2resp / n)
  log_or <- log(or_num) - log(or_denom)
  or <- exp(log_or)

  term1_num <- sum((n1resp + n2non) * n1resp * n2non / n^2)
  term1_denom <- 2 * (sum(n1resp * n2non / n))^2

  term2_num <- sum(((n1resp + n2non) * n1non * n2resp + (n1non + n2resp) * n1resp * n2non) / n^2)
  term2_denom <- 2 * (sum(n1resp * n2non / n)) * (sum(n1non * n2resp / n))

  term3_num <- sum((n1non + n2resp) * n1non * n2resp / n^2)
  term3_denom <- 2 * (sum(n1non * n2resp / n))^2

  var_log_or <- term1_num / term1_denom + term2_num / term2_denom + term3_num / term3_denom
  se_log_or <- sqrt(var_log_or)

  z <- stats::qnorm((1 + conf_level) / 2)
  log_rel_or <- log_or + c(-1, +1) * z * se_log_or
  ci <- exp(log_rel_or)
  or_ci <- stats::setNames(c(or, ci), c("est", "lcl", "ucl"))

  pval <- stats::mantelhaen.test(t_tbl, correct = FALSE)$p.value

  n_tot <- stats::setNames(sum(n), "n_tot")

  list(or_ci = or_ci, n_tot = n_tot, pval = pval)
}
