#' Relative risk estimation
#'
#' The analysis function [a_relative_risk()] is used to create a layout element
#' to estimate the relative risk for response within a studied population using
#' stratified CMH or unstratified Wald methods.
#' The primary analysis variable, `vars`, is a logical variable indicating
#' whether a response has occurred for each record.
#' For CMH, a stratification variable must be supplied via the
#' `strata` element of the `variables` argument.
#'
#' @details The variance of the CMH relative risk estimate is calculated using
#'   the Greenland and Robins (1985) variance estimation.
#'   The unstratified `wald` and `waldcc` methods use a normal approximation
#'   for the log relative risk.
#'   `waldcc` adds 0.5 to
#'   each response count and each group total in both the estimate and variance.
#'   With `wald`, a zero response count gives undefined confidence limits and
#'   p-value (`NA`).
#'
#' @param df (`data.frame`)\cr input data frame.
#' @param .var (`string`)\cr name of the response variable.
#' @param ref_path (`character`)\cr path to the reference group.
#' @param .spl_context (`environment`)\cr split context environment.
#' @param ... Additional arguments passed to the statistics function.
#' @param .stats (`character`)\cr statistics to calculate.
#' @param .formats (`list`)\cr formats for the statistics.
#' @param .labels (`list`)\cr labels for the statistics.
#' @param .indent_mods (`list`)\cr indentation modifications for the statistics.
#' @param .ref_group (`data.frame`)\cr reference group data frame.
#' @param .in_ref_col (`logical`)\cr whether the current column is the reference column.
#' @param variables (`list`)\cr list with strata variable names.
#' @param conf_level (`numeric`)\cr confidence level for the confidence interval.
#' @param method (`string`)\cr relative risk method: `cmh`, `wald`, or `waldcc`.
#' @param weights_method (`string`)\cr method to use for weights calculation in stratified analysis.
#'
#' @name relative_risk
NULL

#' @describeIn relative_risk Statistics function estimating the relative risk for response.
#'
#' @return
#' * `s_relative_risk()` returns a named list of elements `rel_risk_ci` and `pval`.
#'
#' @examples
#' nex <- 100
#' dta <- data.frame(
#'   "rsp" = sample(c(TRUE, FALSE), nex, TRUE),
#'   "grp" = sample(c("A", "B"), nex, TRUE),
#'   "f1" = sample(c("a1", "a2"), nex, TRUE),
#'   "f2" = sample(c("x", "y", "z"), nex, TRUE),
#'   stringsAsFactors = TRUE
#' )
#'
#' s_relative_risk(
#'   df = subset(dta, grp == "A"),
#'   .var = "rsp",
#'   .ref_group = subset(dta, grp == "B"),
#'   .in_ref_col = FALSE,
#'   variables = list(strata = c("f1", "f2")),
#'   conf_level = 0.90
#' )
#' @export
s_relative_risk <- function(
  df,
  .var,
  .ref_group,
  .in_ref_col,
  variables = list(strata = NULL),
  conf_level = 0.95,
  method = c("cmh", "wald", "waldcc"),
  weights_method = "cmh"
) {
  method <- match.arg(method)
  weights_method <- match.arg(weights_method)
  y <- list(rel_risk_ci = list(), pval = list())

  if (!.in_ref_col) {
    # Note: We could use tern:::assert_stratification_compatibility if it was
    # exported, or consider moving to tern.
    if (method == "cmh") {
      checkmate::assert_character(variables$strata, null.ok = FALSE)
    } else {
      checkmate::assert_character(variables$strata, len = 0, null.ok = TRUE)
    }
    checkmate::assert_logical(df[[.var]], any.missing = FALSE)
    checkmate::assert_logical(.ref_group[[.var]], any.missing = FALSE)
    prepared <- tern::h_prepare_rsp_table(
      df = df,
      df_ref = .ref_group,
      var = .var,
      strata_vars = if (method == "cmh") variables$strata else NULL
    )

    if (method == "cmh") {
      y <- prop_ratio_cmh(tbl = prepared$tbl, conf_level = conf_level)
    } else {
      y <- prop_ratio_wald(tbl = prepared$tbl, conf_level = conf_level, correction = method == "waldcc")
    }
  }

  y$rel_risk_ci <- with_label(
    x = y$rel_risk_ci,
    label = paste0("Relative risk (", f_conf_level(conf_level), ")")
  )
  y$pval <- unname(y$pval)

  y
}

#' @describeIn relative_risk Formatted analysis function which is used as `afun`. Note that the
#'   junco specific `ref_path` and `.spl_context` arguments are used for reference column information.
#'
#' @return
#' * `a_relative_risk()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @examples
#' nex <- 100
#' dta <- data.frame(
#'   "rsp" = sample(c(TRUE, FALSE), nex, TRUE),
#'   "grp" = sample(c("A", "B"), nex, TRUE),
#'   "f1" = sample(c("a1", "a2"), nex, TRUE),
#'   "f2" = sample(c("x", "y", "z"), nex, TRUE),
#'   stringsAsFactors = TRUE
#' )
#'
#' l <- basic_table() |>
#'   split_cols_by(var = "grp") |>
#'   analyze(
#'     vars = "rsp",
#'     afun = a_relative_risk,
#'     extra_args = list(
#'       conf_level = 0.90,
#'       variables = list(strata = "f1"),
#'       ref_path = c("grp", "B")
#'     )
#'   )
#'
#' build_table(l, df = dta)
#' @export
#' @order 2
a_relative_risk <- function(
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
    default_stat_fnc = s_relative_risk,
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
    method_groups = "relative_risk",
    stats_in = .stats,
    formats_in = .formats,
    labels_in = .labels,
    indents_in = .indent_mods
  )
}


#' @keywords internal
safe_mh_test <- function(...) {
  tryCatch(
    stats::mantelhaen.test(...),
    error = function(e) list(p.value = NA_real_)
  )
}

#' Relative Risk CMH Statistic
#'
#' Calculates the relative risk which is defined as the ratio between the
#' response rates between the experimental treatment group and the control treatment group, adjusted
#' for stratification factors by applying Cochran-Mantel-Haenszel (CMH) weights.
#'
#' @inheritParams proposal_argument_convention
#' @param tbl (`array`)\cr a 2 by 2 by stratum contingency table, as returned
#'   in the `tbl` element of [tern::h_prepare_rsp_table()]. Rows are reference
#'   then treatment; columns are responders then nonresponders.
#'
#' @return A list with elements `rel_risk_ci` and `pval`.
#' @examples
#'
#' set.seed(2)
#' rsp <- sample(c(TRUE, FALSE), 100, TRUE)
#' grp <- sample(c("Placebo", "Treatment"), 100, TRUE)
#' grp <- factor(grp, levels = c("Placebo", "Treatment"))
#' strata_data <- data.frame(
#'   "f1" = sample(c("a", "b"), 100, TRUE),
#'   "f2" = sample(c("x", "y", "z"), 100, TRUE),
#'   stringsAsFactors = TRUE
#' )
#'
#' prop_ratio_cmh(
#'   tbl = table(grp, factor(rsp, levels = c(TRUE, FALSE)), interaction(strata_data)),
#'   conf_level = 0.90
#' )
#'
#' @export
prop_ratio_cmh <- function(tbl, conf_level = 0.95) {
  checkmate::assert_array(tbl, mode = "numeric", d = 3, any.missing = FALSE)
  checkmate::assert_true(identical(dim(tbl)[1:2], c(2L, 2L)))
  checkmate::assert_numeric(as.vector(tbl), lower = 0, finite = TRUE)
  checkmate::assert_number(conf_level, lower = 0, upper = 1)
  if (any(apply(tbl, 3, sum) < 5)) {
    warning("Less than 5 observations in some strata.")
  }
  # Rows are reference then treatment; columns are strata, including a single stratum.
  n_resp <- matrix(tbl[, 1, ], nrow = 2)
  n <- apply(tbl, c(1, 3), sum)
  checkmate::assert_integerish(as.vector(n), lower = 0)

  # CMH statistic for relative risk, Treatment over Control
  use_stratum <- colSums(n > 0) == 2L
  n <- n[, use_stratum, drop = FALSE]
  n_resp <- n_resp[, use_stratum, drop = FALSE]
  n_total <- colSums(n)

  rel_risk_num <- sum(n_resp[2, ] * n[1, ] / n_total)
  rel_risk_denom <- sum(n_resp[1, ] * n[2, ] / n_total)
  log_rel_risk <- log(rel_risk_num) - log(rel_risk_denom)
  rel_risk <- exp(log_rel_risk)

  var_num <- sum((n[1, ] * n[2, ] * colSums(n_resp) - n_resp[1, ] * n_resp[2, ] * n_total) / n_total^2)
  var_denom <- rel_risk_num * rel_risk_denom
  var_log_rel_risk <- var_num / var_denom
  se_log_rel_risk <- sqrt(var_log_rel_risk)

  z <- stats::qnorm((1 + conf_level) / 2)
  log_rel_risk_ci <- log_rel_risk + c(-1, +1) * z * se_log_rel_risk
  ci <- exp(log_rel_risk_ci)
  rel_risk_ci <- stats::setNames(c(rel_risk, ci), c("est", "lcl", "ucl"))

  if (any(rowSums(tbl[, 1, , drop = FALSE]) == 0)) {
    rel_risk_ci <- c(est = NA_real_, lcl = 0, ucl = Inf)
  }

  pval <- safe_mh_test(tbl, correct = FALSE)$p.value

  list(rel_risk_ci = rel_risk_ci, pval = pval)
}


#' Relative Risk Wald Statistic
#'
#' Estimates the relative response risk in the second group divided by that in the first
#' group, with log-scale Wald confidence limits and a two-sided Wald p-value.
#'
#' @inheritParams proposal_argument_convention
#' @param tbl (`matrix`)\cr a 2 by 2 contingency table, as returned in the
#'   `tbl` element of [tern::h_prepare_rsp_table()]. Rows are reference then
#'   treatment; columns are responders then nonresponders.
#' @param correction (`flag`)\cr whether to add 0.5 to the response counts and
#'   group totals, in order to support the case where there are zero responses in either group.
#' @return A list with elements `rel_risk_ci` (named `est`, `lcl`, `ucl`) and `pval`.
#'
#' @examples
#' prop_ratio_wald(
#'   tbl = rbind(ref = c(1, 1), trt = c(2, 1)),
#'   correction = TRUE
#' )
#' @export
prop_ratio_wald <- function(tbl, conf_level = 0.95, correction = FALSE) {
  checkmate::assert_matrix(tbl, mode = "numeric", nrows = 2, ncols = 2, any.missing = FALSE)
  checkmate::assert_numeric(as.vector(tbl), lower = 0, finite = TRUE)
  checkmate::assert_number(conf_level, lower = 0, upper = 1)
  checkmate::assert_flag(correction)

  n_resp <- tbl[, 1]
  n <- rowSums(tbl)
  checkmate::assert_true(all(n > 0))

  addon <- if (correction) 0.5 else 0
  n_resp <- n_resp + addon
  n <- n + addon

  estimate <- (n_resp[2] / n[2]) / (n_resp[1] / n[1])
  if (any(n_resp == 0)) {
    if (is.nan(estimate)) {
      estimate <- NA_real_
    }
    ci <- c(NA_real_, NA_real_)
    pval <- NA_real_
  } else {
    se <- sqrt(sum(1 / n_resp - 1 / n))
    log_rr <- log(estimate)
    z <- stats::qnorm((1 + conf_level) / 2)
    ci <- exp(log_rr + c(-1, 1) * z * se)
    pval <- if (se == 0) 1 else 2 * stats::pnorm(-abs(log_rr / se))
  }

  list(
    rel_risk_ci = stats::setNames(
      c(unname(estimate), ci),
      c("est", "lcl", "ucl")
    ),
    pval = unname(pval)
  )
}
