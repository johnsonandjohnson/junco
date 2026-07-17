#' Impute missing binary responses and analyze each imputed data set
#'
#' Performs independent Bernoulli imputations for missing binary responses in
#' the control and treatment groups, then calculates a CMH risk difference and
#' p-value for each imputed data set. Only observations in `ctrlab` and
#' `trtlab` are included in the analysis.
#'
#' @param dat (`data.frame`)
#'   Analysis data containing treatment, response, and stratification variables.
#' @param p_ctrl (`numeric(1)`)
#'   Probability of response used to impute missing control-group responses.
#' @param p_trt (`numeric(1)`)
#'   Probability of response used to impute missing treatment-group responses.
#' @param trtvar (`string`)
#'   Name of the treatment variable in `dat`.
#' @param ctrlab (`string`)
#'   Value of `trtvar` identifying the control group.
#' @param trtlab (`string`)
#'   Value of `trtvar` identifying the treatment group.
#' @param respvar (`string`)
#'   Name of a logical binary response variable in `dat`. Missing values are
#'   imputed; observed values are retained.
#' @param stratvar (`character`)
#'   Names of one or more stratification variables in `dat`. Stratification
#'   variables must not be missing in the analysis groups.
#' @param n_imputations (`count`)
#'   Number of independently imputed data sets to analyze.
#'
#' @return A tibble with one row per imputation and the following columns:
#'
#' * `rd_est`: CMH risk-difference estimate.
#' * `rd_se`: standard error of the risk-difference estimate.
#' * `rd_var`: variance of the risk-difference estimate.
#' * `p_cmh`: CMH p-value using the Wilson-Hilferty transformation.
#' * `z_stat`: Wilson-Hilferty z statistic used to calculate `p_cmh`.
#'
#' @keywords internal
h_impute_analyze_resp <- function(
  dat,
  p_ctrl,
  p_trt,
  trtvar,
  ctrlab,
  trtlab,
  respvar,
  stratvar,
  n_imputations
) {
  checkmate::assert_data_frame(dat)
  checkmate::assert_number(p_ctrl, lower = 0, upper = 1, finite = TRUE)
  checkmate::assert_number(p_trt, lower = 0, upper = 1, finite = TRUE)
  checkmate::assert_string(trtvar)
  checkmate::assert_string(ctrlab)
  checkmate::assert_string(trtlab)
  checkmate::assert_string(respvar)
  checkmate::assert_character(stratvar, min.len = 1, any.missing = FALSE, unique = TRUE)
  checkmate::assert_count(n_imputations, positive = TRUE)
  checkmate::assert_subset(c(trtvar, respvar, stratvar), choices = names(dat))
  checkmate::assert_logical(dat[[respvar]], any.missing = TRUE)
  checkmate::assert_true(ctrlab != trtlab)

  treatment <- as.character(dat[[trtvar]])
  ctrl_data <- dat[which(treatment == ctrlab), , drop = FALSE]
  trt_data <- dat[which(treatment == trtlab), , drop = FALSE]

  checkmate::assert_true(nrow(ctrl_data) > 0)
  checkmate::assert_true(nrow(trt_data) > 0)

  analysis_data <- rbind(ctrl_data, trt_data)
  strata <- interaction(analysis_data[stratvar], drop = TRUE)
  response <- c(ctrl_data[[respvar]], trt_data[[respvar]])
  group <- factor(
    rep(c("ref", "Not-ref"), c(nrow(ctrl_data), nrow(trt_data))),
    levels = c("ref", "Not-ref")
  )
  missing_response <- is.na(response)
  missing_ctrl_response <- group == "ref" & missing_response
  missing_trt_response <- group == "Not-ref" & missing_response
  n_missing_ctrl <- sum(missing_ctrl_response)
  n_missing_trt <- sum(missing_trt_response)

  results <- vector("list", n_imputations)
  for (i in seq_len(n_imputations)) {
    imputed_response <- response
    if (n_missing_ctrl > 0) {
      imputed_response[missing_ctrl_response] <- as.logical(stats::rbinom(
        n = n_missing_ctrl,
        size = 1,
        prob = p_ctrl
      ))
    }
    if (n_missing_trt > 0) {
      imputed_response[missing_trt_response] <- as.logical(stats::rbinom(
        n = n_missing_trt,
        size = 1,
        prob = p_trt
      ))
    }
    rd_result <- tern::prop_diff_cmh(imputed_response, group, strata, diff_se = "standard")
    cmh_result <- tern::prop_cmh(
      table(group, imputed_response, strata),
      transform = "wilson_hilferty"
    )
    results[[i]] <- c(
      rd_est = rd_result$diff,
      rd_se = rd_result$se_diff,
      p_cmh = as.numeric(cmh_result),
      z_stat = attr(cmh_result, "z_stat")
    )
  }

  results <- as.data.frame(do.call(rbind, results))
  tibble::tibble(
    rd_est = results$rd_est,
    rd_se = results$rd_se,
    rd_var = results$rd_se^2,
    p_cmh = results$p_cmh,
    z_stat = results$z_stat
  )
}

#' Calculate results across binary-imputation scenarios
#'
#' Runs `h_impute_analyze_resp()` for each paired control and treatment response
#' probability, pools estimates across imputations using Rubin's rules, and
#' pools Wilson-Hilferty z statistics to obtain a p-value. A scenario where both
#' probabilities are zero or one is deterministic and is therefore analyzed
#' once rather than repeatedly imputed.
#'
#' @param dat (`data.frame`)
#'   Analysis data containing treatment, response, and stratification variables.
#' @param p_ctrl (`numeric`)
#'   Control-group response probabilities, one for each scenario.
#' @param p_trt (`numeric`)
#'   Treatment-group response probabilities, paired with `p_ctrl` by position.
#' @param trtvar (`string`)
#'   Name of the treatment variable in `dat`.
#' @param ctrlab (`string`)
#'   Value of `trtvar` identifying the control group.
#' @param trtlab (`string`)
#'   Value of `trtvar` identifying the treatment group.
#' @param respvar (`string`)
#'   Name of a logical binary response variable in `dat`.
#' @param stratvar (`character`)
#'   Names of one or more stratification variables in `dat`.
#' @param n_imputations (`count`)
#'   Number of imputations for non-deterministic scenarios.
#' @param pvalcat (named `list`)
#'   P-value categories passed to [categorize_pval()].
#'
#' @return A tibble with one row per scenario. `rd` and `rd_se` are the pooled
#' risk-difference estimate and standard error; `p_value` and `p_cat` are the
#' pooled p-value and its category. `marker_flag` and `effect_label` contain
#' `"*"` when a deterministic scenario or a single-imputation p-value fallback
#' was used.
#'
#' @examples
#' dat <- data.frame(
#'   arm = rep(c("Control", "Treatment"), each = 4),
#'   response = c(TRUE, FALSE, NA, TRUE, TRUE, FALSE, NA, FALSE),
#'   strata = rep(c("A", "B"), 4)
#' )
#' pvalcat <- list("<0.05" = c(0, 0.05), ">=0.05" = c(0.05, 1))
#'
#' set.seed(123)
#' resp_multiple_imputation(
#'   dat,
#'   p_ctrl = c(0, 0.25),
#'   p_trt = c(1, 0.75),
#'   trtvar = "arm",
#'   ctrlab = "Control",
#'   trtlab = "Treatment",
#'   respvar = "response",
#'   stratvar = "strata",
#'   n_imputations = 5,
#'   pvalcat = pvalcat
#' )
#' @export
resp_multiple_imputation <- function(
  dat,
  p_ctrl,
  p_trt,
  trtvar,
  ctrlab,
  trtlab,
  respvar,
  stratvar,
  n_imputations,
  pvalcat
) {
  checkmate::assert_numeric(p_ctrl, min.len = 1, lower = 0, upper = 1, finite = TRUE)
  checkmate::assert_numeric(p_trt, min.len = 1, lower = 0, upper = 1, finite = TRUE)
  checkmate::assert_true(length(p_ctrl) == length(p_trt))
  checkmate::assert_count(n_imputations, positive = TRUE)
  checkmate::assert_list(h_normalize_pvalcat(pvalcat)) # Just to check that it works.

  results <- lapply(seq_along(p_ctrl), function(i) {
    control_probability <- p_ctrl[[i]]
    treatment_probability <- p_trt[[i]]
    m <- if (all(c(control_probability, treatment_probability) %in% c(0, 1))) 1L else n_imputations

    per_imputation <- h_impute_analyze_resp(
      dat = dat,
      p_ctrl = control_probability,
      p_trt = treatment_probability,
      trtvar = trtvar,
      ctrlab = ctrlab,
      trtlab = trtlab,
      respvar = respvar,
      stratvar = stratvar,
      n_imputations = m
    )

    if (m == 1) {
      estimate <- per_imputation$rd_est[[1]]
      standard_error <- per_imputation$rd_se[[1]]
      p_value <- per_imputation$p_cmh[[1]]
      used_fallback <- TRUE
    } else {
      pooled_rd <- pool_rubin_scalar(per_imputation$rd_est, per_imputation$rd_var)
      estimate <- pooled_rd$est
      standard_error <- pooled_rd$se
      pooled_p_value <- pool_z_stat(per_imputation$z_stat)$p
      used_fallback <- is.na(pooled_p_value) || !is.finite(pooled_p_value)
      p_value <- if (used_fallback) per_imputation$p_cmh[[1]] else pooled_p_value
    }

    marker_flag <- if (m == 1 || used_fallback) "*" else ""
    tibble::tibble(
      p_ctrl = control_probability,
      p_trt = treatment_probability,
      m = m,
      rd = estimate,
      rd_se = standard_error,
      p_value = p_value,
      p_cat = categorize_pval(p_value, pvalcat),
      marker_flag = marker_flag,
      effect_label = paste0(
        formatC(100 * estimate, format = "f", digits = 1),
        "%",
        marker_flag
      )
    )
  })

  tibble::as_tibble(do.call(rbind, results))
}
