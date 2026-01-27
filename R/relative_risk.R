#' Relative risk estimation
#'
#' The analysis function [a_relative_risk()] is used to create a layout element
#' to estimate the relative risk for response within a studied population. Only
#' the CMH method is available currently.
#' The primary analysis variable, `vars`, is a logical variable indicating
#' whether a response has occurred for each record.
#' A stratification variable must be supplied via the
#' `strata` element of the `variables` argument.
#'
#' @details The variance of the CMH relative risk estimate is calculated using
#'   the Greenland and Robins (1985) variance estimation.
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
#' @param method (`string`)\cr method to use for relative risk calculation.
#' @param weights_method (`string`)\cr method to use for weights calculation in stratified analysis.
#'
#' @note This has been adapted from the `odds_ratio` functions in the `tern` package.
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
    method = "cmh",
    weights_method = "cmh") {
  method <- match.arg(method)
  weights_method <- match.arg(weights_method)
  checkmate::assert_character(variables$strata, null.ok = FALSE)
  y <- list(rel_risk_ci = list(), pval = list())

  if (!.in_ref_col) {
    rsp <- c(.ref_group[[.var]], df[[.var]])
    grp <- factor(rep(c("ref", "Not-ref"), c(nrow(.ref_group), nrow(df))), levels = c("ref", "Not-ref"))

    strata_colnames <- variables$strata
    checkmate::assert_character(strata_colnames, null.ok = FALSE)
    strata_vars <- stats::setNames(as.list(strata_colnames), strata_colnames)

    assert_df_with_variables(df, strata_vars)
    assert_df_with_variables(.ref_group, strata_vars)

    # Merging interaction strata for reference group rows data and remaining
    strata <- c(interaction(.ref_group[strata_colnames]), interaction(df[strata_colnames]))
    strata <- as.factor(strata)

    y <- prop_ratio_cmh(rsp, grp, strata, conf_level)[c("rel_risk_ci", "pval")]

    one_group_no_events <- (sum(.ref_group[[.var]]) == 0) || (sum(df[[.var]]) == 0)
    if (one_group_no_events) {
      y$rel_risk_ci <- c(est = NA_real_, lcl = 0, ucl = Inf)
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
    .indent_mods = NULL) {
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
#' @param strata (`factor`)\cr variable with one level per stratum and same length as `rsp`.
#'
#' @return a list with elements `rel_risk_ci` and `pval`.
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
#'   rsp = rsp, grp = grp, strata = interaction(strata_data),
#'   conf_level = 0.90
#' )
#'
#' @export
prop_ratio_cmh <- function(rsp, grp, strata, conf_level = 0.95) {
  grp <- as_factor_keep_attributes(grp)
  strata <- as_factor_keep_attributes(strata)
  has_single_stratum <- nlevels(strata) == 1
  check_diff_prop_ci(
    rsp = rsp,
    grp = grp,
    conf_level = conf_level,
    strata = strata
  )

  if (any(tapply(rsp, strata, length) < 5)) {
    warning("Less than 5 observations in some strata.")
  }

  # first dimension: treatment, control - therefore we reverse the levels order 2nd dimension: TRUE, FALSE 3rd
  # dimension: levels of strata rsp as factor rsp to handle edge case of no FALSE (or TRUE) rsp records
  t_tbl <- table(factor(grp, levels = rev(levels(grp))), factor(rsp, levels = c("TRUE", "FALSE")), strata)

  # Index 1 is for treatment, index 2 is for control, e.g. n1 is number of treatment patients per stratum.
  n1resp <- t_tbl[1, 1, levels(strata)]
  n2resp <- t_tbl[2, 1, levels(strata)]

  t1 <- t_tbl[1, 1:2, , drop = TRUE]
  t2 <- t_tbl[2, 1:2, , drop = TRUE]
  if (has_single_stratum) {
    n1 <- sum(t1)
    n2 <- sum(t2)
    checkmate::assert_count(n1)
    checkmate::assert_count(n2)
  } else {
    n1 <- colSums(t1)
    n2 <- colSums(t2)
    checkmate::assert_integerish(n1)
    checkmate::assert_integerish(n2)
    checkmate::assert_true(identical(length(n1), length(n2)))
  }

  # CMH statistic for relative risk, Treatment over Control
  use_stratum <- (n1 > 0) & (n2 > 0)
  n1 <- n1[use_stratum]
  n2 <- n2[use_stratum]
  n1resp <- n1resp[use_stratum]
  n2resp <- n2resp[use_stratum]
  nresp <- n1resp + n2resp
  n <- n1 + n2

  rel_risk_num <- sum(n1resp * n2 / n)
  rel_risk_denom <- sum(n2resp * n1 / n)
  log_rel_risk <- log(rel_risk_num) - log(rel_risk_denom)
  rel_risk <- exp(log_rel_risk)

  var_num <- sum((n1 * n2 * nresp - n1resp * n2resp * n) / n^2)
  var_denom <- rel_risk_num * rel_risk_denom
  var_log_rel_risk <- var_num / var_denom
  se_log_rel_risk <- sqrt(var_log_rel_risk)

  z <- stats::qnorm((1 + conf_level) / 2)
  log_rel_risk_ci <- log_rel_risk + c(-1, +1) * z * se_log_rel_risk
  ci <- exp(log_rel_risk_ci)
  rel_risk_ci <- stats::setNames(c(rel_risk, ci), c("est", "lcl", "ucl"))

  pval <- safe_mh_test(t_tbl, correct = FALSE)$p.value

  list(rel_risk_ci = rel_risk_ci, pval = pval)
}
