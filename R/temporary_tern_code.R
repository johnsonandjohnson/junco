# Moved from tern prop_diff.R ----

#' @name temporary_tern_code
#'
#' @title Temporary tern functions
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Functions extracted from `{tern}` until the issues
#' https://github.com/pharmaverse/tern/issues/1535 and
#' https://github.com/pharmaverse/tern/issues/1539 are resolved.
#'
#' They are temporarily copied from prop_diff.R and prop_diff_test.R.
#' from `{tern}` PRs:
#' https://github.com/pharmaverse/tern/pull/1538 and
#' https://github.com/pharmaverse/tern/pull/1542.
#'
#' @order 1
#' @keywords internal
NULL

#' @describeIn temporary_tern_code Temporarily moved from `{tern}`
s_proportion_diff_jtemp <- function(df,
                                    .var,
                                    .ref_group = NULL,
                                    .in_ref_col = NULL,
                                    variables = list(strata = NULL),
                                    conf_level = 0.95,
                                    method = c(
                                      "waldcc", "wald", "cmh", "cmh_sato", "cmh_mn",
                                      "ha", "newcombe", "newcombecc",
                                      "strat_newcombe", "strat_newcombecc",
                                      "uncond_exact_diff"
                                    ),
                                    weights_method = c("cmh", "wilson_h"),
                                    val = TRUE,
                                    ...) {
  checkmate::assert_data_frame(df)
  checkmate::assert_string(.var)
  checkmate::assert_subset(.var, colnames(df), empty.ok = FALSE)
  checkmate::assert_data_frame(.ref_group, null.ok = TRUE)
  checkmate::assert_flag(.in_ref_col, null.ok = TRUE)
  checkmate::assert_list(variables, null.ok = TRUE)
  if (!is.null(variables)) {
    checkmate::assert_set_equal(names(variables), "strata")
  }
  checkmate::assert_atomic(val)

  method <- match.arg(method)

  if (is.null(.in_ref_col) || .in_ref_col) {
    y <- list(diff = numeric(), diff_ci = numeric(), diff_est_ci = numeric())
  } else {
    checkmate::assert_false(is.null(.ref_group))
    utils::getFromNamespace("assert_stratification_compatibility", "tern")(
      method = method,
      stratified_methods = c(
        "cmh", "cmh_sato", "cmh_mn", "strat_newcombe", "strat_newcombecc"
      ),
      strata_vars = variables$strata
    )

    rsp_list <- tern::h_prepare_rsp_table(
      df = df, df_ref = .ref_group, var = .var, val = val,
      strata_vars = variables$strata,
      complete_cases = TRUE
    )
    rsp <- rsp_list$rsp
    grp <- rsp_list$grp
    strata <- rsp_list$strata

    cmh_stats <- c("diff", "diff_ci", "se_diff")
    y <- switch(method,
      "wald" = tern::prop_diff_wald(rsp, grp, conf_level, correct = FALSE),
      "waldcc" = tern::prop_diff_wald(rsp, grp, conf_level, correct = TRUE),
      "ha" = tern::prop_diff_ha(rsp, grp, conf_level),
      "newcombe" = tern::prop_diff_nc(rsp, grp, conf_level, correct = FALSE),
      "newcombecc" = tern::prop_diff_nc(rsp, grp, conf_level, correct = TRUE),
      "strat_newcombe" = prop_diff_strat_nc_jtemp(
        rsp, grp, strata, weights_method, conf_level,
        correct = FALSE
      ),
      "strat_newcombecc" = prop_diff_strat_nc_jtemp(
        rsp, grp, strata, weights_method, conf_level,
        correct = TRUE
      ),
      "cmh" = prop_diff_cmh_jtemp(rsp, grp, strata, conf_level, diff_se = "standard")[cmh_stats],
      "cmh_sato" = prop_diff_cmh_jtemp(rsp, grp, strata, conf_level, diff_se = "sato")[cmh_stats],
      "cmh_mn" = prop_diff_cmh_jtemp(rsp, grp, strata, conf_level, diff_se = "miettinen_nurminen")[cmh_stats],
      "uncond_exact_diff" = prop_diff_uncond_exact_jtemp(rsp, grp, conf_level)
    )

    y$diff <- setNames(y$diff * 100, paste0("diff_", method))
    y$diff_ci <- setNames(y$diff_ci * 100, paste0("diff_ci_", method, c("_l", "_u")))
    y$diff_est_ci <- c(y$diff, y$diff_ci)
    if (!is.null(y$se_diff)) {
      y$se_diff <- setNames(y$se_diff * 100, paste0("se_diff_", method))
    }
  }

  attr(y$diff, "label") <- "Difference in Response rate (%)"
  attr(y$diff_ci, "label") <- tern::d_proportion_diff(conf_level, method, long = FALSE)
  attr(y$diff_est_ci, "label") <- paste(attr(y$diff, "label"), "and", attr(y$diff_ci, "label"))
  if (!is.null(y$se_diff)) {
    attr(y$se_diff, "label") <- paste("Standard Error of", attr(y$diff, "label"))
  }

  y
}

#' @describeIn temporary_tern_code Temporarily moved from `{tern}`
prop_diff_cmh_jtemp <- function(rsp,
                                grp,
                                strata,
                                conf_level = 0.95,
                                diff_se = c("standard", "sato", "miettinen_nurminen")) {
  diff_se <- match.arg(diff_se)

  grp <- tern::as_factor_keep_attributes(grp)
  strata <- tern::as_factor_keep_attributes(strata)
  tern::check_diff_prop_ci(
    rsp = rsp, grp = grp, conf_level = conf_level, strata = strata
  )

  # 1st dimension: CONTROL, TX
  # 2nd dimension: TRUE, FALSE
  # 3rd dimension: levels of strata
  # Note: rsp needs to be a factor to handle edge case of no FALSE (or TRUE).
  tbl <- table(grp, factor(rsp, levels = c("TRUE", "FALSE")), strata)

  if (any(marginSums(tbl, margin = 3L) < 5L)) {
    warning("Less than 5 observations in some strata.")
  }

  prop <- h_prop_cmh_jtemp(tbl, conf_level = conf_level)
  prop_diff_est <- unname(prop$est2 - prop$est1)

  if (diff_se %in% c("standard", "sato")) {
    prop_diff_var <- if (diff_se == "standard") {
      unname(prop$var1 + prop$var2)
    } else { # "sato"
      h_cmh_sato_var_jtemp(prop)
    }
    prop_diff_se <- sqrt(prop_diff_var)
    z <- stats::qnorm((1 + conf_level) / 2)
    prop_diff_ci <- prop_diff_est + c(-1, 1) * z * prop_diff_se
  } else { # "miettinen_nurminen"
    mn <- h_miettinen_nurminen_stratified_ci_jtemp(prop, conf_level = conf_level)
    prop_diff_se <- mn$se
    prop_diff_ci <- mn$ci
  }

  list(
    prop = prop$est_both_groups,
    prop_ci = prop$ci_both_groups,
    diff = prop_diff_est,
    diff_ci = prop_diff_ci,
    se_diff = prop_diff_se,
    weights = prop$w_normalized,
    n1 = prop$n1,
    n2 = prop$n2
  )
}

#' @describeIn temporary_tern_code Temporarily moved from `{tern}`
prop_diff_strat_nc_jtemp <- function(rsp,
                                     grp,
                                     strata,
                                     weights_method = c("cmh", "wilson_h"),
                                     conf_level = 0.95,
                                     correct = FALSE) {
  weights_method <- match.arg(weights_method)
  grp <- as_factor_keep_attributes(grp)
  strata <- as_factor_keep_attributes(strata)
  tern::check_diff_prop_ci(
    rsp = rsp, grp = grp, conf_level = conf_level, strata = strata
  )
  checkmate::assert_number(conf_level, lower = 0, upper = 1)
  checkmate::assert_flag(correct)
  if (any(tapply(rsp, strata, length) < 5)) {
    warning("Less than 5 observations in some strata.")
  }

  # Finding the weights
  weights <- if (weights_method == "cmh") {
    prop_diff_cmh_jtemp(rsp = rsp, grp = grp, strata = strata)$weights
  } else if (weights_method == "wilson_h") {
    tern::prop_strat_wilson(rsp, strata, conf_level = conf_level, correct = correct)$weights
  }
  weights[levels(strata)[!levels(strata) %in% names(weights)]] <- 0

  # Calculating lower (`l`) and upper (`u`) confidence bounds per group.
  rsp_by_grp <- split(rsp, f = grp)
  strata_by_grp <- split(strata, f = grp)
  strat_wilson_by_grp <- Map(
    prop_strat_wilson,
    rsp = rsp_by_grp,
    strata = strata_by_grp,
    weights = list(weights, weights),
    conf_level = conf_level,
    correct = correct
  )

  ci_ref <- strat_wilson_by_grp[[1]]
  ci_trt <- strat_wilson_by_grp[[2]]
  l_ref <- as.numeric(ci_ref$conf_int[1])
  u_ref <- as.numeric(ci_ref$conf_int[2])
  l_trt <- as.numeric(ci_trt$conf_int[1])
  u_trt <- as.numeric(ci_trt$conf_int[2])

  # Estimating the diff and n_ref, n_trt (it allows different weights to be used)
  t_tbl <- table(
    factor(rsp, levels = c("FALSE", "TRUE")),
    grp,
    strata
  )
  n_ref <- colSums(t_tbl[1:2, 1, ])
  n_trt <- colSums(t_tbl[1:2, 2, ])
  use_stratum <- (n_ref > 0) & (n_trt > 0)
  n_ref <- n_ref[use_stratum]
  n_trt <- n_trt[use_stratum]
  p_ref <- t_tbl[2, 1, use_stratum] / n_ref
  p_trt <- t_tbl[2, 2, use_stratum] / n_trt
  est1 <- sum(weights * p_ref)
  est2 <- sum(weights * p_trt)
  diff_est <- est2 - est1

  lambda1 <- sum(weights^2 / n_ref)
  lambda2 <- sum(weights^2 / n_trt)
  z <- stats::qnorm((1 + conf_level) / 2)

  lower <- diff_est - z * sqrt(lambda2 * l_trt * (1 - l_trt) + lambda1 * u_ref * (1 - u_ref))
  upper <- diff_est + z * sqrt(lambda1 * l_ref * (1 - l_ref) + lambda2 * u_trt * (1 - u_trt))

  list(
    "diff" = diff_est,
    "diff_ci" = c("lower" = lower, "upper" = upper)
  )
}

#' @describeIn temporary_tern_code Temporarily moved from `{tern}`
prop_diff_uncond_exact_jtemp <- function(rsp,
                                         grp,
                                         conf_level = 0.95) {
  grp <- tern::as_factor_keep_attributes(grp)
  tern::check_diff_prop_ci(rsp = rsp, grp = grp, conf_level = conf_level)

  alpha <- 1 - conf_level
  cutoff <- alpha / 2

  tbl <- table(grp, factor(rsp, levels = c(TRUE, FALSE)))

  # Step 0: Calculate the observed difference in proportions
  # and the observed test statistic value.
  # Store counts as doubles to avoid 32-bit integer overflow in cross-products.
  n2 <- as.double(sum(tbl[1, ]))
  n1 <- as.double(sum(tbl[2, ]))

  if (n1 == 0 || n2 == 0) {
    return(list(
      diff = NaN,
      diff_ci = c(NaN, NaN)
    ))
  }

  n21_obs <- tbl[1, 1]
  n11_obs <- tbl[2, 1]
  diff_est <- n11_obs / n1 - n21_obs / n2

  # Step 1: Enumerate all tables in A with fixed row margins
  # n1 and n2.
  if (n1 * n2 > 2^53) {
    stop("uncond_exact_diff: Sample sizes exceed the exact integer comparison limit.")
  }
  if (n1 * n2 > 1e5) {
    warning("uncond_exact_diff: Large sample sizes may lead to long computation time.")
  }
  tables <- expand.grid(
    n11 = 0:n1,
    n21 = 0:n2
  )

  # Step 2: Compare integer numerators of T(a) = n11 / n1 - n21 / n2.
  # The positive denominator n1 * n2 is common to all tables. These cross-products
  # and their differences are exact for n1 * n2 <= 2^53, preserving ties without
  # a floating-point tolerance. Compute the observed numerator from counts too.
  t_values <- tables$n11 * n2 - tables$n21 * n1
  t0 <- n11_obs * n2 - n21_obs * n1

  # Step 3: For each hypothesized difference d*, compute the worst-case
  # tail probabilities P_U(d*) and P_L(d*) by maximizing over the nuisance
  # parameter p2.
  p_upper <- function(d_star) {
    # Step 4a: Compute worst-case one-sided tail probability:
    # P_U(d*) = sup_p2 sum_{T(a) >= t0} f(...)
    utils::getFromNamespace("h_worst_case_tail_probability", "tern")(
      d_star = d_star,
      n1 = n1,
      n2 = n2,
      t_values = t_values,
      t0 = t0,
      tables = tables,
      tail = "upper"
    )
  }
  p_lower <- function(d_star) {
    # Step 4b: Compute worst-case one-sided tail probability:
    # P_L(d*) = sup_p2 sum_{T(a) <= t0} f(...)
    utils::getFromNamespace("h_worst_case_tail_probability", "tern")(
      d_star = d_star,
      n1 = n1,
      n2 = n2,
      t_values = t_values,
      t0 = t0,
      tables = tables,
      tail = "lower"
    )
  }

  # Step 5: Invert one-sided tests to obtain the two-sided
  # 100 * (1 - alpha)% CI for d = p1 - p2.
  # For monotone one-sided p-value functions, use uniroot to solve
  # P_U(d) = alpha/2 and P_L(d) = alpha/2 directly.
  diff_ci <- c(
    utils::getFromNamespace("h_find_ci_bound_uniroot", "tern")(p_upper, cutoff = cutoff, direction = "increasing"),
    utils::getFromNamespace("h_find_ci_bound_uniroot", "tern")(p_lower, cutoff = cutoff, direction = "decreasing")
  )

  list(
    diff = diff_est,
    diff_ci = diff_ci
  )
}

#' @describeIn temporary_tern_code Temporarily moved from `{tern}`
h_prop_cmh_jtemp <- function(tbl, conf_level = 0.95) {
  checkmate::assert_array(tbl, mode = "integerish", any.missing = FALSE, d = 3L)
  checkmate::assert_true(nrow(tbl) == 2L)
  checkmate::assert_true(ncol(tbl) == 2L)
  checkmate::assert_true(dim(tbl)[3L] > 0L)
  checkmate::assert_true(identical(dimnames(tbl)[[2]], c("TRUE", "FALSE")))
  checkmate::assert_true(all(tbl >= 0))
  checkmate::assert_true(all(is.finite(tbl)))
  tern::assert_proportion_value(conf_level)

  strata_names <- dimnames(tbl)[[3L]] # Can be NULL.

  x1 <- setNames(tbl[1L, "TRUE", ], strata_names)
  x2 <- setNames(tbl[2L, "TRUE", ], strata_names)
  n1 <- apply(tbl[1L, , , drop = FALSE], MARGIN = 3L, sum)
  n2 <- apply(tbl[2L, , , drop = FALSE], MARGIN = 3L, sum)
  p1 <- ifelse(n1 > 0, x1 / n1, NA_real_)
  p2 <- ifelse(n2 > 0, x2 / n2, NA_real_)

  # CMH weights.
  w <- ifelse(n1 + n2 > 0, (n1 * n2) / (n1 + n2), NA_real_)
  w_sum <- sum(w, na.rm = TRUE)

  if (w_sum > 0) {
    # In addition to ensuring a non-zero denominator, w_sum > 0 ensures that
    # for at least one stratum h, w[h] is non-NA and > 0, and therefore,
    # n1[h] > 0 and n2[h] > 0.
    # Consequently, p1[h], p2[h], and w_normalized[h] are all non-NA.
    # Thus, all four sums below contain at least one non-NA element and cannot
    # yield an unjustified 0.
    # This is important to note because sum(numeric(0)) returns 0.
    w_normalized <- w / w_sum
    est1 <- sum(w_normalized * p1, na.rm = TRUE)
    est2 <- sum(w_normalized * p2, na.rm = TRUE)

    var1 <- sum(w_normalized^2 * p1 * (1 - p1) / n1, na.rm = TRUE)
    var2 <- sum(w_normalized^2 * p2 * (1 - p2) / n2, na.rm = TRUE)
    z <- stats::qnorm((1 + conf_level) / 2)
    ci1 <- est1 + c(-1, 1) * z * sqrt(var1)
    ci2 <- est2 + c(-1, 1) * z * sqrt(var2)
  } else {
    w_normalized <- setNames(rep(NA_real_, dim(tbl)[3L]), strata_names)
    est1 <- est2 <- var1 <- var2 <- NA_real_
    ci1 <- ci2 <- c(NA_real_, NA_real_)
  }

  group_names <- dimnames(tbl)[[1L]] # Can be NULL.

  list(
    x1 = x1, n1 = n1, p1 = p1, # Quantities for group 1.
    x2 = x2, n2 = n2, p2 = p2, # Quantities for group 2.
    w = w,
    w_normalized = w_normalized,
    est1 = setNames(est1, group_names[1L]),
    est2 = setNames(est2, group_names[2L]),
    est_both_groups = setNames(c(est1, est2), group_names),
    var1 = setNames(var1, group_names[1L]),
    var2 = setNames(var2, group_names[2L]),
    ci_both_groups = setNames(list(ci1, ci2), group_names)
  )
}

#' @describeIn temporary_tern_code Temporarily moved from `{tern}`
h_cmh_sato_var_jtemp <- function(prop) {
  checkmate::assert_list(prop, min.len = 7L, names = "named")
  checkmate::assert_subset(c("est1", "est2", "x1", "x2", "n1", "n2", "w"), names(prop))
  checkmate::assert_number(prop$est1, lower = -1, upper = 1, na.ok = TRUE, finite = TRUE)
  checkmate::assert_number(prop$est2, lower = -1, upper = 1, na.ok = TRUE, finite = TRUE)
  checkmate::assert_integerish(prop$x1, min.len = 1L, lower = 0, any.missing = FALSE)
  checkmate::assert_integerish(prop$x2, len = length(prop$x1), lower = 0, any.missing = FALSE)
  checkmate::assert_integerish(prop$n1, len = length(prop$x1), lower = 0, any.missing = FALSE)
  checkmate::assert_integerish(prop$n2, len = length(prop$x1), lower = 0, any.missing = FALSE)
  checkmate::assert_true(all(prop$x1 <= prop$n1))
  checkmate::assert_true(all(prop$x2 <= prop$n2))
  checkmate::assert_numeric(prop$w, len = length(prop$x1), lower = 0, finite = TRUE)

  # For easier readability of the formulas below.
  est1 <- prop$est1
  est2 <- prop$est2
  x1 <- prop$x1
  x2 <- prop$x2
  n1 <- prop$n1
  n2 <- prop$n2
  w_unnormalized <- prop$w

  n <- n1 + n2

  p_numerator <- n2^2 * x1 - n1^2 * x2 + n1 * n2 * (n1 - n2) / 2
  p <- ifelse(n > 0, p_numerator / n^2, NA_real_)

  q_numerator <- x1 * (n2 - x2) + x2 * (n1 - x1)
  q <- ifelse(n > 0, q_numerator / (2 * n), NA_real_)

  w_sum <- sum(w_unnormalized, na.rm = TRUE)
  if (any(!is.na(p)) && w_sum > 0) { # Note: any(!is.na(p)) == TRUE <=> any(!is.na(q)) == TRUE.
    num <- (est2 - est1) * sum(p, na.rm = TRUE) + sum(q, na.rm = TRUE)
    unname(num) / w_sum^2
  } else {
    NA_real_
  }
}

#' @describeIn temporary_tern_code Temporarily moved from `{tern}`
h_miettinen_nurminen_var_jtemp <- function(est1, est2, x1, x2, n1, n2) {
  checkmate::assert_number(est1, lower = -1, upper = 1, na.ok = TRUE, finite = TRUE)
  checkmate::assert_number(est2, lower = -1, upper = 1, na.ok = TRUE, finite = TRUE)
  checkmate::assert_integerish(x1, min.len = 1L, lower = 0, any.missing = FALSE)
  checkmate::assert_integerish(x2, len = length(x1), lower = 0, any.missing = FALSE)
  checkmate::assert_integerish(n1, len = length(x1), lower = 0, any.missing = FALSE)
  checkmate::assert_integerish(n2, len = length(x1), lower = 0, any.missing = FALSE)
  checkmate::assert_true(all(x1 <= n1))
  checkmate::assert_true(all(x2 <= n2))

  # nolint start
  # Translate to the notation in the paper.
  S0 <- n1
  S1 <- n2
  c0 <- x1
  c1 <- x2
  RD <- est2 - est1

  # Further definitions.
  S <- S0 + S1
  c <- c0 + c1

  # Coefficients of the third-degree polynomial.
  L3 <- S
  L2 <- (S1 + 2 * S0) * RD - S - c
  L1 <- (S0 * RD - S - 2 * c0) * RD + c
  L0 <- c0 * RD * (1 - RD)
  # nolint end

  # Solution for group 1 proportion.
  q <- L2^3 / (3 * L3)^3 - L1 * L2 / (6 * L3^2) + L0 / (2 * L3)
  p <- sign(q) * sqrt(L2^2 / (3 * L3)^2 - L1 / (3 * L3))
  a <- (1 / 3) * (base::pi + acos(q / p^3))
  p1_mle <- 2 * p * cos(a) - L2 / (3 * L3)

  # Estimated group 2 proportion.
  p2_mle <- p1_mle + RD

  # Variance estimate.
  var_est <- ifelse(
    n1 > 0 & n2 > 0 & S > 1,
    (p1_mle * (1 - p1_mle) / n1 + p2_mle * (1 - p2_mle) / n2) * S / (S - 1),
    NA_real_
  )

  list(
    p1_est = p1_mle,
    p2_est = p2_mle,
    var_est = var_est
  )
}

#' @describeIn temporary_tern_code Temporarily moved from `{tern}`
h_miettinen_nurminen_stratified_ci_jtemp <- function(prop, conf_level = 0.95) {
  checkmate::assert_list(prop, min.len = 10L, names = "named")
  checkmate::assert_subset(
    c("est1", "est2", "x1", "x2", "n1", "n2", "p1", "p2", "w", "w_normalized"),
    names(prop)
  )
  checkmate::assert_number(prop$est1, lower = -1, upper = 1, na.ok = TRUE, finite = TRUE)
  checkmate::assert_number(prop$est2, lower = -1, upper = 1, na.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(prop$p1, min.len = 1L, lower = 0, , upper = 1, finite = TRUE)
  checkmate::assert_numeric(prop$p2, len = length(prop$p1), lower = 0, upper = 1, finite = TRUE)
  checkmate::assert_numeric(prop$w, len = length(prop$p1), lower = 0, finite = TRUE)
  checkmate::assert_numeric(prop$w_normalized, len = length(prop$p1), lower = 0, finite = TRUE)
  tern::assert_proportion_value(conf_level)

  p <- prop # For easier readability of the formulas below.

  if (is.na(p$est1) || is.na(p$est2)) {
    return(list(ci = c(NA_real_, NA_real_), se = NA_real_))
  }

  # Calculate the standard error.
  var_est <- h_miettinen_nurminen_var_jtemp(
    est1 = p$est1, est2 = p$est2,
    x1 = p$x1, x2 = p$x2,
    n1 = p$n1, n2 = p$n2
  )$var_est

  w_var <- p$w_normalized^2 * var_est
  se <- if (any(!is.na(w_var))) {
    sqrt(sum(w_var, na.rm = TRUE))
  } else {
    NA_real_
  }

  # Calculate the confidence interval.

  # Stratified Miettinen-Nurminen score function.
  score_fun <- function(delta) {
    var_est <- h_miettinen_nurminen_var_jtemp(
      est1 = 0, est2 = delta,
      x1 = p$x1, x2 = p$x2,
      n1 = p$n1, n2 = p$n2
    )$var_est

    # Ensure that both the numerator and denominator are computed
    # using the same set of strata.
    non_na <- !is.na(p$w) & !is.na(p$p1) & !is.na(p$p2) & !is.na(var_est)

    denom <- sqrt(sum(p$w[non_na]^2 * var_est[non_na]))
    if (any(non_na) && denom > 0) {
      sum(p$w[non_na] * (p$p2[non_na] - p$p1[non_na] - delta)) / denom
    } else {
      NA_real_
    }
  }

  # Confidence interval consists of all values of delta for which
  # score_fun(delta) falls in the two-sided acceptance region,
  # {delta: -z <= score_fun(delta) <= z}, where z = z_{1 - alpha/2}.
  z <- stats::qnorm((1 + conf_level) / 2)
  root_lower <- function(delta) score_fun(delta) - z
  root_upper <- function(delta) score_fun(delta) + z
  ci <- c(
    uniroot_catch_na_jtemp(root_lower, interval = c(-0.99, p$est2 - p$est1)),
    uniroot_catch_na_jtemp(root_upper, interval = c(p$est2 - p$est1, 0.99))
  )

  list(ci = ci, se = se)
}

# Moved from tern prop_diff_test.R ----

#' @describeIn temporary_tern_code Temporarily moved from `{tern}`
s_test_proportion_diff_jtemp <- function(df,
                                         .var,
                                         .ref_group = NULL,
                                         .in_ref_col = NULL,
                                         variables = list(strata = NULL),
                                         method = c(
                                           "chisq", "schouten", "fisher",
                                           "cmh", "cmh_sato", "cmh_wh"
                                         ),
                                         alternative = c("two.sided", "less", "greater"),
                                         val = TRUE,
                                         ...) {
  checkmate::assert_data_frame(df)
  checkmate::assert_string(.var)
  checkmate::assert_subset(.var, colnames(df), empty.ok = FALSE)
  checkmate::assert_data_frame(.ref_group, null.ok = TRUE)
  checkmate::assert_flag(.in_ref_col, null.ok = TRUE)
  checkmate::assert_list(variables, null.ok = TRUE)
  if (!is.null(variables)) {
    checkmate::assert_set_equal(names(variables), "strata")
  }
  checkmate::assert_atomic(val)

  method <- match.arg(method)

  pval <- if (is.null(.in_ref_col) || .in_ref_col) {
    numeric()
  } else {
    checkmate::assert_false(is.null(.ref_group))
    utils::getFromNamespace("assert_stratification_compatibility", "tern")(
      method = method,
      stratified_methods = c("cmh", "cmh_sato", "cmh_wh"),
      strata_vars = variables$strata
    )

    rsp_list <- tern::h_prepare_rsp_table(
      df = df, df_ref = .ref_group, var = .var, val = val,
      strata_vars = variables$strata,
      complete_cases = TRUE
    )
    rsp_tbl <- rsp_list$tbl

    switch(method,
      cmh = prop_cmh_jtemp(rsp_tbl, alternative = alternative),
      cmh_sato = prop_cmh_jtemp(rsp_tbl, alternative = alternative, diff_se = "sato"),
      cmh_wh = prop_cmh_jtemp(rsp_tbl, alternative = alternative, transform = "wilson_hilferty"),
      fisher = tern::prop_fisher(rsp_tbl, alternative = alternative),
      chisq = tern::prop_chisq(rsp_tbl, alternative = alternative),
      schouten = tern::prop_schouten(rsp_tbl, alternative = alternative)
    )
  }

  list(
    pval = formatters::with_label(
      pval,
      tern::d_test_proportion_diff(method, alternative = alternative)
    )
  )
}

#' @describeIn temporary_tern_code Temporarily moved from `{tern}`
prop_cmh_jtemp <- function(ary,
                           alternative = c("two.sided", "less", "greater"),
                           diff_se = c("standard", "sato"),
                           transform = c("none", "wilson_hilferty")) {
  checkmate::assert_array(ary)
  checkmate::assert_integer(c(ncol(ary), nrow(ary)), lower = 2, upper = 2)
  checkmate::assert_integer(length(dim(ary)), lower = 3, upper = 3)
  alternative <- match.arg(alternative)
  diff_se <- match.arg(diff_se)
  transform <- match.arg(transform)

  strata_sizes <- apply(ary, MARGIN = 3, sum)
  if (any(strata_sizes < 5)) {
    warning("<5 data points in some strata. CMH test may be incorrect.")
    ary <- ary[, , strata_sizes > 1]
  }

  z_stat <- if (diff_se == "standard") {
    mh_res <- stats::mantelhaen.test(ary, correct = FALSE, alternative = alternative)
    checkmate::assert_true(mh_res$parameter == 1)
    # Note: The odds ratio (OR) estimate from `mantelhaen.test` and the proportion difference
    # always agree in direction, therefore we can use the OR estimate to determine the sign here.
    stat_sign <- ifelse(unname(mh_res$estimate) < 1, 1, -1)
    sqrt(unname(mh_res$statistic)) * stat_sign
  } else {
    # Use the Sato variance estimator.
    prop <- h_prop_cmh_jtemp(ary)
    prop_diff_var <- h_cmh_sato_var_jtemp(prop)
    unname(prop$est2 - prop$est1) / sqrt(prop_diff_var)
  }

  if (transform == "wilson_hilferty") {
    if (diff_se == "sato") {
      warning(
        "Wilson-Hilferty transformation was not designed ",
        "for use with the Sato variance estimator"
      )
    }
    chisq_stat <- z_stat^2
    df <- 1 # Because we only compare two groups.
    num <- (1 - 2 / (9 * df)) - (chisq_stat / df)^(1 / 3)
    denom <- sqrt(2 / (9 * df))
    z_stat <- num / denom * sign(z_stat) # Preserve the direction of effect.
  }

  pval <- if (alternative == "two.sided") {
    2 * stats::pnorm(-abs(z_stat))
  } else {
    stats::pnorm(z_stat, lower.tail = (alternative == "greater"))
  }

  structure(
    pval,
    z_stat = z_stat
  )
}

# Moved from tern utils.R ----

uniroot_catch_na_jtemp <- function(...) {
  tryCatch(
    stats::uniroot(...)$root,
    error = function(e) {
      if (grepl("is NA", conditionMessage(e), fixed = TRUE)) {
        NA_real_
      } else {
        stop(e)
      }
    }
  )
}
