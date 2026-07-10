pvalcat <- list(
  "<0.001" = c(0, 0.001),
  "0.001 to <0.05" = c(0.001, 0.05),
  ">=0.05" = c(0.05, 1)
)

# Convert the named list into a 2-col matrix + ordered category names.
normalize_pvalcat <- function(pvalcat) {
  bounds <- matrix(unlist(pvalcat), ncol = 2, byrow = TRUE)
  cats <- names(pvalcat)
  list(bounds = bounds, cats = cats)
}

# Categorize a p-value based on the pvalcat parameter.
categorize_pval_from_param <- function(p, pvalcat) {
  info <- normalize_pvalcat(pvalcat)
  bounds <- info$bounds
  cats <- info$cats
  last_row <- nrow(bounds)

  vapply(
    p,
    function(x) {
      if (is.na(x)) {
        return(NA_character_)
      }
      # low inclusive for all; high exclusive except last category (inclusive)
      idx <- which(bounds[, 1] <= x & (x < bounds[, 2] | (seq_len(last_row) == last_row & x <= bounds[, 2])))
      if (length(idx) == 0) {
        return(NA_character_)
      }
      cats[idx[1]]
    },
    FUN.VALUE = character(1)
  )
}

# One function to impute and analyze for n_imputations times.
# This will be more efficient because we only need to work with the data frame in the beginning.
impute_and_analyze <- function(dat, p_ctrl, p_trt, trtvar, ctrlab, trtlab, respvar, stratvar, n_imputations) {
  # Parse from data frame.
  is_trt <- dat[[trtvar]] == trtlab
  is_ctrl <- dat[[trtvar]] == ctrlab
  df_trt <- dat[is_trt, ]
  df_ctrl <- dat[is_ctrl, ]
  strata <- c(interaction(df_ctrl[stratvar]), interaction(df_trt[stratvar]))
  strata <- as.factor(strata)
  rsp <- c(df_ctrl[[respvar]], df_trt[[respvar]])
  grp <- factor(
    rep(c("ref", "Not-ref"), c(nrow(df_ctrl), nrow(df_trt))),
    levels = c("ref", "Not-ref")
  )

  # Determine which responses are missing.
  is_missing <- is.na(rsp)
  missing_trt_rsp <- (grp == "Not-ref") & is_missing
  missing_ctrl_rsp <- (grp == "ref") & is_missing
  n_missing_trt_rsp <- sum(missing_trt_rsp)
  n_missing_ctrl_rsp <- sum(missing_ctrl_rsp)

  # Initialize containers for results.
  rd_est <- rd_se <- p_cmh <- z_stat <- numeric(n_imputations)

  # Imputation loop.
  for (i in seq_len(n_imputations)) {
    if (n_missing_trt_rsp > 0) {
      rsp[missing_trt_rsp] <- as.logical(rbinom(n = n_missing_trt_rsp, size = 1, prob = p_trt))
    }
    if (n_missing_ctrl_rsp > 0) {
      rsp[missing_ctrl_rsp] <- as.logical(rbinom(n = n_missing_ctrl_rsp, size = 1, prob = p_ctrl))
    }
    rd_res <- prop_diff_cmh(rsp, grp, strata, diff_se = "standard")
    tbl <- table(grp, rsp, strata)
    test_res <- prop_cmh(tbl, transform = "wilson_hilferty")
    rd_est[i] <- rd_res$diff
    rd_se[i] <- rd_res$se_diff
    p_cmh <- as.numeric(test_res)
    z_stat <- attr(test_res, "z_stat")
  }

  tibble(
    rd_est = rd_est,
    rd_se = rd_se,
    rd_var = rd_se^2,
    p_cmh = p_cmh,
    z_stat = z_stat
  )
}


pool_rubin_scalar <- function(q, u) {
  # q: vector of estimates
  # u: vector of within-imputation variances
  m <- length(q)
  qbar <- mean(q, na.rm = TRUE)
  ubar <- mean(u, na.rm = TRUE)
  b <- stats::var(q, na.rm = TRUE)
  tvar <- ubar + (1 + 1 / m) * b
  se <- sqrt(tvar)
  list(est = qbar, se = se, var = tvar, m = m)
}

pool_wh_pvalue <- function(z_vals) {
  m <- length(z_vals)
  qbar <- mean(z_vals, na.rm = TRUE)
  b <- stats::var(z_vals, na.rm = TRUE)
  ubar <- 1 # approx var(z) ~ 1 after WH transform
  tvar <- ubar + (1 + 1 / m) * b
  z_pool <- qbar / sqrt(tvar)
  p_pool <- 2 * stats::pnorm(abs(z_pool), lower.tail = FALSE)
  list(z = z_pool, p = p_pool)
}


# Main scenario loop

tic()
set.seed(54321)
scenario_results <- purrr::pmap_dfr(
  list(respprob_grid_2d[[ctrlab]], respprob_grid_2d[[trtlab]]),
  function(p_ctrl, p_trt) {
    m <- if (is_corner(p_ctrl, p_trt)) 1 else n_samples

    per_imp <- impute_and_analyze(
      ana,
      p_ctrl = p_ctrl,
      p_trt = p_trt,
      trtvar = trtvar,
      ctrlab = ctrlab,
      trtlab = trtlab,
      respvar = "response",
      stratvar = stratvar,
      n_imputations = m
    )

    # Point estimate pooling
    if (m == 1) {
      est <- per_imp$rd_est[1]
      se <- per_imp$rd_se[1]
      p_final <- per_imp$p_cmh[1]
      used_fallback <- TRUE
    } else {
      pooled_rd <- pool_rubin_scalar(per_imp$rd_est, per_imp$rd_var)
      est <- pooled_rd$est
      se <- pooled_rd$se

      # Pool p-value evidence via WH; fallback to first single-analysis p-value if missing
      pooled_p <- pool_wh_pvalue(per_imp$z_stat)$p
      if (is.na(pooled_p) || !is.finite(pooled_p)) {
        p_final <- per_imp$p_cmh[1]
        used_fallback <- TRUE
      } else {
        p_final <- pooled_p
        used_fallback <- FALSE
      }
    }

    tibble::tibble(
      p_ctrl = p_ctrl,
      p_trt = p_trt,
      m = m,
      rd = est, # proportion scale
      rd_se = se, # proportion scale
      p_value = p_final,
      p_cat = categorize_pval(p_final),
      marker_flag = ifelse(m == 1 || used_fallback, "*", ""),
      effect_label = paste0(
        formatC(100 * est, format = "f", digits = 1),
        "%",
        ifelse(m == 1 || used_fallback, "*", "")
      )
    )
  }
)
toc()
