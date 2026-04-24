#' Helpers for Processing Least Square Means
#'
#' @inheritParams fit_mmrm_j
#' @param fit result of model fitting function, e.g. [mmrm::mmrm()] or [stats::lm()].
#' @param averages (`list`)\cr optional named list of visit levels which should be averaged
#'   and reported along side the single visits.
#' @param weights (`string`)\cr argument from [emmeans::emmeans()], 'counterfactual' by default.
#' @param specs (`list`)\cr list of least square means specifications, with
#'   elements `coefs` (coefficient list) and `grid` (corresponding `data.frame`).
#' @param emmeans_res (`list`)\cr initial `emmeans` result from [h_get_emmeans_res()].
#'
#' @name lsmeans_helpers
#' @keywords internal
NULL

#' @describeIn lsmeans_helpers partial matching of a set of `options` to a
#' character vector `x`. Here partial matching means that for each element of `x` we look for the
#' (first) element of `options` which is contained in the element of `x`. Then we return
#' the matched `options` in the same order as `x`.
#' @param options (`character`)\cr vector of options to match against `x`.
#' @param x (`character`)\cr vector of strings to be partially matched with `options`
h_partial_match <- function(options, x) {
  checkmate::assert_character(options)
  checkmate::assert_character(x)

  result <- sapply(x, function(xi) {
    matches <- vapply(
      options,
      function(opt) grepl(opt, xi, fixed = TRUE),
      logical(1)
    )
    options[which(matches)[1L]]
  }, USE.NAMES = FALSE)
  if (anyNA(result)) {
    warning(
      "Some elements of 'x' did not match any of the 'options': ",
      paste(x[is.na(result)], collapse = ", ")
    )
  }
  result
}

#' @describeIn lsmeans_helpers returns a list with
#'   `object` (`emmGrid` object containing `emmeans` results) and `grid`
#'   (`data.frame` containing the (optional) subgroup, arm and the visit variables
#'   together with the sample size `n` for each combination).
h_get_emmeans_res <- function(fit, vars, weights) {
  data_complete <- stats::model.frame(fit)
  checkmate::assert_data_frame(data_complete)
  checkmate::assert_list(vars)

  if (!is.null(vars$subgroup)) {
    checkmate::assert_factor(data_complete[[vars$subgroup]])
  }
  emmeans_object <- emmeans::emmeans(
    fit,
    data = data_complete,
    specs = c(vars$subgroup, vars$visit, vars$arm),
    weights = weights
  )

  # Save grid with renamed number of subjects column.
  visit_arm_grid <- emmeans_object@grid
  wgt_index <- match(".wgt.", names(visit_arm_grid))
  names(visit_arm_grid)[wgt_index] <- "n"
  visit_arm_grid$n <- as.integer(visit_arm_grid$n)

  list(object = emmeans_object, grid = visit_arm_grid)
}

#' @describeIn lsmeans_helpers constructs average of visits specifications.
h_get_average_visit_specs <- function(emmeans_res, vars, averages, fit) {
  visit_grid <- emmeans_res$grid[[vars$visit]]
  model_frame <- stats::model.frame(fit)
  averages_list <- list()
  visit_vec <- n_vec <- c()
  if (!is.null(vars$subgroup)) {
    subgroup_grid <- emmeans_res$grid[[vars$subgroup]]
    subgroup_vec <- c()
  }
  if (!is.null(vars$arm)) {
    arm_grid <- emmeans_res$grid[[vars$arm]]
    arm_vec <- c()
  }
  for (i in seq_along(averages)) {
    average_label <- names(averages)[i]
    visits_average <- averages[[i]]
    checkmate::assert_subset(visits_average, choices = levels(visit_grid))
    which_visits_in_average <- visit_grid %in% visits_average
    average_coefs <- as.integer(which_visits_in_average) / length(visits_average)
    zero_coefs <- numeric(length = length(average_coefs))

    if (is.null(vars$arm) && is.null(vars$subgroup)) {
      averages_list[[average_label]] <- average_coefs
      visit_vec <- c(visit_vec, average_label)
      is_in_subset <- (model_frame[[vars$visit]] %in% visits_average)
      this_n <- length(unique(model_frame[is_in_subset, vars$id]))
      n_vec <- c(n_vec, this_n)
    } else if (is.null(vars$arm) && !is.null(vars$subgroup)) {
      for (this_subgroup in levels(subgroup_grid)) {
        this_coefs <- zero_coefs
        subgroup_average_label <- paste(this_subgroup, average_label, sep = ".")
        which_subgroup <- subgroup_grid == this_subgroup
        which_visits_and_subgroup <- which_visits_in_average & which_subgroup
        this_coefs[which_visits_and_subgroup] <- 1 / sum(which_visits_and_subgroup)
        averages_list[[subgroup_average_label]] <- this_coefs
        subgroup_vec <- c(subgroup_vec, this_subgroup)
        visit_vec <- c(visit_vec, average_label)
        is_in_subset <- (model_frame[[vars$subgroup]] == this_subgroup) &
          (model_frame[[vars$visit]] %in% visits_average)
        this_n <- length(unique(model_frame[is_in_subset, vars$id]))
        n_vec <- c(n_vec, this_n)
      }
    } else if (!is.null(vars$arm) && is.null(vars$subgroup)) {
      for (this_arm in levels(arm_grid)) {
        this_coefs <- zero_coefs
        arm_average_label <- paste(this_arm, average_label, sep = ".")
        which_arm <- arm_grid == this_arm
        this_coefs[which_arm] <- average_coefs[which_arm]
        averages_list[[arm_average_label]] <- this_coefs
        arm_vec <- c(arm_vec, this_arm)
        visit_vec <- c(visit_vec, average_label)
        is_in_subset <- (model_frame[[vars$arm]] == this_arm) & (model_frame[[vars$visit]] %in% visits_average)
        this_n <- length(unique(model_frame[is_in_subset, vars$id]))
        n_vec <- c(n_vec, this_n)
      }
    } else {
      for (this_arm in levels(arm_grid)) {
        for (this_subgroup in levels(subgroup_grid)) {
          this_coefs <- zero_coefs
          arm_subgroup_average_label <- paste(this_subgroup, this_arm, average_label, sep = ".")
          which_arm <- arm_grid == this_arm
          which_subgroup <- subgroup_grid == this_subgroup
          which_visits_and_arm_and_subgroup <- which_visits_in_average & which_arm & which_subgroup
          this_coefs[which_visits_and_arm_and_subgroup] <- 1 / sum(which_visits_and_arm_and_subgroup)
          averages_list[[arm_subgroup_average_label]] <- this_coefs
          arm_vec <- c(arm_vec, this_arm)
          subgroup_vec <- c(subgroup_vec, this_subgroup)
          visit_vec <- c(visit_vec, average_label)
          is_in_subset <- (model_frame[[vars$arm]] == this_arm) &
            (model_frame[[vars$subgroup]] == this_subgroup) &
            (model_frame[[vars$visit]] %in% visits_average)
          this_n <- length(unique(model_frame[is_in_subset, vars$id]))
          n_vec <- c(n_vec, this_n)
        }
      }
    }
  }
  if (is.null(vars$arm) && is.null(vars$subgroup)) {
    averages_grid <- data.frame(visit = visit_vec, n = n_vec)
    names(averages_grid) <- c(vars$visit, "n")
  } else if (!is.null(vars$arm) && is.null(vars$subgroup)) {
    averages_grid <- data.frame(arm = arm_vec, visit = visit_vec, n = n_vec)
    names(averages_grid) <- c(vars$arm, vars$visit, "n")
  } else if (is.null(vars$arm) && !is.null(vars$subgroup)) {
    averages_grid <- data.frame(subgroup = subgroup_vec, visit = visit_vec, n = n_vec)
    names(averages_grid) <- c(vars$subgroup, vars$visit, "n")
  } else {
    averages_grid <- data.frame(subgroup = subgroup_vec, arm = arm_vec, visit = visit_vec, n = n_vec)
    names(averages_grid) <- c(vars$subgroup, vars$arm, vars$visit, "n")
  }
  list(coefs = averages_list, grid = averages_grid)
}

#' @describeIn lsmeans_helpers estimates least square means as a `data.frame`
#'   given specifications.
#'
#' @note The difference here compared to the original tern.mmrm::h_get_spec_visit_estimates()
#'   function is that additional arguments for [emmeans::contrast()] can be passed via the
#'   dots (`...`) argument.
#'   Once this has been added to the `tern.mmrm` package then its functions can be used instead.
#'
#' @param tests (`flag`)\cr whether to add test results to the estimates.
#' @param ... additional arguments for [emmeans::contrast()].
h_get_spec_visit_estimates <- function(emmeans_res, specs, conf_level, tests = FALSE, ...) {
  checkmate::assert_list(emmeans_res)
  checkmate::assert_list(specs)
  checkmate::assert_number(conf_level)
  checkmate::assert_flag(tests)

  conts <- emmeans::contrast(emmeans_res$object, specs$coefs, ...)
  cis <- stats::confint(conts, level = conf_level)
  res <- cbind(
    specs$grid,
    data.frame(estimate = cis$estimate, se = cis$SE, df = cis$df, lower_cl = cis$lower.CL, upper_cl = cis$upper.CL)
  )
  if (tests) {
    conts_df <- as.data.frame(conts)
    res$t_stat <- conts_df$t.ratio
    res$p_value <- conts_df$p.value
    res$p_value_less <- stats::pt(conts_df$t.ratio, df = conts_df$df, lower.tail = TRUE)
    res$p_value_greater <- stats::pt(conts_df$t.ratio, df = conts_df$df, lower.tail = FALSE)
  }
  res
}

#' @describeIn lsmeans_helpers estimates least square means for single visits.
h_get_single_visit_estimates <- function(emmeans_res, conf_level) {
  checkmate::assert_list(emmeans_res)
  checkmate::assert_number(conf_level)

  cis <- stats::confint(emmeans_res$object, level = conf_level)
  cbind(
    emmeans_res$grid[, setdiff(names(emmeans_res$grid), "n"), drop = FALSE],
    data.frame(estimate = cis$emmean, se = cis$SE, df = cis$df, lower_cl = cis$lower.CL, upper_cl = cis$upper.CL),
    emmeans_res$grid[, "n", drop = FALSE]
  )
}

#' @describeIn lsmeans_helpers constructs `data.frame` with
#'   relative reduction vs. reference arm based on single visit estimates.
#' @param estimates (`data.frame`)\cr single visit least square mean estimates.
h_get_relative_reduc_df <- function(estimates, vars) {
  checkmate::assert_data_frame(estimates)
  checkmate::assert_list(vars)

  ref_arm_level <- estimates[[vars$arm]][1L]
  ref_estimates <- estimates[estimates[[vars$arm]] == ref_arm_level, c(vars$subgroup, vars$visit, "estimate")]
  names(ref_estimates)[names(ref_estimates) == "estimate"] <- "ref"
  result <- merge(
    estimates[estimates[[vars$arm]] != ref_arm_level, ],
    ref_estimates,
    by = c(vars$subgroup, vars$visit),
    sort = FALSE
  )
  result$relative_reduc <- (result$ref - result$estimate) / result$ref
  result[, c(vars$subgroup, vars$visit, vars$arm, "relative_reduc")]
}

#' @describeIn lsmeans_helpers constructs single visit contrast specifications.
h_single_visit_contrast_specs <- function(emmeans_res, vars) {
  checkmate::assert_list(emmeans_res)
  checkmate::assert_list(vars)

  emmeans_res$grid$index <- seq_len(nrow(emmeans_res$grid))

  grid_by_subgroup_visit <- split(emmeans_res$grid, emmeans_res$grid[c(vars$subgroup, vars$visit)])

  arm_levels <- emmeans_res$object@levels[[vars$arm]]
  visit_levels <- emmeans_res$object@levels[[vars$visit]]
  if (!is.null(vars$subgroup)) {
    subgroup_levels <- emmeans_res$object@levels[[vars$subgroup]]
  }
  ref_arm_level <- arm_levels[1L]
  zeros_coefs <- numeric(nrow(emmeans_res$grid))
  overall_list <- list()
  arm_vec <- visit_vec <- subgroup_vec <- c()
  for (j in seq_along(grid_by_subgroup_visit)) {
    this_grid <- grid_by_subgroup_visit[[j]]
    ref_index <- which(this_grid[[vars$arm]] == ref_arm_level)
    this_visit <- as.character(unique(this_grid[[vars$visit]]))
    checkmate::assert_true(length(this_visit) == 1L)
    if (!is.null(vars$subgroup)) {
      this_subgroup <- as.character(unique(this_grid[[vars$subgroup]]))
      checkmate::assert_true(length(this_subgroup) == 1L)
    }
    this_ref_coefs <- zeros_coefs
    this_ref_coefs[this_grid$index[ref_index]] <- -1
    this_list <- list()
    for (i in seq_len(nrow(this_grid))[-ref_index]) {
      this_coefs <- this_ref_coefs
      this_coefs[this_grid$index[i]] <- 1
      this_arm <- as.character(this_grid[[vars$arm]][i])
      arm_vec <- c(arm_vec, this_arm)
      visit_vec <- c(visit_vec, this_visit)
      if (!is.null(vars$subgroup)) {
        subgroup_vec <- c(subgroup_vec, this_subgroup)
      }
      this_label <- paste(this_arm, this_visit, sep = ".")
      if (!is.null(vars$subgroup)) {
        this_label <- paste(this_subgroup, this_label, sep = ".")
      }
      this_list[[this_label]] <- this_coefs
    }
    overall_list <- c(overall_list, this_list)
  }

  grid <- data.frame(arm = arm_vec, visit = visit_vec)
  if (!is.null(vars$subgroup)) {
    grid[[vars$subgroup]] <- subgroup_vec
  }
  names(grid) <- c(vars$arm, vars$visit, vars$subgroup)
  list(coefs = overall_list, grid = grid)
}

#' @describeIn lsmeans_helpers constructs average visits contrast specifications,
#'   given the `specs` for single visit contrasts and the averages required.
h_average_visit_contrast_specs <- function(specs, averages, vars) {
  subgroup_arm_visit_grid <- specs$grid
  arm_levels <- unique(subgroup_arm_visit_grid[[vars$arm]])
  if (!is.null(vars$subgroup)) {
    subgroup_levels <- unique(subgroup_arm_visit_grid[[vars$subgroup]])
  }
  subgroup_arm_visit_grid$index <- seq_len(nrow(subgroup_arm_visit_grid))
  grid_by_arm_subgroup <- split(subgroup_arm_visit_grid, subgroup_arm_visit_grid[c(vars$arm, vars$subgroup)])
  overall_list <- list()
  subgroup_vec <- arm_vec <- visit_vec <- c()
  for (j in seq_along(grid_by_arm_subgroup)) {
    this_grid <- grid_by_arm_subgroup[[j]]
    this_arm <- as.character(unique(this_grid[[vars$arm]]))
    checkmate::assert_true(length(this_arm) == 1L)
    if (!is.null(vars$subgroup)) {
      this_subgroup <- as.character(unique(this_grid[[vars$subgroup]]))
      checkmate::assert_true(length(this_subgroup) == 1L)
    }
    for (i in seq_along(averages)) {
      average_label <- names(averages)[i]
      visits_average <- averages[[i]]
      which_visits_in_average <- this_grid[, vars$visit] %in% visits_average
      averaged_indices <- this_grid$index[which_visits_in_average]
      this_comb <- paste(this_arm, average_label, sep = ".")
      if (!is.null(vars$subgroup)) {
        this_comb <- paste(this_subgroup, this_comb, sep = ".")
      }
      averaged_coefs <- colMeans(do.call(rbind, specs$coefs[averaged_indices]))
      overall_list[[this_comb]] <- averaged_coefs
      arm_vec <- c(arm_vec, this_arm)
      if (!is.null(vars$subgroup)) {
        subgroup_vec <- c(subgroup_vec, this_subgroup)
      }
      visit_vec <- c(visit_vec, average_label)
    }
  }

  grid <- data.frame(arm = arm_vec, visit = visit_vec)
  if (!is.null(vars$subgroup)) {
    grid[[vars$subgroup]] <- subgroup_vec
  }
  names(grid) <- c(vars$arm, vars$visit, vars$subgroup)

  list(coefs = overall_list, grid = grid)
}

#' @describeIn lsmeans_helpers Computes multiplicity-adjusted p-values and
#'   confidence intervals for treatment-vs-reference contrasts using `multcomp`.
#'   Returns a `data.frame` with the arm and visit grid columns plus
#'   `p_value`, `p_value_less`, `p_value_greater`, `lower_cl`, and `upper_cl`.
#'
#' @param contrast_df (`numeric`)
#'   Degrees of freedom from the unadjusted contrast estimates, used to derive
#'   a single df for [emmeans::as.glht()].
h_get_mult_adj_estimates <- function(emmeans_res, vars, mult_adj, conf_level, contrast_df) {
  checkmate::assert_list(emmeans_res)
  checkmate::assert_list(vars)
  checkmate::assert_string(mult_adj)
  checkmate::assert_number(conf_level)
  checkmate::assert_numeric(contrast_df)

  emmeans_conts <- emmeans::contrast(
    emmeans_res$object,
    method = "trt.vs.ctrl",
    by = c(vars$subgroup, vars$visit),
    adjust = "none" # To avoid a note below. Does not matter.
  )
  if (!requireNamespace("multcomp", quietly = TRUE)) {
    stop("please install multcomp for multiplicity adjustment")
  }
  glht_type <- switch(mult_adj,
    dunnett = "single-step",
    `step-down-dunnett` = "free"
  )
  glht_test <- multcomp::adjusted(type = glht_type)
  # We need a single df for the as.glht below. We give it explicitly to avoid a message.
  glht_df <- floor(mean(contrast_df, na.rm = TRUE))

  glht_conts_greater <- emmeans::as.glht(emmeans_conts, df = glht_df, alternative = "greater")
  glht_summary_greater <- summary(glht_conts_greater, test = glht_test)
  pvals_greater <- do.call(c, lapply(glht_summary_greater, function(x) x$test$pvalues))

  glht_conts_lower <- emmeans::as.glht(emmeans_conts, df = glht_df, alternative = "less")
  glht_summary_lower <- summary(glht_conts_lower, test = glht_test)
  pvals_lower <- do.call(c, lapply(glht_summary_lower, function(x) x$test$pvalues))

  glht_conts <- emmeans::as.glht(emmeans_conts, df = glht_df, alternative = "two.sided")
  glht_summary <- summary(glht_conts, test = glht_test)
  pvals <- do.call(c, lapply(glht_summary, function(x) x$test$pvalues))

  glht_ci <- stats::confint(glht_conts, level = conf_level)
  glht_ci_matrix <- do.call(rbind, lapply(glht_ci, function(x) x$confint[, c("lwr", "upr")]))

  arm_levels <- emmeans_res$object@levels[[vars$arm]]
  non_ref_arms <- arm_levels[-1L]

  grid <- emmeans_conts@grid
  arm_vec <- h_partial_match(options = non_ref_arms, x = as.character(grid$contrast))
  grid <- cbind(
    arm = arm_vec,
    grid[c(vars$subgroup, vars$visit)]
  )
  names(grid) <- c(vars$arm, vars$subgroup, vars$visit)

  data.frame(
    grid,
    p_value = pvals,
    p_value_less = pvals_lower,
    p_value_greater = pvals_greater,
    lower_cl = glht_ci_matrix[, "lwr"],
    upper_cl = glht_ci_matrix[, "upr"],
    row.names = NULL
  )
}
