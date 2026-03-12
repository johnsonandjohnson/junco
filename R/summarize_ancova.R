#' @note This has been forked from [tern::h_ancova()], because the new
#'   `weights_emmeans` option was added here.
h_ancova <- function(
    .var,
    .df_row,
    variables,
    weights_emmeans,
    interaction_item = NULL) {
  checkmate::assert_string(.var)
  checkmate::assert_list(variables)
  checkmate::assert_subset(names(variables), c("arm", "covariates"))

  assert_df_with_variables(
    .df_row,
    list(rsp = .var)
  )
  arm <- variables$arm
  covariates <- variables$covariates
  if (!is.null(covariates) && length(covariates) > 0) {
    var_list <- get_covariates(covariates)
    assert_df_with_variables(
      .df_row,
      var_list
    )
  }
  covariates_part <- paste(covariates, collapse = " + ")
  formula_str <- paste0(.var, " ~ ", arm)
  if (covariates_part != "") {
    formula_str <- paste0(formula_str, "+", covariates_part)
  }
  formula <- stats::as.formula(formula_str)
  specs <- arm
  if (!is.null(interaction_item)) {
    specs <- c(specs, interaction_item)
  }
  lm_fit <- stats::lm(formula = formula, data = .df_row)
  emmeans::emmeans(
    lm_fit,
    specs = specs,
    data = .df_row,
    weights = weights_emmeans
  )
}

h_ancova_est_single <- function(emmeans_fit,
                                sum_level,
                                interaction_y,
                                interaction_item,
                                df,
                                .var,
                                .df_row,
                                .ref_group,
                                arm,
                                conf_level,
                                .in_ref_col) {
  # Ensure that there is only one element in sum_level.
  checkmate::assert_scalar(sum_level)

  sum_fit <- summary(
    emmeans_fit,
    level = conf_level
  )

  sum_fit_level <- sum_fit[sum_fit[[arm]] == sum_level, ]

  # Get the index of the ref arm
  if (interaction_y != FALSE) {
    y <- unlist(df[(df[[interaction_item]] == interaction_y), .var])
    # convert characters selected in interaction_y into the numeric order
    interaction_y <- which(sum_fit_level[[interaction_item]] == interaction_y)
    sum_fit_level <- sum_fit_level[interaction_y, ]
    # if interaction is called, reset the index
    ref_key <- seq(sum_fit[[arm]][unique(.ref_group[[arm]])])
    ref_key <- utils::tail(ref_key, n = 1)
    ref_key <- (interaction_y - 1) * length(unique(.df_row[[arm]])) + ref_key
  } else {
    y <- df[[.var]]
    # Get the index of the ref arm when interaction is not called
    ref_key <- seq(sum_fit[[arm]][unique(.ref_group[[arm]])])
    ref_key <- utils::tail(ref_key, n = 1)
  }

  if (.in_ref_col) {
    sum_contrasts_level <- list()
    sum_contrasts_level[["estimate"]] <- numeric()
    sum_contrasts_level[["lower.CL"]] <- numeric()
    sum_contrasts_level[["upper.CL"]] <- numeric()
    sum_contrasts_level[["p.value"]] <- numeric()
  } else {
    if (!is.null(.ref_group)) {
      # Estimate the differences between the marginal means.
      emmeans_contrasts <- emmeans::contrast(
        emmeans_fit,
        # Compare all arms versus the control arm.
        method = "trt.vs.ctrl",
        # Take the arm factor from .ref_group as the control arm.
        ref = ref_key,
        level = conf_level
      )
      sum_contrasts <- summary(
        emmeans_contrasts,
        # Derive confidence intervals, t-tests and p-values.
        infer = TRUE,
        # Do not adjust the p-values for multiplicity.
        adjust = "none"
      )

      contrast_lvls <- gsub(
        "^\\(|\\)$", "", gsub(paste0(" - \\(*", .ref_group[[arm]][1], ".*"), "", sum_contrasts$contrast)
      )
      if (!is.null(interaction_item)) {
        sum_contrasts_level <- sum_contrasts[
          grepl(sum_level, contrast_lvls, fixed = TRUE), ,
          drop = FALSE
        ]
      } else {
        sum_contrasts_level <- sum_contrasts[
          (sum_level == contrast_lvls | paste0("(", sum_level, ")") == contrast_lvls), ,
          drop = FALSE
        ]
      }
      if (interaction_y != FALSE) {
        sum_contrasts_level <- sum_contrasts_level[interaction_y, , drop = FALSE]
      }
    } else {
      sum_contrasts_level <- list()
      sum_contrasts_level[["estimate"]] <- NA
      sum_contrasts_level[["lower.CL"]] <- NA
      sum_contrasts_level[["upper.CL"]] <- NA
      sum_contrasts_level[["p.value"]] <- NA
    }
  }
  ret <- list(
    sum_fit_level = sum_fit_level,
    sum_contrasts_level = sum_contrasts_level,
    y = y
  )
  ret
}

make_contrast_weights <- function(weight_mode = c("equal", "proportional"),
                                  normalize = TRUE,
                                  sum_fit,
                                  sum_levels,
                                  arm,
                                  data) {
  weight_mode <- match.arg(weight_mode)
  trt_levels <- levels(sum_fit[[arm]])
  # weights for contrast contains all trt_levels from sum_fit, not just the sum_levels
  # zero for all levels
  w_all <- setNames(numeric(length(trt_levels)), trt_levels)

  if (weight_mode == "equal") {
    w <- rep(1, length(sum_levels))
    names(w) <- sum_levels
  } else if (weight_mode == "proportional") {
    w <- table(data[[arm]])[sum_levels]
  }

  if (normalize) {
    w <- w / sum(w)
  }

  # update the weights for sum_levels with the above values
  w_all[names(w)] <- w[names(w)]
  w_all
}

h_ancova_est_combined <- function(emmeans_fit,
                                  sum_level,
                                  interaction_y,
                                  interaction_item,
                                  df,
                                  .var,
                                  .df_row,
                                  .ref_group,
                                  arm,
                                  conf_level,
                                  .in_ref_col,
                                  weights_combo) {
  if (.in_ref_col) {
    stop("not intended for usage in reference column")
  }
  if (is.null(weights_combo)) {
    stop("In a combined column specifications for weights_combo must be non-NULL.")
  } else {
    weights_combo <- match.arg(weights_combo,
      choices = c("equal", "proportional", "proportional_marginal"),
      several.ok = FALSE
    )
  }
  sum_fit <- summary(
    emmeans_fit,
    level = conf_level
  )
  ref_name <- unique(.ref_group[[arm]])

  # weights for contrasts, proportional/proportianal_marginal or equal
  # proportional_marginal is only relevant for an interaction term between arm and interaction_item
  # when no interaction proportional_marginal and proportional yield the same weights
  # when interaction :
  # weights with proportional will be within the given level of the interaction_item (ie interaction_y)
  # weights with proportional_marginal will across all levels of the interaction_item
  if (weights_combo == "proportional" && (interaction_y != FALSE)) {
    data_weights <- df[!is.na(.var), ]
    data_weights <- data_weights[(data_weights[[interaction_item]] == interaction_y), ]
  } else if (weights_combo %in% c("proportional", "proportional_marginal")) {
    data_weights <- df[!is.na(.var), ]
    weights_combo <- "proportional"
  } else {
    data_weights <- NULL
  }

  w <- make_contrast_weights(
    weight_mode = weights_combo,
    normalize = TRUE,
    sum_fit = sum_fit,
    sum_levels = sum_level,
    arm = arm,
    data = data_weights
  )
  w2 <- w
  w2[ref_name] <- -1

  if (interaction_y == FALSE) {
    y <- df[[.var]]
    w_start <- as.data.frame(emmeans_fit)
    w_start[["weights1"]] <- w
    w_start[["weights2"]] <- w2
  } else {
    y <- unlist(df[(df[[interaction_item]] == interaction_y), .var])

    w_start <- as.data.frame(emmeans_fit)
    w_start[["weights1"]] <- 0
    w_start[["weights2"]] <- 0
    w_start[["weights1"]][w_start[[interaction_item]] == interaction_y] <- w
    w_start[["weights2"]][w_start[[interaction_item]] == interaction_y] <- w2
  }

  contr <- emmeans::contrast(emmeans_fit, list(
    setNames(list(w_start[["weights1"]]), "Estimate contrast"),
    setNames(list(w_start[["weights2"]]), "Estimate contrast vs ref")
  ),
  # Derive confidence intervals, t-tests and p-values.
  infer = TRUE,
  # Do not adjust the p-values for multiplicity.
  adjust = "none"
  )
  contr_df <- as.data.frame(contr)

  contr_df[["emmeans"]] <- contr_df[["estimate"]]
  sum_fit_level <- contr_df[contr_df$contrast == "Estimate contrast", , drop = FALSE]
  sum_contrasts_level <- contr_df[contr_df$contrast == "Estimate contrast vs ref", , drop = FALSE]

  if (length(ref_name) == 0) {
    sum_contrasts_level <- list()
    sum_contrasts_level[["estimate"]] <- numeric()
    sum_contrasts_level[["lower.CL"]] <- numeric()
    sum_contrasts_level[["upper.CL"]] <- numeric()
    sum_contrasts_level[["p.value"]] <- numeric()
  }

  ret <- list(
    sum_fit_level = sum_fit_level,
    sum_contrasts_level = sum_contrasts_level,
    y = y
  )
  ret
}

#' @title Junco Extended ANCOVA Function
#' @name s_ancova_j
#' @inheritParams tern::s_ancova
#' @param weights_emmeans (`string`)\cr argument from [emmeans::emmeans()], `"counterfactual"` by default.
#' @param method_combo (`string`)\cr Method for derivations in combined column.
#'   * `contrast` Derivations for the combined level are done through contrasts from the original model
#' (using weights per `weights_combo` specifications). 
#'   * `collapse` The ancova model for the combined group will be performed with group levels that contribute 
#' to the combination collapsed into a single combined level.
#' @param weights_combo (`string`)\cr Weights for the contrasts of the combined levels.
#'   * `equal` 1/(number of levels from arm variable included in the combination)
#'   * `proportional`, `proportional_marginal` weight for each level included in the combination is proportional to number of observations in that level
#'\cr The difference between `proportional` and `proportional_marginal` is only relevant when the model includes an interaction between arm and other factor variable (`interaction_item`).
#'\cr `proportional_marginal` interprets proportional over all levels of `interaction_item`, ie, the same weights will be used for all levels of `interaction_item`.
#'\cr For `proportional` the weights will be derived within the requested level (`interaction_y`) for `interaction_item`.
#' @description Extension to tern:::s_ancova, 3 extra statistics are returned:
#'   * `lsmean_se`: Marginal mean and estimated SE in the group.
#'   * `lsmean_ci`: Marginal mean and associated confidence interval in the group.
#'   * `lsmean_diffci`: Difference in mean and associated confidence level in one combined statistic.
#'   In addition, the LS mean weights can be specified.
#'   In addition, also a NULL .ref_group can be specified, the lsmean_diff related estimates will be returned as NA.
#' @export
#' @return Returns a named list of 8 statistics (3 extra compared to `tern:::s_ancova()`).
#' @family Inclusion of ANCOVA Functions
#' @examples
#' library(dplyr)
#' library(tern)
#'
#' df <- iris |> filter(Species == "virginica")
#' .df_row <- iris
#' .var <- "Petal.Length"
#' variables <- list(arm = "Species", covariates = "Sepal.Length * Sepal.Width")
#' .ref_group <- iris |> filter(Species == "setosa")
#' conf_level <- 0.95
#' s_ancova_j(df, .var, .df_row, variables, .ref_group, .in_ref_col = FALSE, conf_level)
s_ancova_j <- function(
    df,
    .var,
    .df_row,
    variables,
    .ref_group,
    .in_ref_col,
    conf_level,
    interaction_y = FALSE,
    interaction_item = NULL,
    weights_emmeans = "counterfactual",
    method_combo = c("contrasts", "collapse"),
    weights_combo = NULL) {
  if ((!is.null(interaction_item) & interaction_y == FALSE) ||
    (is.null(interaction_item) & interaction_y != FALSE)) {
    stop(
      "If interaction is needed it is required to set interaction_item to variable for interaction",
      "\n and interaction_y to a level of the interaction variable.",
      "\n If interaction is not needed, set interaction_y to FALSE and interaction_item to NULL."
    )
  }
  arm <- variables$arm
  method_combo <- match.arg(method_combo)

  .df_row <- subset(.df_row, !is.na(.df_row[[.var]]))
  df <- subset(df, !is.na(df[[.var]]))
  .ref_group <- subset(.ref_group, !is.na(.ref_group[[.var]]))

  n_obs_trt_lvls <- length(unique(.df_row[[arm]]))
  
  arm <- variables$arm
  sum_level_start <- as.character(unique(df[[arm]]))
  
  incombo <- length(sum_level_start) > 1
  if (incombo && method_combo == "collapse"){
    # proceed with model on updated levels for treatment group
    orig_levels <- levels(.df_row[[arm]])
    new_levels <- orig_levels
    new_levels[orig_levels %in% sum_level_start] <- "Combined"

    df[[arm]] <- factor(as.character(df[[arm]]), 
                             levels = orig_levels,
                             labels = new_levels)
    .df_row[[arm]] <- factor(as.character(.df_row[[arm]]), 
                             levels = orig_levels,
                             labels = new_levels)
    .ref_group[[arm]] <- factor(as.character(.ref_group[[arm]]), 
                             levels = orig_levels,
                             labels = new_levels)    
  }

  ### sparse data problems with underlying ancova function
  if (NROW(.df_row) == 0) {
    ret <- NULL
  } else if (NROW(df) == 0 || n_obs_trt_lvls < 2) {
    ## all stats are NA
    ret <- list(
      n = with_label(0, "n"),
      lsmean = with_label(NA, "Adjusted Mean"),
      lsmean_se = with_label(rep(NA, 2), "Adjusted Mean (SE)"),
      lsmean_ci = with_label(
        rep(NA, 3),
        paste0("Adjusted Mean", " (", f_conf_level(conf_level), ")")
      ),
      lsmean_diff = with_label(NA, "Difference in Adjusted Means"),
      lsmean_diff_ci = with_label(
        rep(NA, 2),
        paste("Difference in Adjusted Means", f_conf_level(conf_level))
      ),
      lsmean_diffci = with_label(
        rep(NA, 3),
        paste0(
          "Difference in Adjusted Means",
          " (",
          f_conf_level(conf_level),
          ")"
        )
      ),
      pval = with_label(NA, "p-value")
    )
  } else {
    if (NROW(.ref_group) == 0) {
      .ref_group <- NULL
    } else if (length(levels(.df_row[[arm]])) > n_obs_trt_lvls) {
      # missing arm levels need to be removed from the factor, in order to have the correct estimates,
      # and avoid errors with further code
      .df_row[[arm]] <- droplevels(.df_row[[arm]])
      df[[arm]] <- factor(
        as.character(df[[arm]]),
        levels = levels(.df_row[[arm]])
      )
      .ref_group[[arm]] <- factor(
        as.character(.ref_group[[arm]]),
        levels = levels(.df_row[[arm]])
      )
    }

    emmeans_fit <- h_ancova(
      .var = .var,
      variables = variables,
      .df_row = .df_row,
      weights_emmeans = weights_emmeans,
      interaction_item = interaction_item
    )
    sum_fit <- summary(
      emmeans_fit,
      level = conf_level
    )
    sum_level <- as.character(unique(df[[arm]]))

    if (length(sum_level) == 1) {
      est_single_fit <- h_ancova_est_single(
        emmeans_fit,
        sum_level,
        interaction_y,
        interaction_item,
        df,
        .var,
        .df_row,
        .ref_group,
        arm,
        conf_level,
        .in_ref_col
      )

      sum_fit_level <- est_single_fit[["sum_fit_level"]]
      sum_contrasts_level <- est_single_fit[["sum_contrasts_level"]]
      y <- est_single_fit[["y"]]
    } else {
      est_combined_fit <- h_ancova_est_combined(
        emmeans_fit,
        sum_level,
        interaction_y,
        interaction_item,
        df,
        .var,
        .df_row,
        .ref_group,
        arm,
        conf_level,
        .in_ref_col,
        weights_combo
      )

      sum_fit_level <- est_combined_fit[["sum_fit_level"]]
      sum_contrasts_level <- est_combined_fit[["sum_contrasts_level"]]
      y <- est_combined_fit[["y"]]
    }

    ret <- list(
      n_fit = with_label(length(y[!is.na(y)]), "n"),
      lsmean = with_label(sum_fit_level$emmean, "Adjusted Mean"),
      lsmean_se = with_label(
        c(sum_fit_level$emmean, sum_fit_level$SE),
        "Adjusted Mean (SE)"
      ),
      lsmean_ci = with_label(
        c(
          sum_fit_level$emmean,
          sum_fit_level$lower.CL,
          sum_fit_level$upper.CL
        ),
        paste0("Adjusted Mean", " (", f_conf_level(conf_level), ")")
      ),
      lsmean_diff = with_label(
        sum_contrasts_level$estimate,
        "Difference in Adjusted Means"
      ),
      lsmean_diff_ci = with_label(
        c(sum_contrasts_level$lower.CL, sum_contrasts_level$upper.CL),
        paste("Difference in Adjusted Means", f_conf_level(conf_level))
      ),
      lsmean_diffci = with_label(
        c(
          sum_contrasts_level$estimate,
          sum_contrasts_level$lower.CL,
          sum_contrasts_level$upper.CL
        ),
        paste0(
          "Difference in Adjusted Means",
          " (",
          f_conf_level(conf_level),
          ")"
        )
      ),
      pval = with_label(sum_contrasts_level$p.value, "p-value")
    )
  }
  return(ret)
}

#' @name s_summarize_ancova_j
#' @title ANCOVA Summary Function
#' @description Combination of [tern::s_summary], and ANCOVA based estimates for mean and diff between columns,
#' based on ANCOVA function `s_ancova_j`.
#' @inherit s_ancova_j
#' @param ... Additional arguments passed to `s_ancova_j`.
#' @details Combination of [tern::s_summary], and ANCOVA based estimates for mean and diff between columns,
#' based on ANCOVA function `s_ancova_j`
#' @return returns the statistics from `tern::s_summary(x)`, appended with a new statistics based upon ANCOVA
#' @export
#' @family Inclusion of ANCOVA Functions
# @seealso s_ancova_j
#' @examples
#' library(dplyr)
#' library(tern)
#'
#' df <- iris |> filter(Species == "virginica")
#' .df_row <- iris
#' .var <- "Petal.Length"
#' variables <- list(arm = "Species", covariates = "Sepal.Length * Sepal.Width")
#' .ref_group <- iris |> filter(Species == "setosa")
#' conf_level <- 0.95
#' s_summarize_ancova_j(
#'   df,
#'   .var = .var,
#'   .df_row = .df_row,
#'   variables = variables,
#'   .ref_group = .ref_group,
#'   .in_ref_col = FALSE,
#'   conf_level = conf_level
#' )
s_summarize_ancova_j <- function(
    df,
    .var,
    .df_row,
    .ref_group,
    .in_ref_col,
    ...) {
  x <- df[[.var]]
  y1 <- s_summary(x)
  y2 <- s_ancova_j(
    df = df,
    .var = .var,
    .ref_group = .ref_group,
    .in_ref_col = .in_ref_col,
    .df_row = .df_row,
    ...
  )
  c(y1, y2)
}

#' @describeIn s_summarize_ancova_j Formatted analysis function which is used as `afun`. Note that the
#'   junco specific `ref_path` and `.spl_context` arguments are used for reference column information.
#'
#' @param ref_path (`character`)\cr path to the reference group.
#' @param .spl_context (`environment`)\cr split context environment.
#' @param ... Additional arguments passed to the statistics function.
#' @param .stats (`character`)\cr statistics to calculate.
#' @param .formats (`list`)\cr formats for the statistics.
#' @param .labels (`list`)\cr labels for the statistics.
#' @param .indent_mods (`list`)\cr indentation modifications for the statistics.
#'
#' @return
#' * `a_summarize_ancova_j()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @examples
#'
#' basic_table() |>
#'   split_cols_by("Species") |>
#'   add_colcounts() |>
#'   analyze(
#'     vars = "Petal.Length",
#'     afun = a_summarize_ancova_j,
#'     show_labels = "hidden",
#'     na_str = tern::default_na_str(),
#'     table_names = "unadj",
#'     var_labels = "Unadjusted comparison",
#'     extra_args = list(
#'       variables = list(arm = "Species", covariates = NULL),
#'       conf_level = 0.95,
#'       .labels = c(lsmean = "Mean", lsmean_diff = "Difference in Means"),
#'       ref_path = c("Species", "setosa")
#'     )
#'   ) |>
#'   analyze(
#'     vars = "Petal.Length",
#'     afun = a_summarize_ancova_j,
#'     show_labels = "hidden",
#'     na_str = tern::default_na_str(),
#'     table_names = "adj",
#'     var_labels = "Adjusted comparison (covariates: Sepal.Length and Sepal.Width)",
#'     extra_args = list(
#'       variables = list(
#'         arm = "Species",
#'         covariates = c("Sepal.Length", "Sepal.Width")
#'       ),
#'       conf_level = 0.95,
#'       ref_path = c("Species", "setosa")
#'     )
#'   ) |>
#'   build_table(iris)
#'
#' @export
#' @order 2
a_summarize_ancova_j <- function(
    df,
    .var,
    .df_row,
    ref_path,
    .spl_context,
    ...,
    .stats =
      c(
        "n",
        "mean_sd",
        "median",
        "range",
        "quantiles",
        "lsmean_se",
        "lsmean_ci",
        "lsmean_diffci",
        "pval"
      ),
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
    default_stat_fnc = s_summarize_ancova_j,
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
    method_groups = "summarize_ancova_j",
    stats_in = .stats,
    formats_in = .formats,
    labels_in = .labels,
    indents_in = .indent_mods
  )
}
