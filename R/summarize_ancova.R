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

#' @title Junco Extended ANCOVA Function
#' @name s_ancova_j
#' @inheritParams tern::s_ancova
#' @param weights_emmeans (`string`)\cr argument from [emmeans::emmeans()], `"counterfactual"` by default.
#' @param in_combo (`logical`)\cr if `TRUE` the model will be restricted to data from reference group and df.
#' \cr If `FALSE`, it will be evaluated from the input data df to check if in combined column.
#' \cr This check from within data is not robust enough to cover sparse data situations, this can be handled more
#' securely within a_fun using .spl_context information.
#' @description Extension to tern:::s_ancova, 3 extra statistics are returned:
#'   * `lsmean_se`: Marginal mean and estimated SE in the group.
#'   * `lsmean_ci`: Marginal mean and associated confidence interval in the group.
#'   * `lsmean_diffci`: Difference in mean and associated confidence level in one combined statistic.
#'   In addition, the LS mean weights can be specified.
#'   In addition, also a NULL .ref_group can be specified, the lsmean_diff related estimates will be returned as NA.
#'   In addition, added logic when input df dataset is from a combination of levels of arm variable.
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
    in_combo = FALSE) {
  arm <- variables$arm

  .df_row <- subset(.df_row, !is.na(.df_row[[.var]]))
  df <- subset(df, !is.na(df[[.var]]))
  .ref_group <- subset(.ref_group, !is.na(.ref_group[[.var]]))

  n_obs_trt_lvls_df <- length(unique(df[[arm]]))
  in_combo <- (n_obs_trt_lvls_df > 1) || in_combo
  # note the above check in_combo will not trigger the following situation of being in
  # a combined column where the actual data only has 1 observed arm level
  # if this happens in a situation of 3-levels for arm, the adjustments for sparse data
  # would capture the required updates
  # in a situation of 4-levels for arm, the resulting model would not yet be as wanted
  # this is the reason of the argument in_combo
  if (in_combo) {
    # as per JJ approach in combined group
    # perform model on data with 2 levels : ref_group + combined group (as single level)
    # so restrict .df_row to ref_group and df and update factor levels for trt
    df[[arm]] <- factor(
      as.character("Combined"),
      levels = c("Combined", unique(as.character(.ref_group[[arm]])))
    )
    .ref_group[[arm]] <- factor(
      as.character(.ref_group[[arm]]),
      levels = levels(df[[arm]])
    )
    .df_row <- rbind(df, .ref_group)
    # now we can proceed as before
  }

  n_obs_trt_lvls <- length(unique(.df_row[[arm]]))

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
    arm <- variables$arm
    sum_level <- as.character(unique(df[[arm]]))

    # Ensure that there is only one element in sum_level.
    checkmate::assert_scalar(sum_level)

    sum_fit_level <- sum_fit[sum_fit[[arm]] == sum_level, ]

    # Get the index of the ref arm
    if (isTRUE(interaction_y)) {
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
      ret <- list(
        n = with_label(length(y[!is.na(y)]), "n"),
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
          character(),
          "Difference in Adjusted Means"
        ),
        lsmean_diff_ci = with_label(
          character(),
          paste("Difference in Adjusted Means", f_conf_level(conf_level))
        ),
        lsmean_diffci = with_label(
          character(),
          paste0(
            "Difference in Adjusted Means",
            " (",
            f_conf_level(conf_level),
            ")"
          )
        ),
        pval = with_label(character(), "p-value")
      )
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
          paste0(" - ", .ref_group[[arm]][1], ".*"),
          "",
          sum_contrasts$contrast
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
      ret <- list(
        n = with_label(length(y[!is.na(y)]), "n"),
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

  # check for in_combo based on .spl_context
  # add to dots_extra_args
  trtvar <- tail(ref_path, 2)[[1]]
  in_combo <- h_in_combo(trtvar, .spl_context)
  dots_extra_args[["in_combo"]] <- in_combo

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
