#' Tabulation for Exposure Tables
#'
#'
#' @details
#' Creates statistics needed for standard exposure table
#' This includes differences and 95% CI and total treatment years.
#' This is designed to be used as an analysis (afun in `analyze`) function.
#'
#' @name a_summarize_ex_j
NULL


#' @inheritParams proposal_argument_convention
#' @describeIn a_summarize_ex_j Statistics function needed for the exposure tables
#'
#' @param daysconv conversion required to get the values into days
#' (i.e 1 if original PARAMCD unit is days, 30.4375 if original PARAMCD unit is in months)
#' @param ancova (`logical`)\cr If FALSE, only descriptive methods will be used. \cr
#' If TRUE Ancova methods will be used for each of the columns : AVAL, CHG, DIFF. \cr
#' @param comp_btw_group (`logical`)\cr If TRUE,
#' \cr When ancova = FALSE, the estimate of between group difference (on CHG) will be based upon two-sample t-test. \cr
#' \cr When ancova = TRUE, the same ancova model will be used for the estimate of between group difference (on CHG).
#'
#' @param interaction_y (`character`)\cr Will be passed onto the `tern` function `s_ancova`, when ancova = TRUE.
#' @param interaction_item (`character`)\cr Will be passed onto the `tern` function `s_ancova`, when ancova = TRUE.
#' @param conf_level (`proportion`)\cr Confidence level of the interval
#' @param variables (named list of strings)\cr
#' list of additional analysis variables, with expected elements:
#'    * arm (string)\cr
#' group variable, for which the covariate adjusted means of multiple groups will be summarized.
#' Specifically, the first level of arm variable is taken as the reference group.
#'    * covariates (character)\cr
#' a vector that can contain single variable names (such as 'X1'), and/or interaction terms indicated by 'X1 * X2'.
#'
s_summarize_ex_j <- function(
  df,
  .var,
  .df_row,
  .spl_context,
  comp_btw_group = TRUE,
  ref_path = NULL,
  ancova = FALSE,
  interaction_y,
  interaction_item,
  conf_level,
  daysconv,
  variables
) {
  control <- control_analyze_vars()
  control$conf_level <- conf_level
  x_stats <- s_summary(df[[.var]], na.rm = TRUE, .var, control = control)
  ## add extra for subject years
  subj_years <- x_stats[["sum"]] * daysconv / 365.25
  x_stats[["total_subject_years"]] <- c(x_stats[["sum"]], subj_years)
  names(x_stats[["total_subject_years"]]) <- c("total", "subject_years")

  cur_col_id <- .spl_context$cur_col_id[[length(.spl_context$split)]]
  indiffcol <- grepl("difference", tolower(cur_col_id), fixed = TRUE)

  if (indiffcol) {
    # blank out all stats
    x_stats <- sapply(
      names(x_stats),
      FUN = function(x) {
        x_stats[[x]] <- NULL
      },
      simplify = FALSE,
      USE.NAMES = TRUE
    )
    # diff between group will be updated in mean_sd stat
    if (comp_btw_group) {
      trt_var_refpath <- h_get_trtvar_refpath(ref_path, .spl_context, df)
      # trt_var_refpath is list with elements trt_var trt_var_refspec cur_trt_grp ctrl_grp make these elements
      # available in current environment
      trt_var <- trt_var_refpath$trt_var
      trt_var_refspec <- trt_var_refpath$trt_var_refspec
      cur_trt_grp <- trt_var_refpath$cur_trt_grp
      ctrl_grp <- trt_var_refpath$ctrl_grp

      .in_ref_col <- FALSE
      if (trt_var == ctrl_grp) .in_ref_col <- TRUE

      .ref_group <- .df_row[.df_row[[trt_var]] == ctrl_grp, ]

      if (ancova) {
        # ancova method for diff between group
        x_stats2 <- s_summarize_ancova_j(
          df = df,
          .var = .var,
          .ref_group = .ref_group,
          .in_ref_col = .in_ref_col,
          .df_row = .df_row,
          conf_level = conf_level,
          interaction_y = interaction_y,
          interaction_item = interaction_item,
          variables = variables
        )
        diffstat <- x_stats2[["lsmean_diffci"]]
      } else {
        # descriptive method for diff between group
        x_stats2 <- s_summarize_desc_j(
          df = df,
          .var = .var,
          .ref_group = .ref_group,
          .in_ref_col = .in_ref_col,
          control = control
        )
        diffstat <- x_stats2[["mean_diffci"]]
      }
      # actual update with the diffstat
      x_stats[["mean_sd"]] <- diffstat
    }
  }

  return(x_stats)
}

#' @title Analysis Function For Exposure Tables
#' @description
#' A function to create the appropriate statistics needed for exposure table
#' @details
#' Creates statistics needed for table. This includes differences and 95% CI and total treatment years.
#' This is designed to be used as an analysis (afun in `analyze`) function.
#' @inheritParams proposal_argument_convention
#'
#' @describeIn a_summarize_ex_j Formatted analysis function which is used as `afun`.
#'
#' @return
#' * `a_summarize_ex_j()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @aliases a_summarize_ex_j
#' @examples
#' library(dplyr)
#'
#' ADEX <- data.frame(
#'   USUBJID = c(
#'     'XXXXX01', 'XXXXX02', 'XXXXX03', 'XXXXX04', 'XXXXX05',
#'     'XXXXX06', 'XXXXX07', 'XXXXX08', 'XXXXX09', 'XXXXX10'
#'   ),
#'   TRT01A = c(
#'     'ARMA', 'ARMA', 'ARMA', 'ARMA', 'ARMA',
#'     'Placebo', 'Placebo', 'Placebo', 'ARMA', 'ARMA'
#'   ),
#'   AVAL = c(56, 78, 67, 87, 88, 93, 39, 87, 65, 55)
#' )
#'
#' ADEX <- ADEX |>
#'   mutate(TRT01A = as.factor(TRT01A))
#'
#' ADEX$colspan_trt <- factor(ifelse(ADEX$TRT01A == 'Placebo', ' ', 'Active Study Agent'),
#'   levels = c('Active Study Agent', ' ')
#' )
#'
#' ADEX$diff_header <- 'Difference in Means (95% CI)'
#' ADEX$diff_label <- paste(ADEX$TRT01A, paste('vs', 'Placebo'))
#'
#' colspan_trt_map <- create_colspan_map(ADEX,
#'   non_active_grp = 'Placebo',
#'   non_active_grp_span_lbl = ' ',
#'   active_grp_span_lbl = 'Active Study Agent',
#'   colspan_var = 'colspan_trt',
#'   trt_var = 'TRT01A'
#' )
#' ref_path <- c('colspan_trt', '', 'TRT01A', 'Placebo')
#'
#' lyt <- basic_table() |>
#'   split_cols_by(
#'     'colspan_trt',
#'     split_fun = trim_levels_to_map(map = colspan_trt_map)
#'   ) |>
#'   split_cols_by('TRT01A') |>
#'   split_cols_by('diff_header', nested = FALSE) |>
#'   split_cols_by(
#'     'TRT01A',
#'     split_fun = remove_split_levels('Placebo'),
#'     labels_var = 'diff_label'
#'   ) |>
#'   analyze('AVAL',
#'     afun = a_summarize_ex_j, var_labels = 'Duration of treatment (Days)',
#'     show_labels = 'visible',
#'     indent_mod = 0L,
#'     extra_args = list(
#'       daysconv = 1,
#'       ref_path = ref_path,
#'       variables = list(arm = 'TRT01A', covariates = NULL),
#'       ancova = TRUE,
#'       comp_btw_group = TRUE
#'     )
#'   )
#'
#' result <- build_table(lyt, ADEX)
#'
#' result
#' @export
a_summarize_ex_j <- function(
  df,
  .var,
  .df_row,
  .spl_context,
  comp_btw_group = TRUE,
  ref_path = NULL,
  ancova = FALSE,
  interaction_y = FALSE,
  interaction_item = NULL,
  conf_level = 0.95,
  variables,
  .stats = c("mean_sd", "median", "range", "quantiles", "total_subject_years"),
  .formats = c(diff_mean_est_ci = jjcsformat_xx("xx.xx (xx.xx, xx.xx)")),
  .labels = c(quantiles = "Interquartile range"),
  .indent_mods = NULL,
  na_str = rep("NA", 3),
  daysconv = 1
) {
  if (!is.numeric(df[[.var]])) {
    stop("a_summarize_ex_j issue: input variable must be numeric.")
  }

  if (comp_btw_group && is.null(ref_path)) {
    stop("a_summarize_ex_j issue: argument ref_path cannot be NULL.")
  }

  if (comp_btw_group && ancova && is.null(variables)) {
    stop("a_summarize_ex_j issue: argument variables must be defined when ancova is requested.")
  }

  x_stats <- s_summarize_ex_j(
    df = df,
    .var = .var,
    .df_row = .df_row,
    .spl_context = .spl_context,
    comp_btw_group = comp_btw_group,
    ref_path = ref_path,
    ancova = ancova,
    interaction_y = interaction_y,
    interaction_item = interaction_item,
    conf_level = conf_level,
    daysconv = daysconv,
    variables = variables
  )

  # Fill in formatting defaults
  .stats_in <- .stats
  .stats <- tern_get_stats("analyze_vars_numeric", stats_in = .stats, custom_stats_in = NULL)
  if ("total_subject_years" %in% .stats_in) {
    # place the extra statistic at the appropriate place within .stats vector
    i <- match("total_subject_years", .stats_in)
    x <- .stats_in[i:length(.stats_in)]
    if (length(x) == 1) {
      .stats <- c(.stats, "total_subject_years")
    } else {
      i2 <- min(match(x, .stats), na.rm = TRUE)
      if (i2 == 1) {
        .stats <- c("total_subject_years", .stats)
      } else {
        .stats <- c(.stats[1:(i2 - 1)], "total_subject_years", .stats[i2:length(.stats)])
      }
    }
  }

  .stats_ext <- c(.stats, "diff_mean_est_ci")

  .formats <- junco_get_formats_from_stats(.stats_ext, .formats)
  .labels <- junco_get_labels_from_stats(.stats, .labels, label_attr_from_stats = get_label_attr_from_stats(x_stats))
  .indent_mods <- junco_get_indents_from_stats(.stats, .indent_mods)

  .names <- names(.labels)
  .labels <- .unlist_keep_nulls(.labels)
  .indent_mods <- .unlist_keep_nulls(.indent_mods)

  cur_col_id <- .spl_context$cur_col_id[[length(.spl_context$split)]]
  indiffcol <- grepl("difference", tolower(cur_col_id), fixed = TRUE)

  if (indiffcol && comp_btw_group) {
    .formats[["mean_sd"]] <- .formats[["diff_mean_est_ci"]]
  }
  .formats[["diff_mean_est_ci"]] <- NULL

  if (!is.null(na_str)) {
    .format_na_strs <- lapply(names(.formats), FUN = function(x) {
      na_str
    })
  } else {
    .format_na_strs <- NULL
  }

  x_stats <- x_stats[.stats]
  ret <- in_rows(
    .list = x_stats,
    .formats = .formats,
    .names = .names,
    .labels = .labels,
    .indent_mods = .indent_mods,
    .format_na_strs = .format_na_strs
  )
  return(ret)
}
