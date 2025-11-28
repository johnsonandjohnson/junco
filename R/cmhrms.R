#' Cochran-Mantel-Haenszel Row Mean Scores test
#'
#' See <https://psiaims.github.io/CAMIS/Comp/r-sas_cmh.html> for a general comparison 
#' overview between R and SAS.
#' 
#' @inheritParams proposal_argument_convention
#'
#' @name cmhrms
#' @return
#' * `s_cmhrms_j` a single element list containing the p-value from row mean score test.
#' * `a_cmhrms_j` a `VerticalRowsSection`` object (single row).
#' @order 1
NULL


#' @describeIn cmhrms Statistics function for the calculation of the p-value
#' based upon the row mean scores test.
#'
#' @export
#' @order 3
#' @inheritParams proposal_argument_convention
#' @param variables (`list`)\cr list with arm and strata variable names.
#' @param collapse_combo (`logical`)\cr If TRUE, multiple arm levels from df will be combined into 1 level.
#'
s_cmhrms_j <- function(df, .var, .ref_group, .in_ref_col, ..., .df_row, variables, collapse_combo = TRUE) {
  x <- .df_row[[.var]]
  x <- x[!is.na(x)]

  colvar <- variables$arm
  if (collapse_combo && length(unique(df[[colvar]]))>=2){
    df[[colvar]] <- "Combined"
  }
  pwdf <- rbind(df, .ref_group)
  if (.in_ref_col) {
    # overall p-value -- however not needed on output?
    inputdf <- .df_row
  } else {
    # pairwise p-value
    inputdf <- pwdf
  }
  
  inputdf[[colvar]] <- as.character(inputdf[[colvar]])
  inputdf[[.var]] <- droplevels(inputdf[[.var]])

  strata <- variables$strata

  if (!is.null(strata) && length(strata) > 0) {
    get_covariates <- utils::getFromNamespace("get_covariates", "tern")
    var_list <- get_covariates(strata)
    assert_df_with_variables(.df_row, var_list)
  }
  strata_part <- paste(strata, collapse = " + ")
  if (strata_part != "") {
    formula <- stats::as.formula(paste0("Freq ~ ", colvar, " + ", .var, " | ", strata_part))
  } else {
    formula <- stats::as.formula(paste0("Freq ~ ", colvar, " + ", .var))
  }

  x_stats <- list()

  if (length(x) == 0 || length(unique(inputdf[[colvar]])) < 2) {
    pval <- numeric(0)
  } else {
    x_stats_cmh <-
      tryCatch(
        vcdExtra::CMHtest(
          formula,
          data = inputdf,
          overall = TRUE, details = TRUE
        ),
        error = function(cond) {
          message(conditionMessage(cond))
          # Choose a return value in case of error
          NULL
        }
      )

    if ((strata_part != "")) {
      x_stats_cmh <- x_stats_cmh$ALL$table
    } else {
      x_stats_cmh <- x_stats_cmh$table
    }

    rmeans <- x_stats_cmh["rmeans", ]
    rmeans_pval <- rmeans[["Prob"]]

    if (is.null(x_stats_cmh)) {
      rmeans_pval <- NA
    }
    if (.in_ref_col) {
      pval <- numeric(0)
    } else {
      pval <- rmeans_pval
    }
  }
  x_stats <- list(pval = with_label(pval, "p-value"))
  return(x_stats)
}


#' @describeIn cmhrms Formatted analysis function which is used as `afun`.
#' @export
#' @order 2
a_cmhrms_j <- function(df, .var,
                       ref_path,
                       .spl_context,
                       .ref_group,
                       .in_ref_col,
                       .df_row,
                       ...,
                       variables,
                       collapse_combo = TRUE,
                       .stats = NULL,
                       .formats = NULL,
                       .indent_mods = NULL,
                       .labels = NULL) {
  dots_extra_args <- list(...)
  ref <- get_ref_info(ref_path, .spl_context)
  .in_ref_col <- ref$in_ref_col

  x_stats <-
    .apply_stat_functions(
      default_stat_fnc = s_cmhrms_j,
      custom_stat_fnc_list = NULL,
      args_list = c(
        df = list(df),
        .var = .var,
        .ref_group = list(ref$ref_group),
        .in_ref_col = .in_ref_col,
        .df_row = list(.df_row),
        variables = list(variables),
        collapse_combo = collapse_combo,
        dots_extra_args
      )
    )

  # Format according to specifications
  format_stats(
    x_stats,
    method_groups = "cmhrms",
    stats_in = .stats,
    formats_in = .formats,
    labels_in = .labels,
    indents_in = .indent_mods
  )
}

#' @describeIn cmhrms Wrapper for the `afun` which can exclude
#'   row split levels from producing the analysis. These have to be specified in the 
#'   `exclude_levels` argument, see `?do_exclude_split` for details.
#' @export
#' @order 3
a_cmhrms_j_with_exclude <- function(
  df,
  exclude_levels,
  .var,
  .spl_context,
  .ref_group,
  .in_ref_col,
  .df_row,
  ...,
  .stats = NULL,
  .formats = NULL,
  .indent_mods = NULL,
  .labels = NULL
) {
  if (do_exclude_split(exclude_levels, .spl_context)) {
    NULL
  } else {
    a_cmhrms_j(
      df = df,
      .var = .var,
      .spl_context = .spl_context,
      .ref_group = .ref_group,
      .in_ref_col = .in_ref_col,
      .df_row = .df_row,
      ...,
      .stats = .stats,
      .formats = .formats,
      .indent_mods = .indent_mods,
      .labels = .labels
    )
  }
}
