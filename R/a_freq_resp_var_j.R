#' Analysis Function for Response Variables
#'
#' This function calculates counts and percentages for response variables (Y/N values),
#' with optional risk difference calculations.
#'
#' @param df (`data.frame`)\cr data set containing all analysis variables.
#' @param .var (`string`)\cr variable name that is passed by `rtables`.
#' @param .df_row (`data.frame`)\cr data frame across all of the columns for the given row split.
#' @param .N_col (`integer`)\cr column-wise N (column count) for the full column being analyzed.
#' @param .spl_context (`data.frame`)\cr gives information about ancestor split states.
#' @param resp_var (`string`)\cr response variable name containing Y/N values.
#' @param id (`string`)\cr subject variable name.
#' @param drop_levels (`logical`)\cr if TRUE, non-observed levels will not be included.
#' @param riskdiff (`logical`)\cr if TRUE, risk difference calculations will be performed.
#' @param ref_path (`string`)\cr column path specifications for the control group.
#' @param variables (`list`)\cr variables to include in the analysis.
#' @param conf_level (`proportion`)\cr confidence level of the interval.
#' @param method (`character`)\cr method for calculating confidence intervals.
#' @param weights_method (`character`)\cr method for calculating weights.
#' @param ... Additional arguments passed to other functions.
#'
#' @return Formatted analysis function which is used as `afun` in `analyze_vars()`
#' and as `cfun` in `summarize_row_groups()`.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' ADSL <- ex_adsl |> select(USUBJID, ARM, SEX)
#'
#' ADAE <- ex_adae |> select(USUBJID, ARM, SEX, AEBODSYS, AEDECOD)
#'
#' ADAE <- ADAE |>
#'   mutate(TRTEMFL = "Y")
#'
#' lyt <- basic_table(show_colcounts = TRUE) |>
#'   split_cols_by("ARM") |>
#'   analyze("SEX",
#'           show_labels = "visible",
#'           afun = a_freq_resp_var_j,
#'           extra_args = list(resp_var = "TRTEMFL", riskdiff = FALSE)
#'   )
#'
#' result <- build_table(lyt, df = ADAE, alt_counts_df = ADSL)
#'
#' result
a_freq_resp_var_j <- function(
    df,
    .var,
    .df_row,
    .N_col,
    .spl_context,
    resp_var = NULL,
    id = "USUBJID",
    drop_levels = FALSE,
    riskdiff = TRUE,
    ref_path = NULL,
    variables = formals(s_proportion_diff)$variables,
    conf_level = formals(s_proportion_diff)$conf_level,
    method = c(
      "wald",
      "waldcc",
      "cmh",
      "ha",
      "newcombe",
      "newcombecc",
      "strat_newcombe",
      "strat_newcombecc"
    ),
    weights_method = formals(s_proportion_diff)$weights_method,
    ...) {
  # ---- Derive statistics: xx / xx (xx.x%)

  if (is.null(resp_var)) {
    stop(
      "afun a_freq_resp_var_j: resp_var cannot be NULL."
    )
  }

  resp_var_values <- unique(df[[resp_var]][!is.na(df[[resp_var]])])
  if (
    is.character(df[[resp_var]]) &&
      any(is.na(df[[resp_var]])) &&
      all(resp_var_values == "Y")
  ) {
    stop(
      paste0(
        "afun a_freq_resp_var_j: it is not clear if missing resp_var should be considered non-response. ",
        "Please make it a factor with appropriate Y(/N) levels."
      )
    )
  }

  if (length(setdiff(resp_var_values, c("Y", "N"))) > 0) {
    stop("afun a_freq_resp_var_j: resp_var must contain only Y/N values.")
  }

  if (is.character(df[[resp_var]])) {
    df[[resp_var]] <- factor(df[[resp_var]], levels = c("Y", "N"))
  }

  df <- df[!is.na(df[[.var]]), ]

  # nolint start
  if ((is.factor(df[[resp_var]]) &&
    (identical(levels(df[[resp_var]]), c("Y", "N")) || identical(levels(df[[resp_var]]), c("N", "Y")))) ||
    is.character(df[[resp_var]])
  ) { # nolint end
    # missing values in resp_var should be excluded, not considered as not met response
    # subject will then not contribute to denominator
    df <- df[!is.na(df[[resp_var]]), ]
    .df_row <- .df_row[!is.na(.df_row[[resp_var]]), ]
  }

  if (drop_levels) {
    obs_levs <- unique(.df_row[[.var]])
    obs_levs <- intersect(levels(.df_row[[.var]]), obs_levs)

    val_var <- obs_levs
    # restrict the levels to the ones specified in val_var
    df <- df[df[[.var]] %in% val_var, ]
    .df_row <- .df_row[.df_row[[.var]] %in% val_var, ]

    df <- h_update_factor(df, .var, val_var)
  }

  varvec <- df[[.var]]

  levs <- if (is.factor(varvec)) levels(varvec) else sort(unique(varvec))

  colid <- .spl_context$cur_col_id[[1]]
  inriskdiffcol <- grepl("difference", tolower(colid), fixed = TRUE)

  if (riskdiff) {
    trt_var_refpath <- h_get_trtvar_refpath(
      ref_path,
      .spl_context,
      df
    )
    # trt_var_refpath is list with elements
    # trt_var trt_var_refspec cur_trt_grp ctrl_grp
    # make these elements available in current environment
    trt_var <- trt_var_refpath$trt_var
    trt_var_refspec <- trt_var_refpath$trt_var_refspec
    cur_trt_grp <- trt_var_refpath$cur_trt_grp
    ctrl_grp <- trt_var_refpath$ctrl_grp
  }

  fn <- function(levii) {
    dfii <- df[df[[.var]] == levii, ]
    dfrowii <- .df_row[.df_row[[.var]] == levii, ]

    if (!inriskdiffcol) {
      # use the core s_freq_j function on the current level of the incoming variable (.var)
      # note that the response variable will become .var in the below call
      # val is restricted to Y to show number of response on the current level of .var
      rslt <- s_freq_j(
        dfii,
        .df_row = dfrowii,
        .var = resp_var,
        alt_df = NULL,
        parent_df = NULL,
        val = "Y",
        denom = "n_df"
      )

      .stat <- "count_unique_denom_fraction"
      x_stat <- rslt[[.stat]]$Y
      rslt <- rcell(x_stat, format = jjcsformat_count_denom_fraction)
    } else {
      # use the risk differenc function s_rel_risk_val_j on the current level of the incoming variable (.var)
      # note that the response variable will become .var in the below call
      # val is restricted to Y to show number of response on the current level of .var
      denom_df <- dfrowii

      rslt <- s_rel_risk_val_j(
        df = dfii,
        .var = resp_var,
        .df_row = dfrowii,
        val = "Y",
        denom_df = denom_df,
        id = id,
        riskdiff = riskdiff,
        # treatment/ref group related arguments
        trt_var = trt_var,
        ctrl_grp = ctrl_grp,
        cur_trt_grp = cur_trt_grp,
        # relrisk specific arguments
        variables = variables,
        conf_level = conf_level,
        method = method,
        weights_method = weights_method
      )
      x_stat <- rslt[["rr_ci_3d"]]$Y
      rslt <- rcell(
        x_stat,
        format = jjcsformat_xx("xx.x (xx.x, xx.x)"),
        format_na_str = rep("NA", 3)
      )
    }

    # rslt is a single rcell row
    return(rslt)
  }

  ### apply function fn to all levels of incoming .var
  ### cls is a list of single rcell rows - ie one line per level is presented
  cls <- lapply(levs, fn)

  names(cls) <- levs
  return(cls)
}
