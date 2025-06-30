#' Statistical Function for Patient Years
#'
#' This function calculates patient years based on the provided data.
#'
#' @param df (`data.frame`)\cr data set containing all analysis variables.
#' @param .var (`string`)\cr variable name containing the patient years data.
#' @param id (`string`)\cr subject variable name.
#' @param .alt_df_full (`dataframe`)\cr alternative dataset for calculations.
#' @param source (`string`)\cr source of data, either "alt_df" or "df".
#' @param inriskdiffcol (`logical`)\cr flag indicating if the function is called within a risk difference column.
#'
#' @return A list containing the patient years statistics.
#'
#' @keywords internal
s_patyrs_j <- function(
  df,
  .var,
  id = "USUBJID",
  .alt_df_full,
  source = c("alt_df", "df"),
  inriskdiffcol = FALSE
) {
  source <- match.arg(source)

  if (source == "alt_df") {
    if (is.null(.alt_df_full)) {
      stop(paste(
        "s_patyrs_j with source = alt_df requires a non-null .alt_df_full"
      ))
    }
    df <- .alt_df_full
  }
  df <- unique(df[, c(id, .var)])

  x <- list()

  if (!inriskdiffcol) {
    patyrs <- sum(df[[.var]])
  } else {
    patyrs <- list(x = NULL)
  }
  x[["patyrs"]] <- stats::setNames(patyrs, nm = "patyrs")

  return(x)
}

#' Calculate Patient Years
#'
#' This function calculates patient years based on the provided data.
#'
#' @param df (`data.frame`)\cr data set containing all analysis variables.
#' @param .var (`string`)\cr variable name containing the patient years data.
#' @param .df_row (`data.frame`)\cr data frame across all of the columns for the given row split.
#' @param id (`string`)\cr subject variable name.
#' @param .alt_df_full (`dataframe`)\cr alternative dataset for calculations.
#' @param .formats (named 'character' or 'list')\cr formats for the statistics.
#' @param .labels (named 'character')\cr labels for the statistics.
#' @param source (`string`)\cr source of data, either "alt_df" or "df".
#' @param .spl_context (`data.frame`)\cr gives information about ancestor split states.
#' @param .stats (`character`)\cr statistics to select for the table.
#'
#' @return A list of in_rows objects containing the patient years statistics.
#'
#' @export
#' @keywords internal
a_patyrs_j <- function(
  df,
  .var,
  .df_row,
  id = "USUBJID",
  .alt_df_full = NULL,
  .formats = NULL,
  .labels = NULL,
  source = c("alt_df", "df"),
  .spl_context,
  .stats = "patyrs"
) {
  source <- match.arg(source)

  if (length(.stats) > 1 || (length(.stats) == 1 && .stats != "patyrs")) {
    stop("a_patyrs_j: .stats must be 'patyrs'.")
  }

  if (source == "alt_df" && is.null(.alt_df_full)) {
    stop(paste(
      "a_patyrs_j: .alt_df_full cannot be NULL when source = 'alt_df'."
    ))
  }

  col_expr <- .spl_context$cur_col_expr[[1]]
  ## colid can be used to figure out if we're in the relative risk columns or not
  colid <- .spl_context$cur_col_id[[1]]
  inriskdiffcol <- grepl("difference", tolower(colid), fixed = TRUE)

  if (source == "alt_df") {
    ### derive appropriate alt_df based upon .spl_context and .alt_df_full
    ### note that this is not yet within the current column

    alt_df <- h_create_altdf(
      .spl_context,
      .df_row,
      .alt_df_full,
      denom_by = NULL,
      id = id,
      variables = NULL,
      denom = "n_altdf"
    )
    ## restrict to current column
    new_denomdf <- subset(alt_df, eval(col_expr))
  } else {
    new_denomdf <- df
  }

  x_stats <- s_patyrs_j(
    df,
    .alt_df_full = new_denomdf,
    .var = .var,
    id = id,
    source = source,
    inriskdiffcol = inriskdiffcol
  )

  x_stats <- x_stats[.stats]

  levels_per_stats <- lapply(x_stats, names)

  .formats <- junco_get_formats_from_stats(.stats, .formats, levels_per_stats)
  .labels <- junco_get_labels_from_stats(.stats, .labels, levels_per_stats)
  .labels <- .unlist_keep_nulls(.labels)

  x_stats <- x_stats[.stats]

  # Unlist stats + names
  x_stats <- .unlist_keep_nulls(x_stats)
  names(x_stats) <- names(.formats)

  ### final step: turn requested stats into rtables rows
  inrows <- in_rows(
    .list = x_stats,
    .formats = .formats,
    .labels = .labels
  )

  return(inrows)
}

#' Statistical Function for Exposure-Adjusted Incidence Rate
#'
#' This function calculates exposure-adjusted incidence rates (EAIR) per 100 person-years for a
#' specific level of a variable.
#'
#' @param levii (`string`)\cr the specific level of the variable to calculate EAIR for.
#' @param df (`data.frame`)\cr data set containing all analysis variables.
#' @param .df_row (`data.frame`)\cr data frame across all of the columns for the given row split.
#' @param .var (`string`)\cr variable name that is passed by `rtables`.
#' @param .alt_df_full (`dataframe`)\cr alternative dataset for calculations.
#' @param id (`string`)\cr subject variable name.
#' @param diff (`logical`)\cr if TRUE, risk difference calculations will be performed.
#' @param conf_level (`proportion`)\cr confidence level of the interval.
#' @param trt_var (`string`)\cr treatment variable name.
#' @param ctrl_grp (`string`)\cr control group value.
#' @param cur_trt_grp (`string`)\cr current treatment group value.
#' @param inriskdiffcol (`logical`)\cr flag indicating if the function is called within a risk difference column.
#' @param fup_var (`string`)\cr follow-up variable name.
#' @param occ_var (`string`)\cr occurrence variable name.
#' @param occ_dy (`string`)\cr occurrence day variable name.
#'
#' @return A list containing the following statistics:
#' \itemize{
#'   \item n_event: Number of events
#'   \item person_years: Total person-years of follow-up
#'   \item eair: Exposure-adjusted incidence rate per 100 person-years
#'   \item eair_diff: Risk difference in EAIR (if diff=TRUE and inriskdiffcol=TRUE)
#'   \item eair_diff_ci: Confidence interval for the risk difference (if diff=TRUE and inriskdiffcol=TRUE)
#' }
#'
#' @keywords internal
s_eair100_levii_j <- function(
  levii,
  df,
  .df_row,
  .var,
  .alt_df_full = NULL,
  id = "USUBJID",
  diff = FALSE,
  # treatment/ref group related arguments
  conf_level = 0.95,
  trt_var = NULL,
  ctrl_grp = NULL,
  cur_trt_grp = NULL,
  inriskdiffcol = FALSE,
  fup_var,
  occ_var,
  occ_dy
) {
  if (diff && inriskdiffcol) {
    .alt_df_full_cur_group <- get_ctrl_subset(
      .alt_df_full,
      trt_var = trt_var,
      ctrl_grp = cur_trt_grp
    )
  } else {
    ### within a_eair100_j we need to ensure proper dataframe will be passed to .alt_df_full
    .alt_df_full_cur_group <- .alt_df_full
  }
  cur_dfs <- h_get_eair_df(
    levii,
    df,
    denom_df = .alt_df_full_cur_group,
    .var = .var,
    id = id,
    fup_var = fup_var,
    occ_var = occ_var,
    occ_dy = occ_dy
  )
  cur_df_num <- cur_dfs$df_num
  cur_df_denom <- cur_dfs$df_denom

  ### statistics derivation
  cur_AECOUNT <- length(unique(cur_df_num[[id]]))
  cur_YRSFUP <- sum(cur_df_denom[["mod_fup_var"]])
  cur_eair <- 100 * cur_AECOUNT / cur_YRSFUP

  x <- list()
  x$n_event <- c("n_event" = cur_AECOUNT)
  x$person_years <- c("person_years" = cur_YRSFUP)
  x$eair <- c("eair" = cur_eair)

  if (diff && inriskdiffcol) {
    x$n_event <- c("n_event" = NULL)
    x$person_years <- c("person_years" = NULL)
    x$eair <- c("eair" = NULL)

    alt_df_full_ref_group <- get_ctrl_subset(
      .alt_df_full,
      trt_var = trt_var,
      ctrl_grp = ctrl_grp
    )

    ref_group <- get_ctrl_subset(
      .df_row,
      trt_var = trt_var,
      ctrl_grp = ctrl_grp
    )

    ref_dfs <- h_get_eair_df(
      levii,
      df = ref_group,
      denom_df = alt_df_full_ref_group,
      .var = .var,
      id = id,
      fup_var = fup_var,
      occ_var = occ_var,
      occ_dy = occ_dy
    )

    ref_df_num <- ref_dfs$df_num
    ref_df_denom <- ref_dfs$df_denom

    ### statistics derivation
    ref_AECOUNT <- length(unique(ref_df_num[[id]]))
    ref_YRSFUP <- sum(ref_df_denom[["mod_fup_var"]])
    ref_eair <- 100 * ref_AECOUNT / ref_YRSFUP

    rdiff <- cur_eair - ref_eair

    sd <- sqrt(cur_AECOUNT / cur_YRSFUP^2 + ref_AECOUNT / ref_YRSFUP^2) * 100

    coeff <- stats::qnorm((1 + conf_level) / 2)
    lcl <- rdiff - (coeff * sd)
    ucl <- rdiff + (coeff * sd)

    eair_diff <- c(rdiff, lcl, ucl)

    x$eair_diff <- stats::setNames(
      c("eair_diff" = eair_diff),
      nm = c("estimate", "lcl", "ucl")
    )
  } else {
    x$eair_diff <- c("eair_diff" = NULL)
  }

  return(x)
}

#' Calculate Exposure-Adjusted Incidence Rate per 100 Patient-Years
#'
#' This function calculates the exposure-adjusted incidence rate (EAIR) per 100 patient-years.
#' It can also calculate the difference in EAIR between treatment groups.
#'
#' @param df (`data.frame`)\cr data set containing all analysis variables.
#' @param labelstr (`string`)\cr label string for the row.
#' @param .var (`string`)\cr variable name for analysis.
#' @param .df_row (`data.frame`)\cr data frame across all of the columns for the given row split.
#' @param .spl_context (`data.frame`)\cr gives information about ancestor split states.
#' @param .alt_df_full (`dataframe`)\cr denominator dataset for calculations.
#' @param id (`string`)\cr subject variable name.
#' @param drop_levels (`logical`)\cr if TRUE, non-observed levels will not be included.
#' @param riskdiff (`logical`)\cr if TRUE, risk difference calculations will be performed.
#' @param ref_path (`string`)\cr column path specifications for the control group.
#' @param .stats (`character`)\cr statistics to select for the table.
#' @param .formats (named 'character' or 'list')\cr formats for the statistics.
#' @param .labels (named 'character')\cr labels for the statistics.
#' @param .indent_mods (named `integer`)\cr indent modifiers for the labels.
#' @param na_str (`string`)\cr string used to replace all NA or empty values in the output.
#' @param conf_level (`proportion`)\cr confidence level of the interval.
#' @param fup_var (`string`)\cr variable name for follow-up time.
#' @param occ_var (`string`)\cr variable name for occurrence.
#' @param occ_dy (`string`)\cr variable name for occurrence day.
#'
#' @return A list of in_rows objects containing the EAIR statistics.
#'
#' @export
#' @keywords internal
a_eair100_j <- function(
  df,
  labelstr = NULL,
  .var,
  .df_row,
  .spl_context,
  .alt_df_full = NULL,
  id = "USUBJID",
  drop_levels = FALSE,
  riskdiff = TRUE,
  ref_path = NULL,
  .stats = c("eair"),
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL,
  na_str = rep("NA", 3),
  # treatment/ref group related arguments
  conf_level = 0.95,
  fup_var,
  occ_var,
  occ_dy
) {
  ## prepare for column based split
  col_expr <- .spl_context$cur_col_expr[[1]]
  ## colid can be used to figure out if we're in the relative risk columns or not
  colid <- .spl_context$cur_col_id[[1]]
  inriskdiffcol <- grepl("difference", tolower(colid), fixed = TRUE)

  # if no stats requested, get all stats
  .stats <- junco_get_stats(
    "a_eair100_j",
    stats_in = .stats,
    custom_stats_in = NULL
  )

  ### combine all preprocessing of incoming df/.df_row in one function
  ### do this outside stats derivation functions (s_freq_j/)
  ### use all of val/excl_levels/drop_levels//new_levels/label/label_map/labelstr/label_fstr
  upd_dfrow <- h_upd_dfrow(
    .df_row,
    .var,
    val = NULL,
    excl_levels = NULL,
    drop_levels = drop_levels,
    new_levels = NULL,
    new_levels_after = FALSE,
    label = NULL,
    label_map = NULL,
    labelstr = labelstr,
    label_fstr = NULL,
    .spl_context = .spl_context
  )

  .df_row <- upd_dfrow$df_row
  df <- upd_dfrow$df

  if (is.null(.alt_df_full)) {
    stop(paste("a_eair100_j: .alt_df_full cannot be NULL."))
  }

  ### derive appropriate alt_df based upon .spl_context and .alt_df_full
  ### note that only row-based splits are done
  ### for now only for variables from the first split_rows_by
  alt_df <- h_create_altdf(
    .spl_context,
    .df_row,
    .alt_df_full,
    denom_by = NULL,
    id = id,
    variables = NULL,
    denom = "n_altdf"
  )
  new_denomdf <- alt_df

  fn_Args <- list(
    df = df,
    .df_row = .df_row,
    .var = .var,
    id = id,
    diff = riskdiff,
    inriskdiffcol = inriskdiffcol,
    fup_var = fup_var,
    occ_var = occ_var,
    occ_dy = occ_dy
  )

  if (riskdiff && inriskdiffcol) {
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

    fn_args_x <- list(
      .alt_df_full = .alt_df_full,
      # treatment/ref group related arguments
      conf_level = conf_level,
      trt_var = trt_var,
      ctrl_grp = ctrl_grp,
      cur_trt_grp = cur_trt_grp
    )
  } else {
    new_denomdf <- subset(new_denomdf, eval(col_expr))
    fn_args_x <- list(.alt_df_full = new_denomdf)
  }

  fn_Args <- append(fn_Args, fn_args_x)

  levs <- levels(.df_row[[.var]])
  y <- mapply(
    s_eair100_levii_j,
    levii = levs,
    MoreArgs = fn_Args,
    SIMPLIFY = FALSE
  )

  ### rearrange list y to  list to x_stats
  #### this is to ensure the remainder of the code can stay the same as in a_freq_j
  stnms <- c("eair", "eair_diff", "n_event", "person_years")
  x_stats <- extract_x_stats(y, stnms)

  if (!inriskdiffcol) {
    .stats_adj <- .stats
  } else {
    .stats_adj <- replace(.stats, .stats %in% c("eair"), "eair_diff")
  }

  .stats <- .stats_adj

  # Fill in formatting defaults

  if (length(levs) > 1 && length(.stats) > 1) {
    message(
      "a_eair100_j : with multiple stats and multiple levels of analysis
      variable it is recommended to apply an extra split_rows_by on the analysis variable"
    )
  }

  x_stats <- x_stats[.stats]

  levels_per_stats <- lapply(x_stats, names)

  .formats <- junco_get_formats_from_stats(.stats, .formats, levels_per_stats)
  .labels <- junco_get_labels_from_stats(.stats, .labels, levels_per_stats)
  .labels <- .unlist_keep_nulls(.labels)

  .indent_mods <- junco_get_indents_from_stats(
    .stats,
    .indent_mods,
    levels_per_stats
  )
  .indent_mods <- .unlist_keep_nulls(.indent_mods)

  # .format_na_strs processing
  # if na_str = c(NA, NA, NA)
  # this will ensure the ci (NA, NA, NA) will be represented as NE (NE, NE)
  # the value NE is defined as the default to replace NA in our jjcs format

  if (!is.null(na_str)) {
    .format_na_strs <- lapply(names(.formats), FUN = function(x) {
      na_str
    })
  } else {
    .format_na_strs <- NULL
  }

  # Unlist stats + names
  x_stats <- .unlist_keep_nulls(x_stats)
  names(x_stats) <- names(.formats)

  ### final step: turn requested stats into rtables rows
  inrows <- in_rows(
    .list = x_stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods,
    .format_na_strs = .format_na_strs
  )

  return(inrows)
}
