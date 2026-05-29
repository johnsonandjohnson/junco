#' Patient years exposure
#' @description Statistical/Analysis Function for presenting Patient years exposure summary data
#'
#' @name a_patyrs_j
#' @order 1
NULL

#' @describeIn a_patyrs_j Statistical Function for Patient years exposure summary data
#' @param df (`data.frame`)\cr data set containing all analysis variables.
#' @param .var (`string`)\cr variable name containing the patient years data.
#' @param id (`string`)\cr subject variable name.
#' @param .alt_df_full (`dataframe`)\cr alternative dataset for calculations.
#' @param source (`string`)\cr source of data, either "alt_df" or "df".
#' @param inriskdiffcol (`logical`)\cr flag indicating if the function is called within a risk difference column.
#'
#' @return
#' * `s_patyrs_j()` returns a list containing the patient years statistics.
#' The list of available statistics for can be viewed by running `junco_get_stats("a_patyrs_j")`.
#' Currently, this is just a single statistic `patyrs`, patient years of exposure.
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

#' @describeIn a_patyrs_j Formatted analysis function for patient years summary which is used
#' as `afun` in `analyze` or `cfun` in `summarize_row_groups`.
#'
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
#' @return
#' * `a_patyrs_j` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @export
#'
#' @examples
#' library(tern)
#' library(dplyr)
#' trtvar <- "ARM"
#' ctrl_grp <- "B: Placebo"
#' cutoffd <- as.Date("2023-09-24")
#'
#'
#' adexsum <- ex_adsl |>
#'   create_colspan_var(
#'     non_active_grp          = ctrl_grp,
#'     non_active_grp_span_lbl = " ",
#'     active_grp_span_lbl     = "Active Study Agent",
#'     colspan_var             = "colspan_trt",
#'     trt_var                 = trtvar
#'   ) |>
#'   mutate(
#'     rrisk_header = "Risk Difference (95% CI)",
#'     rrisk_label = paste(!!rlang::sym(trtvar), "vs", ctrl_grp),
#'     TRTDURY = case_when(
#'       !is.na(EOSDY) ~ EOSDY,
#'       TRUE ~ as.integer(cutoffd - as.Date(TRTSDTM) + 1)
#'     )
#'   ) |>
#'   select(USUBJID, !!rlang::sym(trtvar), colspan_trt, rrisk_header, rrisk_label, TRTDURY)
#'
#' adae <- ex_adae |>
#'   group_by(USUBJID, AEDECOD) |>
#'   select(USUBJID, AEDECOD, ASTDY) |>
#'   mutate(rwnum = row_number()) |>
#'   mutate(AOCCPFL = case_when(
#'     rwnum == 1 ~ "Y",
#'     TRUE ~ NA
#'   )) |>
#'   filter(AOCCPFL == "Y")
#'
#' aefup <- left_join(adae, adexsum, by = "USUBJID")
#'
#' colspan_trt_map <- create_colspan_map(adexsum,
#'   non_active_grp = ctrl_grp,
#'   non_active_grp_span_lbl = " ",
#'   active_grp_span_lbl = "Active Study Agent",
#'   colspan_var = "colspan_trt",
#'   trt_var = trtvar
#' )
#'
#' ref_path <- c("colspan_trt", " ", trtvar, ctrl_grp)
#'
#' ################################################################################
#' # Define layout and build table:
#' ################################################################################
#'
#' lyt <- basic_table(show_colcounts = TRUE, colcount_format = "N=xx", top_level_section_div = " ") |>
#'   split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map)) |>
#'   split_cols_by(trtvar) |>
#'   split_cols_by("rrisk_header", nested = FALSE) |>
#'   split_cols_by(trtvar, labels_var = "rrisk_label", split_fun = remove_split_levels(ctrl_grp)) |>
#'   analyze("TRTDURY",
#'     nested = FALSE,
#'     show_labels = "hidden",
#'     afun = a_patyrs_j
#'   )
#' result <- build_table(lyt, aefup, alt_counts_df = adexsum)
#' result
#'
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

  check_alt_df_full(source, "alt_df", .alt_df_full)

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

#' Exposure-Adjusted Incidence Rate
#' @description
#' Statistical/Analysis Function for presenting Exposure-Adjusted Incidence Rate summary data
#'
#'
#' @name a_eair100_j
NULL

#' @describeIn a_eair100_j
#' calculates exposure-adjusted incidence rates (EAIR) per `num_p_year` person-years for a
#' specific level of a variable.
#'
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
#' @param fup_var (`string`)\cr name of the variable containing the total follow-up duration
#'   for each subject, expressed in **years**.
#'   Used as the at-risk exposure time for subjects who did **not** experience the event.
#' @param occ_var (`string`)\cr name of the flag variable identifying the
#'   **first** occurrence of the event within each subject and level, encoded as `"Y"`.
#'   Only records where `occ_var == "Y"` contribute to the event count (`n_event`).
#' @param occ_dy (`string`)\cr name of the variable containing the relative day of the
#'   first event occurrence, expressed in **days** (e.g., `ASTDY`).
#'   For subjects with an event (`occ_var == "Y"`), this value is converted to years
#'   as `occ_dy / 365.25` and used as the at-risk exposure time instead of `fup_var`.
#'   Must be on the same time origin as `fup_var` (i.e., relative to start of exposure).
#' @param num_p_year (`numeric`)\cr scaling factor for the incidence rate, representing
#'   the number of person-years used as the denominator base.
#'   The EAIR is calculated as `num_p_year * n_event / total_person_years`.
#'   Defaults to `100`, yielding a rate per 100 person-years.
#'   For example, use `1` for a rate per person-year or `1000` for a rate per
#'   1000 person-years.
#'
#' @return
#'  * `s_eair100_levii_j()` returns a list containing the following statistics:
#' \itemize{
#'   \item n_event: Number of events
#'   \item person_years: Total person-years of follow-up
#'   \item eair: Exposure-adjusted incidence rate per `num_p_year` person-years
#'   \item n_eair: Combination of `n_event` and `eair` statistics.
#'   \item eair_diff: Difference in EAIR between current group and reference group
#'    (if diff=TRUE and inriskdiffcol=TRUE), together with it's Wald based confidence interval.
#' }\cr
#' The list of available statistics (core columns) can also be viewed by
#' running `junco_get_stats("a_eair100_j")`.
#' @details
#' The exposure-adjusted incidence rate (EAIR) is defined as the number of subjects
#' with at least one occurrence of a specified adverse event divided by the total
#' at-risk exposure time across all subjects.
#'
#' At-risk exposure time is calculated for each subject
#' as:
#' - for subjects with the event: relative day (from the start of exposure) of
#' the onset date of the **first** occurrence of the specified event (`occ_var` and `occ_dy`),
#' transformed from days into years as `occ_dy/365.25`, or
#' - for subjects without the event: total duration of follow-up (expressed in years)
#' (e.g., treatment discontinuation, completion, or censoring) (`fup_var`).
#'
#' The EAIR is expressed as:
#'
#' \deqn{
#'   \mathrm{EAIR} = \hat r = \frac{n}{T}
#' }
#'
#' where:
#' \itemize{
#'   \item \eqn{n} is the number of subjects with at least one occurrence of the event
#'   \item T = \eqn{ \sum_{i=1}^{N} t_i}, with
#'   \item \eqn{t_i} is the at-risk exposure time for subject \eqn{i},
#'   expressed in subject-years
#' }
#'
#' The difference between two EAIRs is
#' \eqn{\hat \theta = (\hat r_1 - \hat r_2)}.
#'
#' Wald's method for the confidence interval approximation is used (Liu iet al., 2006), which uses the standard error
#' \deqn{
#' \mathrm{se}(\hat r_1 - \hat r_2) =
#' \sqrt{\frac{n_1}{T_1^2} + \frac{n_2}{T_2^2}}
#' }{
#' se = sqrt(n1/T1^2 + n2/T2^2)
#' }
#' and the \eqn{100(1-\alpha)\%} confidence interval is
#' \deqn{
#' (\hat r_1 - \hat r_2) \pm z_{1-\alpha/2}\,\mathrm{se}(\hat r_1 - \hat r_2)
#' }{
#' (r1 - r2) +/- z_(1-a/2) * SE
#' }
#' where \eqn{z_{1-\alpha/2}} is the standard normal quantile. [1](https://metricgate.com/docs/poisson-rate-comparison/)
#'
#' EAIR is commonly reported per 100 subject-years:
#'
#' \deqn{
#'   \mathrm{EAIR}_{100} = 100 \times \hat r
#' }
#' @references
#' Liu, G. F., Wang, J., Liu, K., and Snavely, D. B. (2006).
#' Confidence intervals for an exposure adjusted incidence rate difference
#' with applications to clinical trials.
#' \emph{Statistics in Medicine}, \strong{25}(8), 1275--1286.
#' \doi{10.1002/sim.2335}
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
  occ_dy,
  num_p_year = 100
){
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
  cur_eair <- num_p_year * cur_AECOUNT / cur_YRSFUP
  cur_n_eair <- c(cur_AECOUNT, cur_eair)
  cur_eair <- num_p_year * cur_AECOUNT / cur_YRSFUP
  cur_n_eair <- c(cur_AECOUNT, cur_eair)

  x <- list()
  eair_lbl <- paste0("eair (per ", num_p_year, " person years)")
  eair_lbl <- paste0("eair (per ", num_p_year, " person years)")
  x$n_event <- c("n_event" = cur_AECOUNT)
  x$person_years <- c("person_years" = cur_YRSFUP)
  x$eair <- stats::setNames(c("eair" = cur_eair), eair_lbl)
  x$n_eair <- stats::setNames(c("n_eair" = cur_n_eair), c("n_event", eair_lbl))
  x$eair <- stats::setNames(c("eair" = cur_eair), eair_lbl)
  x$n_eair <- stats::setNames(c("n_eair" = cur_n_eair), c("n_event", eair_lbl))

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
    ref_eair <- num_p_year * ref_AECOUNT / ref_YRSFUP
    ref_n_eair <- c(ref_AECOUNT, ref_eair)
    ref_eair <- num_p_year * ref_AECOUNT / ref_YRSFUP
    ref_n_eair <- c(ref_AECOUNT, ref_eair)

    rdiff <- cur_eair - ref_eair

    se <- sqrt(cur_AECOUNT / cur_YRSFUP^2 + ref_AECOUNT / ref_YRSFUP^2) * num_p_year
    se <- sqrt(cur_AECOUNT / cur_YRSFUP^2 + ref_AECOUNT / ref_YRSFUP^2) * num_p_year

    coeff <- stats::qnorm((1 + conf_level) / 2)
    lcl <- rdiff - (coeff * se)
    ucl <- rdiff + (coeff * se)
    lcl <- rdiff - (coeff * se)
    ucl <- rdiff + (coeff * se)

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

#' @describeIn a_eair100_j
#' Formatted analysis function for exposure adjusted incidence rate summary which is
#' used as `afun` in `analyze` or `cfun` in `summarize_row_groups`.\cr
#'
#' @param labelstr (`string`)\cr Not intended to be set by user, only present to be able to use function as a `cfun`.
#' @param .spl_context (`data.frame`)\cr gives information about ancestor split states.
#' @param drop_levels (`logical`)\cr if TRUE, non-observed levels will not be included.
#' @param riskdiff (`logical`)\cr if TRUE, risk difference calculations will be performed.
#' @param ref_path (`string`)\cr column path specifications for the control group.
#' @param .stats (`character`)\cr statistics to select for the table.\cr
#' @param .formats (named 'character' or 'list')\cr formats for the statistics.
#' @param .labels (named 'character')\cr labels for the statistics.
#' @param .indent_mods (named `integer`)\cr indent modifiers for the labels.
#' @param na_str (`string`)\cr string used to replace all NA or empty values in the output.
#'
#' @return
#'  * `a_eair100_j` returns the corresponding list with formatted [rtables::CellValue()].\cr
#' Within a risk difference column, the statistics `eair` and `n_eair` are replaced by `eair_diff`
#' (difference in EAIR between current group and reference group, together with it's confidence interval).

#' @export
#'
#' @examples
#' library(tern)
#' library(dplyr)
#' trtvar <- "ARM"
#' ctrl_grp <- "B: Placebo"
#' cutoffd <- as.Date("2023-09-24")
#'
#'
#' adexsum <- ex_adsl |>
#'   create_colspan_var(
#'     non_active_grp          = ctrl_grp,
#'     non_active_grp_span_lbl = " ",
#'     active_grp_span_lbl     = "Active Study Agent",
#'     colspan_var             = "colspan_trt",
#'     trt_var                 = trtvar
#'   ) |>
#'   mutate(
#'     rrisk_header = "Risk Difference (95% CI)",
#'     rrisk_label = paste(!!rlang::sym(trtvar), "vs", ctrl_grp),
#'     TRTDURY = case_when(
#'       !is.na(EOSDY) ~ EOSDY,
#'       TRUE ~ as.integer(cutoffd - as.Date(TRTSDTM) + 1)
#'     )
#'   ) |>
#'   select(USUBJID, !!rlang::sym(trtvar), colspan_trt, rrisk_header, rrisk_label, TRTDURY)
#'
#' adexsum$TRTDURY <- as.numeric(adexsum$TRTDURY)
#'
#' adae <- ex_adae |>
#'   group_by(USUBJID, AEDECOD) |>
#'   select(USUBJID, AEDECOD, ASTDY) |>
#'   mutate(rwnum = row_number()) |>
#'   mutate(AOCCPFL = case_when(
#'     rwnum == 1 ~ "Y",
#'     TRUE ~ NA
#'   )) |>
#'   filter(AOCCPFL == "Y")
#'
#' aefup <- left_join(adae, adexsum, by = "USUBJID")
#'
#' colspan_trt_map <- create_colspan_map(adexsum,
#'   non_active_grp = ctrl_grp,
#'   non_active_grp_span_lbl = " ",
#'   active_grp_span_lbl = "Active Study Agent",
#'   colspan_var = "colspan_trt",
#'   trt_var = trtvar
#' )
#'
#' ref_path <- c("colspan_trt", " ", trtvar, ctrl_grp)
#'
#' ################################################################################
#' # Define layout and build table:
#' ################################################################################
#'
#' lyt <- basic_table(show_colcounts = TRUE, colcount_format = "N=xx", top_level_section_div = " ") |>
#'   split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map)) |>
#'   split_cols_by(trtvar) |>
#'   split_cols_by("rrisk_header", nested = FALSE) |>
#'   split_cols_by(trtvar, labels_var = "rrisk_label", split_fun = remove_split_levels(ctrl_grp)) |>
#'   analyze("TRTDURY",
#'     show_labels = "hidden",
#'     afun = a_patyrs_j
#'   ) |>
#'   analyze(
#'     vars = "AEDECOD",
#'     afun = a_eair100_j,
#'     extra_args = list(
#'       fup_var = "TRTDURY",
#'       occ_var = "AOCCPFL",
#'       occ_dy = "ASTDY",
#'       ref_path = ref_path,
#'       drop_levels = TRUE
#'     )
#'   )
#'
#' result <- build_table(lyt, aefup, alt_counts_df = adexsum)
#' head(result, 5)
#'
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
  occ_dy,
  num_p_year = 100
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

  checkmate::assert_string(fup_var, null.ok = FALSE)
  checkmate::assert_string(occ_dy, null.ok = FALSE)
  checkmate::assert_string(occ_var, null.ok = FALSE)
  checkmate::assert_names(colnames(df), must.include = c(occ_var, fup_var, occ_dy))
    
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
    stop(paste("a_eair100_j: .alt_df_full cannot be NULL. Specify `alt_counts_df` to `build_table`"))
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
    occ_dy = occ_dy,
    num_p_year = num_p_year
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

    # for combined facet, .alt_df_full value for the treatment group needs update
    .alt_df_full <- upd_denom_df_combo(
      .alt_df_full,
      trt_var,
      cur_trt_grp,
      .spl_context
    )

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
  stnms <- c("eair", "eair_diff", "n_event", "person_years", "n_eair")
  stnms <- c("eair", "eair_diff", "n_event", "person_years", "n_eair")
  x_stats <- extract_x_stats(y, stnms)

  if (!inriskdiffcol) {
    .stats_adj <- .stats
  } else {
    .stats_adj <- replace(.stats, .stats %in% c("eair", "n_eair"), "eair_diff")
    .stats_adj <- replace(.stats, .stats %in% c("eair", "n_eair"), "eair_diff")
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
