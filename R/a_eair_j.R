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

  cur_col_expr <- .spl_context$cur_col_expr[[1]]
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
    new_denomdf <- subset(alt_df, eval(cur_col_expr))
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
#' @param fup_var (`string`)\cr name of the variable containing the total follow-up duration
#'   for each subject, expressed in **years**.
#'   Used as the at-risk exposure time for subjects who did **not** experience the event.
#' @param occ_var (`string` or `NULL`)\cr name of the flag variable identifying the
#'   **first** occurrence of the event within each subject and level, encoded as `"Y"`.
#'   Only records where `occ_var == "Y"` contribute to the event count (`n_event`),
#'  and only one event record per subject is allowed.
#'   \cr If `NULL`, event occurrence filtering is skipped and time at risk is taken
#'   from `fup_var` in `alt_counts_df` for all subjects.
#' @param occ_dy (`string` or `NULL`)\cr When `occ_var` not `NULL`: name of the variable
#'  containing the relative day of the
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
#' @param count_multiple_events (`logical`)\cr if `TRUE`, counts the total number of events
#'   per subject rather than treating each subject as contributing 1 to `n_event`.
#'   Only considered when `occ_var = NULL`.
#'   Defaults to `FALSE`.
#'
#' @param conf_level (`proportion`)\cr confidence level of the interval.
#' @param conf_type_diff (`string`)\cr method used to compute the confidence interval for the EAIR difference.
#'   One of `"wald"` (Wald without continuity correction), `"waldcc"` (Wald with continuity correction),
#'   `"mn"` (Miettinen-Nurminen score), or `"scas"` (skewness-corrected asymptotic score).
#'   Defaults to `"wald"`.
#' @param conf_type_eair (`string`)\cr method used to compute the confidence interval for the EAIR rate.
#'   One of `"exact"`, `"normal"`, `"normal_log"`, `"byar"`
#'   \cr See `?tern:::h_incidence_rate` and `?tern::incidence_rate`, argument `control` for more information.
#'   Defaults to `"exact"`.
#' @param vs_ref_group (`logical`)\cr if TRUE, risk difference (against reference group) calculations will be performed.
#' @param cur_col_expr (`expression`)\cr current column expression taken from `rtables` `.spl_context`
#' splitting machinery (`cur_col_expr`).
#' @param ref_col_expr (`expression` or `NULL`)\cr column expression for the reference group.
#' Required to be an expression if `vs_ref_group` is `TRUE`
#' @param inriskdiffcol (`logical`)\cr flag indicating if the function is called within a risk difference column.
#'
#' @return
#'  * `s_eair100_levii_j()` returns a list containing the following statistics:
#' \itemize{
#'   \item n_event: Number of subjects with the event, or total event count if `count_multiple_events = TRUE`
#'   \item person_years: Total person-years of follow-up
#'   \item eair: Exposure-adjusted incidence rate per `num_p_year` person-years
#'   \item n_eair: Combination of `n_event` and `eair` statistics.
#'   \item eair_n: Combination of `eair` and `n_event` statistics.
#'   \item eair_n_py: Combination of `eair`, `n_event` and `person_years` statistics.
#'   \item eair_ci: Confidence interval for `eair`, method specified in `conf_type_eair`.
#'   \item eair_est_ci: Combination of `eair` and `eair_ci`.
#'   \item eair_diff_est: Difference in EAIR between current group and reference group
#'    (if `diff`=TRUE and `inriskdiffcol`=TRUE)
#'   \item eair_diff_ci: Confidence interval for difference in EAIR between current group and reference group
#'    (if `diff`=TRUE and `inriskdiffcol`=TRUE)
#'   \item eair_diff_est_ci: Combination of `eair_diff_est` and `eair_diff_ci`.
#' }\cr
#' The list of available statistics (core columns) can also be viewed by
#' running `junco_get_stats("a_eair100_j")`.
#' @details
#' The exposure-adjusted incidence rate (EAIR) is defined as:
#' - When (`occ_var` not `NULL`) or `count_multiple_events = FALSE`:
#' \cr the **number of subjects**
#' with at least one occurrence of a specified adverse event divided by the total
#' at-risk exposure time across all subjects.
#'
#' - When `occ_var = NULL` and `count_multiple_events = TRUE`:
#' \cr the total **number of events** across of a specified adverse event divided by the total
#' at-risk exposure time across all subjects.
#'
#' Total at-risk exposure time is calculated for each subject
#' as:
#' - When `occ_var` not `NULL`
#' \itemize{
#' \item for subjects with the event: relative day (from the start of exposure) of
#' the onset date of the **first** occurrence of the specified event (`occ_var` and `occ_dy`),
#' transformed from days into years as `occ_dy/365.25`, or
#' \item for subjects without the event: total duration of follow-up (expressed in years)
#' (e.g., treatment discontinuation, completion, or censoring) (`fup_var`).
#' }
#' - When `occ_var = NULL`\cr
#' for all subjects: total duration of follow-up (expressed in years)
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
#' Wald's method for the confidence interval approximation is used (Liu et al., 2006), which uses the standard error
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
#' \cr
#' \cr Other methods for confidence interval for difference can be specified in `conf_type_diff` argument.
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
  fup_var,
  occ_var,
  occ_dy,
  num_p_year = 100,
  count_multiple_events = FALSE,
  conf_level = 0.95,
  conf_type_diff = c("wald", "waldcc", "mn", "scas"),
  conf_type_eair = c("exact", "normal", "normal_log", "byar"),
  vs_ref_group = FALSE,
  .in_ref_col,
  cur_col_expr,
  ref_col_expr,
  inriskdiffcol = FALSE
) {
  conf_type_diff <- match.arg(conf_type_diff)
  method_part <- switch(conf_type_diff,
    "wald" = "Wald, without correction",
    "waldcc" = "Wald, with correction",
    "mn" = "Miettinen and Nurminen",
    "scas" = "skewness-corrected asymptotic score",
    stop(paste(conf_type_diff, "does not have a description"))
  )
  method_part <- paste0("(", method_part, ")")
  conf_type_eair <- match.arg(conf_type_eair)
  method_eair_part <- switch(conf_type_eair,
    "exact" = "Exact",
    "byar" = "byar",
    "normal" = "normal",
    "normal_log" = "log-normal",
    stop(paste(conf_type_eair, "does not have a description"))
  )
  method_eair_part <- paste0(" (", method_eair_part, ")")

  if (is.null(cur_col_expr)) {
    stop("cur_col_expr must be passed onto s_eair100_levii_j")
  }
  # subset .alt_df_full to current column
  .alt_df_full_cur_group <- subset(.alt_df_full, eval(cur_col_expr))
  cur_dfs <- h_get_eair_df(
    levii,
    df,
    denom_df = .alt_df_full_cur_group,
    .var = .var,
    id = id,
    fup_var = fup_var,
    occ_var = occ_var,
    occ_dy = occ_dy,
    count_multiple_events = count_multiple_events
  )
  cur_df_num <- cur_dfs$df_num
  cur_df_denom <- cur_dfs$df_denom
  ### statistics derivation
  # note: variables n_events and mod_fup_var are derived by h_get_eair_df
  # when count_multiple_events == TRUE, n_events: number of events per subject
  # when count_multiple_events == FALSE, n_events: 1 for subjects with event
  x1 <- sum(cur_df_num[["n_events"]])
  n1 <- sum(cur_df_denom[["mod_fup_var"]])
  cur_eair <- num_p_year * x1 / n1
  cur_n_eair <- c(x1, cur_eair)

  x <- list()
  eair_lbl <- paste0("eair (per ", num_p_year, " person years)")
  n_event_lbl <-
    if (is.null(occ_var) && count_multiple_events) {
      "Number of events"
    } else {
      "Number of subjects with event"
    }

  x$n_event <- c("n_event" = x1)
  x$person_years <- c("person_years" = n1)
  x$eair <- stats::setNames(c("eair" = cur_eair), "eair")
  x$n_eair <- stats::setNames(c("n_eair" = cur_n_eair), c("n_event", "eair"))
  x$eair_n <- stats::setNames(c("eair_n" = x$n_eair[c(2, 1)]), c("eair", "n_event"))
  x$eair_n_py <- stats::setNames(c("eair_n_py" = c(x$eair_n, n1)), c("eair", "n_event", "py"))

  attr(x$n_event, "label") <- n_event_lbl
  attr(x$eair, "label") <- eair_lbl
  attr(x$n_eair, "label") <- paste0(n_event_lbl, " (", eair_lbl, ")")
  attr(x$eair_n, "label") <- paste0(eair_lbl, " (", n_event_lbl, ")")
  attr(x$eair_n_py, "label") <- paste0(eair_lbl, " (", n_event_lbl, "/", "person-years", ")")

  # ci for rates - utilize tern function to derive rate_ci
  # h_incidence_rate - not exported
  h_incidence_rate <- utils::getFromNamespace("h_incidence_rate", "tern")
  tern_incid_rate <- h_incidence_rate(
    person_years = n1,
    n_events = x1,
    control = list(
      num_pt_year = num_p_year,
      input_time_unit = "year",
      conf_level = conf_level,
      conf_type = conf_type_eair
    )
  )

  x$eair_ci <- stats::setNames(c("eair_ci" = tern_incid_rate$rate_ci), "eair_ci")
  attr(x$eair_ci, "label") <- paste0(f_conf_level(conf_level), method_eair_part)

  x$eair_est_ci <- stats::setNames(c("eair_est_ci" = c(x$eair, tern_incid_rate$rate_ci)), "eair_est_ci")
  attr(x$eair_est_ci, "label") <- paste0(eair_lbl, " with (", f_conf_level(conf_level), method_eair_part, ")")


  if (vs_ref_group) {
    # perform vs ref group derivations
    if (inriskdiffcol) {
      # stats that will be blanked out in risk diff columns (only applicable to horizontal layout)
      x$n_event <- c("n_event" = NULL)
      x$person_years <- c("person_years" = NULL)
      x$eair_ci <- c("eair_ci" = NULL)
    }

    # retrieve ref_group info from ref_col_expr for both .alt_df_full and .df_row
    alt_df_full_ref_group <- subset(.alt_df_full, eval(ref_col_expr))
    ref_group <- subset(.df_row, eval(ref_col_expr))

    ref_dfs <- h_get_eair_df(
      levii,
      df = ref_group,
      denom_df = alt_df_full_ref_group,
      .var = .var,
      id = id,
      fup_var = fup_var,
      occ_var = occ_var,
      occ_dy = occ_dy,
      count_multiple_events = count_multiple_events
    )

    ref_df_num <- ref_dfs$df_num
    ref_df_denom <- ref_dfs$df_denom

    ### statistics derivation
    x2 <- sum(ref_df_num[["n_events"]])
    n2 <- sum(ref_df_denom[["mod_fup_var"]])

    ref_eair <- num_p_year * x2 / n2
    ref_n_eair <- c(x2, ref_eair)

    rdiff <- cur_eair - ref_eair
    eair_diff_est_stats <- h_s_eair_diff(x1, n1,
      x2, n2,
      conf_type = conf_type_diff,
      conf_level = conf_level
    )
    # combine est of difference with its ci
    eair_diff_est_ci <- num_p_year *
      c(eair_diff_est_stats$diff_est, eair_diff_est_stats$diff_lcl, eair_diff_est_stats$diff_ucl)

    x$eair_diff_est_ci <- stats::setNames(
      c("eair_diff_est_ci" = eair_diff_est_ci),
      nm = c("estimate", "lcl", "ucl")
    )
    attr(x$eair_diff_est_ci, "label") <-
      paste0("Difference in eair with (", f_conf_level(conf_level), ") ", method_part)

    x$eair_diff_ci <- x$eair_diff_est_ci[2:3]
    attr(x$eair_diff_ci, "label") <-
      paste0(f_conf_level(conf_level), " for difference in eair ", method_part)

    x$eair_diff_est <- x$eair_diff_est_ci[1]
    attr(x$eair_diff_est, "label") <- "Difference in eair"
  } else {
    # no risk diff calculations - set stats to NULL, ie blank
    x$eair_diff_est_ci <- c("eair_diff_est_ci" = NULL)
    x$eair_diff_est <- c("eair_diff_est" = NULL)
    x$eair_diff_ci <- c("eair_diff_ci" = NULL)
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
#' @param row_labels_adj (`logical`)\cr If `TRUE` and number of requested statistics = 1,
#' the row label on the table will reflect the level of the incoming variable rather than
#' the label for the statistic.
#' \cr Set to default `TRUE` for backward-compatibility reasons.
#' @param riskdiff_setup (`string`)\cr Are risk differences presented in horizontal way, or vertical way.
#' \cr When horizontal, the statistics for `eair`, `n_eair`, `eair_n` will be replaced by
#' `ear_diff_est_ci` in the risk difference columns.
#' If `.stats` include any of difference in eair rates statistics,
#' these will be removed as `ear_diff_est_ci` will be displayed in the risk difference columns
#'
#' @return
#'  * `a_eair100_j` returns the corresponding list with formatted [rtables::CellValue()].\cr
#' Within a risk difference column, the statistics `eair` and `n_eair` are replaced by `eair_diff_est_ci`
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
  fup_var,
  occ_var,
  occ_dy = NULL,
  num_p_year = 100,
  count_multiple_events = FALSE,
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
  conf_type_diff = c("wald", "waldcc", "mn", "scas"),
  conf_type_eair = c("normal", "normal_log", "exact", "byar"),
  row_labels_adj = FALSE,
  riskdiff_setup = c("horizontal", "vertical")
) {
  conf_type_diff <- match.arg(conf_type_diff)
  conf_type_eair <- match.arg(conf_type_eair)
  riskdiff_setup <- match.arg(riskdiff_setup)

  if (riskdiff && is.null(ref_path)) {
    stop("ref_path cannot be NULL when riskdiff = TRUE, please specify it. See ?get_ref_info for details.")
  }

  ## prepare for column based split
  cur_col_expr <- .spl_context$cur_col_expr[[1]]
  ## colid can be used to figure out if we're in the relative risk columns or not
  colid <- .spl_context$cur_col_id[[1]]
  inriskdiffcol <- grepl("difference", tolower(colid), fixed = TRUE)

  # if no stats requested, get all stats
  .stats <- junco_get_stats(
    "a_eair100_j",
    stats_in = .stats,
    custom_stats_in = NULL
  )
  if (any(.stats %in% c("eair_diff_est_ci", "eair_diff_est", "eair_diff_ci")) && riskdiff_setup == "horizontal") {
    .stats <- .stats[!(.stats %in% c("eair_diff_est_ci", "eair_diff_est", "eair_diff_ci"))]
  }

  if (!is.null(ref_path)) {
    ref <- get_ref_info(ref_path, .spl_context)
    .in_ref_col <- ref$in_ref_col
    ref_col_expr <- h_get_ref_col_expr(ref_path)
  }

  # !perform_vs_ref_stats will be passed as .in_ref_col in the s_function s_eair100_levii_j
  # while it does not entirely reflect
  # whether in ref column or not
  # it is the behaviour of performing vs ref group calculations what is important
  if (riskdiff && riskdiff_setup == "vertical" && is.null(.in_ref_col)) {
    perform_vs_ref_stats <- FALSE
  } else if (riskdiff && riskdiff_setup == "vertical" && is.logical(.in_ref_col)) {
    perform_vs_ref_stats <- !.in_ref_col
  } else if (
    riskdiff &&
      riskdiff_setup == "horizontal" &&
      inriskdiffcol &&
      !identical(df, subset(.df_row, eval(ref_col_expr)))
  ) {
    perform_vs_ref_stats <- TRUE
  } else {
    perform_vs_ref_stats <- FALSE
  }

  checkmate::assert_string(fup_var, null.ok = FALSE)
  checkmate::assert_string(occ_var, null.ok = TRUE)
  if (!is.null(occ_var)) {
    checkmate::assert_string(occ_dy, null.ok = FALSE)
  }

  checkmate::assert_names(colnames(df), must.include = c(occ_var, occ_dy))

  if (is.null(.alt_df_full)) {
    stop(paste("a_eair100_j: .alt_df_full cannot be NULL. Specify `alt_counts_df` to `build_table`"))
  }
  checkmate::assert_names(colnames(.alt_df_full), must.include = c(fup_var))

  inrowsplit <- FALSE
  if (.var == tail(.spl_context$split, 1L)) {
    # note: this will catch the rowsplit only if parent_name in split_rows_by call is .var (default)
    drop_levels <- TRUE
    inrowsplit <- TRUE
  }

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

  # gather arguments to pass onto s_function
  fn_Args <- list(
    df = df,
    .df_row = .df_row,
    .var = .var,
    id = id,
    fup_var = fup_var,
    occ_var = occ_var,
    occ_dy = occ_dy,
    num_p_year = num_p_year,
    count_multiple_events = count_multiple_events,
    cur_col_expr = cur_col_expr,
    ref_col_expr = ref_col_expr,
    vs_ref_group = perform_vs_ref_stats,
    .in_ref_col = !perform_vs_ref_stats,
    inriskdiffcol = riskdiff_setup == "horizontal" && inriskdiffcol,
    conf_level = conf_level,
    conf_type_eair = conf_type_eair,
    .alt_df_full = .alt_df_full,
    conf_type_diff = conf_type_diff
  )

  levs <- levels(.df_row[[.var]])
  y <- mapply(
    s_eair100_levii_j,
    levii = levs,
    MoreArgs = fn_Args,
    SIMPLIFY = FALSE
  )

  ### rearrange list y to  list to x_stats
  # y is a nested list with
  # outer elements levels of incoming variable
  # inner elements stat elements statistics from

  #### this is to ensure the remainder of the code can stay the same as in a_freq_j
  stnms <- junco_get_stats("a_eair100_j")
  x_stats <- extract_x_stats(y, stnms)

  # x_stats is a nested list with
  # outer elements stat elements statistics (all 5 from stnms)
  # inner elements levels of incoming variable
  # this will ensure we can perform subsetting on .stats
  # like x_stats[.stats]

  # transpose_named_list cannot be used as this will not ensure all 5 poss stats are in
  if (inriskdiffcol && riskdiff_setup == "horizontal") {
    .stats_adj <- replace(.stats, .stats %in% c("eair", "n_eair", "eair_n", "eair_n_py"), "eair_diff_est_ci")
  } else {
    .stats_adj <- .stats
  }

  .stats <- .stats_adj

  # Fill in formatting defaults


  x_stats <- x_stats[.stats]
  x_stats_orig <- x_stats
  # from now onwards, retransform x_stats back into structure
  # list with outer elements stats levels from incoming variable
  # inner elements statistics
  x_stats <- transpose_named_list(x_stats)

  levels_per_stats <- lapply(x_stats, names)
  # in case no default label get the label from the x_stats directly
  # this is to cover statistics with dynamic label, such as
  # n_event Either Number of events or Number of subjects with events
  .labels_pre <- junco_get_labels_from_attrib(.stats, .labels, x_stats)

  .formats <- junco_get_formats_from_stats(.stats, .formats, levels_per_stats)
  .labels <- junco_get_labels_from_stats(.stats, .labels_pre, levels_per_stats)
  .labels <- .unlist_keep_nulls(.labels)

  # make adjustments to rowlabels  when not inrowsplit
  if (!inrowsplit && length(.stats) > 1) {
    # if more than one stat requested and variable is not in a prior splitrowscall
    # prepend the level of the incoming variable (which is now in names of x_stats) to the label
    .labels <- paste(rep(names(x_stats), each = length(.stats)), .labels)
  } else if (!inrowsplit && length(.stats) == 1 && row_labels_adj) {
    # if one stat requested and variable is not in a prior splitrowscall
    # show the levels of the incoming variable, rather than the statistic
    .labels <- names(x_stats)
  }
  .labels <- gsub("^\\s+", "", .labels)

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
