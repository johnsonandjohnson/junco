#' Stratified Exposure-Adjusted Incidence Rate (EAIR)
#' @name a_eair_strat_j
#' @description
#' Statistical/Analysis function for presenting a stratified exposure-adjusted
#' incidence rate (EAIR) and its Wald confidence interval across strata
#' (e.g., study size weighted exposure).
#'
#' This function is an extension to the unstratified function `a_eair100_j()`.
#'
#' @details
#' The weighting method uses raw event counts `n_ti` in both the rate and variance.
#'   \deqn{\hat r_{ti} = \texttt{num\_p\_year} \times \frac{n_{ti}}{T_{ti}}}
#'
#'
#' The strata weight is `w_i = T_0i + T_1i` (total person-years across
#' **all** groups in strata `i`), and the pooled weighted rate is:
#' \deqn{\hat r_t = \sum_i \frac{w_i}{W} \hat r_{ti}, \quad W = \sum_i w_i}
#'
#' The Wald 95% CI for the pooled rate is:
#' \deqn{\hat r_t \pm z_{1-\alpha/2} \times \frac{\texttt{num\_p\_year}}{W}
#'   \sqrt{\sum_i w_i^2 \frac{n_{ti}}{T_{ti}^2}}}.
#'
#' The weighted rate difference and its Wald CI are:
#' \deqn{\hat\delta = \sum_i \frac{w_i}{W}(\hat r_{1i} - \hat r_{0i})}
#' \deqn{\hat\delta \pm z_{1-\alpha/2} \times \frac{\texttt{num\_p\_year}}{W}
#'   \sqrt{\sum_i w_i^2 \left(\frac{n_{1i}+c}{T_{1i}^2} + \frac{n_{0i}}{T_{0i}^2}\right)}}
#'
#' @inherit a_eair100_j params
#' @inherit s_eair100_levii_j params
#'
#' @param strata (`string`)\cr variable identifying strata (e.g., study identifier).
#'
#' @return
#' * `s_eair_strat_levii_j()` returns a list containing the following statistics:
#' \itemize{
#'   \item `n_event_total`: total event count across strata (current arm)
#'   \item `person_years_total`: total person-years across strata (current arm)
#'   \item `eair_strat`: stratified pooled EAIR per `num_p_year` person-years
#'   \item `n_eair_strat`: composite of `n_event_total` and `eair_strat`
#'   \item `eair_strat_ci`: Wald CI for `eair_strat`
#'   \item `eair_strat_est_ci`: composite of `eair_strat` and `eair_strat_ci`
#'   \item `eair_strat_n`: composite of `eair_strat` and `n_event_total`
#'   \item `eair_strat_n_py`: composite of `eair_strat`, `n_event_total` and `person_years_total`
#'   \item `eair_strat_diff_est_ci`: stratified rate difference with Wald CI
#'     (only when `vs_ref_group = TRUE`, otherwise `NULL`)
#'   \item `eair_strat_diff_est`: estimate for the difference in stratified rate
#'   \item `eair_strat_diff_ci`: CI (Wald type) for the difference in stratified rate
#' }\cr
#' The list of available statistics (core columns) can also be viewed by
#' running `junco_get_stats("a_eair_strat_j")`.
#' @seealso [a_eair100_j()]
#' @references
#' Scosyrev, E & Pethe, A (2022). Confidence intervals for exposure-adjusted rate
#' differences in randomized trials.
#' Pharmaceutical Statistics, 2022;21:103–121.
#' doi: 10.1002/pst.2155
#'
#' Crowe, B., Chuang-Stein, C., Lettis, S., & Brueckner, A. (2016).
#' Reporting adverse drug reactions in product labels.
#' Therapeutic Innovation & Regulatory Science, 50(4), 455-463.
#' doi: 10.1177/2168479016628574


#'
#'
#' @keywords internal
s_eair_strat_levii_j <- function(
  levii,
  df,
  .df_row,
  .alt_df_full,
  .var,
  strata,
  id = "USUBJID",
  fup_var,
  occ_var = NULL,
  occ_dy = NULL,
  num_p_year = 100,
  count_multiple_events = FALSE,
  conf_level = 0.95,
  vs_ref_group = FALSE,
  .in_ref_col,
  cur_col_expr,
  ref_col_expr,
  inriskdiffcol = FALSE
) {
  checkmate::assert_string(strata)
  checkmate::assert_string(fup_var)
  checkmate::assert_string(occ_var, null.ok = TRUE)
  checkmate::assert_flag(vs_ref_group)
  if (!is.null(occ_var)) {
    checkmate::assert_string(occ_dy, null.ok = FALSE)
  }
  checkmate::assert_names(colnames(.alt_df_full), must.include = c(id, fup_var, strata))

  denom_df_full_mod <- h_get_eair_df(
    levii = levii,
    df = .df_row,
    denom_df = .alt_df_full,
    .var = .var,
    id = id,
    fup_var = fup_var,
    occ_var = occ_var,
    occ_dy = occ_dy,
    count_multiple_events = count_multiple_events,
    strata = strata
  )
  denom_df_full_mod <- denom_df_full_mod$df_denom

  # study weights from FULL denominator (all arms combined per study)
  w_by_strata <- tapply(
    denom_df_full_mod[["mod_fup_var"]],
    denom_df_full_mod[[strata]],
    sum,
    na.rm = TRUE
  )
  strata_levels <- names(w_by_strata)

  # gather common arguments for .get_stratum
  .alt_df_full_cur_group <- subset(.alt_df_full, eval(cur_col_expr))
  args_get_stratum <- list(
    strata = strata,
    strata_levels = strata_levels,
    levii = levii,
    .var = .var,
    id = id,
    fup_var = fup_var,
    occ_var = occ_var,
    occ_dy = occ_dy,
    count_multiple_events = count_multiple_events
  )
  stratum_stats <- do.call(
    .get_stratum_stats,
    args = append(
      list(
        df_arm = df,
        denom_arm = .alt_df_full_cur_group
      ),
      args_get_stratum
    )
  )

  # gather common arguments for h_s_eair_strat_numbers
  args_get_numbers <- list(
    strata = strata,
    w_by_strata = w_by_strata,
    num_p_year = num_p_year
  )

  curcore <- do.call(
    h_s_eair_strat_numbers,
    args = append(
      list(stratum_stats = stratum_stats),
      args_get_numbers
    )
  )

  n_vec <- curcore$n_vec
  py_vec <- curcore$py_vec
  r_vec <- curcore$r_vec
  w_vec <- curcore$w_vec

  .stats <- h_s_eair_strat_estimates(
    n_vec = n_vec,
    py_vec = py_vec,
    r_vec = r_vec,
    w_vec = w_vec,
    conf_level = conf_level,
    num_p_year = num_p_year
  )

  # rate difference vs reference arm
  diff_stats <- list()
  if (vs_ref_group) {
    ref_denom_df <- subset(.alt_df_full, eval(ref_col_expr))
    ref_df <- subset(.df_row, eval(ref_col_expr))

    ref_stratum_stats <- do.call(
      .get_stratum_stats,
      args = append(
        list(
          df_arm = ref_df,
          denom_arm = ref_denom_df
        ),
        args_get_stratum
      )
    )

    refcore <- do.call(
      h_s_eair_strat_numbers,
      args = append(
        list(stratum_stats = ref_stratum_stats),
        args_get_numbers
      )
    )

    ref_n_vec <- refcore$n_vec
    ref_py_vec <- refcore$py_vec
    ref_r_vec <- refcore$r_vec

    diff_stats <- h_s_eair_strat_diff_estimates(
      n_vec = n_vec,
      py_vec = py_vec,
      r_vec = r_vec,
      w_vec = w_vec,
      ref_n_vec = ref_n_vec,
      ref_py_vec = ref_py_vec,
      ref_r_vec = ref_r_vec,
      num_p_year = num_p_year,
      conf_level = conf_level
    )
  } else {
    diff_stats$eair_strat_diff_est_ci <- numeric(0)
  }

  attr(diff_stats$eair_strat_diff_est_ci, "label") <- paste0(
    "Difference in stratified EAIR (",
    formatC(conf_level * 100, format = "f", digits = 0), "% CI, Wald)"
  )

  .stats <- append(.stats, diff_stats)
  .stats
}

#' @describeIn a_eair_strat_j
#' Formatted analysis function for stratified (eg study size-weighted) EAIR, used as `afun`
#' in [rtables::analyze()].
#'
#'
#' @return
#' * `a_eair_strat_j()` returns a list of formatted [rtables::CellValue()] rows.
#' Within a risk difference column,
#' the statistics `eair_strat` and `n_eair_strat` are replaced by `eair_strat_diff_est_ci`
#' (difference in EAIR between current group and reference group,
#' together with it's confidence interval).
#' @export
#'
#' @examples
#' library(tern)
#' library(dplyr)
#'
#' set.seed(42)
#' n <- 120
#' adsl <- data.frame(
#'   USUBJID = paste0("S", seq_len(n)),
#'   ARM = rep(c("Treatment", "Placebo"), each = n / 2),
#'   STUDY = rep(c("Study1", "Study2", "Study3"), times = n / 3),
#'   TRTDURY = runif(n, 0.5, 3),
#'   stringsAsFactors = FALSE
#' )
#' adae <- data.frame(
#'   USUBJID = sample(adsl$USUBJID, 40),
#'   AEDECOD = "Headache",
#'   ASTDY = sample(30:500, 40, replace = TRUE),
#'   AOCCPFL = "Y",
#'   stringsAsFactors = FALSE
#' )
#' aefup <- merge(adae, adsl, by = "USUBJID")
#'
#' # Standard method (matches rdcomaeinci SAS macro)
#' lyt <- basic_table() |>
#'   split_cols_by("ARM") |>
#'   analyze(
#'     "AEDECOD",
#'     afun = a_eair_strat_j,
#'     extra_args = list(
#'       strata = "STUDY",
#'       fup_var = "TRTDURY",
#'       occ_var = "AOCCPFL",
#'       occ_dy = "ASTDY",
#'       .stats = c("eair_strat", "eair_strat_ci")
#'     )
#'   )
#' result <- build_table(lyt, aefup, alt_counts_df = adsl)
#' result
a_eair_strat_j <- function(
  df,
  labelstr = NULL,
  .var,
  .df_row,
  .spl_context,
  .alt_df_full = NULL,
  id = "USUBJID",
  strata,
  fup_var,
  occ_var = NULL,
  occ_dy = NULL,
  num_p_year = 100,
  count_multiple_events = FALSE,
  drop_levels = FALSE,
  riskdiff = FALSE,
  ref_path = NULL,
  conf_level = 0.95,
  .stats = "eair_strat",
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL,
  na_str = rep("NA", 3),
  row_labels_adj = FALSE,
  riskdiff_setup = c("horizontal", "vertical")
) {
  riskdiff_setup <- match.arg(riskdiff_setup)

  if (is.null(.alt_df_full)) {
    stop("a_eair_strat_j: .alt_df_full cannot be NULL. Specify `alt_counts_df` in `build_table`.")
  }
  if (riskdiff && is.null(ref_path)) {
    stop("a_eair_strat_j: ref_path cannot be NULL when riskdiff = TRUE.")
  }
  checkmate::assert_string(strata)
  checkmate::assert_names(colnames(.alt_df_full), must.include = c(id, fup_var, strata))

  # if no stats requested, get all stats
  .stats <- junco_get_stats(
    "a_eair_strat_j",
    stats_in = .stats,
    custom_stats_in = NULL
  )
  if (
    any(
      .stats %in% c("eair_strat_diff_est_ci", "eair_strat_diff_est", "eair_strat_diff_ci")
    ) &&
      riskdiff_setup == "horizontal"
  ) {
    .stats <-
      .stats[!(.stats %in% c("eair_strat_diff_est_ci", "eair_strat_diff_est", "eair_strat_diff_ci"))]
  }

  cur_col_expr <- .spl_context$cur_col_expr[[1]]
  colid <- .spl_context$cur_col_id[[1]]
  inriskdiffcol <- grepl("difference", tolower(colid), fixed = TRUE)

  # reference group expressions and data (for rate difference)
  ref_col_expr <- NULL
  ref_df <- NULL
  ref_denom_df <- NULL
  .in_ref_col <- NULL

  ref_info <- get_ref_info_expanded(
    df,
    .var,
    .df_row,
    .spl_context,
    ref_path,
    riskdiff,
    riskdiff_setup
  )

  perform_vs_ref_stats <- ref_info$perform_vs_ref_stats
  ref_col_expr <- ref_info$ref_col_expr
  .in_ref_col <- ref_info$.in_ref_col

  if (riskdiff) {
    ref_denom_df <- subset(.alt_df_full, eval(ref_col_expr))
    ref_df <- subset(.df_row, eval(ref_col_expr))
  }

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

  levs <- levels(.df_row[[.var]])
  if (is.null(levs)) levs <- unique(.df_row[[.var]])

  fn_Args <- list(
    strata = strata,
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
    .alt_df_full = .alt_df_full
  )

  y <- mapply(
    s_eair_strat_levii_j,
    levii = levs,
    MoreArgs = fn_Args,
    SIMPLIFY = FALSE
  )

  ### rearrange list y to  list to x_stats
  # y is a nested list with
  # outer elements levels of incoming variable
  # inner elements stat elements statistics from

  #### this is to ensure the remainder of the code can stay the same as in a_freq_j
  stnms <- junco_get_stats("a_eair_strat_j")
  x_stats <- extract_x_stats(y, stnms)

  # x_stats is a nested list with
  # outer elements stat elements statistics (all 5 from stnms)
  # inner elements levels of incoming variable
  # this will ensure we can perform subsetting on .stats
  # like x_stats[.stats]

  # transpose_named_list cannot be used as this will not ensure all 5 poss stats are in
  if (inriskdiffcol && riskdiff_setup == "horizontal") {
    .stats_adj <-
      replace(
        .stats,
        .stats %in%
          c("eair_strat", "n_eair_strat", "eair_strat_n", "eair_strat_n_py"),
        "eair_strat_diff_est_ci"
      )
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
}


h_s_eair_strat_numbers <- function(strata,
                                   w_by_strata,
                                   stratum_stats,
                                   num_p_year) {
  w_vec <- as.numeric(w_by_strata)
  W <- sum(w_vec)
  cur_stats <- stratum_stats
  n_vec <- sapply(cur_stats, `[[`, "n")
  py_vec <- sapply(cur_stats, `[[`, "py")

  # per-study rates
  r_vec <- num_p_year * (n_vec) / py_vec

  return(list(
    n_vec = n_vec,
    py_vec = py_vec,
    r_vec = r_vec,
    w_vec = w_vec
  ))
}

h_s_eair_strat_estimates <- function(n_vec,
                                     py_vec,
                                     r_vec,
                                     w_vec,
                                     conf_level,
                                     num_p_year) {
  W <- sum(w_vec)

  # weighted pooled rate
  eair_strat <- if (W > 0) sum(w_vec * r_vec) / W else NA_real_

  # Wald variance for pooled rate: (num_p_year/W)^2 * sum(w_i^2 * (n_i) / T_i^2)
  root_sum <- sum(w_vec^2 * (n_vec) / py_vec^2)
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  se_rate <- (num_p_year / W) * sqrt(root_sum)
  eair_ci <- c(max(0, eair_strat - z * se_rate), eair_strat + z * se_rate)
  n_total <- sum(n_vec)
  py_total <- sum(py_vec)

  # assemble return list
  x <- list()

  x$n_event_total <- stats::setNames(n_total, "n_event_total")
  attr(x$n_event_total, "label") <- "n (events)"

  x$person_years_total <- stats::setNames(py_total, "person_years_total")
  attr(x$person_years_total, "label") <- "Total person-years"

  x$eair_strat <- stats::setNames(eair_strat, "eair_strat")
  attr(x$eair_strat, "label") <- paste0(
    "Stratified EAIR per ", num_p_year, " PY"
  )

  x$n_eair_strat <- stats::setNames(
    c(n_total, eair_strat),
    c("n_event_total", "eair_strat")
  )
  attr(x$n_eair_strat, "label") <- paste0("n (stratified EAIR per ", num_p_year, " PY)")

  x$eair_strat_ci <- stats::setNames(eair_ci, c("lcl", "ucl"))
  attr(x$eair_strat_ci, "label") <- paste0(
    formatC(conf_level * 100, format = "f", digits = 0), "% CI (Wald)"
  )

  x$eair_strat_est_ci <- stats::setNames(
    c(eair_strat, eair_ci),
    c("eair_strat", "lcl", "ucl")
  )
  attr(x$eair_strat_est_ci, "label") <- paste0(
    "Stratified EAIR (", formatC(conf_level * 100, format = "f", digits = 0), "% CI)"
  )

  x$eair_strat_n <- stats::setNames(
    c(eair_strat, n_total),
    c("eair_strat", "n_event_total")
  )
  attr(x$eair_strat_n, "label") <- paste0("stratified EAIR per ", num_p_year, " PY (n)")

  x$eair_strat_n_py <- stats::setNames(
    c(eair_strat, n_total),
    c("eair_strat", "n_event_total")
  )
  attr(x$eair_strat_n_py, "label") <- paste0("stratified EAIR per ", num_p_year, " PY (n/person-years)")

  x
}

h_s_eair_strat_diff_estimates <- function(n_vec,
                                          py_vec,
                                          r_vec,
                                          w_vec,
                                          ref_n_vec,
                                          ref_py_vec,
                                          ref_r_vec,
                                          num_p_year,
                                          conf_level) {
  W <- sum(w_vec)
  z <- stats::qnorm(1 - (1 - conf_level) / 2)

  d_vec <- r_vec - ref_r_vec
  d_adj <- if (W > 0) sum(w_vec * d_vec) / W else NA_real_

  root_diff <- sum(w_vec^2 * ((n_vec) / py_vec^2 + (ref_n_vec) / ref_py_vec^2))
  se_diff <- (num_p_year / W) * sqrt(root_diff)

  diff_ci <- c(d_adj - z * se_diff, d_adj + z * se_diff)

  x <- list()
  x$eair_strat_diff_est_ci <- stats::setNames(
    c(d_adj, diff_ci),
    c("estimate", "lcl", "ucl")
  )
  attr(x$eair_strat_diff_est_ci, "label") <- paste0(
    "Difference in stratified EAIR (",
    formatC(conf_level * 100, format = "f", digits = 0), "% CI, Wald)"
  )

  x$eair_strat_diff_est <- stats::setNames(
    c(d_adj),
    c("estimate")
  )
  attr(x$eair_strat_diff_est, "label") <- "Difference in stratified EAIR"

  x$eair_strat_diff_ci <- stats::setNames(
    c(diff_ci),
    c("lcl", "ucl")
  )
  attr(x$eair_strat_diff_ci, "label") <- paste0(
    formatC(conf_level * 100, format = "f", digits = 0), "% CI (Wald)"
  )

  x
}
