#' @keywords internal
invert_list <- function(x, simplify = FALSE) {
  stopifnot(is.list(x), !is.null(names(x)))
  vals <- unlist(x, use.names = FALSE)
  parents <- rep(names(x), lengths(x))
  out <- split(parents, vals)
  if (simplify && all(lengths(out) == 1L)) {
    out <- unlist(out, use.names = TRUE)
  }
  out
}

#' @keywords internal
all_stats_junco_method <- junco_default_stats

#' @keywords internal
all_stats_junco_stat <- invert_list(all_stats_junco_method)

#' @keywords internal
junco_def_d_all_start <- get_fmt_details(junco_default_formats)

# add method_group information
#' @keywords internal
junco_def_d_all_start$method_group <- all_stats_junco_stat[junco_def_d_all_start$stat]

#' @keywords internal
str_not_d_style <- c(
  "xx%", "xx.x%", "xx.xx%", "xx.xxx%", "(N=xx)", ">999.9", ">999.99", "x.xxxx | (<0.0001)",
  "N=xx (xx%)", "xx (xx%)", "xx (xx.%)", "xx (xx.x%)", "xx (xx.xx%)", "xx. (xx.%)", "xx.x (xx.x%)", "xx.xx (xx.xx%)",
  "xx / xx (xx.%)", "xx / xx (xx.x%)", "xx / xx (xx.xx%)",
  "xx.", "xx"
)

#' @keywords internal
stat_not_d_style <- c(
  "additional_title_row",
  "all",
  "count",
  "count_fraction",
  "count_fraction_fixed_dp",
  "count_unique",
  "count_unique_denom_fraction",
  "count_unique_fraction",
  "denom",
  "diff_means_n1",
  "diff_means_n2",
  "eair",
  "eair_ci",
  "eair_diff_ci",
  "eair_diff_est",
  "eair_diff_est_ci",
  "eair_n",
  "eair_strat_diff_est_ci",
  "eair_strat_est_ci",
  "eair_strat_n",
  "eair_strat_n_py",
  "eair_est_ci",
  "eair_n_py",
  "eair_strat",
  "eair_strat_ci",
  "event_free_ci",
  "event_free_rate",
  "event_free_rate_3d",
  "fraction",
  "hr",
  "hr_ci",
  "hr_ci_3d",
  "lr_stat_df",
  "mean_pval",
  "n",
  "n_altdf",
  "n_blq",
  "n_df",
  "n_eair",
  "n_eair_strat",
  "n_event",
  "n_event_total",
  "n_events",
  "n_fit",
  "n_parentdf",
  "n_patients",
  "n_prop",
  "n_rate",
  "n_rowdf",
  "n_rsp",
  "n_tot",
  "n_tot_events",
  "n_unique",
  "nonunique",
  "or",
  "or_ci",
  "p_value",
  "patyrs",
  "person_years",
  "person_years_total",
  "prop",
  "prop_ci",
  "pt_at_risk",
  "pval",
  "pval_inter",
  "pvalue",
  "rate",
  "rate_ci",
  "rate_diff",
  "rate_diff_ci",
  "rate_diff_ci_3d",
  "rate_ratio",
  "rate_ratio_ci",
  "rate_se",
  "rel_risk_ci",
  "riskdiff",
  "rr_ci_3d",
  "total_subject_years",
  "unique",
  "unique_count",
  "ztest_pval"
)


#' @keywords internal
target_d_style <- within(junco_def_d_all_start, {
  target_d <- fun_fact == "jjcsformat_xx" |
    (fmt_package == "formatters" & !(str %in% str_not_d_style))

  target_d[stat %in% stat_not_d_style] <- FALSE

  # d = 1 corresponds to default
  str_d <- ifelse(
    target_d,
    gsub("xx.x", "xx.d", str, fixed = TRUE),
    ifelse(!is_fun, str, NA)
  )
})

# proposed default formats for d-style processing ----------------
junco_def_d_all1 <- target_d_style[["fun"]]
junco_def_d_all1[!is.na(target_d_style[["str_d"]])] <-
  target_d_style[["str_d"]][!is.na(target_d_style[["str_d"]])]
names(junco_def_d_all1) <- target_d_style[["stat"]]

#' @describeIn fmt_spec_d Named vector of default format specification using d-style notation.
#'
#' @format  * `junco_def_d_all` Used as default input vector for argument `fmt_d_def`
#' in functions `fmt_spec_single_d()`, `fmt_spec_df_d()` and `fmt_spec_var_d()`
#' (corresponds to the named vector of with default available formats [junco_default_formats]
#'  with d = 1)
#' @export
junco_def_d_all <- junco_def_d_all1

# proposed alternative 1 ----------------
junco_def_d_alt1 <- junco_def_d_all
junco_def_d_alt1[!is.na(target_d_style[["str_d"]])] <-
  gsub("xx.dxx", "xx.dx", junco_def_d_alt1[!is.na(target_d_style[["str_d"]])], fixed = TRUE)


#' @describeIn fmt_spec_d Alternative vector of d-style specification defaults.
#' @format * `junco_def_d_alt1` Alternative vector of d-style specification defaults.
#' @export
junco_def_d_alt1 <- junco_def_d_alt1

# proposed alternative 2 ----------------
junco_def_d_alt2 <- junco_def_d_alt1
junco_def_d_alt2[!is.na(target_d_style[["str_d"]])] <-
  gsub("xx.dx", "xx.d", junco_def_d_alt2[!is.na(target_d_style[["str_d"]])], fixed = TRUE)


#' @describeIn fmt_spec_d Another alternative vector of d-style specification defaults.
#' @format * `junco_def_d_alt2` Another lternative vector of d-style specification defaults.
#' @export
junco_def_d_alt2 <- junco_def_d_alt2
