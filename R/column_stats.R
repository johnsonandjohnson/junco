calc_one_visit_j <- function(datvec, statnm, visit, varnm, exclude_visits,
                             var_names = c("AVAL", "CHG", "BASE")) {
  checkmate::assert_string(statnm)
  checkmate::assert_choice(statnm, tern::get_stats(method_groups = "analyze_vars_numeric"))
  if ((varnm == var_names[2] || varnm == var_names[3]) && (visit %in% exclude_visits)) {
    stat <- list(NULL)
    names(stat) <- statnm
  } else {
    stats <- tern::s_summary(datvec)
    stat <- stats[statnm]
  }
  stat
}

#' @name column_stats
#' @title Statistics within the column space
#' @description
#' A function used for obtaining statistics within the columns of your table.
#' Used in change from baseline tables. This takes the visit names as its row labels.
#' @importFrom assertthat is.string
#' @param exclude_visits (`character vector`)\cr Vector of visit(s) for which you do not want the statistics displayed
#' in the baseline mean or change from baseline sections of the table.
#' @param var_names (`character vector`)\cr Vector of variable names to use instead of the default AVAL, CHG, BASE.
#' By default, the function expects these specific variable names in your data,
#' but you can customize them to match your dataset's column names.
#' @param .formats (named `character` or `list`)\cr formats for the statistics.
#' When set to `"default"` `formats_var` argument will be used for format setup.
#' @param formats_var (`string` or `NULL`)\cr `NULL` (the default) or the name of the list column containing named
#' lists of default formats to use. These will not override .formats (other than `"default"`).
#' @param exclude_visits Visits to exclude for the second/third part of the columns ("BASE", "CHG").
#' @param .spl_context (`data.frame`)\cr gives information about ancestor split states.
#' @param df (`data.frame` or `tibble`)\cr dataset.
#' @param .var (`string`)\cr single variable name that is passed by `rtables`
#' @return An analysis function (for use with [rtables::analyze]) implementing
#'   the specified statistics.\cr
#'   Typically used in a layout that has been setup by [stats_in_cols_setup()]
#' @examples
#' # example code
#' library(dplyr)
#' advs <- ex_advs |>
#'   filter(AVISIT %in% toupper(c("Screening", "Baseline", "Week 1 Day 8", "Week 2 Day 15"))) |>
#'   mutate(AVISIT = droplevels(AVISIT)) |>
#'   filter(PARAMCD %in% c("DIABP", "SYSBP"))
#' advs_fmt <- tribble(
#'   ~PARAMCD, ~fmt_d,
#'   "DIABP", list(
#'     mean = "xx.xx", sd = "xx.xxx", se = "xx.xxx", median = "xx.xx",
#'     min = "xx.x", max = "xx.x", mean_sd = "xx.xx (xx.xxx)"
#'   ),
#'   "SYSBP", list(
#'     mean = "xx.x", sd = "xx.xx", se = "xx.xx", median = "xx.x",
#'     min = "xx.", max = "xx.", mean_sd = "xx.x (xx.xx)"
#'   )
#' )
#'
#' advs <- left_join(advs, advs_fmt)
#'
#' mysplitfun <- make_split_fun(
#'   post = list(stats_in_cols_setup())
#' )
#'
#' lyt <- basic_table() |>
#'   split_cols_by_multivar(
#'     c("AVAL", "BASE", "CHG"),
#'     varlabels = c("Visit Values", " ", "Change from Baseline")
#'   ) |>
#'   split_cols_by("STUDYID", split_fun = mysplitfun) |>
#'   split_rows_by("PARAM", split_fun = drop_split_levels) |>
#'   split_rows_by("ARMCD", labels_var = "ARM") |>
#'   analyze("AVISIT", afun = column_stats, extra_args = list(
#'     exclude_visits = toupper(c("Baseline", "Screening")),
#'     .formats = "default",
#'     formats_var = "fmt_d"
#'   ))
#' result <- build_table(lyt, advs, round_type = "sas")
#' head(result, 20)
#' @export
column_stats <- function(df, .var, .spl_context,
                         .formats = NULL,
                         formats_var = NULL,
                         exclude_visits = c("Baseline (DB)"),
                         var_names = c("AVAL", "CHG", "BASE")) {
  allcolsplvals <- .spl_context[nrow(.spl_context), "cur_col_split_val"][[1]]
  statnm <- utils::tail(allcolsplvals, 1)
  varnm <- allcolsplvals[length(allcolsplvals) - 1]
  datvec <- df[[varnm]]
  datpervis <- split(datvec, df[[.var]]) ## ,var is AVISIT
  # derivation of statistics - current column - different rows for avisit
  stats_rows <- mapply(
    calc_one_visit_j,
    datvec = datpervis,
    visit = names(datpervis),
    MoreArgs = list(
      statnm = statnm,
      varnm = varnm,
      exclude_visits = exclude_visits,
      var_names = var_names
    )
  )
  # get appropriate format for statistic -
  if (identical(.formats, "default") && assertthat::is.string(formats_var)) {
    checkmate::assert_names(colnames(df), must.include = formats_var)
    myfmt <- df[[formats_var]][[1]][[statnm]]
  } else {
    .formats <- junco_get_formats_from_stats(
      stats = statnm,
      formats_in = .formats
    )
    myfmt <- .formats[[statnm]]
  }
  # inrows with appropriate formatting
  in_rows(
    .list = stats_rows,
    .names = names(datpervis),
    .formats = myfmt,
    .labels = names(datpervis)
  )
}

calc_N <- function(datvec, statnm, trt, varnm) {
  if (varnm != "AVAL") {
    return(NULL)
  }
  length(stats::na.omit(datvec))
}

#' Function factory to be used as Post-processing split function for setup of stats in column structure
#'
#' @description
#' This helper is designed to be used in the `post` argument of
#' [rtables::make_split_fun()] to expand a column facet (e.g. AVAL/BASE/CHG)
#' into the specific statistics to be analyzed for each subfacet. It returns a
#' split result instructing rtables which values/labels/subsets to create.
#'
#' Typical usage:
#'
#' -  construct a split function like `mysplitfun <- rtables::make_split_fun(post = list(stats_in_cols_setup()))`\cr
#' Then use `mysplitfun` in your table layout where you split columns by a
#' variable whose levels are one of "AVAL", "BASE", or "CHG" and want to
#' analyze different statistics for each.
#'
#' - column layout setup from
#' [rtables::split_cols_by_multivar()], followed by split_cols_by using `stats_in_cols_setup` in
#' conjunction with an analyze call with `afun` = [column_stats()]
#'
#' See examples for typical usage.
#'
#' @param stats_list `(list)` Named list with statistics to present for each variable.
#' Any statistic from tern::s_summary can be selected.
#' @param statlbls_list `` Named vector with labels to use for the statistic in column header.
#' If NULL, or a statistic name is not included, the default label for the statistic will be used.
#' @return A function that can be used inside [rtables::make_split_result()]
#'   statistics for the current column level.
#' @seealso [rtables::make_split_fun()], [rtables::make_split_result()].
#' @examples
#' # example code
#' library(dplyr)
#' advs <- ex_advs |>
#'   filter(AVISIT %in% toupper(c("Screening", "Baseline", "Week 1 Day 8", "Week 2 Day 15"))) |>
#'   mutate(AVISIT = droplevels(AVISIT))
#'
#' mysplitfun <- make_split_fun(
#'   post = list(stats_in_cols_setup())
#' )

#' lyt <- basic_table() |>
#'   split_cols_by_multivar(
#'     c("AVAL", "BASE", "CHG"),
#'     varlabels = c("Visit Values", " ", "Change from Baseline")
#'   ) |>
#'   split_cols_by("STUDYID", split_fun = mysplitfun) |>
#'   split_rows_by("PARAM") |>
#'   split_rows_by("ARMCD", labels_var = "ARM") |>
#'   analyze("AVISIT",
#'    afun = column_stats,
#'    extra_args = list(exclude_visits = toupper(c("Baseline", "Screening")))
#'   )
#' result <- build_table(lyt, advs, round_type = "sas")
#' head(result, 20)
#'
#' @export
stats_in_cols_setup <- function(
  stats_list = list(
    "AVAL" = c("n", "mean", "sd", "se", "median", "min", "max"),
    "BASE" = c("mean_sd"),
    "CHG" = c("n", "mean", "sd", "se", "median", "min", "max")
  ),
  statlbls_list = c(median = "Med", min = "Min", max = "Max", mean_sd = "Base Mean (SD)", n = "N")
) {
  function(ret, spl, fulldf, .spl_context) {
    all_expr <- expression(TRUE)
    colset <- .spl_context[nrow(.spl_context), "value"][[1]]

    if (!(colset %in% names(stats_list))) {
      stop(paste("something bad happened :(",
        paste0("incoming Variable to analyze (", colset, ")"),
        paste0("is not named in stats_list - names: ", paste0(names(stats_list), collapse = ", ")),
        sep = "\n"
      ))
    }

    stats <- stats_list[[colset]]
    statlbl <- junco_get_labels_from_stats(
      stats = stats,
      labels_in = statlbls_list,
      levels_per_stats = NULL,
      label_attr_from_stats = NULL
    )

    vals <- stats
    lbls <- statlbl[stats]
    datasplit <- rep(list(fulldf), length(stats))
    names(datasplit) <- stats
    subset_exprs <- rep(list(all_expr), length(stats))

    ret <- make_split_result(
      values = vals,
      labels = lbls,
      datasplit = datasplit,
      subset_exprs = subset_exprs
    )

    ret
  }
}
