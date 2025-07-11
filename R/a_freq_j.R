#' @name a_freq_j
#'
#' @title Analysis/statistical function for count and percentage in core columns
#' and (optional) relative risk columns
#'
#' @inheritParams proposal_argument_convention
#' @param val (`character` or NULL)\cr
#' When NULL, all levels of the incoming variable (variable used in the `analyze` call)
#' will be considered.\cr
#' When a single `string`, only that current level/value of the incoming variable
#' will be considered.\cr
#' When multiple levels, only those levels/values of the incoming variable
#' will be considered.\cr
#' When no values are observed (eg zero row input df),
#' a row with row-label `No data reported` will be included in the table.
#' @param drop_levels (`logical`)\cr If `TRUE` non-observed levels
#' (based upon .df_row) will not be included.\cr
#' Cannot be used together with `val`.
#' @param excl_levels (`character` or NULL)\cr
#' When NULL, no levels of the incoming variable (variable used in the `analyze` call)
#' will be excluded.\cr
#' When multiple levels, those levels/values of the incoming variable
#' will be excluded.\cr
#' Cannot be used together with `val`.
#' @param new_levels (list(2) or NULL)\cr List of length 2.\cr
#'     First element : names of the new levels\cr
#'     Second element: list with values of the new levels.\cr
#' @param new_levels_after (`logical`)\cr If `TRUE` new levels will be added after last level.
#' @param denom (`string`)\cr See Details.
#' @param alt_df (`dataframe`)\cr Will be derived based upon alt_df_full and denom_by within a_freq_j.
#' @param parent_df (`dataframe`)\cr Will be derived within a_freq_j based
#' upon the input dataframe that goes into build_table (df) and denom_by.\cr
#' It is a data frame in the higher row-space than the current input df
#' (which underwent row-splitting by the rtables splitting machinery).
#'
#' @param countsource Either `df` or `alt_df`.\cr
#' When `alt_df` the counts will be based upon the alternative dataframe `alt_df`.\cr
#' This is useful for subgroup processing,
#' to present counts of subjects in a subgroup from the alternative dataframe.
#'
#' @details
#'
#' `denom` controls the denominator used to calculate proportions/percents.
#' It must be one of \cr
#' \itemize{
#' \item \strong{N_col} Column count, \cr
#' \item \strong{n_df} Number of patients (based upon the main input dataframe `df`),\cr
#' \item \strong{n_altdf} Number of patients from the secondary dataframe (`.alt_df_full`),\cr
#' Note that argument `denom_by` will perform a row-split on the `.alt_df_full` dataframe.\cr
#' It is a requirement that variables specified in `denom_by` are part of the row split specifications. \cr
#' \item \strong{N_colgroup} Number of patients from the column group variable
#' (note that this is based upon the input .alt_df_full dataframe).\cr
#' Note that the argument `colgroup` (column variable) needs to be provided,
#' as it cannot be retrieved directly from the column layout definition.
#' \item \strong{n_rowdf} Number of patients from the current row-level dataframe
#' (`.row_df` from the rtables splitting machinery).\cr
#' \item \strong{n_parentdf} Number of patients from a higher row-level split than the current split.\cr
#' This higher row-level split is specified in the argument `denom_by`.\cr
#' }
#'
#' @return
#' * `s_freq_j`: returns a list of following statistics\cr
#' \itemize{
#' \item n_df
#' \item n_rowdf
#' \item n_parentdf
#' \item n_altdf
#' \item denom
#' \item count
#' \item count_unique
#' \item count_unique_fraction
#' \item count_unique_denom_fraction
#' }
#'
#' @export
#' @importFrom stats setNames
s_freq_j <- function(
    df,
    .var,
    .df_row,
    val = NULL,
    drop_levels = FALSE,
    excl_levels = NULL,
    alt_df,
    parent_df,
    id = "USUBJID",
    denom = c("n_df", "n_altdf", "N_col", "n_rowdf", "n_parentdf"),
    .N_col,
    countsource = c("df", "altdf")) {
  if (is.na(.var) || is.null(.var)) {
    stop("Argument .var cannot be NA or NULL.")
  }

  countsource <- match.arg(countsource)

  if (countsource == "altdf") {
    df <- alt_df
  }

  .alt_df <- alt_df

  n1 <- length(unique(.alt_df[[id]]))
  n2 <- length(unique(df[[id]]))

  n3 <- length(unique(.df_row[[id]]))

  if (is.null(parent_df)) {
    parent_df <- df
  }
  n4 <- length(unique(parent_df[[id]]))


  denom <- match.arg(denom) |> switch(
    "n_altdf" = n1,
    "n_df" = n2,
    "n_rowdf" = n3,
    "N_col" = .N_col,
    "n_parentdf" = n4
  )

  y <- list()

  y$n_altdf <- c("n_altdf" = n1)
  y$n_df <- c("n_df" = n2)
  y$n_rowdf <- c("n_rowdf" = n3)
  y$n_parentdf <- c("n_parentdf" = n4)
  y$denom <- c("denom" = denom)

  if (drop_levels) {
    obs_levs <- unique(.df_row[[.var]])
    obs_levs <- intersect(levels(.df_row[[.var]]), obs_levs)

    if (!is.null(excl_levels)) obs_levs <- setdiff(obs_levs, excl_levels)

    if (!is.null(val)) {
      stop("argument val cannot be used together with drop_levels = TRUE.")
    }
    val <- obs_levs
  }

  if (!is.null(val)) {
    df <- df[df[[.var]] %in% val, ]
    .df_row <- .df_row[.df_row[[.var]] %in% val, ]

    df <- h_update_factor(df, .var, val)
    .df_row <- h_update_factor(.df_row, .var, val)
  }

  if (!is.null(excl_levels) && drop_levels == FALSE) {
    # restrict the levels to the ones specified in val argument
    df <- df[!(df[[.var]] %in% excl_levels), ]
    .df_row <- .df_row[!(.df_row[[.var]] %in% excl_levels), ]

    df <- h_update_factor(df, .var, excl_levels = excl_levels)
    .df_row <- h_update_factor(.df_row, .var, excl_levels = excl_levels)
  }

  x <- df[[.var]]
  x_unique <- unique(df[, c(.var, id)])[[.var]]

  if (identical(levels(df[[.var]]), no_data_to_report_str)) {
    xy <- list()
    nms <- c(
      "count",
      "count_unique",
      "count_unique_fraction",
      "count_unique_denom_fraction"
    )
    xy <- replicate(length(nms), list(setNames(list(NULL), no_data_to_report_str)))
    names(xy) <- nms
    y <- append(y, xy)
  } else {
    y$count <- lapply(
      as.list(table(x, useNA = "ifany")),
      stats::setNames,
      nm = "count"
    )

    y$count_unique <- lapply(
      as.list(table(x_unique, useNA = "ifany")),
      stats::setNames,
      nm = "count_unique"
    )

    y$count_unique_fraction <- lapply(
      y$count_unique,
      function(x) {
        ## we want to return - when denom = 0
        ## this is built into formatting function, when fraction is NA
        c(x, "p" = ifelse(denom > 0, x / denom, NA))
      }
    )

    y$count_unique_denom_fraction <- lapply(
      y$count_unique,
      function(x) {
        ## we want to return - when denom = 0
        ## this is built into formatting function, when fraction is NA
        c(x, "d" = denom, "p" = ifelse(denom > 0, x / denom, NA))
      }
    )
  }

  return(y)
}

s_rel_risk_levii_j <- function(
    levii,
    df,
    .var,
    ref_df,
    ref_denom_df,
    .in_ref_col,
    curgrp_denom_df,
    id,
    variables,
    conf_level,
    method,
    weights_method) {
  dfii <- df[df[[.var]] == levii & !is.na(df[[.var]]), ]
  ref_dfii <- ref_df[ref_df[[.var]] == levii & !is.na(ref_df[[.var]]), ]

  # construction of df_val, based upon curgrp_denom_df, dfii
  df_val <- curgrp_denom_df
  df_val$rsp <- FALSE
  # subjects with value levii observed in df TRUE
  df_val$rsp[df_val[[id]] %in% unique(dfii[[id]])] <- TRUE

  # repeat for ref group, based upon ref_denom_df, ref_dfii
  ref_df_val <- ref_denom_df
  ref_df_val$rsp <- FALSE
  # subjects with value levii observed in ref_df TRUE
  ref_df_val$rsp[ref_df_val[[id]] %in% unique(ref_dfii[[id]])] <- TRUE

  ### once 3-d version of diff_ci is available in tern::s_proportion_diff
  ### we should call tern::s_proportion_diff directly
  res_ci_3d <- s_proportion_diff_j(
    df_val,
    .var = "rsp",
    .ref_group = ref_df_val,
    .in_ref_col,
    variables = variables,
    conf_level = conf_level,
    method = method,
    weights_method = weights_method
  )$diff_est_ci
}


s_rel_risk_val_j <- function(
    df,
    .var,
    .df_row,
    ctrl_grp,
    cur_trt_grp,
    trt_var,
    val = NULL,
    drop_levels = FALSE,
    excl_levels = NULL,
    denom_df,
    id = "USUBJID",
    riskdiff = TRUE,
    variables = list(strata = NULL),
    conf_level = 0.95,
    method = c(
      "waldcc",
      "wald",
      "cmh",
      "ha",
      "newcombe",
      "newcombecc",
      "strat_newcombe",
      "strat_newcombecc"
    ),
    weights_method = "cmh") {
  if (drop_levels) {
    obs_levs <- unique(.df_row[[.var]])
    obs_levs <- intersect(levels(.df_row[[.var]]), obs_levs)

    if (!is.null(excl_levels)) obs_levs <- setdiff(obs_levs, excl_levels)

    if (!is.null(val)) {
      stop("argument val cannot be used together with drop_levels = TRUE, please specify one or the other.")
    }
    val <- obs_levs
  }

  if (!is.null(val)) {
    # restrict the levels to the ones specified in val argument
    df <- df[df[[.var]] %in% val, ]
    .df_row <- .df_row[.df_row[[.var]] %in% val, ]

    df <- h_update_factor(df, .var, val)
    .df_row <- h_update_factor(.df_row, .var, val)
  }

  if (!is.null(excl_levels) && drop_levels == FALSE) {
    # restrict the levels to the ones specified in val argument
    df <- df[!(df[[.var]] %in% excl_levels), ]
    .df_row <- .df_row[!(.df_row[[.var]] %in% excl_levels), ]

    df <- h_update_factor(df, .var, excl_levels = excl_levels)
    .df_row <- h_update_factor(.df_row, .var, excl_levels = excl_levels)
  }

  levs <- levels(df[[.var]])

  if (identical(levs, no_data_to_report_str)) {
    riskdiff <- FALSE
  }
  if (!riskdiff) {
    return(list(rr_ci_3d = setNames(replicate(length(levs), list(NULL)), levs)))
  }
  ### check on denom_df
  if (NROW(denom_df[[id]]) > length(unique(denom_df[[id]]))) {
    stop(
      "\nProblem: a_freq_j \n
           Denominator has multiple records per id. \n
           Please specify colgroup and/or denom_by to refine your denominator for proper relative risk derivation."
    )
  }

  ### are we in reference column?
  .in_ref_col <- (cur_trt_grp == ctrl_grp)

  ### data from reference group - df based
  ref_df <- get_ctrl_subset(.df_row, trt_var = trt_var, ctrl_grp = ctrl_grp)

  ### denominator data from reference group - denom_df based
  ref_denom_df <- get_ctrl_subset(
    denom_df,
    trt_var = trt_var,
    ctrl_grp = ctrl_grp
  )

  # ensure this is unique record per subject
  ref_denom_df <- unique(ref_denom_df[, c(id, variables$strata), drop = FALSE])

  ### denominator data from current group - denom_df based ---
  curgrp_denom_df <- get_ctrl_subset(
    denom_df,
    trt_var = trt_var,
    ctrl_grp = cur_trt_grp
  )

  # ensure this is unique record per subject
  curgrp_denom_df <- unique(curgrp_denom_df[, c(id, variables$strata), drop = FALSE])

  # calculate the stats for each of the levels in levs
  rr_ci_3d <- sapply(
    levs,
    s_rel_risk_levii_j,
    df = df,
    .var = .var,
    ref_df = ref_df,
    ref_denom_df = ref_denom_df,
    .in_ref_col = .in_ref_col,
    curgrp_denom_df = curgrp_denom_df,
    id = id,
    variables = variables,
    conf_level = conf_level,
    method = method,
    weights_method = weights_method,
    USE.NAMES = TRUE,
    simplify = FALSE
  )
  list(rr_ci_3d = rr_ci_3d)
}


#' @name a_freq_j
#'
#'
#' @inheritParams proposal_argument_convention
#' @param .stats (`character`)\cr statistics to select for the table.
#' See Value for list of available statistics.
#' @param riskdiff (`logical`)\cr
#' When `TRUE`, risk difference calculations will be performed and
#' presented (if required risk difference column splits are included).\cr
#' When `FALSE`, risk difference columns will remain blank
#' (if required risk difference column splits are included).
#' @param ref_path (`string`)\cr Column path specifications for
#' the control group for the relative risk derivation.
#' @param variables Will be passed onto the relative risk function
#' (internal function s_rel_risk_val_j), which is based upon [tern::s_proportion_diff()].\cr
#' See `?tern::s_proportion_diff` for details.
#' @param method Will be passed onto the relative risk function (internal function s_rel_risk_val_j).\cr
#' @param weights_method Will be passed onto the relative risk function (internal function s_rel_risk_val_j).\cr
#' @param label (`string`)\cr
#' When `val` is a single `string`,
#' the row label to be shown on the output can be specified using this argument.\cr
#' When `val` is a `character vector`, the `label_map` argument can be specified
#' to control the row-labels.
#' @param labelstr An argument to ensure this function can be used
#' as a `cfun` in a `summarize_row_groups` call.\cr
#' It is recommended not to utilize this argument for other purposes.\cr
#' The label argument could be used instead (if `val` is a single string)\cr
#' An another approach could be to utilize the `label_map` argument
#' to control the row labels of the incoming analysis variable.
#' @param label_fstr (`string`)\cr
#' a sprintf style format string.
#' It can contain up to one `"%s"`, which takes the current split value and
#' generates the row/column label.\cr
#' It will be combined with the `labelstr` argument,
#' when utilizing this function as
#' a `cfun` in a `summarize_row_groups` call.\cr
#' It is recommended not to utilize this argument for other purposes.
#' The label argument could be used instead (if `val` is a single string)\cr
#' @param label_map (`tibble`)\cr
#' A mapping tibble to translate levels from the incoming variable into
#' a different row label to be presented on the table.\cr
#' @param .alt_df_full (`dataframe`)\cr Denominator dataset
#' for fraction and relative risk calculations.\cr
#' this argument gets populated by the rtables
#' split machinery (see [rtables::additional_fun_params]).
#' @param denom_by (`character`)\cr Variables from row-split
#' to be used in the denominator derivation.\cr
#' This controls both `denom = "n_parentdf"` and `denom = "n_altdf"`.\cr
#' When `denom = "n_altdf"`, the denominator is derived from `.alt_df_full`
#' in combination with `denom_by` argument
#' @param .labels_n (named `character`)\cr
#' String to control row labels for the 'n'-statistics.\cr
#' Only useful when more than one 'n'-statistic is requested (rare situations only).
#' @param .formats (named 'character' or 'list')\cr
#' formats for the statistics.
#' @param extrablankline (`logical`)\cr
#' When `TRUE`, an extra blank line will be added after the last value.\cr
#' Avoid using this in template scripts, use section_div = " " instead (once PR for rtables is available)\cr
#' @param extrablanklineafter (`string`)\cr
#' When the row-label matches the string, an extra blank line will be added after
#' that value.
#' @param restr_columns `character`\cr
#' If not NULL, columns not defined in `restr_columns` will be blanked out.
#' @param colgroup The name of the column group variable that is used as source
#' for denominator calculation.\cr
#' Required to be specified when `denom = "N_colgroup"`.
#' @param addstr2levs string, if not NULL will be appended to the rowlabel for that level,
#' eg to add ",n (percent)" at the end of the rowlabels
#'
#' @examples
#' library(dplyr)
#'
#' adsl <- ex_adsl |> select("USUBJID", "SEX", "ARM")
#' adae <- ex_adae |> select("USUBJID", "AEBODSYS", "AEDECOD")
#' adae[["TRTEMFL"]] <- "Y"
#'
#' trtvar <- "ARM"
#' ctrl_grp <- "B: Placebo"
#' adsl$colspan_trt <- factor(ifelse(adsl[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
#'   levels = c("Active Study Agent", " ")
#' )
#'
#' adsl$rrisk_header <- "Risk Difference (%) (95% CI)"
#' adsl$rrisk_label <- paste(adsl[[trtvar]], paste("vs", ctrl_grp))
#'
#' adae <- adae |> left_join(adsl)
#'
#' colspan_trt_map <- create_colspan_map(adsl,
#'   non_active_grp = "B: Placebo",
#'   non_active_grp_span_lbl = " ",
#'   active_grp_span_lbl = "Active Study Agent",
#'   colspan_var = "colspan_trt",
#'   trt_var = trtvar
#' )
#'
#' ref_path <- c("colspan_trt", " ", trtvar, ctrl_grp)
#'
#' lyt <- basic_table(show_colcounts = TRUE) |>
#'   split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map)) |>
#'   split_cols_by(trtvar) |>
#'   split_cols_by("rrisk_header", nested = FALSE) |>
#'   split_cols_by(trtvar, labels_var = "rrisk_label", split_fun = remove_split_levels(ctrl_grp))
#'
#' lyt1 <- lyt |>
#'   analyze("TRTEMFL",
#'     show_labels = "hidden",
#'     afun = a_freq_j,
#'     extra_args = list(
#'       method = "wald",
#'       .stats = c("count_unique_denom_fraction"),
#'       ref_path = ref_path
#'     )
#'   )
#'
#' result1 <- build_table(lyt1, adae, alt_counts_df = adsl)
#'
#' result1
#'
#' x_drug_x <- list(length(unique(subset(adae, adae[[trtvar]] == "A: Drug X")[["USUBJID"]])))
#' N_x_drug_x <- length(unique(subset(adsl, adsl[[trtvar]] == "A: Drug X")[["USUBJID"]]))
#' y_placebo <- list(length(unique(subset(adae, adae[[trtvar]] == ctrl_grp)[["USUBJID"]])))
#' N_y_placebo <- length(unique(subset(adsl, adsl[[trtvar]] == ctrl_grp)[["USUBJID"]]))
#'
#' tern::stat_propdiff_ci(
#'   x = x_drug_x,
#'   N_x = N_x_drug_x,
#'   y = y_placebo,
#'   N_y = N_y_placebo
#' )
#'
#' x_combo <- list(length(unique(subset(adae, adae[[trtvar]] == "C: Combination")[["USUBJID"]])))
#' N_x_combo <- length(unique(subset(adsl, adsl[[trtvar]] == "C: Combination")[["USUBJID"]]))
#'
#' tern::stat_propdiff_ci(
#'   x = x_combo,
#'   N_x = N_x_combo,
#'   y = y_placebo,
#'   N_y = N_y_placebo
#' )
#'
#'
#' extra_args_rr <- list(
#'   denom = "n_altdf",
#'   denom_by = "SEX",
#'   riskdiff = FALSE,
#'   .stats = c("count_unique")
#' )
#'
#' extra_args_rr2 <- list(
#'   denom = "n_altdf",
#'   denom_by = "SEX",
#'   riskdiff = TRUE,
#'   ref_path = ref_path,
#'   method = "wald",
#'   .stats = c("count_unique_denom_fraction"),
#'   na_str = rep("NA", 3)
#' )
#'
#' lyt2 <- basic_table(
#'   top_level_section_div = " ",
#'   colcount_format = "N=xx"
#' ) |>
#'   split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map)) |>
#'   split_cols_by(trtvar, show_colcounts = TRUE) |>
#'   split_cols_by("rrisk_header", nested = FALSE) |>
#'   split_cols_by(trtvar,
#'     labels_var = "rrisk_label", split_fun = remove_split_levels("B: Placebo"),
#'     show_colcounts = FALSE
#'   ) |>
#'   split_rows_by("SEX", split_fun = drop_split_levels) |>
#'   summarize_row_groups("SEX",
#'     cfun = a_freq_j,
#'     extra_args = append(extra_args_rr, list(label_fstr = "Gender: %s"))
#'   ) |>
#'   split_rows_by("TRTEMFL",
#'     split_fun = keep_split_levels("Y"),
#'     indent_mod = -1L,
#'     section_div = c(" ")
#'   ) |>
#'   summarize_row_groups("TRTEMFL",
#'     cfun = a_freq_j,
#'     extra_args = append(extra_args_rr2, list(
#'       label =
#'         "Subjects with >=1 AE", extrablankline = TRUE
#'     ))
#'   ) |>
#'   split_rows_by("AEBODSYS",
#'     split_label = "System Organ Class",
#'     split_fun = trim_levels_in_group("AEDECOD"),
#'     label_pos = "topleft",
#'     section_div = c(" "),
#'     nested = TRUE
#'   ) |>
#'   summarize_row_groups("AEBODSYS",
#'     cfun = a_freq_j,
#'     extra_args = extra_args_rr2
#'   ) |>
#'   analyze("AEDECOD",
#'     afun = a_freq_j,
#'     extra_args = extra_args_rr2
#'   )
#'
#' result2 <- build_table(lyt2, adae, alt_counts_df = adsl)
#'
#' @return
#' * `a_freq_j`: returns a list of requested statistics with formatted `rtables::CellValue()`.\cr
#' Within the relative risk difference columns, the following stats are blanked out:
#' \itemize{
#' \item any of the n-statistics (n_df, n_altdf, n_parentdf, n_rowdf, denom)
#' \item count
#' \item count_unique
#' }
#' For the others (count_unique_fraction, count_unique_denom_fraction),
#' the statistic is replaced by the relative risk difference + confidence interval.
#' @export
a_freq_j <- function(
    df,
    labelstr = NULL,
    .var = NA,
    val = NULL,
    drop_levels = FALSE,
    excl_levels = NULL,
    new_levels = NULL,
    new_levels_after = FALSE,
    addstr2levs = NULL,
    .df_row,
    .spl_context,
    .N_col,
    id = "USUBJID",
    denom = c("N_col", "n_df", "n_altdf", "N_colgroup", "n_rowdf", "n_parentdf"),
    riskdiff = TRUE,
    ref_path = NULL,
    variables = list(strata = NULL),
    conf_level = 0.95,
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
    weights_method = "cmh",
    label = NULL,
    label_fstr = NULL,
    label_map = NULL,
    .alt_df_full = NULL,
    denom_by = NULL,
    .stats = c("count_unique_denom_fraction"),
    .formats = NULL,
    .indent_mods = NULL,
    na_str = rep("NA", 3),
    .labels_n = NULL,
    extrablankline = FALSE,
    extrablanklineafter = NULL,
    restr_columns = NULL,
    colgroup = NULL,
    countsource = c("df", "altdf")) {
  denom <- match.arg(denom)
  method <- match.arg(method)

  if (!is.null(labelstr) && is.na(.var)) {
    stop(
      "Please specify var call to summarize_row_groups when using cfun = a_freq_j, i.e.,\n",
      "summarize_row_groups('varname', cfun = a_freq_j)"
    )
  }

  if (denom == "N_colgroup") {
    if (is.null(colgroup)) {
      stop("Colgroup must be specified when denom = N_colgroup.")
    }

    checkmate::assert_character(colgroup, null.ok = FALSE, max.len = 1)

    if (colgroup == tail(.spl_context$cur_col_split[[1]], 1)) {
      stop(
        "N_colgroup cannot be used when colgroup is lowest column split."
      )
    }
  }

  check_alt_df_full(denom, c("n_altdf", "N_colgroup"), .alt_df_full)

  res_dataprep <- h_a_freq_dataprep(
    df = df,
    labelstr = labelstr,
    .var = .var,
    val = val,
    drop_levels = drop_levels,
    excl_levels = excl_levels,
    new_levels = new_levels,
    new_levels_after = new_levels_after,
    addstr2levs = addstr2levs,
    .df_row = .df_row,
    .spl_context = .spl_context,
    .N_col = .N_col,
    id = id,
    denom = denom,
    variables = variables,
    label = label,
    label_fstr = label_fstr,
    label_map = label_map,
    .alt_df_full = .alt_df_full,
    denom_by = denom_by,
    .stats = .stats
  )
  # res_dataprep is list with elements
  # df .df_row val
  # drop_levels excl_levels
  # alt_df parentdf new_denomdf
  # .stats
  # make these elements available in current environment
  df <- res_dataprep$df
  .df_row <- res_dataprep$.df_row
  val <- res_dataprep$val
  drop_levels <- res_dataprep$drop_levels
  excl_levels <- res_dataprep$excl_levels
  alt_df <- res_dataprep$alt_df
  parentdf <- res_dataprep$parentdf
  new_denomdf <- res_dataprep$new_denomdf
  .stats <- .stats

  ## prepare for column based split
  col_expr <- .spl_context$cur_col_expr[[1]]
  ## colid can be used to figure out if we're in the relative risk columns or not
  colid <- .spl_context$cur_col_id[[1]]
  inriskdiffcol <- grepl("difference", tolower(colid), fixed = TRUE)

  if (!is.null(colgroup)) {
    colexpr_substr <- h_colexpr_substr(colgroup, .spl_context$cur_col_expr[[1]])

    if (is.null(colexpr_substr)) {
      stop("\n Problem a_freq_j: incorrect colgroup specification.")
    }

    new_denomdf <- subset(.alt_df_full, eval(parse(text = colexpr_substr)))
    .df_row <- subset(.df_row, eval(parse(text = colexpr_substr)))
  }

  if (!inriskdiffcol) {
    if (denom != "N_colgroup" && !is.null(new_denomdf)) {
      ### for this part : perform column split on denominator dataset
      new_denomdf <- subset(new_denomdf, eval(col_expr))
    }
    if (denom == "N_colgroup") {
      denom <- "n_altdf"
    }

    x_stats <- s_freq_j(
      df,
      .var = .var,
      .df_row = .df_row,
      val = val,
      drop_levels = drop_levels,
      excl_levels = excl_levels,
      alt_df = new_denomdf,
      parent_df = new_denomdf,
      id = id,
      denom = denom,
      .N_col = .N_col,
      countsource = countsource
    )
    ## remove relrisk stat from .stats
    .stats_adj <- .stats[!(.stats %in% "rr_ci_3d")]
  } else {
    if (riskdiff && is.null(ref_path)) {
      stop("argument ref_path cannot be NULL.")
    }
    ### denom N_colgroup should not be used in layout with risk diff columns
    if (denom == "N_colgroup") {
      stop(
        "denom N_colgroup cannot be used in a layout with risk diff columns."
      )
    }
    if (!riskdiff) {
      trt_var <- NULL
      ctrl_grp <- NULL
      cur_trt_grp <- NULL
    }

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

      if (!is.null(colgroup) && trt_var == colgroup) {
        stop(
          "\n Problem: a_freq_j: colgroup and treatment variable from ref_path are the same.
             This is not intented for usage with relative risk columns.
             Either remove risk difference columns from layout, set riskdiff = FALSE, or update colgroup."
        )
      }
    }

    x_stats <- s_rel_risk_val_j(
      df,
      .var = .var,
      .df_row = .df_row,
      val = val,
      drop_levels = drop_levels,
      excl_levels = excl_levels,
      denom_df = new_denomdf,
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

    ## this will ensure the following stats will be shown as empty column in relative risk column
    xy <- sapply(
      c(
        "count",
        "count_unique",
        "n_df",
        "n_altdf",
        "n_rowdf",
        "n_parentdf",
        "denom"
      ),
      function(x) {
        stats::setNames(list(x = NULL), x)
      },
      USE.NAMES = TRUE,
      simplify = FALSE
    )
    x_stats <- append(x_stats, xy)

    ## restrict to relrisk stat from .stats
    # when both count_unique_fraction and count_unique_denom_fraction are requested, the rr_ci_3d stat is in here twice
    # this does not seem to introduce a problem, although might not be ideal
    # see further
    .stats_adj <- replace(
      .stats,
      .stats %in%
        c(
          "count_unique_fraction",
          "count_unique_denom_fraction",
          "fraction_count_unique_denom"
        ),
      "rr_ci_3d"
    )
  }

  res_prepinrows <- h_a_freq_prepinrows(
    x_stats,
    .stats_adj,
    .formats,
    labelstr,
    label_fstr,
    label,
    .indent_mods,
    .labels_n,
    na_str
  )
  # res_prepinrows is list with elements
  # x_stats .formats .labels .indent_mods .format_na_strs
  # make these elements available in current environment
  x_stats <- res_prepinrows$x_stats
  .formats <- res_prepinrows$.formats
  .labels <- res_prepinrows$.labels
  .indent_mods <- res_prepinrows$.indent_mods
  .format_na_strs <- res_prepinrows$.format_na_strs

  ### blank out columns not in restr_columns
  # get column label
  colid_lbl <- utils::tail(
    .spl_context$cur_col_split_val[[NROW(.spl_context)]],
    1
  )
  if (!is.null(restr_columns) && !(tolower(colid_lbl) %in% tolower(restr_columns))) {
    x_stats <- lapply(x_stats, FUN = function(x) {
      NULL
    })
  }

  ### final step: turn requested stats into rtables rows
  inrows <- in_rows(
    .list = x_stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods,
    .format_na_strs = .format_na_strs
  )

  ### add extra blankline to the end of inrows --- as long as section_div is not working as expected
  # nolint start
   if (!is.null(inrows) && extrablankline ||
    (!is.null(extrablanklineafter) && length(.labels) == 1 && .labels == extrablanklineafter)) {
    inrows <- add_blank_line_rcells(inrows)
  } # nolint end

  return(inrows)
}
