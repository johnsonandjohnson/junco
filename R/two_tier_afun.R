## -------------------------------------------------------------------------------------
.two_tier_afun_inner <- function(df,
                                 labelstr = NULL,
                                 .var,
                                 .N_col,
                                 .df_row,
                                 inner_var,
                                 drill_down_levs,
                                 .spl_context,
                                 use_all_levels,
                                 grp_fun,
                                 detail_fun,
                                 ...) {
  cur_grp <- tail(.spl_context$value, 1)
  df[[.var]] <- factor(df[[.var]], levels = cur_grp)
  .df_row[[.var]] <- factor(.df_row[[.var]], levels = cur_grp)
  args <- list(
    labelstr = labelstr,
    .var = .var,
    .N_col = .N_col,
    .df_row = .df_row,
    .spl_context = .spl_context,
    ...
  )
  if (names(formals(grp_fun))[1] == "df") {
    args <- c(list(df = df), args)
  } else {
    args <- c(list(x = df[[.var]]), args)
  }
  cell_vals <- unclass(
    do.call(grp_fun, args)
  )
  names(cell_vals) <- attr(cell_vals, "row_names")
  ## calculate the drill-down values if necessary
  if (cur_grp %in% drill_down_levs) {
    ## have to make sure we use all levels for the whole row group
    ## so that each column gets the same number of values in the same order
    all_inner_levs <- levels(df[[inner_var]])
    if (use_all_levels) {
      detail_levs <- all_inner_levs
    } else {
      detail_levs <- all_inner_levs[all_inner_levs %in% all_inner_levs[.df_row[[inner_var]]]]
    }

    inner_vec <- factor(df[[inner_var]], levels = detail_levs)
    df[[inner_var]] <- factor(df[[inner_var]], levels = detail_levs)
    .df_row[[inner_var]] <- factor(.df_row[[inner_var]], levels = detail_levs)
    det_args <- list(
      .var = inner_var,
      .N_col = .N_col,
      .df_row = .df_row,
      .spl_context = .spl_context,
      ...
    )
    if (names(formals(detail_fun))[1] == "df") {
      det_args <- c(list(df = df), det_args)
    } else {
      det_args <- c(list(x = df[[.inner_var]]), det_args)
    }

    detail_vals <- unclass(
      do.call(detail_fun, det_args)
    )

    names(detail_vals) <- attr(detail_vals, "row_names")
  } else {
    detail_vals <- list()
  }
  in_rows(
    .list = c(cell_vals, detail_vals),
    .indent_mods = c(0, rep(1, length.out = length(detail_vals)))
  )
}

#' Two Tier Analysis Function
#'
#' These analysis functions simulate an final additional
#' level of nesting with a traditional analyze within it.
#' This allows them to create what *appear as* group summary
#' or content rows and *optionally or conditionally* create
#' one or more "detail" rows underneath it. For example,
#' in a disposition table, one might want counts for completed
#' and ongoing patients with no further detail underneath them,
#' but a breakdown of detailed reasons underneath the count for
#' patients who discontinued treatment. See examples.
#'
#' @param df (`data.frame`)\cr Passed by `rtables`, do not set manually.
#' @param labelstr (`character(1)`)\cr Passed by `rtables`, do not set manually.
#' @param .var (`character(1)`)\cr Passed by `rtables`, do not set manually.
#' @param .N_col (`integer(1)`)\cr
#' @param .df_row (`data.frame`)\cr Passed by `rtables`, do not set manually.
#' @param inner_var (`character(1)`)\cr The the variable to use when generating
#' the detail rows.
#' @param drill_dow_levs (`character`)\cr The level(s) of `.var` under which
#' detail rows should be generated.
#' @param .spl_context (`data.frame`)\cr Passed by `rtables`, do not set manually.
#' @param grp_fun (`function`)\cr Analysis function to be used when generating
#' the "group summary" outer rows.
#' @param detail_fun (`function`)\cr Analysis function to be used when generating
#' "detail" inner rows.
#' @param dots Passed directly to `grp_fun` and `detail_fun`.
#' @details Both the analysis variable and `inner_var` must be factors.
#'
#' `two_tier_obs_levels` and `two_tier_all_levels` differ only in what
#' factor levels will be present on `inner_var` (in both `df`/`x` and in
#' `.df_row`) when calling `detail_fun`. `two_tier_obs_levels` will include only
#' levels that are observed *anywhere in the row group, i.e., within `.df_row`*,
#' while `two_tier_all_levels` will include all incoming levels (those present
#' on the factor `.df_row[[inner_var]]`), *regardless if the level is observed
#' in the row group or not.*
#'
#' Detail rows are differentiated by having an indent mod of one, causing
#' them to hang indented under their corresponding group row.
#' @returns A RowsVerticalSection object including both the group row and
#' all detail rows, if applicable, for the facet.
#' @note In their current form these functions will may not retain any formatting
#' or labeling instructions added by `grp_fun` or `detail_fun`, and will
#' override indent_mod values specified by them. This may change in future
#' versions.
#' @export
#' @examples
#' lyt <- basic_table() |>
#'   split_cols_by("ARM") |>
#'   split_rows_by("EOSSTT", child_labels = "hidden") |>
#'   analyze("EOSSTT",
#'     afun = two_tier_obs_levels,
#'     extra_args = list(
#'       grp_fun = simple_analysis,
#'       detail_fun = simple_analysis,
#'       inner_var = "DCSREAS",
#'       drill_down_levs = "DISCONTINUED"
#'     )
#'   )
#'
#' build_table(lyt, ex_adsl)
#'
two_tier_all_levels <- function(df,
                                labelstr = NULL,
                                .var,
                                .N_col,
                                .df_row,
                                inner_var,
                                drill_down_levs,
                                .spl_context,
                                grp_fun,
                                detail_fun,
                                ...) {
  .two_tier_afun_inner(
    df = df,
    labelstr = labelstr,
    .var = .var,
    .N_col = .N_col,
    .df_row = .df_row,
    inner_var = inner_var,
    drill_down_levs = drill_down_levs,
    .spl_context = .spl_context,
    use_all_levels = TRUE,
    grp_fun = grp_fun,
    detail_fun = detail_fun,
    ...
  )
}

#' @rdname two_tier_all_levels
#' @export
two_tier_obs_levels <- function(df,
                                labelstr = NULL,
                                .var,
                                .N_col,
                                .df_row,
                                inner_var,
                                drill_down_levs,
                                .spl_context,
                                grp_fun,
                                detail_fun,
                                ...) {
  .two_tier_afun_inner(
    df = df,
    labelstr = labelstr,
    .var = .var,
    .N_col = .N_col,
    .df_row = .df_row,
    inner_var = inner_var,
    drill_down_levs = drill_down_levs,
    .spl_context = .spl_context,
    use_all_levels = FALSE,
    grp_fun = grp_fun,
    detail_fun = detail_fun,
    ...
  )
}




## -------------------------------------------------------------------------------------
#' Two-Tiered Count-Percent Analysis With Relative Risk Columns
#'
#' A two-tiered version of the analysis functions created by the
#' `cpct_relrisk_fact` function factory.
#'
#' @inheritParams two_tier_all_levels
#' @param use_all_levels (`logical(1)`)\cr Should all levels be used when
#' creating detail rows (`TRUE`) or should only levels observed within the
#' row group be used (`FALSE`, the default).
#' @param ctrl_grp (`character(1)`)\cr The control group specification to
#' be passed to `cpct_relrisk_fact`. Defaults to `"Placebo"`.
#' @seealso two_tier_all_obs
#' @export
#' @examples
#' library(dplyr)
#' trtvar <- "ARM"
#' ctrl_grp <- "B: Placebo"
#' adsl <- ex_adsl |>
#'   create_colspan_var(
#'     non_active_grp          = ctrl_grp,
#'     non_active_grp_span_lbl = " ",
#'     active_grp_span_lbl     = "Active Study Agent",
#'     colspan_var             = "colspan_trt",
#'     trt_var                 = trtvar
#'   ) |>
#'   mutate(
#'     rrisk_header = "Risk Difference (%) (95% CI)",
#'     rrisk_label = paste(!!sym(trtvar), "vs B: Placebo")
#'   )
#'
#' colspan_trt_map <- create_colspan_map(adsl,
#'   non_active_grp = ctrl_grp,
#'   non_active_grp_span_lbl = " ",
#'   active_grp_span_lbl = "Active Study Agent",
#'   colspan_var = "colspan_trt",
#'   trt_var = trtvar
#' )
#'
#' afun <- cpct_relrisk_fact(ctrl_grp = ctrl_grp)
#'
#' lyt <- basic_table() |>
#'   split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map)) |>
#'   split_cols_by(trtvar) |>
#'   split_cols_by("rrisk_header", nested = FALSE) |>
#'   split_cols_by(trtvar, labels_var = "rrisk_label", split_fun = remove_split_levels("B: Placebo")) |>
#'   split_rows_by("STRATA1") |>
#'   split_rows_by("EOSSTT", child_labels = "hidden") |>
#'   analyze("EOSSTT", afun = two_tier_cpct_relrisk, extra_args = list(inner_var = "DCSREAS", drill_down_levs = "DISCONTINUED", ctrl_grp = ctrl_grp), format = "xx (xx.x%)")
#'
#'
#' tbl <- build_table(lyt, subset(adsl, EOSSTT != "COMPLETED" & (is.na(DCSREAS) | DCSREAS != "PROTOCOL VIOLATION")))
#' tbl
#'
two_tier_cpct_relrisk <- function(df,
                                  labelstr = NULL,
                                  .var,
                                  .N_col,
                                  .df_row,
                                  inner_var,
                                  drill_down_levs,
                                  .spl_context,
                                  use_all_levels = FALSE,
                                  ctrl_grp = "Placebo",
                                  ...) {
  cur_grp <- tail(.spl_context$value, 1)
  df[[.var]] <- factor(df[[.var]], levels = cur_grp)
  .df_row[[.var]] <- factor(.df_row[[.var]], levels = cur_grp)
  grp_fun <- cpct_relrisk_fact(val = NA, ctrl_grp = ctrl_grp)
  cell_vals <- unclass(
    grp_fun(
      df,
      labelstr = labelstr,
      .var = .var,
      .N_col = .N_col,
      .df_row = .df_row,
      .spl_context = .spl_context,
      ...
    )
  )
  names(cell_vals) <- attr(cell_vals, "row_names")
  ## calculate the drill-down values if necessary
  if (cur_grp %in% drill_down_levs) {
    ## have to make sure we use all levels for the whole row group
    ## so that each column gets the same number of values in the same order
    all_inner_levs <- levels(df[[inner_var]])
    if (use_all_levels) {
      detail_levs <- all_inner_levs
    } else {
      detail_levs <- all_inner_levs[all_inner_levs %in% all_inner_levs[.df_row[[inner_var]]]]
    }

    inner_vec <- factor(df[[inner_var]], levels = detail_levs)
    df[[inner_var]] <- inner_vec
    .df_row[[inner_var]] <- factor(.df_row[[inner_var]], levels = detail_levs)
    detail_fun <- cpct_relrisk_fact(val = NA, ctrl_grp = ctrl_grp)
    detail_vals <- unclass(
      detail_fun(
        df = df,
        .var = inner_var,
        .N_col = .N_col,
        .df_row = .df_row,
        .spl_context = .spl_context,
        ...
      )
    )
    names(detail_vals) <- attr(detail_vals, "row_names")
  } else {
    detail_vals <- list()
  }
  in_rows(
    .list = c(cell_vals, detail_vals),
    .indent_mods = c(0, rep(1, length.out = length(detail_vals)))
  )
}
