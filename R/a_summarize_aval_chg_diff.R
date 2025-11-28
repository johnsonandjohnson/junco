s_summarize_desc_j <- function(df, .var, .ref_group, .in_ref_col, control = control_analyze_vars()) {
  x <- df[[.var]]
  y1 <- s_summary(x)
  y2 <- NULL

  # diff in means versus control group, based upon 2 sample t.test
  y2$mean_diffci <- numeric()
  if (!is.null(.ref_group) && !.in_ref_col) {
    x1 <- df[[.var]]
    x2 <- .ref_group[[.var]]

    x1 <- x1[!is.na(x1)]
    x2 <- x2[!is.na(x2)]

    if ((length(x1) > 1 && length(x2) > 1) && !(stats::var(x1) == 0 && stats::var(x2) == 0)) {
      ttest_stat <- stats::t.test(x1, x2, conf.level = control$conf_level)

      stat <- ttest_stat[c("estimate", "conf.int")]
      stat$diff <- stat$estimate[1] - stat$estimate[2]
      stat <- c(stat$diff, stat$conf.int)

      y2$mean_diffci <- with_label(
        c(mean_diffci = stat),
        paste("Difference in Mean + ", f_conf_level(control$conf_level))
      )
    } else {
      y2b <- s_summary(.ref_group[[.var]])
      diff <- y1[["mean"]] - y2b[["mean"]]
      stat <- c(diff, NA, NA)

      y2$mean_diffci <- with_label(
        c(mean_diffci = stat),
        paste("Difference in Mean + ", f_conf_level(control$conf_level))
      )
    }
  }
  y <- c(y1, y2)

  return(y)
}

s_aval_chg_col1 <- function(df, .var, denom, .N_col, id, indatavar) {
  ## First column AVAL - show n/N (%)
  mystat <- "n"

  if (!is.null(indatavar)) {
    df <- subset(df, !is.na(df[[indatavar]]))
  }

  x <- df[[.var]]

  x_stats <- s_summary(x)
  x_stats <- x_stats[mystat]

  ### Ndenom derivation in case denom = N
  Ndenom <- .N_col
  if (denom == "N") {
    ### as our input dataset has ensured we have unique subjects we can just use the length of x here still safer
    ### to use id variable
    nsub <- length(unique(df[[id]]))
    Ndenom <- nsub
  }

  count_denom_frac <- c(x_stats$n, Ndenom, x_stats$n / Ndenom)
  names(count_denom_frac) <- c("n", "N", "fraction")

  count_frac <- count_denom_frac[c("n", "fraction")]
  count <- count_denom_frac[c("n")]

  y <- list()
  y$count_denom_frac <- count_denom_frac
  y$count_frac <- count_frac
  y$count <- count

  return(y)
}

s_aval_chg_col23_diff <- function(
    df,
    .var,
    .df_row,
    .ref_group,
    .in_ref_col,
    ancova,
    interaction_y,
    interaction_item,
    conf_level,
    variables,
    trt_var,
    ctrl_grp,
    cur_param,
    cur_lvl) {
  .df_row <- subset(.df_row, !is.na(.df_row[[.var]]))
  df <- subset(df, !is.na(df[[.var]]))
  .ref_group <- subset(.ref_group, !is.na(.ref_group[[.var]]))

  if (nrow(.df_row) == 0) {
    #### this is only when input row no non-missing records in any of the columns expected to occur for baseline
    #### timepoint for analysis variable change only here we want a blank cell, not a cell with all NA's NULL is
    #### generating a blank cell
    x_stats <- NULL
    mystat1 <- c("mean_ci_3d", "mean_diffci")
  } else if (!ancova) {
    mystat1 <- c("mean_ci_3d", "mean_diffci")

    control <- control_analyze_vars()
    control$conf_level <- conf_level
    x_stats <- s_summarize_desc_j(
      df = df,
      .var = .var,
      .ref_group = .ref_group,
      .in_ref_col = .in_ref_col,
      control = control
    )
  } else {
    mystat1 <- c("lsmean_ci", "lsmean_diffci")

    ### sparse data problems with underlying ancova function 1/ if nrow(.df_row) = 0 NULL (blank columns)

    ### 2/ if nrow(df) = 0 no ancova - lsmean and lsmean diff should be na

    ### 3/ if nrow(df) > 0, & nrow(.ref_group) = 0 ancova for ls mean only, NULL .ref_group -- lsmean diff should
    ### be na

    ### 4/ if nrow(df) > 0, & nrow(.ref_group) > 0 and at least one group with 0 data update levels in
    ### .df_row/df/.ref_group to avoid problems for contrast

    #### by making updates to .df_row/df/.ref_group, situation 3 and 4 could be used together with general case

    .df_row_trtlevels <- unique(.df_row[[trt_var]])

    if (NROW(.df_row_trtlevels) == 0) {
      ### No data at all
      x_stats <- NULL
    } else if (NROW(df) == 0 || NROW(.df_row_trtlevels) == 1) {
      ### current column no data/less than 2 treatment levels

      x_stats <- list()
      x_stats[[mystat1[1]]] <- rep(NA, 3)
      x_stats[[mystat1[2]]] <- rep(NA, 3)
    } else {
      ### ancova situation, and some updates to prevent problems with sparse data
      if ((!ctrl_grp %in% .df_row_trtlevels)) {
        # LS means for current group can be estimated from model, but not difference in LS means set .ref_group
        # to NULL to proceed with s_summarize_ancova_j function update to underlying s_ancova_j to cover NULL
        # ref_group in call
        .ref_group <- NULL
      }
      if (NROW(.df_row_trtlevels) < length(levels(.df_row[[trt_var]]))) {
        # missing levels need to be removed from the factor, in order to have the correct estimates, and avoid
        # errors with underlying tern:::s_ancova function
        .df_row[[trt_var]] <- droplevels(.df_row[[trt_var]])
        df[[trt_var]] <- factor(as.character(df[[trt_var]]), levels = levels(.df_row[[trt_var]]))
        .ref_group[[trt_var]] <- factor(as.character(.ref_group[[trt_var]]), levels = levels(.df_row[[trt_var]]))
      }

      x_stats <- s_summarize_ancova_j(
        df = df,
        .var = .var,
        .ref_group = .ref_group,
        .in_ref_col = .in_ref_col,
        .df_row = .df_row,
        conf_level = conf_level,
        interaction_y = interaction_y,
        interaction_item = interaction_item,
        variables = variables
      )
    }
  }

  y <- list(mean_ci_3d = x_stats[[mystat1[1]]], meandiff_ci_3d = x_stats[[mystat1[2]]])
  return(y)
}


xxd_to_xx <- function(str, d = 0) {
  checkmate::assert_integerish(d, null.ok = TRUE)
  if (checkmate::test_list(str, null.ok = FALSE)) {
    checkmate::assert_list(str, null.ok = FALSE)
    # Or it may be a vector of characters
  } else {
    checkmate::assert_character(str, null.ok = FALSE)
  }

  nmstr <- names(str)

  if (any(grepl("xx.d", str, fixed = TRUE))) {
    checkmate::assert_integerish(d)
    str <- gsub("xx.d", paste0("xx.", strrep("x", times = d)), str, fixed = TRUE)
  }
  str <- stats::setNames(str, nmstr)
  return(str)
}

format_xxd <- function(str, d = 0, .df_row, formatting_fun = NULL) {
  # Handling of data precision
  if (!is.numeric(d)) {
    if (is.character(d) && length(d) == 1) {
      # check if d is a variable name available in .df_row
      if (d %in% names(.df_row)) {
        d <- max(.df_row[[d]], na.rm = TRUE)
      } else {
        message(paste("precision has been reset to d = 0, as variable", d, "not present on input"))
        d <- 0
      }
    }
  }
  # convert xxd type of string to xx
  fmt <- xxd_to_xx(str = str, d = d)

  if (!is.null(formatting_fun)) {
    fmt <- formatting_fun(fmt)
  }

  return(fmt)
}

#' @name a_summarize_aval_chg_diff_j
#'
#' @title Analysis function 3-column presentation
#'
#' @inherit proposal_argument_convention
#'
#' @description Analysis functions to produce a 1-row summary presented in
#' a 3-column layout in the columns: column 1: N, column 2: Value, column 3: change\cr
#' In the difference columns, only 1 column will be presented : difference + CI\cr
#' When ancova = `TRUE`, the presented statistics will be based on ANCOVA method (`s_summarize_ancova_j`).\cr
#' mean and ci (both for Value (column 2) and Chg (column 3)) using statistic `lsmean_ci`\cr
#' mean and ci for the difference column are based on same ANCOVA model using statistic `lsmean_diffci`\cr
#' When ancova = `FALSE`, descriptive statistics will be used instead.\cr
#' In the difference column, the 2-sample t-test will be used.
#'
#' @details See Description
#'
#' @inheritParams proposal_argument_convention
#' @param denom (`string`)\cr choice of denominator for proportions. Options are:
#'   * `N`: number of records in this column/row split.
#' \cr There is no check in place that the current split only has one record per subject.
#' Users should be careful with this.
#'   * `.N_col`: number of records in this column intersection (based on alt_counts_df dataset)
#'   \cr (when alt_counts_df is a single record per subjects, this will match number of subjects)
#'
#' @param d (default = 1) \cr choice of Decimal precision.
#' Note that one extra precision will be added, as means are presented.
#'  \cr Options are:
#'   * numerical(1)
#'   * variable name containing information on the precision, this variable
#'   should be available on input dataset. The content of this variable should
#'   then be an integer.
#'
#' @param ancova (`logical`)\cr If FALSE, only descriptive methods will be used. \cr
#' If TRUE Ancova methods will be used for each of the columns : AVAL, CHG, DIFF. \cr
#' @param comp_btw_group (`logical`)\cr If TRUE,
#' \cr When ancova = FALSE, the estimate of between group difference (on CHG)
#' will be based upon a two-sample t-test. \cr
#' \cr When ancova = TRUE, the same ancova model will be used for the estimate of between group difference (on CHG).
#'
#' @param interaction_y (`character`)\cr Will be passed onto the `tern` function `s_ancova`, when ancova = TRUE.
#' @param interaction_item (`character`)\cr Will be passed onto the `tern` function `s_ancova`, when ancova = TRUE.
#' @param conf_level (`proportion`)\cr Confidence level of the interval
#' @param variables (named list of strings)\cr
#' list of additional analysis variables, with expected elements:
#'    * arm (string)\cr
#' group variable, for which the covariate adjusted means of multiple groups will be summarized.
#' Specifically, the first level of arm variable is taken as the reference group.
#'    * covariates (character)\cr
#' a vector that can contain single variable names (such as 'X1'), and/or interaction terms indicated by 'X1 * X2'.
#'
#'
#' @param format_na_str (`string`)\cr
#'
#' @param indatavar (`string`)\cr If not null, variable name to extra subset
#' incoming df to non-missing values of this variable.
#' @param multivars (`string(3)`)\cr Variables names to use in 3-col layout.
#'
#' @param .stats  (named `list`)\cr column statistics to select for the table.
#' The following column names are to be used: `col1`, `col23`, `coldiff`.\cr
#' For `col1`, the following stats can be specified.\cr
#' For `col23`, only `mean_ci_3d` is available. When ancova=`TRUE` these are LS Means, otherwise, arithmetic means.\cr
#' For `coldiff`, only `meandiff_ci_3d` is available. When ancova=`TRUE` these
#' are LS difference in means, otherwise, difference in means based upon 2-sample t-test.\cr
#' @param .formats (named `list`)\cr formats for the column statistics. `xx.d` style formats can be used.
#' @param .formats_fun (named `list`)\cr formatting functions for the column
#' statistics, to be applied after the conversion of `xx.d` style to the
#' appropriate precision.
#'
#' @rdname a_summarize_aval_chg_diff_j
#' @return A function that can be used in an analyze function call
#' @export
#'
#'
#' @examples
#'
#' library(dplyr)
#'
#' ADEG <- data.frame(
#'   STUDYID = c(
#'     "DUMMY", "DUMMY", "DUMMY", "DUMMY", "DUMMY",
#'     "DUMMY", "DUMMY", "DUMMY", "DUMMY", "DUMMY"
#'   ),
#'   USUBJID = c(
#'     "XXXXX01", "XXXXX02", "XXXXX03", "XXXXX04", "XXXXX05",
#'     "XXXXX06", "XXXXX07", "XXXXX08", "XXXXX09", "XXXXX10"
#'   ),
#'   TRT01A = c(
#'     "ARMA", "ARMA", "ARMA", "ARMA", "ARMA", "Placebo",
#'     "Placebo", "Placebo", "ARMA", "ARMA"
#'   ),
#'   PARAM = c("BP", "BP", "BP", "BP", "BP", "BP", "BP", "BP", "BP", "BP"),
#'   AVISIT = c(
#'     "Visit 1", "Visit 1", "Visit 1", "Visit 1", "Visit 1",
#'     "Visit 1", "Visit 1", "Visit 1", "Visit 1", "Visit 1"
#'   ),
#'   AVAL = c(56, 78, 67, 87, 88, 93, 39, 87, 65, 55),
#'   CHG = c(2, 3, -1, 9, -2, 0, 6, -2, 5, 2)
#' )
#'
#' ADEG <- ADEG |>
#'   mutate(
#'     TRT01A = as.factor(TRT01A),
#'     STUDYID = as.factor(STUDYID)
#'   )
#'
#' ADEG$colspan_trt <- factor(ifelse(ADEG$TRT01A == "Placebo", " ", "Active Study Agent"),
#'   levels = c("Active Study Agent", " ")
#' )
#' ADEG$rrisk_header <- "Risk Difference (%) (95% CI)"
#' ADEG$rrisk_label <- paste(ADEG$TRT01A, paste("vs", "Placebo"))
#'
#' colspan_trt_map <- create_colspan_map(ADEG,
#'   non_active_grp = "Placebo",
#'   non_active_grp_span_lbl = " ",
#'   active_grp_span_lbl = "Active Study Agent",
#'   colspan_var = "colspan_trt",
#'   trt_var = "TRT01A"
#' )
#' ref_path <- c("colspan_trt", " ", "TRT01A", "Placebo")
#'
#' lyt <- basic_table() |>
#'   split_cols_by(
#'     "colspan_trt",
#'     split_fun = trim_levels_to_map(map = colspan_trt_map)
#'   ) |>
#'   split_cols_by("TRT01A") |>
#'   split_rows_by(
#'     "PARAM",
#'     label_pos = "topleft",
#'     split_label = "Blood Pressure",
#'     section_div = " ",
#'     split_fun = drop_split_levels
#'   ) |>
#'   split_rows_by(
#'     "AVISIT",
#'     label_pos = "topleft",
#'     split_label = "Study Visit",
#'     split_fun = drop_split_levels,
#'     child_labels = "hidden"
#'   ) |>
#'   split_cols_by_multivar(
#'     c("AVAL", "AVAL", "CHG"),
#'     varlabels = c("n/N (%)", "Mean (CI)", "CFB (CI)")
#'   ) |>
#'   split_cols_by("rrisk_header", nested = FALSE) |>
#'   split_cols_by(
#'     "TRT01A",
#'     split_fun = remove_split_levels("Placebo"),
#'     labels_var = "rrisk_label"
#'   ) |>
#'   split_cols_by_multivar(c("CHG"), varlabels = c(" ")) |>
#'   analyze("STUDYID",
#'     afun = a_summarize_aval_chg_diff_j,
#'     extra_args = list(
#'       format_na_str = "-", d = 0,
#'       ref_path = ref_path, variables = list(arm = "TRT01A", covariates = NULL)
#'     )
#'   )
#'
#' result <- build_table(lyt, ADEG)
#'
#' result
#' @seealso s_summarize_ancova_j
#' @family Inclusion of ANCOVA Functions
a_summarize_aval_chg_diff_j <- function(
    df,
    .df_row,
    .spl_context,
    ancova = FALSE,
    comp_btw_group = TRUE,
    ref_path = NULL,
    .N_col,
    denom = c("N", ".N_col"),
    indatavar = NULL,
    d = 0,
    id = "USUBJID",
    interaction_y = FALSE,
    interaction_item = NULL,
    conf_level = 0.95,
    variables = list(arm = "TRT01A", covariates = NULL),
    format_na_str = "",
    .stats = list(col1 = "count_denom_frac", col23 = "mean_ci_3d", coldiff = "meandiff_ci_3d"),
    .formats = list(col1 = NULL, col23 = "xx.dx (xx.dx, xx.dx)", coldiff = "xx.dx (xx.dx, xx.dx)"),
    .formats_fun = list(col1 = jjcsformat_count_denom_fraction, col23 = jjcsformat_xx, coldiff = jjcsformat_xx),
    multivars = c("AVAL", "AVAL", "CHG")) {
  denom <- match.arg(denom)

  if (comp_btw_group && is.null(ref_path)) {
    stop("ref_path cannot be NULL, please specify it. See ?get_ref_info for details.")
  }

  if (!(length(multivars) == 3)) {
    stop("argument multivars must be of length 3.")
  }

  if (!(all(multivars %in% names(df)))) {
    stop("all variables specified in multivars must be available on input dataset df.")
  }

  if (multivars[1] == multivars[2]) {
    multivars[2] <- paste0(multivars[2], "._[[2]]_.")
  }

  if (
    !is.list(.stats) ||
      !all(c("col1", "col23", "coldiff") %in% names(.stats)) ||
      !is.list(.formats) ||
      !all(c("col1", "col23", "coldiff") %in% names(.formats)) ||
      !is.list(.formats_fun) ||
      !all(c("col1", "col23", "coldiff") %in% names(.formats_fun))
  ) {
    stop(paste(
      "Issue a_summarize_aval_chg_diff_j \n.stats/.formats/.formats_fun must",
      "be a list with names c(\"col1\", \"col23\", \"coldiff\")."
    ))
  }

  row_vars <- .spl_context$split
  row_val <- .spl_context$cur_col_split_val

  col_split <- .spl_context$cur_col_split

  ## treatment group value/variable KEY assumption : treatment variable is the last split_col prior to
  ## split_cols_by_multivar
  colvars <- col_split[[NROW(.spl_context)]]
  colvars_multivars <- which(colvars == "multivars")
  if (identical(colvars_multivars, integer(0))) {
    stop("Layout must at least contain a split_cols_by_multivar call.")
  }
  if (colvars_multivars == 1) {
    stop("Layout must at least contain a split_cols_by prior to split_cols_by_multivar call.")
  }
  ## KEY assumption : treatment variable is the last split_col prior to split_cols_by_multivar
  trt_var <- .spl_context$cur_col_split[[NROW(.spl_context)]][colvars_multivars - 1]
  trt_val <- .spl_context$cur_col_split_val[[NROW(.spl_context)]][colvars_multivars - 1]

  mysplitlevel <- length(.spl_context$split)
  cur_lvl <- .spl_context$value[[mysplitlevel]]
  cur_col_id <- .spl_context$cur_col_id[[mysplitlevel]]

  cur_param <- .spl_context$value[[mysplitlevel - 1]]

  indiffcol <- grepl("difference", tolower(cur_col_id), fixed = TRUE)

  # Early return if we're not comparing between groups but we're in a difference column
  if (!comp_btw_group && indiffcol) {
    return(rcell(NULL, format = NULL, label = cur_lvl, format_na_str = format_na_str))
  }

  ## variable that is analyzed
  last_val <- utils::tail(.spl_context$cur_col_split_val[[NROW(.spl_context)]], 1)

  # as AVAL will be utilized twice, the second call results in last_val == 'AVAL._[[2]]_.'  take the first part of it
  # in flast_val (will become AVAL) flast_val <- stringr::str_split_1(last_val, pattern = '._')[1]
  flast_val <- unlist(strsplit(last_val, "._"))[1]

  x_stats <- NULL
  fmt_d <- NULL

  .in_ref_col <- FALSE
  .ref_group <- NULL
  if (comp_btw_group) {
    trt_var_refspec <- utils::tail(ref_path, n = 2)[1]
    checkmate::assert_true(identical(trt_var, trt_var_refspec))
    # ctrl_grp
    ctrl_grp <- utils::tail(ref_path, n = 1)

    ### check that ctrl_grp is a level of the treatment variable, in case riskdiff is requested
    if (!ctrl_grp %in% levels(df[[trt_var]])) {
      stop(paste0(
        "control group specification in ref_path argument (",
        ctrl_grp,
        ") is not a level of your treatment group variable (",
        trt_var,
        ")."
      ))
    }

    if (trt_val == ctrl_grp) .in_ref_col <- TRUE

    .ref_group <- .df_row[.df_row[[trt_var]] == ctrl_grp, ]
  }

  if (last_val == multivars[1] && !indiffcol) {
    mystat <- .stats[["col1"]]
    if (is.null(.formats[["col1"]])) {
      fmt <- .formats_fun[["col1"]]
    } else {
      fmt <- .formats[["col1"]]
    }

    x_stats <- s_aval_chg_col1(
      df = df,
      .var = flast_val,
      denom = denom,
      .N_col = .N_col,
      id = id,
      indatavar = indatavar
    )
  } else if (last_val %in% multivars[2:3]) {
    x_stats <- s_aval_chg_col23_diff(
      df = df,
      .var = flast_val,
      .df_row = .df_row,
      .ref_group = .ref_group,
      .in_ref_col = .in_ref_col,
      ancova = ancova,
      interaction_y = interaction_y,
      interaction_item = interaction_item,
      conf_level = conf_level,
      variables = variables,
      trt_var = trt_var,
      ctrl_grp = ctrl_grp,
      cur_param = cur_param,
      cur_lvl = cur_lvl
    )

    if (comp_btw_group && indiffcol) {
      mystat1 <- "coldiff"
    } else {
      mystat1 <- "col23"
    }
    mystat <- .stats[[mystat1]]

    fmt_d <- .formats[[mystat1]]
    formatting_fun <- .formats_fun[[mystat1]]

    fmt <- format_xxd(fmt_d, d = d, .df_row = .df_row, formatting_fun = formatting_fun)
  }
  x_stats <- x_stats[[mystat]]

  ##
  ret <- rcell(x_stats, format = fmt, label = cur_lvl, format_na_str = format_na_str)

  return(ret)
}
