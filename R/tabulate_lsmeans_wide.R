#' Function Factory to Create Padded In Rows Content
#'
#' @param length_out (`numeric` or `NULL`)\cr full length which should be padded
#'   by `NA` which will be printed as empty strings.
#' @param label (`string`)\cr row label to be used for the first row only.
#'
#' @return The function of `content` and `.formats`.
#' @keywords internal
pad_in_rows_fct <- function(length_out = NULL, label = "") {
  checkmate::assert_count(length_out, null.ok = TRUE)
  checkmate::assert_string(label)

  function(content, .formats) {
    content_list <- as.list(content)
    if (!is.null(length_out)) {
      missing_length <- length_out - length(content_list)
      if (missing_length > 0) {
        content_list <- c(content_list, rep(NA, missing_length))
      }
    }
    in_rows(
      .list = content_list,
      .names = as.character(seq_along(content_list)),
      .labels = c(
        label,
        rep(
          "",
          length(content_list) -
            1L
        )
      ),
      .formats = .formats,
      .format_na_strs = ""
    )
  }
}

#' First Level Column Split for LS Means Wide Table Layouts
#'
#' @inheritParams proposal_argument_convention
#'
#' @keywords internal
lsmeans_wide_first_split_fun_fct <- function(include_variance) {
  checkmate::assert_flag(include_variance)

  post_fun <- function(ret, spl, fulldf, .spl_context) {
    if (include_variance) {
      short_split_result(
        reference_group = "Reference Group",
        testing_group = "Testing Group",
        variance = "",
        comparison = "Testing - Reference",
        fulldf = fulldf
      )
    } else {
      short_split_result(
        reference_group = "Reference Group",
        testing_group = "Testing Group",
        comparison = "Testing - Reference",
        fulldf = fulldf
      )
    }
  }
  make_split_fun(post = list(post_fun))
}

#' Second Level Column Split for LS Means Wide Table Layouts
#'
#' @inheritParams proposal_argument_convention
#' @param include_pval (`flag`)\cr whether to include the p-value column.
#'
#' @keywords internal
lsmeans_wide_second_split_fun_fct <- function(pval_sided, conf_level, include_pval) {
  checkmate::assert_flag(include_pval)

  post_fun <- function(ret, spl, fulldf, .spl_context) {
    colset <- .spl_context[nrow(.spl_context), "value"][[1]]
    if (colset %in% c("reference_group", "testing_group")) {
      short_split_result(treatment = "Treatment", n = "N", lsmean = "LS Mean", fulldf = fulldf)
    } else if (colset == "variance") {
      short_split_result(mse = "M. S. Error", df = "Error DF", fulldf = fulldf)
    } else {
      if (include_pval) {
        short_split_result(
          lsmean = "LS Mean",
          se = "SE",
          ci = f_conf_level(conf_level),
          pval = paste0(abs(as.numeric(pval_sided)), "-sided p-value~[super a]"),
          fulldf = fulldf
        )
      } else {
        short_split_result(lsmean = "LS Mean", se = "SE", ci = f_conf_level(conf_level), fulldf = fulldf)
      }
    }
  }
  make_split_fun(post = list(post_fun))
}

#' Content Row Analysis Function for LS Means Wide Table Layouts
#'
#' @inheritParams proposal_argument_convention
#' @param variables (`list`)\cr see [fit_ancova()] for required variable
#'   specifications.
#' @param ref_level (`string`)\cr the reference level of the treatment arm variable.
#' @param treatment_levels (`character`)\cr the non-reference levels of the treatment arm
#'   variable.
#' @param pval_sided (`string`)\cr either '2' for two-sided or '1' for 1-sided with greater than
#'   control or '-1' for 1-sided with smaller than control alternative hypothesis.
#' @param formats (`list`)\cr including `lsmean`, `mse`, `df`, `lsmean_diff`, `se`,
#'   `ci`, `pval` formats.
#'
#' @details This assumes a lot of structure of the layout, and is only intended to be used inside
#'   [summarize_lsmeans_wide()], please see there for the layout structure that is needed.
#'
#' @keywords internal
lsmeans_wide_cfun <- function(
    df,
    labelstr,
    .spl_context,
    variables,
    ref_level,
    treatment_levels,
    pval_sided = c("2", "1", "-1"),
    conf_level,
    formats) {
  this_col_split <- .spl_context[nrow(.spl_context), "cur_col_split_val"][[1]]
  pad_in_rows <- pad_in_rows_fct(length_out = length(treatment_levels), label = labelstr)
  if (this_col_split[1] %in% c("reference_group", "testing_group")) {
    this_level <- if (this_col_split[1] == "reference_group") {
      ref_level
    } else {
      treatment_levels
    }
    has_this_level <- df[[variables$arm]] %in% this_level
    if (this_col_split[2] == "treatment") {
      pad_in_rows(this_level, .formats = "xx")
    } else if (this_col_split[2] == "n") {
      pad_in_rows(df$n[has_this_level], .formats = "xx")
    } else {
      pad_in_rows(df$estimate_est[has_this_level], .formats = formats$lsmean)
    }
  } else if (this_col_split[1] == "variance") {
    has_trt <- df[[variables$arm]] %in% treatment_levels
    all_the_same <- (length(unique(df$mse[has_trt])) == 1) && (length(unique(df$df[has_trt])) == 1)
    # Note: We only take the first value, because in this ANCOVA case they are identical and we don't want to
    # repeat it in the table.
    inds <- if (all_the_same) 1 else seq_len(sum(has_trt))
    if (this_col_split[2] == "mse") {
      pad_in_rows(df$mse[has_trt][inds], .formats = formats$mse)
    } else {
      pad_in_rows(df$df[has_trt][inds], .formats = formats$df)
    }
  } else {
    has_trt <- df[[variables$arm]] %in% treatment_levels
    switch(this_col_split[2],
      "lsmean" = pad_in_rows(df$estimate_contr[has_trt], .formats = formats$lsmean_diff),
      "se" = pad_in_rows(df$se_contr[has_trt], .formats = formats$se),
      "ci" = pad_in_rows(
        tern::combine_vectors(df$lower_cl_contr[has_trt], df$upper_cl_contr[has_trt]),
        .formats = formats$ci
      ),
      "pval" = {
        pval_sided <- match.arg(pval_sided)
        pval_col <- switch(pval_sided,
          `2` = "p_value",
          `1` = "p_value_greater",
          `-1` = "p_value_less"
        )
        pad_in_rows(df[[pval_col]][has_trt], .formats = formats$pval)
      }
    )
  }
}

#' Layout Generating Function for LS Means Wide Table Layouts
#'
#' @inheritParams proposal_argument_convention
#' @inheritParams lsmeans_wide_cfun
#' @param lyt (`layout`)\cr empty layout, i.e. result of [rtables::basic_table()]
#' @param include_variance (`flag`)\cr whether to include the variance statistics
#'   (M.S. error and d.f.).
#' @param include_pval (`flag`)\cr whether to include the p-value column.
#'
#' @return Modified layout.
#' @export
#' @examples
#' variables <- list(
#'   response = "FEV1",
#'   covariates = c("RACE", "SEX"),
#'   arm = "ARMCD",
#'   id = "USUBJID",
#'   visit = "AVISIT"
#' )
#' fit <- fit_ancova(
#'   vars = variables,
#'   data = mmrm::fev_data,
#'   conf_level = 0.9,
#'   weights_emmeans = "equal"
#' )
#' anl <- broom::tidy(fit)
#' basic_table() |>
#'   summarize_lsmeans_wide(
#'     variables = variables,
#'     ref_level = fit$ref_level,
#'     treatment_levels = fit$treatment_levels,
#'     pval_sided = "2",
#'     conf_level = 0.8
#'   ) |>
#'   build_table(df = anl)
summarize_lsmeans_wide <- function(
    lyt,
    variables,
    ref_level,
    treatment_levels,
    conf_level,
    pval_sided = "2",
    include_variance = TRUE,
    include_pval = TRUE,
    formats = list(
      lsmean = jjcsformat_xx("xx.x"),
      mse = jjcsformat_xx("xx.x"),
      df = jjcsformat_xx("xx."),
      lsmean_diff = jjcsformat_xx("xx.x"),
      se = jjcsformat_xx("xx.xx"),
      ci = jjcsformat_xx("(xx.xx, xx.xx)"),
      pval = jjcsformat_pval_fct(0)
    )) {
  # Check that all required format elements are present in the formats parameter
  checkmate::assert_names(
    names(formats),
    must.include = c("lsmean", "mse", "df", "lsmean_diff", "se", "ci", "pval"),
    .var.name = "formats"
  )
  lyt |>
    split_cols_by(
      variables$arm,
      split_fun = lsmeans_wide_first_split_fun_fct(include_variance = include_variance)
    ) |>
    split_cols_by(
      variables$arm,
      split_fun = lsmeans_wide_second_split_fun_fct(
        include_pval = include_pval,
        pval_sided = pval_sided,
        conf_level = conf_level
      )
    ) |>
    split_rows_by(variables$visit, section_div = "") |>
    summarize_row_groups(
      var = variables$arm,
      cfun = lsmeans_wide_cfun,
      extra_args = list(
        variables = variables,
        ref_level = ref_level,
        treatment_levels = treatment_levels,
        pval_sided = pval_sided,
        conf_level = conf_level,
        formats = formats
      )
    )
}
