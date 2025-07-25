#' Split Function Factory for the Response Tables (RESP01)
#'
#' The main purpose here is to have a column dependent split into either comparative
#' statistic (relative risk or odds ratio with p-value) in the 'Overall' column,
#' and count proportions and corresponding confidence intervals in the other treatment
#' arm columns.
#'
#' @inheritParams proposal_argument_convention
#' @param method (`string`)\cr which method to use for the comparative statistics.
#'
#' @return A split function for use in the response table RESP01 and similar ones.
#' @seealso [rtables::make_split_fun()] describing the requirements for this kind of
#'   post-processing function.
#' @export
#'
#' @examples
#' split_fun <- resp01_split_fun_fct(
#'   method = "or_cmh",
#'   conf_level = 0.95
#' )
resp01_split_fun_fct <- function(method = c("rr", "or_logistic", "or_cmh"), conf_level) {
  method <- match.arg(method)
  (assert_proportion_value)(conf_level)

  post_fun <- function(ret, spl, fulldf, .spl_context) {
    all_expr <- expression(TRUE)
    # Here check if we are in 'Overall' column or not.
    this_col <- .spl_context[nrow(.spl_context), "value"][[1]]
    in_overall <- this_col == "Overall"
    # Accordingly, return left and right column labels and value identifiers.
    if (in_overall) {
      # In the Overall column, we want the comparison statistic and corresponding p-value.
      comp_stat_name <- if (method == "rr") "Relative Risk" else "Odds Ratio"
      short_split_result(
        comp_stat_ci = paste0(comp_stat_name, " (", f_conf_level(conf_level), ")~[super a]"),
        pval = "p-value~[super b]",
        fulldf = fulldf
      )
    } else {
      # In the treatment arm columns, we want counts and CI for proportions.
      short_split_result(
        count_prop = "n (%)",
        prop_ci = paste0(f_conf_level(conf_level), " for %"),
        fulldf = fulldf
      )
    }
  }
  make_split_fun(post = list(post_fun))
}

#' Content Row Function for Counts of Subgroups in Response Tables (RESP01)
#'
#' @inheritParams proposal_argument_convention
#' @param .alt_df (`data.frame`)\cr alternative data frame used for denominator calculation.
#' @param label_fstr (`string`)\cr format string for the label.
#'
#' @return The correct [rtables::in_rows()] result.
#' @export
#'
#' @examples
#' fake_spl_context <- data.frame(
#'   cur_col_split_val = I(list(c(ARM = "A: Drug X", count_prop = "count_prop")))
#' )
#' resp01_counts_cfun(
#'   df = DM,
#'   labelstr = "Blue",
#'   .spl_context = fake_spl_context,
#'   .alt_df = DM,
#'   label_fstr = "Color: %s"
#' )
resp01_counts_cfun <- function(df, labelstr, .spl_context, .alt_df, label_fstr) {
  this_col_split <- .spl_context[nrow(.spl_context), "cur_col_split_val"][[1]]
  if (this_col_split[1] != "Overall" && this_col_split[2] == "count_prop") {
    in_rows(nrow(.alt_df), .formats = "xx", .labels = sprintf(label_fstr, labelstr))
  } else {
    NULL
  }
}

#' Formatted Analysis Function for Comparative Statistic in Response Tables (RESP01)
#'
#' This function applies to a `logical` column called `.var` from `df`.
#' The response proportion is compared between the treatment arms identified
#' by column `arm`.
#'
#' @inheritParams proposal_argument_convention
#' @param include (`flag`)\cr whether to include the results for this variable.
#' @param arm (`string`)\cr column name in the data frame that identifies the treatment arms.
#' @param formats (`list`)\cr containing formats for `comp_stat_ci` and `pval`.
#' @param methods (`list`)\cr containing methods for comparative statistics. The element `comp_stat_ci` can be
#'   'rr' (relative risk), 'or_cmh' (odds ratio with CMH estimation and p-value) or 'or_logistic' (odds ratio
#'   estimated by conditional or standard logistic regression). The element `pval` can be 'fisher' (Fisher's
#'   exact test) or 'chisq' (chi-square test), only used when using unstratified analyses with 'or_logistic'.
#' @param stat (`string`)\cr the statistic to return, either `comp_stat_ci`
#'   or `pval`.
#'
#' @return The formatted result as [rtables::rcell()].
#' @seealso [resp01_a_comp_stat_factor()] for the `factor` equivalent.
#' @export
#'
#' @examples
#' dm <- droplevels(subset(formatters::DM, SEX %in% c("F", "M")))
#' dm$RESP <- as.logical(sample(c(TRUE, FALSE), size = nrow(DM), replace = TRUE))
#'
#' resp01_a_comp_stat_logical(
#'   dm,
#'   .var = "RESP",
#'   conf_level = 0.9,
#'   include = TRUE,
#'   arm = "SEX",
#'   strata = "RACE",
#'   stat = "comp_stat_ci",
#'   methods = list(comp_stat_ci = "or_cmh"),
#'   formats = list(
#'     comp_stat_ci = jjcsformat_xx("xx.xx (xx.xx - xx.xx)"),
#'     pval = jjcsformat_pval_fct(0.05)
#'   )
#' )
resp01_a_comp_stat_logical <- function(df,
                                       .var,
                                       conf_level,
                                       include,
                                       arm,
                                       strata,
                                       formats,
                                       methods,
                                       stat = c("comp_stat_ci", "pval")) {
  checkmate::assert_logical(df[[.var]])
  checkmate::assert_factor(df[[arm]], n.levels = 2L)
  checkmate::assert_flag(include)
  stat <- match.arg(stat)

  if (include) {
    checkmate::assert_list(methods)
    checkmate::assert_subset(methods$comp_stat_ci, c("rr", "or_cmh", "or_logistic"))

    df_by <- split(df, f = df[[arm]])
    this_df <- df_by[[1]]
    this_ref_group <- df_by[[2]]

    this_res <- if (methods$comp_stat_ci == "rr") {
      # Stratified CMH test for point estimate and p-value, and Wald statistic for the confidence interval
      s_res <- s_relative_risk(
        this_df,
        .var = .var,
        .ref_group = this_ref_group,
        .in_ref_col = FALSE,
        variables = list(strata = strata),
        conf_level = conf_level
      )
      list(comp_stat_ci = s_res$rel_risk_ci, pval = s_res$pval)
    } else if (methods$comp_stat_ci == "or_logistic") {
      # Odds Ratio Variant 1: (Conditional/Standard) Logistic regression for odds ratio point estimate and
      # confidence interval.
      s_res <- s_odds_ratio_j(
        this_df,
        .var = .var,
        .ref_group = this_ref_group,
        .in_ref_col = FALSE,
        .df_row = rbind(this_df, this_ref_group),
        variables = list(arm = arm, strata = strata),
        conf_level = conf_level,
        method = "exact" # Only used if strata are present.
      )
      pval <- if (is.null(strata)) {
        checkmate::assert_subset(methods$pval, c("fisher", "chisq"))

        # If not stratified, then the p-value can come either from Fisher's exact or chi-square test.
        p_res <- s_test_proportion_diff(
          this_df,
          .var = .var,
          .ref_group = this_ref_group,
          .in_ref_col = FALSE,
          method = methods$pval
        )
        p_res$pval
      } else {
        s_res$pval
      }
      list(comp_stat_ci = s_res$or_ci, pval = pval)
    } else if (methods$comp_stat_ci == "or_cmh") {
      # Odds Ratio Variant 2: Stratified CMH test for odds ratio point estimate and confidence interval and
      # corresponding CMH test p-value.
      s_res <- s_odds_ratio_j(
        this_df,
        .var = .var,
        .ref_group = this_ref_group,
        .in_ref_col = FALSE,
        .df_row = rbind(this_df, this_ref_group),
        variables = list(arm = arm, strata = strata),
        conf_level = conf_level,
        method = "cmh"
      )
      list(comp_stat_ci = s_res$or_ci, pval = s_res$pval)
    }
    rcell(this_res[[stat]], format = formats[[stat]])
  } else {
    NULL
  }
}

#' Formatted Analysis Function for Comparative Statistic in Response Tables (RESP01)
#'
#' This function applies to a `factor` column called `.var` from `df`.
#'
#' @inheritParams proposal_argument_convention
#' @param include (`character`)\cr for which factor levels to include the comparison
#'   statistic results.
#' @param \dots see [resp01_a_comp_stat_logical()] for additional required arguments.
#'
#' @return The formatted result as [rtables::rcell()].
#' @export
#'
#' @examples
#' dm <- droplevels(subset(formatters::DM, SEX %in% c("F", "M")))
#'
#' resp01_a_comp_stat_factor(
#'   dm,
#'   .var = "COUNTRY",
#'   conf_level = 0.9,
#'   include = c("USA", "CHN"),
#'   arm = "SEX",
#'   strata = "RACE",
#'   stat = "comp_stat_ci",
#'   methods = list(comp_stat_ci = "or_cmh"),
#'   formats = list(
#'     comp_stat_ci = jjcsformat_xx("xx.xx (xx.xx - xx.xx)"),
#'     pval = jjcsformat_pval_fct(0.05)
#'   )
#' )
resp01_a_comp_stat_factor <- function(df,
                                      .var,
                                      include,
                                      ...) {
  checkmate::assert_factor(df[[.var]])
  checkmate::assert_character(include, null.ok = TRUE)
  checkmate::assert_subset(x = include, choices = levels(df[[.var]]))

  design_df <- as.data.frame(h_get_design_mat(df, .var))

  res <- sapply(X = levels(df[[.var]]), simplify = FALSE, FUN = function(lvl) {
    df[[.var]] <- design_df[[lvl]]
    resp01_a_comp_stat_logical(df = df, .var = .var, include = lvl %in% include, ...)
  })
  in_rows(.list = res, .labels = levels(df[[.var]]))
}

#' Formatted Analysis and Content Summary Function for Response Tables (RESP01)
#'
#' This function applies to both `factor` and `logical` columns called
#' `.var` from `df`. Depending on the position in the split, it returns the
#' right formatted results for the RESP01 and related layouts.
#'
#' @inheritParams proposal_argument_convention
#' @inheritParams resp01_a_comp_stat_logical
#' @param include_comp (`character` or `flag`)\cr whether to include comparative
#'   statistic results, either `character` for factors or `flag` for logicals.
#' @param .alt_df (`data.frame`)\cr alternative data frame used for denominator calculation.
#' @param arm (`string`)\cr column name in the data frame that identifies the treatment arms.
#' @param label (`string`)\cr only for logicals, which label to use. (For factors, the
#'   labels are the factor levels.)
#' @param formats (`list`)\cr containing formats for `prop_ci`, `comp_stat_ci`
#'   and `pval`.
#' @param methods (`list`)\cr containing methods for comparative statistics. The element `comp_stat_ci` can be
#'   'rr' (relative risk), 'or_cmh' (odds ratio with CMH estimation and p-value) or 'or_logistic' (odds ratio
#'   estimated by conditional or standard logistic regression). The element `pval` can be 'fisher' (Fisher's
#'   exact test) or 'chisq' (chi-square test), only used when using unstratified analyses with 'or_logistic'.
#'   The element `prop_ci` specifies the method for proportion confidence interval calculation.
#'
#' @return The formatted result as [rtables::in_rows()] result.
#' @export
#'
#' @examples
#' fake_spl_context <- data.frame(
#'   cur_col_split_val = I(list(c(ARM = "A: Drug X", count_prop = "count_prop")))
#' )
#' dm <- droplevels(subset(DM, SEX %in% c("F", "M")))
#' resp01_acfun(
#'   dm,
#'   .alt_df = dm,
#'   .var = "COUNTRY",
#'   .spl_context = fake_spl_context,
#'   conf_level = 0.9,
#'   include_comp = c("USA", "CHN"),
#'   arm = "SEX",
#'   strata = "RACE",
#'   methods = list(
#'     comp_stat_ci = "or_cmh",
#'     pval = "",
#'     prop_ci = "wald"
#'   ),
#'   formats = list(
#'     prop_ci = jjcsformat_xx("xx.% - xx.%"),
#'     comp_stat_ci = jjcsformat_xx("xx.xx (xx.xx - xx.xx)"),
#'     pval = jjcsformat_pval_fct(0.05)
#'   )
#' )
#' fake_spl_context2 <- data.frame(
#'   cur_col_split_val = I(list(c(ARM = "Overall", comp_stat_ci = "comp_stat_ci")))
#' )
#' resp01_acfun(
#'   dm,
#'   .alt_df = dm,
#'   .var = "COUNTRY",
#'   .spl_context = fake_spl_context2,
#'   conf_level = 0.9,
#'   include_comp = c("USA", "CHN"),
#'   arm = "SEX",
#'   strata = "RACE",
#'   methods = list(
#'     comp_stat_ci = "or_cmh",
#'     pval = "",
#'     prop_ci = "wald"
#'   ),
#'   formats = list(
#'     prop_ci = jjcsformat_xx("xx.% - xx.%"),
#'     comp_stat_ci = jjcsformat_xx("xx.xx (xx.xx - xx.xx)"),
#'     pval = jjcsformat_pval_fct(0.05)
#'   )
#' )
resp01_acfun <- function(df,
                         labelstr = NULL,
                         label = NULL,
                         .var,
                         .spl_context,
                         include_comp,
                         .alt_df,
                         conf_level,
                         arm,
                         strata,
                         formats,
                         methods) {
  this_col_split <- .spl_context[nrow(.spl_context), "cur_col_split_val"][[1]]
  x <- df[[.var]]
  is_factor <- is.factor(x)
  res <- if (this_col_split[1] != "Overall") {
    if (this_col_split[2] == "count_prop") {
      if (is_factor) {
        s_proportion_factor(x, .alt_df = .alt_df)
      } else {
        s_proportion_logical(x, .alt_df = .alt_df)$n_prop
      }
    } else {
      checkmate::assert_true(this_col_split[2] == "prop_ci")
      if (is_factor) {
        a_proportion_ci_factor(
          df,
          .var = .var,
          .alt_df = .alt_df,
          conf_level = conf_level,
          formats = formats,
          method = methods$prop_ci
        )
      } else {
        a_proportion_ci_logical(
          x,
          .alt_df = .alt_df,
          conf_level = conf_level,
          formats = formats,
          method = methods$prop_ci
        )
      }
    }
  } else {
    checkmate::assert_true(this_col_split[1] == "Overall")
    if (is_factor) {
      resp01_a_comp_stat_factor(
        df,
        .var = .var,
        conf_level = conf_level,
        include = include_comp,
        arm = arm,
        strata = strata,
        stat = this_col_split[2],
        formats = formats,
        methods = methods
      )
    } else {
      resp01_a_comp_stat_logical(
        df,
        .var = .var,
        conf_level = conf_level,
        include = include_comp,
        arm = arm,
        strata = strata,
        stat = this_col_split[2],
        formats = formats,
        methods = methods
      )
    }
  }
  if (is_factor) {
    res
  } else {
    if (!is.null(labelstr)) {
      label <- labelstr
    }
    in_rows(res, .labels = label)
  }
}
