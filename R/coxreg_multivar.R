#' Extract Estimates from Multivariate Cox Regression Model Fit Object
#'
#' @param x (`coxreg.multivar`)\cr from [tern::fit_coxreg_multivar()].
#' @export
#' @return A data frame containing Cox regression results with columns for term,
#'   coef_se (coefficient and standard error), p.value, hr (hazard ratio),
#'   hr_ci (confidence interval for hazard ratio), and labels (formatted term labels).
#' @keywords internal
#'
#' @examples
#' anl <- tern::tern_ex_adtte |>
#'   dplyr::mutate(EVENT = 1 - CNSR)
#'
#' variables <- list(
#'   time = "AVAL",
#'   event = "EVENT",
#'   arm = "ARM",
#'   covariates = c("SEX", "AGE")
#' )
#'
#' control <- tern::control_coxreg(
#'   conf_level = 0.9,
#'   ties = "efron"
#' )
#'
#' fit <- tern::fit_coxreg_multivar(
#'   data = anl,
#'   variables = variables,
#'   control = control
#' )
#'
#' h_extract_coxreg_multivar(fit)
h_extract_coxreg_multivar <- function(x) {
  checkmate::assert_class(x, "coxreg.multivar")
  vars <- c(x$vars$arm, x$vars$covariates)

  # Extract the table.
  tab <- broom::tidy(x$mod, exponentiate = FALSE, conf.int = TRUE, conf.level = x$control$conf_level)
  tab$hr <- exp(tab$estimate)
  tab$hr_ci <- Map(lcl = exp(tab$conf.low), ucl = exp(tab$conf.high), f = function(lcl, ucl) c(lcl, ucl))
  tab$coef_se <- Map(coef = tab$estimate, se = tab$std.error, f = function(coef, se) c(coef, se))
  colnms <- c("term", "coef_se", "p.value", "hr", "hr_ci")
  tab <- tab[, colnms, drop = FALSE]

  # Format nicely the term labels.
  var_to_term <- sapply(vars, function(v) which(startsWith(tab$term, v)), USE.NAMES = TRUE, simplify = FALSE)
  tab$labels <- unlist(lapply(vars, function(v) {
    inds <- var_to_term[[v]]
    strip_term <- gsub(paste0("^", v), "", tab$term[inds])
    this_df <- x$dat[v]
    if (is.character(this_df[[1]])) {
      this_df[[1]] <- as.factor(this_df[[1]])
    }
    if (is.factor(this_df[[1]])) {
      ref_level <- levels(this_df[[1]])[1]
      var_name <- if (v == x$vars$arm) "Treatment" else labels_or_names(this_df)
      paste0(var_name, " (", strip_term, " vs. ", ref_level, ")")
    } else {
      labels_or_names(this_df)
    }
  }))
  tab
}

#' First Level Column Split Function for TEFOS03 (mmy) Table Layout
#' @seealso [rtables::make_split_fun()] for details.
#' @keywords internal
tefos03_first_post_fun <- function(ret, spl, fulldf, .spl_context) {
  all_expr <- expression(TRUE)
  short_split_result(model_fit = "Model Fit", hazard_ratio = "Hazard Ratio", fulldf = fulldf)
}
tefos03_first_split_fun <- make_split_fun(post = list(tefos03_first_post_fun))

#' Second Level Column Split Function Factory for TEFOS03 (mmy) Table Layout
#'
#' @inheritParams proposal_argument_convention
#'
#' @return Split function to use in the TEFOS03 (mmy) and related table layouts.
#'
#' @seealso [tefos03_first_post_fun()] for the first level split.
#' @keywords internal
tefos03_second_split_fun_fct <- function(conf_level) {
  post_fun <- function(ret, spl, fulldf, .spl_context) {
    all_expr <- expression(TRUE)
    colset <- .spl_context[nrow(.spl_context), "value"][[1]]
    if (colset == "model_fit") {
      short_split_result(coef_se = "Coeff. (SE)", pval = "p-value", fulldf = fulldf)
    } else {
      short_split_result(hr_est = "Estimate", hr_ci = f_conf_level(conf_level), fulldf = fulldf)
    }
  }
  make_split_fun(post = list(post_fun))
}

#' @importFrom tern fit_coxreg_multivar
#' @keywords internal
memoised_fit_coxreg_multivar <- memoise::memoise(fit_coxreg_multivar)

#' Analysis Function for TEFOS03 and Related Table Layouts
#'
#' @inheritParams proposal_argument_convention
#' @param control (`list`)\cr from [tern::control_coxreg()].
#' @param formats (`list`)\cr including `coef_se`, `hr_est`, `hr_ci` and `pval` formats.
#' @param variables (`list`)\cr see [tern::fit_coxreg_multivar()] for required variable
#'   specifications.
#'
#' @keywords internal
tefos03_afun <- function(df, .var, .spl_context, variables, control, formats) {
  this_col_split <- .spl_context[nrow(.spl_context), "cur_col_split_val"][[1]]
  model_fit <- memoised_fit_coxreg_multivar(data = df, variables = variables, control = control)
  tab <- h_extract_coxreg_multivar(model_fit)
  if (this_col_split[1] == "model_fit") {
    if (this_col_split[2] == "coef_se") {
      in_rows(.list = tab$coef_se, .formats = formats$coef_se, .labels = tab$labels)
    } else {
      in_rows(.list = tab$p.value, .formats = formats$pval, .labels = tab$labels)
    }
  } else {
    if (this_col_split[2] == "hr_est") {
      in_rows(.list = tab$hr, .formats = formats$hr_est, .labels = tab$labels)
    } else {
      in_rows(.list = tab$hr_ci, .formats = formats$hr_ci, .labels = tab$labels)
    }
  }
}

#' Layout Generating Function for TEFOS03 and Related Cox Regression Layouts
#'
#' @inheritParams proposal_argument_convention
#' @inheritParams tefos03_afun
#' @param var (`string`)\cr any variable from the data, because this is not used.
#'
#' @return `lyt` modified to add the desired cox regression table section.
#' @export
#' @examples
#' anl <- tern::tern_ex_adtte |>
#'   dplyr::mutate(EVENT = 1 - CNSR)
#'
#' variables <- list(
#'   time = "AVAL",
#'   event = "EVENT",
#'   arm = "ARM",
#'   covariates = c("SEX", "AGE")
#' )
#'
#' basic_table() |>
#'   summarize_coxreg_multivar(
#'     var = "STUDYID",
#'     variables = variables
#'   ) |>
#'   build_table(df = anl)
summarize_coxreg_multivar <- function(
    lyt,
    var,
    variables,
    control = control_coxreg(),
    formats = list(
      coef_se = jjcsformat_xx("xx.xx (xx.xx)"),
      hr_est = jjcsformat_xx("xx.xx"),
      hr_ci = jjcsformat_xx("(xx.xx, xx.xx)"),
      pval = jjcsformat_pval_fct(0)
    )) {
  second_split_fun <- tefos03_second_split_fun_fct(conf_level = control$conf_level)
  lyt |>
    split_cols_by(var = var, split_fun = tefos03_first_split_fun) |>
    split_cols_by(var = var, split_fun = second_split_fun) |>
    analyze(
      vars = var,
      var_labels = "Model Parameter",
      show_labels = "visible",
      afun = tefos03_afun,
      extra_args = list(variables = variables, control = control, formats = formats)
    )
}
