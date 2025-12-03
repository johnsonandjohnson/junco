#' `ANCOVA` Analysis
#'
#' Performs the `ANCOVA` analysis, separately for each visit.
#'
#' @param vars (named `list` of `string` or `character`)\cr specifying the variables in the `ANCOVA` analysis.
#'   The following elements need to be included as character vectors and match corresponding columns
#'   in `data`:
#'
#'   - `response`: the response variable.
#'   - `covariates`: the additional covariate terms (might also include interactions).
#'   - `id`: the subject ID variable (not really needed for the computations but for internal logistics).
#'   - `arm`: the treatment group variable (factor).
#'   - `visit`: the visit variable (factor).
#'
#'   Note that the `arm` variable is by default included in the model, thus should not be part of `covariates`.
#' @param data (`data.frame`)\cr with all the variables specified in
#'   `vars`. Records with missing values in any independent variables
#'   will be excluded.
#' @param conf_level (`proportion`)\cr confidence level of the interval.
#' @param weights_emmeans (`string`)\cr argument from [emmeans::emmeans()], `'counterfactual'` by default.
#'
#' @return A `tern_model` object which is a list with model results:
#'
#'   - `fit`: A list with a fitted [stats::lm()] result for each visit.
#'   - `mse`: Mean squared error, i.e. variance estimate, for each visit.
#'   - `df`: Degrees of freedom for the variance estimate for each visit.
#'   - `lsmeans`: This is a list with data frames `estimates` and `contrasts`.
#'        The attribute `weights` savse the settings used (`weights_emmeans`).
#'   - `vars`: The variable list.
#'   - `labels`: Corresponding list with variable labels extracted from `data`.
#'   - `ref_level`: The reference level for the arm variable, which is always the first level.
#'   - `treatment_levels`: The treatment levels for the arm variable.
#'   - `conf_level`: The confidence level which was used to construct the `lsmeans` confidence intervals.
#'
#' @export
#' @examples
#' library(mmrm)
#'
#' fit <- fit_ancova(
#'   vars = list(
#'     response = "FEV1",
#'     covariates = c("RACE", "SEX"),
#'     arm = "ARMCD",
#'     id = "USUBJID",
#'     visit = "AVISIT"
#'   ),
#'   data = fev_data,
#'   conf_level = 0.9,
#'   weights_emmeans = "equal"
#' )
#'
fit_ancova <- function(
    vars = list(response = "AVAL", covariates = c(), arm = "ARM", visit = "AVISIT", id = "USUBJID"),
    data,
    conf_level = 0.95,
    weights_emmeans = "proportional") {
  labels <- h_labels(vars, data)

  arm_levels <- levels(data[[vars$arm]])
  ref_level <- arm_levels[1]
  trt_levels <- arm_levels[-1]
  visit_levels <- levels(data[[vars$visit]])
  grid <- list(
    factor(arm_levels, levels = arm_levels),
    factor(rep(visit_levels, each = length(arm_levels)), levels = visit_levels)
  ) |>
    stats::setNames(c(vars$arm, vars$visit)) |>
    as.data.frame()
  checkmate::assert_disjunct(vars$arm, vars$covariates)
  covariates_part <- paste(vars$covariates, collapse = " + ")
  formula <- if (covariates_part != "") {
    stats::as.formula(paste0(vars$response, " ~ ", covariates_part, " + ", vars$arm))
  } else {
    stats::as.formula(paste0(vars$response, " ~ ", vars$arm))
  }
  results_by_visit <- Map(
    data = split(data, data[[vars$visit]]),
    grid = split(grid, grid[[vars$visit]]),
    f = function(data, grid) {
      fit <- stats::lm(formula = formula, data = data)
      summary_fit <- summary(fit)
      emmeans_res <- h_get_emmeans_res(fit, vars = vars["arm"], weights = weights_emmeans)
      estimates <- h_get_single_visit_estimates(emmeans_res, conf_level)
      estimates[[vars$visit]] <- grid[[vars$visit]]
      estimates <- estimates |> dplyr::relocate(!!vars$arm, !!vars$visit)
      contrasts <- h_get_spec_visit_estimates(
        emmeans_res,
        specs = list(
          coefs = "trt.vs.ctrl",
          grid = grid[-1, ] # Without the reference level row.
        ),
        conf_level = conf_level,
        tests = TRUE,
        ref = ref_level
      )
      relative_reduc_df <- h_get_relative_reduc_df(estimates, vars)
      contrasts <- merge(contrasts, relative_reduc_df, by = c(vars$arm, vars$visit), sort = FALSE)
      list(fit = fit, estimates = estimates, contrasts = contrasts, mse = summary_fit$sigma^2, df = summary_fit$df[2])
    }
  )
  lsmeans <- structure(
    list(
      estimates = do.call(rbind, c(lapply(results_by_visit, "[[", "estimates"), make.row.names = FALSE)),
      contrasts = do.call(rbind, c(lapply(results_by_visit, "[[", "contrasts"), make.row.names = FALSE))
    ),
    weights = weights_emmeans
  )
  results <- list(
    fit = lapply(results_by_visit, "[[", "fit"),
    mse = do.call(c, lapply(results_by_visit, "[[", "mse")),
    df = do.call(c, lapply(results_by_visit, "[[", "df")),
    lsmeans = lsmeans,
    vars = vars,
    labels = labels,
    ref_level = ref_level,
    treatment_levels = trt_levels,
    conf_level = conf_level
  )
  class(results) <- "tern_model"
  results
}
