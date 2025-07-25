#' Analysis of Covariance
#'
#' Performs an analysis of covariance between two groups returning the estimated
#' "treatment effect" (i.e. the contrast between the two treatment groups) and
#' the least square means estimates in each group.
#'
#' @param data A `data.frame` containing the data to be used in the model.
#' @param vars A `vars` object as generated by [rbmi::set_vars()]. Only the `group`,
#' `visit`, `outcome` and `covariates` elements are required. See details.
#' @param visits An optional character vector specifying which visits to
#' fit the ancova model at. If `NULL`, a separate ancova model will be fit to the
#' outcomes for each visit (as determined by `unique(data[[vars$visit]])`).
#' See details.
#' @param weights Character, either `"counterfactual"` (default), `"equal"`,
#' `"proportional_em"` or `"proportional"`.
#' Specifies the weighting strategy to be used when calculating the lsmeans.
#' See the weighting section for more details.
#'
#' @details
#' The function works as follows:
#'
#' 1. Select the first value from `visits`.
#' 2. Subset the data to only the observations that occurred on this visit.
#' 3. Fit a linear model as  `vars$outcome ~ vars$group + vars$covariates`.
#' 4. Extract the "treatment effect" & least square means for each treatment group.
#' 5. Repeat points 2-3 for all other values in `visits`.
#'
#' If no value for `visits` is provided then it will be set to
#' `unique(data[[vars$visit]])`.
#'
#' In order to meet the formatting standards set by [rbmi_analyse()] the results will be collapsed
#' into a single list suffixed by the visit name, e.g.:
#' ```
#' list(
#'    var_visit_1 = list(est = ...),
#'    trt_B_visit_1 = list(est = ...),
#'    lsm_A_visit_1 = list(est = ...),
#'    lsm_B_visit_1 = list(est = ...),
#'    var_visit_2 = list(est = ...),
#'    trt_B_visit_2 = list(est = ...),
#'    lsm_A_visit_2 = list(est = ...),
#'    lsm_B_visit_2 = list(est = ...),
#'    ...
#' )
#' ```
#' Please note that "trt" refers to the treatment effects, and "lsm" refers to the least
#' square mean results. In the above example `vars$group` has two factor levels A and B.
#' The new "var" refers to the model estimated variance of the residuals.
#'
#' If you want to include interaction terms in your model this can be done
#' by providing them to the `covariates` argument of [rbmi::set_vars()]
#' e.g. `set_vars(covariates = c("sex*age"))`.
#'
#' @return a list of variance (`var_*`), treatment effect (`trt_*`), and
#'   least square mean (`lsm_*`) estimates for each visit, organized as
#'   described in Details above.
#'
#' @note These functions have the `rbmi_` prefix to distinguish them from the corresponding
#'   `rbmi` package functions, from which they were copied from. Additional features here
#'   include:
#'
#'   * Support for more than two treatment groups.
#'   * Variance estimates are returned.
#'
#' @seealso [rbmi_analyse()]
#' @seealso [stats::lm()]
#' @seealso [rbmi::set_vars()]
#' @export
rbmi_ancova <- function(
    data,
    vars,
    visits = NULL,
    weights = c("counterfactual", "equal", "proportional_em", "proportional")) {
  outcome <- vars[["outcome"]]
  group <- vars[["group"]]
  covariates <- vars[["covariates"]]
  visit <- vars[["visit"]]
  weights <- match.arg(weights)

  expected_vars <- c(
    utils::getFromNamespace("extract_covariates", "rbmi")(covariates),
    outcome,
    group
  )

  if (is.null(visits)) {
    visits <- unique(data[[visit]])
  }

  res <- lapply(
    visits,
    function(x) {
      data2 <- data[data[[visit]] == x, ]
      res <- rbmi_ancova_single(data2, outcome, group, covariates, weights)
      names(res) <- paste0(names(res), "_", x)
      return(res)
    }
  )
  return(unlist(res, recursive = FALSE))
}


#' Implements an Analysis of Covariance (ANCOVA)
#'
#' @description
#' Performance analysis of covariance. See [rbmi_ancova()] for full details.
#'
#' @param outcome string, the name of the outcome variable in `data`.
#' @param group string, the name of the group variable in `data`.
#' @param covariates character vector containing the name of any additional covariates
#' to be included in the model as well as any interaction terms.
#'
#' @inheritParams rbmi_ancova
#'
#' @details
#' - `group` must be a factor variable with only 2 levels.
#' - `outcome` must be a continuous numeric variable.
#' @export
#' @return a list containing `var` with variance estimates as well as
#' `trt_*` and `lsm_*` entries. See [rbmi_ancova()] for full details.
#' @examples
#'
#' iris2 <- iris[iris$Species %in% c("versicolor", "virginica"), ]
#' iris2$Species <- factor(iris2$Species)
#' rbmi_ancova_single(iris2, "Sepal.Length", "Species", c("Petal.Length * Petal.Width"))
#'
#' @seealso [rbmi_ancova()]
rbmi_ancova_single <- function(
    data,
    outcome,
    group,
    covariates,
    weights = c("counterfactual", "equal", "proportional_em", "proportional")) {
  checkmate::assert_string(outcome)
  checkmate::assert_string(group)
  weights <- match.arg(weights)

  checkmate::assert_factor(data[[group]], n.levels = 2L)
  checkmate::assert_numeric(data[[outcome]])
  data2 <- data[, c(
    utils::getFromNamespace("extract_covariates", "rbmi")(covariates),
    outcome,
    group
  )]

  frm <- utils::getFromNamespace("as_simple_formula", "rbmi")(
    outcome,
    c(group, covariates)
  )

  mod <- stats::lm(formula = frm, data = data2)

  args <- list(model = mod, .weights = weights)

  grp_levels <- levels(data2[[group]])

  all_lsm <- lapply(
    grp_levels,
    \(x) {
      do.call(
        utils::getFromNamespace("lsmeans", "rbmi"),
        c(args, stats::setNames(x, group))
      )
    }
  )
  names(all_lsm) <- paste0("lsm_", grp_levels)

  var_est <- summary(mod)$sigma^2
  var_df <- stats::df.residual(mod)

  all_trt <- lapply(
    paste0(group, grp_levels[-1]),
    \(x) {
      list(
        est = stats::coef(mod)[x],
        se = sqrt(stats::vcov(mod)[x, x]),
        df = var_df
      )
    }
  )
  names(all_trt) <- paste0("trt_", grp_levels[-1])

  var_res <- list(
    var = list(
      est = var_est,
      # Reference: Davison, Statistical Methods, p. 371 (just above 8.3.2)
      se = var_est / sqrt(var_df / 2),
      df = var_df
    )
  )

  c(var_res, all_trt, all_lsm)
}
