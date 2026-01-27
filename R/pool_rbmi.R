#' Pool analysis results obtained from the imputed datasets
#'
#' @details This has been forked from the `rbmi` package, mainly to support in
#' addition the pooling of variance estimates. See pool() for more details.
#'
#' @param results an analysis object created by analyse().
#'
#' @param conf.level confidence level of the returned confidence interval.
#' Must be a single number between 0 and 1. Default is 0.95.
#'
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of `"two.sided"` (default), `"greater"` or `"less"`.
#'
#' @param type a character string of either `"percentile"` (default) or
#' `"normal"`. Determines what method should be used to calculate the bootstrap
#' confidence intervals. See details.
#' Only used if `method_condmean(type = "bootstrap")` was specified
#' in the original call to draws().
#'
#' @returns A list of class `pool`.
#'
#' @export
rbmi_pool <- function(
  results,
  conf.level = 0.95,
  alternative = c("two.sided", "less", "greater"),
  type = c("percentile", "normal")
) {
  assert_rbmi()

  rbmi::validate(results)

  alternative <- match.arg(alternative)
  type <- match.arg(type)

  pool_type <- class(results$results)[[1]]
  checkmate::assert_true(identical(pool_type, "rubin"))

  results_transpose <- (utils::getFromNamespace("transpose_results", "rbmi"))(
    results$results,
    (utils::getFromNamespace("get_pool_components", "rbmi"))(pool_type)
  )

  pars <- lapply(
    results_transpose,
    function(x, ...) mod_pool_internal_rubin(x, ...),
    conf.level = conf.level,
    alternative = alternative,
    type = type,
    D = results$method$D
  )

  method <- pool_type

  ret <- list(
    pars = pars,
    conf.level = conf.level,
    alternative = alternative,
    N = length(results$results),
    method = method
  )
  class(ret) <- "pool"
  return(ret)
}

mod_pool_internal_rubin <- function(results, conf.level, alternative, type, D) {
  ests <- results$est
  ses <- results$se
  dfs <- results$df
  alpha <- 1 - conf.level

  # Note: Need to take median here, because in the MMRM case the d.f. will be slightly different for each imputed
  # data set analysis.
  v_com <- stats::median(dfs)

  res_rubin <- (utils::getFromNamespace("rubin_rules", "rbmi"))(ests = ests, ses = ses, v_com = v_com)

  ret <- (utils::getFromNamespace("parametric_ci", "rbmi"))(
    point = res_rubin$est_point,
    se = sqrt(res_rubin$var_t),
    alpha = alpha,
    alternative = alternative,
    qfun = stats::qt,
    pfun = stats::pt,
    df = res_rubin$df
  )
  # Here also return the pooled d.f.:
  ret$df <- res_rubin$df

  return(ret)
}
