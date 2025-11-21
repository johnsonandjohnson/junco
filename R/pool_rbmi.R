#' @note: This has been forked from the `rbmi` package, mainly to support in addition
#'   the pooling of variance estimates.
rbmi_pool <- function(
    results,
    conf.level = 0.95,
    alternative = c("two.sided", "less", "greater"),
    type = c("percentile", "normal")) {
  if (!requireNamespace("rbmi", quietly = TRUE)) {
    stop("The 'rbmi' package is needed for this function to work. Please install it.", call. = FALSE)
  }

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
