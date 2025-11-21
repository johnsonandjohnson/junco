#' @note: This has been forked from the `rbmi` package, mainly to support in addition
#'   the pooling of variance estimates.
rbmi_pool <- function(
    results,
    conf.level = 0.95,
    alternative = c("two.sided", "less", "greater"),
    type = c("percentile", "normal")) {
  pkg <- "rbmi"
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("The 'rbmi' package is needed for this function to work. Please install it.", call. = FALSE)
  }

  if (requireNamespace(pkg, quietly = TRUE)) {
    validate_fn <- utils::getFromNamespace("validate", pkg)
    validate_fn(results)
  }

  alternative <- match.arg(alternative)
  type <- match.arg(type)

  pool_type <- class(results$results)[[1]]
  checkmate::assert_true(identical(pool_type, "rubin"))

  if (requireNamespace(pkg, quietly = TRUE)) {
    transpose_results_fn <- utils::getFromNamespace("transpose_results", pkg)
    get_pool_components_fn <- utils::getFromNamespace("get_pool_components", pkg)

    results_transpose <- transpose_results_fn(
      results$results,
      get_pool_components_fn(pool_type)
    )
  }

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
  pkg <- "rbmi"
  ests <- results$est
  ses <- results$se
  dfs <- results$df
  alpha <- 1 - conf.level

  # Note: Need to take median here, because in the MMRM case the d.f. will be slightly different for each imputed
  # data set analysis.
  v_com <- stats::median(dfs)

  if (requireNamespace(pkg, quietly = TRUE)) {
    rubin_rules_fn <- utils::getFromNamespace("rubin_rules", pkg)
    parametric_ci_fn <- utils::getFromNamespace("parametric_ci", pkg)

    res_rubin <- rubin_rules_fn(ests = ests, ses = ses, v_com = v_com)

    ret <- parametric_ci_fn(
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
  } else {
    stop("The 'rbmi' package is needed for this function to work. Please install it.", call. = FALSE)
  }
}
