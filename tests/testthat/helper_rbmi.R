# Used in pool_rbmi tests.
rbmi_as_analysis <- function(
  results,
  method,
  delta = NULL,
  fun = NULL,
  fun_name = NULL
) {
  next_class <- switch(class(method)[[2]],
    bayes = "rubin",
    approxbayes = "rubin",
    condmean = ifelse(method$type == "jackknife", "jackknife", "bootstrap"),
    bmlmi = "bmlmi"
  )
  assert_that(
    is.list(results),
    length(next_class) == 1,
    is.character(next_class),
    next_class %in% c("jackknife", "bootstrap", "rubin", "bmlmi")
  )
  x <- list(
    results = as_class(results, c(next_class, "list")),
    delta = delta,
    fun = fun,
    fun_name = fun_name,
    method = method
  )
  class(x) <- c("analysis", "list")
  rbmi::validate(x)
  return(x)
}
