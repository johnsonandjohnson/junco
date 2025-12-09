# Used in test-pool_rbmi.R ----
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

# Used in test-rbmi.R ----

set_col_names <- function(x, nam) {
  colnames(x) <- nam
  return(x)
}

f2n <- function(x) as.numeric(x) - 1

get_sim_data <- function(n, sigma, trt = 4) {
  nv <- ncol(sigma)
  covars <- tibble::tibble(
    id = 1:n,
    age = rnorm(n),
    group = factor(
      sample(c("A", "B"), size = n, replace = TRUE),
      levels = c("A", "B")
    ),
    sex = factor(
      sample(c("M", "F"), size = n, replace = TRUE),
      levels = c("M", "F")
    )
  )

  dat <- mvtnorm::rmvnorm(n, sigma = sigma) %>%
    set_col_names(paste0("visit_", 1:nv)) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(id = seq_len(dplyr::n())) %>%
    tidyr::gather("visit", "outcome", -id) %>%
    dplyr::mutate(visit = factor(.data$visit)) %>%
    dplyr::arrange(id, .data$visit) %>%
    dplyr::left_join(covars, by = "id") %>%
    dplyr::mutate(
      outcome = .data$outcome +
        5 +
        3 * .data$age +
        3 * f2n(.data$sex) +
        trt * f2n(.data$group)
    ) %>%
    dplyr::mutate(id = as.factor(id))

  return(dat)
}
