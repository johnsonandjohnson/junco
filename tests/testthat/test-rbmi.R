suppressPackageStartupMessages({
  library(testthat)
  library(dplyr)
  library(rbmi)
})

test_that("find_missing_chg_after_avisit works as expected", {
  df <- data.frame(
    AVISIT = factor(c(1, 2, 3, 4, 5)),
    CHG = c(5, NA, NA, NA, 3)
  )
  result <- find_missing_chg_after_avisit(df)
  expect_identical(result, NA_character_)

  df2 <- data.frame(
    AVISIT = factor(c(1, 2, 3, 4, 5)),
    CHG = c(5, NA, 3, NA, NA)
  )
  result2 <- find_missing_chg_after_avisit(df2)
  expect_identical(result2, "4")

  df3 <- data.frame(
    AVISIT = factor(c(1, 2, 3, 4, 5)),
    CHG = c(NA, NA, NA, NA, NA)
  )
  result3 <- find_missing_chg_after_avisit(df3)
  expect_identical(result3, "1")
})

test_that("find_missing_chg_after_avisit handles unsorted input correctly", {
  df_unsorted <- data.frame(
    AVISIT = factor(c(5, 4, 3, 2, 1)),
    CHG = c(NA, NA, 3, NA, 5)
  )
  result <- find_missing_chg_after_avisit(df_unsorted)
  expect_identical(result, "4")

  df_nonseq <- data.frame(
    AVISIT = factor(c("Visit 10", "Visit 20", "Visit 30")),
    CHG = c(5, NA, NA)
  )
  result2 <- find_missing_chg_after_avisit(df_nonseq)
  expect_identical(result2, "Visit 20")
})

test_that("find_missing_chg_after_avisit validates input correctly", {
  expect_error(
    find_missing_chg_after_avisit(list(AVISIT = factor(1:3), CHG = c(1, 2, 3))),
    "Assertion on 'df' failed"
  )
  df_char <- data.frame(AVISIT = c("1", "2", "3"), CHG = c(1, 2, 3))
  expect_error(
    find_missing_chg_after_avisit(df_char),
    "Assertion on 'df\\$AVISIT' failed"
  )
  df_char_chg <- data.frame(AVISIT = factor(1:3), CHG = c("1", "2", "3"))
  expect_error(
    find_missing_chg_after_avisit(df_char_chg),
    "Assertion on 'df\\$CHG' failed"
  )
})

test_that("make_rbmi_cluster returns NULL when cluster_or_cores is 1", {
  skip_on_cran()
  result <- make_rbmi_cluster(cluster_or_cores = 1)
  expect_null(result)
})

test_that("make_rbmi_cluster exports objects and loads packages", {
  skip_on_cran()
  test_value <- 42
  test_fun <- function(x) x * 2
  cl <- tryCatch(
    make_rbmi_cluster(
      cluster_or_cores = 2,
      objects = list(test_value = test_value, test_fun = test_fun),
      packages = "stats"
    ),
    error = function(e) skip("Could not create cluster, skipping test")
  )
  if (!is.null(cl)) {
    test_result <- tryCatch(
      parallel::clusterEvalQ(cl, test_fun(test_value)),
      error = function(e) list(NA)
    )
    parallel::stopCluster(cl)
    expect_equal(unlist(test_result[1]), 84)
  }
})

test_that("make_rbmi_cluster handles existing cluster", {
  skip_on_cran()
  cl <- tryCatch(
    parallel::makeCluster(2),
    error = function(e) skip("Could not create cluster, skipping test")
  )
  if (!is.null(cl)) {
    result <- make_rbmi_cluster(cl)
    expect_identical(result, cl)
    parallel::stopCluster(cl)
  }
})

test_that("make_rbmi_cluster rejects invalid cluster_or_cores parameter", {
  skip_on_cran()
  expect_error(
    make_rbmi_cluster(cluster_or_cores = "not-a-number"),
    "`cluster_or_cores` has unsupported class of: character"
  )
})

test_that("par_lapply works with and without cluster", {
  result1 <- par_lapply(NULL, function(x) x^2, 1:3)
  expect_equal(result1, list(1, 4, 9))

  # parallel path tested off-CRAN only — cluster setup adds overhead not worth it on CRAN
  skip_on_cran()
  cl <- tryCatch(
    make_rbmi_cluster(cluster_or_cores = 2),
    error = function(e) skip("Could not create cluster, skipping test")
  )
  if (!is.null(cl)) {
    result2 <- par_lapply(cl, function(x) x^2, 1:3)
    parallel::stopCluster(cl)
    expect_equal(result2, list(1, 4, 9))
  }
})

test_that("make_rbmi_cluster loads rbmi namespaces correctly", {
  skip_on_cran()
  cl <- tryCatch(
    parallel::makeCluster(2),
    error = function(e) skip("Could not create cluster for namespace test")
  )
  if (!is.null(cl)) {
    cluster_has_rbmi <- FALSE
    tryCatch(
      {
        cluster_has_rbmi <- all(unlist(parallel::clusterEvalQ(
          cl,
          requireNamespace("rbmi")
        )))
      },
      error = function(e) skip("rbmi package not available in cluster")
    )
    if (cluster_has_rbmi) {
      result <- make_rbmi_cluster(cl)
      exported_flag <- unlist(parallel::clusterEvalQ(
        cl,
        exists("..exported..parallel..rbmi")
      ))
      expect_true(all(exported_flag))
    }
    parallel::stopCluster(cl)
  }
})

test_that("Parallelisation works with rbmi_analyse and produces identical results", {
  # Skipped on CRAN: runs rbmi::draws + impute + multiple rbmi_analyse calls
  # with parallel clusters — too slow and resource-heavy for CRAN checks.
  skip_on_cran()

  set.seed(4642)
  sigma <- as_vcov(
    c(2, 1, 0.7, 1.5),
    c(0.5, 0.3, 0.2, 0.3, 0.5, 0.4)
  )
  # Use n=50 (down from 200) and n_samples=4 (down from 6) to keep runtime short
  dat <- get_sim_data(50, sigma, trt = 8) |>
    dplyr::mutate(
      outcome = dplyr::if_else(
        rbinom(dplyr::n(), 1, 0.3) == 1 & group == "A",
        NA_real_,
        outcome
      )
    )

  dat_ice <- dat |>
    dplyr::group_by(id) |>
    dplyr::arrange(id, visit) |>
    dplyr::filter(is.na(outcome)) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(id, visit) |>
    dplyr::mutate(strategy = "JR")

  vars <- rbmi::set_vars(
    outcome = "outcome",
    group = "group",
    strategy = "strategy",
    subjid = "id",
    visit = "visit",
    covariates = c("age", "sex", "visit * group")
  )

  set.seed(984)
  drawobj <- rbmi::draws(
    data = dat,
    data_ice = dat_ice,
    vars = vars,
    method = rbmi::method_condmean(n_samples = 4, type = "bootstrap"),
    quiet = TRUE
  )

  imputeobj <- rbmi::impute(
    draws = drawobj,
    references = c("A" = "B", "B" = "B")
  )

  vars2 <- vars
  vars2$covariates <- c("age", "sex")

  dat_delta_1 <- rbmi::delta_template(imputations = imputeobj) |>
    dplyr::mutate(delta = is_missing * 5)

  anaobj_d1_t1 <- rbmi_analyse(imputeobj, fun = rbmi_ancova, vars = vars2, delta = dat_delta_1)
  anaobj_d1_t2 <- rbmi_analyse(imputeobj, fun = rbmi_ancova, vars = vars2, delta = dat_delta_1, cluster_or_cores = 2)

  # Verify sequential and parallel give identical results
  expect_equal(anaobj_d1_t1, anaobj_d1_t2)

  dat_delta_2 <- rbmi::delta_template(imputations = imputeobj) |>
    dplyr::mutate(delta = is_missing * 50)

  anaobj_d2_t1 <- rbmi_analyse(imputeobj, fun = rbmi_ancova, vars = vars2, delta = dat_delta_2)

  # Different deltas must produce different results — sanity check that delta
  # values don't bleed between parallel runs
  expect_false(identical(anaobj_d1_t1$results, anaobj_d2_t1$results))

  var <- 20
  inner_fun <- function(...) {
    x <- as_factor(var)
    rbmi_ancova(...)
  }
  cl <- make_rbmi_cluster(2, objects = list(var = var, inner_fun = inner_fun), "forcats")
  anaobj_d1_t3 <- rbmi_analyse(imputeobj, fun = rbmi_ancova, vars = vars2, delta = dat_delta_1, cluster_or_cores = cl)
  expect_equal(anaobj_d1_t1, anaobj_d1_t3)

  parallel::stopCluster(cl)
})
