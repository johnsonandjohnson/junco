suppressPackageStartupMessages({
  library(testthat)
  library(rbmi)
  library(dplyr)
})

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
  # Test with unsorted AVISIT levels
  df_unsorted <- data.frame(
    AVISIT = factor(c(5, 4, 3, 2, 1)),
    CHG = c(NA, NA, 3, NA, 5)
  )
  result <- find_missing_chg_after_avisit(df_unsorted)
  expect_identical(result, "4")

  # Test with levels that aren't in sequential order
  df_nonseq <- data.frame(
    AVISIT = factor(c("Visit 10", "Visit 20", "Visit 30")),
    CHG = c(5, NA, NA)
  )
  result2 <- find_missing_chg_after_avisit(df_nonseq)
  expect_identical(result2, "Visit 20")
})

test_that("find_missing_chg_after_avisit validates input correctly", {
  # Not a data frame
  expect_error(
    find_missing_chg_after_avisit(list(AVISIT = factor(1:3), CHG = c(1, 2, 3))),
    "Assertion on 'df' failed"
  )

  # AVISIT not a factor
  df_char <- data.frame(AVISIT = c("1", "2", "3"), CHG = c(1, 2, 3))
  expect_error(
    find_missing_chg_after_avisit(df_char),
    "Assertion on 'df\\$AVISIT' failed"
  )

  # CHG not numeric
  df_char_chg <- data.frame(AVISIT = factor(1:3), CHG = c("1", "2", "3"))
  expect_error(
    find_missing_chg_after_avisit(df_char_chg),
    "Assertion on 'df\\$CHG' failed"
  )
})

test_that("make_rbmi_cluster returns NULL when cluster_or_cores is 1", {
  result <- make_rbmi_cluster(cluster_or_cores = 1)
  expect_null(result)
})

test_that("make_rbmi_cluster exports objects and loads packages", {
  skip_on_cran()
  # Create a simple test object and function
  test_value <- 42
  test_fun <- function(x) x * 2

  # Test with a small cluster to avoid excessive resource usage
  cl <- tryCatch(
    {
      make_rbmi_cluster(
        cluster_or_cores = 2,
        objects = list(test_value = test_value, test_fun = test_fun),
        packages = "stats"
      )
    },
    error = function(e) {
      skip("Could not create cluster, skipping test")
      NULL
    }
  )

  # Only proceed if we successfully created a cluster
  if (!is.null(cl)) {
    # Check if the objects were exported correctly
    test_result <- tryCatch(
      {
        parallel::clusterEvalQ(cl, test_fun(test_value))
      },
      error = function(e) {
        list(NA)
      }
    )

    # Clean up
    parallel::stopCluster(cl)

    # The function should double our value
    expect_equal(unlist(test_result[1]), 84)
  }
})

test_that("make_rbmi_cluster handles existing cluster", {
  skip_on_cran()
  cl <- tryCatch(
    {
      parallel::makeCluster(2)
    },
    error = function(e) {
      skip("Could not create cluster, skipping test")
      NULL
    }
  )

  if (!is.null(cl)) {
    result <- make_rbmi_cluster(cl)
    expect_identical(result, cl)
    parallel::stopCluster(cl)
  }
})

test_that("make_rbmi_cluster rejects invalid cluster_or_cores parameter", {
  expect_error(
    make_rbmi_cluster(cluster_or_cores = "not-a-number"),
    "`cluster_or_cores` has unsupported class of: character"
  )
})

test_that("par_lapply works with and without cluster", {
  # Test without cluster
  result1 <- par_lapply(NULL, function(x) x^2, 1:3)
  expect_equal(result1, list(1, 4, 9))

  # Test with cluster (if possible)
  skip_on_cran()
  cl <- tryCatch(
    {
      make_rbmi_cluster(cluster_or_cores = 2)
    },
    error = function(e) {
      skip("Could not create cluster, skipping test")
      NULL
    }
  )

  if (!is.null(cl)) {
    result2 <- par_lapply(cl, function(x) x^2, 1:3)
    parallel::stopCluster(cl)
    expect_equal(result2, list(1, 4, 9))
  }
})

test_that("make_rbmi_cluster loads rbmi namespaces correctly", {
  skip_on_cran()

  # Only run this test if we can create a cluster
  cl <- tryCatch(
    {
      parallel::makeCluster(2)
    },
    error = function(e) {
      skip("Could not create cluster for namespace test")
      NULL
    }
  )

  if (!is.null(cl)) {
    # First make sure the rbmi package is available in the cluster
    cluster_has_rbmi <- FALSE
    tryCatch(
      {
        cluster_has_rbmi <- all(unlist(parallel::clusterEvalQ(
          cl,
          requireNamespace("rbmi")
        )))
      },
      error = function(e) {
        # If this fails, it might be because rbmi isn't installed in test env
        skip("rbmi package not available in cluster")
      }
    )

    if (cluster_has_rbmi) {
      result <- make_rbmi_cluster(cl)
      # Check if the exported variable was set correctly
      exported_flag <- unlist(parallel::clusterEvalQ(
        cl,
        exists("..exported..parallel..rbmi")
      ))
      expect_true(all(exported_flag))
    }

    parallel::stopCluster(cl)
  }
})

test_that("Parallisation works with rbmi_analyse and produces identical results", {
  skip_on_cran()
  set.seed(4642)
  sigma <- as_vcov(
    c(2, 1, 0.7, 1.5),
    c(0.5, 0.3, 0.2, 0.3, 0.5, 0.4)
  )
  dat <- get_sim_data(200, sigma, trt = 8) %>%
    mutate(
      outcome = if_else(
        rbinom(n(), 1, 0.3) == 1 & group == "A",
        NA_real_,
        outcome
      )
    )

  dat_ice <- dat %>%
    group_by(id) %>%
    arrange(id, visit) %>%
    filter(is.na(outcome)) %>%
    slice(1) %>%
    ungroup() %>%
    select(id, visit) %>%
    mutate(strategy = "JR")

  vars <- set_vars(
    outcome = "outcome",
    group = "group",
    strategy = "strategy",
    subjid = "id",
    visit = "visit",
    covariates = c("age", "sex", "visit * group")
  )

  set.seed(984)
  drawobj <- draws(
    data = dat,
    data_ice = dat_ice,
    vars = vars,
    method = method_condmean(n_samples = 6, type = "bootstrap"),
    quiet = TRUE
  )

  imputeobj <- impute(
    draws = drawobj,
    references = c("A" = "B", "B" = "B")
  )

  #
  # Here we set up a bunch of different analysis objects using different
  # of parallelisation methods and different dat_delta objects
  #

  ### Delta 1

  dat_delta_1 <- delta_template(imputations = imputeobj) %>%
    mutate(delta = is_missing * 5)

  vars2 <- vars
  vars2$covariates <- c("age", "sex")

  anaobj_d1_t1 <- rbmi_analyse(
    imputeobj,
    fun = rbmi::ancova,
    vars = vars2,
    delta = dat_delta_1
  )

  anaobj_d1_t2 <- rbmi_analyse(
    imputeobj,
    fun = rbmi::ancova,
    vars = vars2,
    delta = dat_delta_1,
    cluster_or_cores = 2
  )

  var <- 20
  inner_fun <- function(...) {
    x <- day(var) # lubridate::day
    rbmi::ancova(...)
  }
  outer_fun <- function(...) {
    inner_fun(...)
  }

  cl <- make_rbmi_cluster(
    2,
    objects = list(var = var, inner_fun = inner_fun),
    "lubridate"
  )
  anaobj_d1_t3 <- rbmi_analyse(
    imputeobj,
    fun = rbmi::ancova,
    vars = vars2,
    delta = dat_delta_1,
    cluster_or_cores = cl
  )

  ### Delta 2

  dat_delta_2 <- delta_template(imputations = imputeobj) %>%
    mutate(delta = is_missing * 50)

  anaobj_d2_t1 <- rbmi_analyse(
    imputeobj,
    fun = rbmi::ancova,
    vars = vars2,
    delta = dat_delta_2
  )
  anaobj_d2_t3 <- rbmi_analyse(
    imputeobj,
    fun = rbmi::ancova,
    vars = vars2,
    delta = dat_delta_2,
    cluster_or_cores = cl
  )

  ### Delta 3 (no delta)

  anaobj_d3_t1 <- rbmi_analyse(
    imputeobj,
    fun = rbmi::ancova,
    vars = vars2
  )
  anaobj_d3_t3 <- rbmi_analyse(
    imputeobj,
    fun = rbmi::ancova,
    vars = vars2,
    cluster_or_cores = cl
  )

  ## Check for internal consistency
  expect_equal(anaobj_d1_t1, anaobj_d1_t2)
  expect_equal(anaobj_d1_t1, anaobj_d1_t3)
  expect_equal(anaobj_d1_t2, anaobj_d1_t3)

  expect_equal(anaobj_d2_t1, anaobj_d2_t3)

  expect_equal(anaobj_d3_t1, anaobj_d3_t3)

  ## Check that they differ (as different deltas have been used)
  ## Main thing is sanity checking that the embedded delta
  ## in the parallel processes hasn't lingered and impacted
  ## future results

  # First assert consistency
  expect_true(identical(anaobj_d1_t1$results, anaobj_d1_t3$results))
  expect_true(identical(anaobj_d2_t1$results, anaobj_d2_t3$results))
  expect_true(identical(anaobj_d3_t1$results, anaobj_d3_t3$results))

  # The ensure they are different
  expect_false(identical(anaobj_d1_t1$results, anaobj_d2_t1$results))
  expect_false(identical(anaobj_d1_t1$results, anaobj_d3_t1$results))
  expect_false(identical(anaobj_d2_t1$results, anaobj_d3_t1$results))
  parallel::stopCluster(cl)
})
