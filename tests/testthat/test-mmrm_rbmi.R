# Skip all tests if rbmi is not available
pkg <- "rbmi"
skip_if_not(requireNamespace(pkg, quietly = TRUE))

suppressPackageStartupMessages({
  if (requireNamespace(pkg, quietly = TRUE)) {
    library(rbmi)
  }
})

f2n <- function(x) as.numeric(x) - 1

test_that("mmrm_rbmi works as expected", {
  npats <- 100
  nvisits <- 3

  set.seed(101)
  pats <- tibble(
    subjid = paste0("PAT", sprintf("%03d", seq_len(npats))),
    age1 = rnorm(npats),
    age2 = rnorm(npats),
    grp = factor(sample(c("A", "B", "C"), size = npats, replace = TRUE))
  )
  dat <- tibble(
    subjid = rep(pats$subjid, each = nvisits),
    visit = factor(rep(paste0("VIS", seq_len(nvisits)), npats))
  ) |>
    dplyr::inner_join(pats, by = "subjid") |>
    dplyr::mutate(
      out = rnorm(
        npats * nvisits,
        mean = 50 + 3 * f2n(grp) + 4 * age1 + 8 * age2,
        sd = 20
      )
    )
  result <- expect_silent(rbmi_mmrm(
    data = dat,
    vars = utils::getFromNamespace("set_vars", pkg)(
      subjid = "subjid",
      outcome = "out",
      group = "grp",
      covariates = c("age1", "age2", "grp:visit"),
      visit = "visit"
    )
  ))
  checkmate::expect_list(result)
  expect_identical(
    names(result),
    c(
      "var_B_VIS1",
      "var_C_VIS1",
      "trt_B_VIS1",
      "trt_C_VIS1",
      "lsm_A_VIS1",
      "lsm_B_VIS1",
      "lsm_C_VIS1",
      "var_B_VIS2",
      "var_C_VIS2",
      "trt_B_VIS2",
      "trt_C_VIS2",
      "lsm_A_VIS2",
      "lsm_B_VIS2",
      "lsm_C_VIS2",
      "var_B_VIS3",
      "var_C_VIS3",
      "trt_B_VIS3",
      "trt_C_VIS3",
      "lsm_A_VIS3",
      "lsm_B_VIS3",
      "lsm_C_VIS3"
    )
  )
  expect_snapshot_value(result$var_B_VIS1, tolerance = 1e-3)
  expect_snapshot_value(result$trt_C_VIS2, tolerance = 1e-3)
  expect_snapshot_value(result$lsm_A_VIS3, tolerance = 1e-3)
})
