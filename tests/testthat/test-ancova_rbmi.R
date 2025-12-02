library(tibble)
library(dplyr)

if (requireNamespace("rbmi", quietly = TRUE)) {
  suppressPackageStartupMessages(library(rbmi))
} else {
  skip("rbmi package not available")
}

f2n <- function(x) as.numeric(x) - 1

test_that("rbmi_ancova_single works also with multiple treatment arms", {
  set.seed(101)
  n <- 900
  dat <- tibble(
    visit = "vis1",
    age1 = rnorm(n),
    age2 = rnorm(n),
    grp = factor(sample(c("A", "B"), size = n, replace = TRUE)),
    out = rnorm(n, mean = 50 + 3 * f2n(grp) + 4 * age1 + 8 * age2, sd = 20)
  )
  result <- rbmi_ancova_single(
    data = dat,
    outcome = "out",
    group = "grp",
    covariates = c("age1", "age2"),
    weights = "equal"
  )
  checkmate::expect_list(result)
  expect_identical(
    names(result),
    c("var", "trt_B", "lsm_A", "lsm_B")
  )
})

test_that("ancova now works also with multiple treatment arms", {
  set.seed(101)

  n <- 1000
  dat <- tibble(
    visit = "vis1",
    age1 = rnorm(n),
    age2 = rnorm(n),
    grp = factor(sample(c("A", "B"), size = n, replace = TRUE)),
    out = rnorm(n, mean = 50 + 3 * f2n(grp) + 4 * age1 + 8 * age2, sd = 20)
  )

  mod <- lm(out ~ age1 + age2 + grp, data = dat)

  result <- rbmi_ancova(
    data = dat,
    vars = list(
      outcome = "out",
      group = "grp",
      covariates = c("age1", "age2"),
      visit = "visit"
    ),
    weights = "counterfactual"
  )["trt_B_vis1"]
  expected <- list(
    "trt_B_vis1" = list(
      "est" = coef(mod)[4],
      "se" = sqrt(vcov(mod)[4, 4]),
      "df" = df.residual(mod)
    )
  )
  expect_equal(result, expected)

  # Multiple Visits

  n <- 900
  dat <- tibble(
    age1 = rnorm(n),
    age2 = rnorm(n),
    vis = sample(c("visit 1", "visit 2"), size = n, replace = TRUE),
    grp = factor(sample(c("A", "B"), size = n, replace = TRUE)),
    out = rnorm(n, mean = 50 + 3 * f2n(grp) + 4 * age1 + 8 * age2, sd = 20)
  )

  mod <- lm(out ~ age1 + age2 + grp, data = filter(dat, vis == "visit 1"))

  result <- rbmi_ancova(
    dat,
    list(
      outcome = "out",
      group = "grp",
      covariates = c("age1", "age2"),
      visit = "vis"
    ),
    visits = "visit 1",
    weights = "counterfactual"
  )["trt_B_visit 1"]

  expected <- list(
    "trt_B_visit 1" = list(
      "est" = coef(mod)[4],
      "se" = sqrt(vcov(mod)[4, 4]),
      "df" = df.residual(mod)
    )
  )
  expect_equal(result, expected)

  result <- rbmi_ancova(
    dat,
    list(
      outcome = "out",
      group = "grp",
      covariates = c("age1", "age2"),
      visit = "vis"
    ),
    visits = c("visit 1", "visit 2"),
    weights = "counterfactual"
  )["trt_B_visit 1"]

  expect_equal(result, expected)

  result <- rbmi_ancova(
    dat,
    list(
      outcome = "out",
      group = "grp",
      covariates = c("age1", "age2"),
      visit = "vis"
    ),
    visits = c("visit 1", "visit 2"),
    weights = "counterfactual"
  )

  mod <- lm(out ~ age1 + age2 + grp, data = filter(dat, vis == "visit 2"))

  expected <- list(
    "trt_B_visit 2" = list(
      "est" = coef(mod)[4],
      "se" = sqrt(vcov(mod)[4, 4]),
      "df" = df.residual(mod)
    )
  )

  expect_equal(result["trt_B_visit 2"], expected)

  expect_identical(
    names(result),
    c(
      "var_visit 1",
      "trt_B_visit 1",
      "lsm_A_visit 1",
      "lsm_B_visit 1",
      "var_visit 2",
      "trt_B_visit 2",
      "lsm_A_visit 2",
      "lsm_B_visit 2"
    )
  )
})
