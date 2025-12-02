library(tern)

test_that("s_kaplan_meier works with default arguments", {
  adtte_f <- tern::tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = tern::day2month(AVAL),
      is_event = CNSR == 0
    )

  result <- expect_silent(s_kaplan_meier(
    df = adtte_f %>% dplyr::filter(ARMCD == "ARM B"),
    .var = "AVAL",
    is_event = "is_event"
  ))

  expect_snapshot(result)
})

test_that("s_kaplan_meier works with customized arguments", {
  adtte_f <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = tern::day2month(AVAL),
      is_event = CNSR == 0
    )
  # Make sure the highest value is censored to check range censor information.
  adtte_f$is_event[adtte_f$AVAL == max(adtte_f$AVAL)] <- FALSE

  result <- expect_silent(s_kaplan_meier(
    adtte_f,
    .var = "AVAL",
    is_event = "is_event",
    control = control_surv_time(
      conf_level = 0.99,
      conf_type = "log-log",
      quantiles = c(0.2, 0.8)
    )
  ))

  expect_snapshot(result)
})

test_that("a_kaplan_meier works with default arguments", {
  adtte_f <- tern::tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = tern::day2month(AVAL),
      is_event = CNSR == 0
    )
  adtte_f$is_event[adtte_f$AVAL == max(adtte_f$AVAL)] <- FALSE

  result <- expect_silent(a_kaplan_meier(
    df = adtte_f,
    .var = "AVAL",
    is_event = "is_event"
  ))

  res <- expect_silent(result)
  expect_snapshot(res)
})

test_that("a_kaplan_meier works with customized arguments", {
  adtte_f <- tern::tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = tern::day2month(AVAL),
      is_event = CNSR == 0
    )
  adtte_f$is_event[adtte_f$AVAL == max(adtte_f$AVAL)] <- FALSE

  result <- expect_silent(a_kaplan_meier(
    df = adtte_f,
    .var = "AVAL",
    is_event = "is_event",
    control = control_surv_time(
      conf_level = 0.99,
      conf_type = "log-log",
      quantiles = c(0.2, 0.8)
    )
  ))

  res <- expect_silent(result)
  expect_snapshot(res)
})

test_that("a_kaplan_meier works inside analyze in table", {
  adtte_f <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = tern::day2month(AVAL),
      is_event = CNSR == 0
    )
  adtte_f$is_event[
    adtte_f$AVAL == max(adtte_f$AVAL[adtte_f$ARMCD == "ARM A"])
  ] <- FALSE

  result <- basic_table(round_type = "sas") %>%
    split_cols_by(
      var = "ARMCD"
    ) %>%
    analyze(
      vars = "AVAL",
      afun = a_kaplan_meier,
      var_labels = "Kaplan-Meier estimate of time to event (months)",
      show_labels = "visible",
      extra_args = list(
        is_event = "is_event"
      )
    ) %>%
    build_table(df = adtte_f)

  res <- expect_silent(result)
  expect_snapshot(res)
})

test_that("a_kaplan_meier works inside analyze in table with custom arguments", {
  adtte_f <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = tern::day2month(AVAL),
      is_event = CNSR == 0
    )
  adtte_f$is_event[
    adtte_f$AVAL == max(adtte_f$AVAL[adtte_f$ARMCD == "ARM A"])
  ] <- FALSE

  result <- basic_table(round_type = "sas") %>%
    split_cols_by(
      var = "ARMCD"
    ) %>%
    analyze(
      vars = "AVAL",
      afun = a_kaplan_meier,
      var_labels = "Kaplan-Meier estimate of time to event (months)",
      show_labels = "visible",
      extra_args = list(
        is_event = "is_event",
        control = tern::control_surv_time(conf_level = 0.9, conf_type = "log"),
        .stats = c("median_ci_3d", "range_with_cens_info"),
        .formats = c(
          median_ci_3d = jjcsformat_xx("xx.xxxx (xx.xxxx, xx.xxxx)")
        ),
        .labels = c(range_with_cens_info = "Min and Max"),
        .indent_mods = c(median_ci_3d = 3L)
      )
    ) %>%
    build_table(df = adtte_f)

  res <- expect_silent(result)
  expect_snapshot(res)
})
