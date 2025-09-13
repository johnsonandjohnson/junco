test_that("s_event_free works with default arguments", {
  adtte_f <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = tern::day2month(AVAL),
      is_event = CNSR == 0
    )

  result <- s_event_free(
    df = adtte_f,
    .var = "AVAL",
    time_point = 6,
    is_event = "is_event",
    time_unit = "month"
  )

  res <- expect_silent(result)
  expect_snapshot(res)
})

test_that("s_event_free works with percent format", {
  adtte_f <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = tern::day2month(AVAL),
      is_event = CNSR == 0
    )

  result <- s_event_free(
    df = adtte_f,
    .var = "AVAL",
    time_point = 6,
    is_event = "is_event",
    time_unit = "month",
    percent = TRUE
  )

  res <- expect_silent(result)
  expect_snapshot(res)
})

test_that("a_event_free works with default arguments in a table layout", {
  adtte_f <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = tern::day2month(AVAL),
      is_event = CNSR == 0
    )

  lyt <- basic_table() %>%
    split_cols_by(var = "ARMCD")
  for (time_point in c(3, 4, 5)) {
    lyt <- lyt |>
      analyze(
        vars = "AVAL",
        afun = a_event_free,
        show_labels = "hidden",
        na_str = default_na_str(),
        table_names = paste0("AVAL_", time_point),
        extra_args = list(
          time_unit = "week",
          time_point = time_point,
          is_event = "is_event",
          .stats = "event_free_ci"
        )
      )
  }
  result <- build_table(lyt, df = adtte_f, round_type = "sas")

  res <- expect_silent(result)
  expect_snapshot(res)
})

test_that("a_event_free works with customized arguments in a table layout", {
  adtte_f <- tern_ex_adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVAL = tern::day2month(AVAL),
      is_event = CNSR == 0
    )

  lyt <- basic_table() %>%
    split_cols_by(var = "ARMCD")
  for (time_point in c(5, 1, 7)) {
    lyt <- lyt |>
      analyze(
        vars = "AVAL",
        afun = a_event_free,
        show_labels = "hidden",
        na_str = default_na_str(),
        table_names = paste0("AVAL_", time_point),
        extra_args = list(
          time_unit = "week",
          time_point = time_point,
          is_event = "is_event",
          control = control_surv_timepoint(
            conf_level = 0.9,
            conf_type = "log-log"
          ),
          percent = TRUE,
          .indent_mods = c(event_free_ci = 1L),
          .stats = "event_free_ci"
        )
      )
  }
  result <- build_table(lyt, df = adtte_f, round_type = "sas")

  res <- expect_silent(result)
  expect_snapshot(res)
})
