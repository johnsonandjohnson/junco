tdir <- tempdir()

library(tibble)

titledf <- tribble(
  ~`TABLE ID`, ~`IDENTIFIER`, ~`TEXT`,
  "tbl001", "TITLE", "My cool title",
  "tbl001", "FOOT1", "My cool footer",
  "tbl001", "FOOT2", "more footer stuff",
  "tbl002", "TITLE", "Different title"
)
write.csv(titledf, file = file.path(tdir, "titles.csv"), row.names = FALSE)

test_that("title files work", {
  suppressMessages(res <- get_titles_from_file("tbl001", input_path = tdir))
  expect_identical(
    res,
    list(
      title = "My cool title",
      subtitles = NULL,
      main_footer = c("My cool footer", "more footer stuff"),
      prov_footer = NULL
    )
  )
  expect_warning(suppressMessages(get_titles_from_file("lololol", input_path = tdir)))

  lyt <- basic_table() |>
    analyze("AGE")
  tbl <- build_table(lyt, DM)
  expect_silent(tbl <- set_titles(tbl, res))
  expect_identical(main_title(tbl), res$title)
  expect_identical(main_footer(tbl), res$main_footer)

  expect_error(get_titles_from_file("tbl001", input_path = "."))
})
