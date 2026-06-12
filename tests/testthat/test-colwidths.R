fontspec <- font_spec("Times", 9L, 1)
ADSL <- data.frame(
  USUBJID = c(
    "XXXXX01",
    "XXXXX02",
    "XXXXX03",
    "XXXXX04",
    "XXXXX05",
    "XXXXX06",
    "XXXXX07",
    "XXXXX08",
    "XXXXX09",
    "XXXXX10"
  ),
  TRT01P = factor(
    c(
      "ARMA",
      "ARMB",
      "ARMA",
      "ARMB",
      "ARMB",
      "Placebo",
      "Placebo",
      "Placebo",
      "ARMA",
      "ARMB"
    )
  ),
  FASFL = c("Y", "Y", "Y", "Y", "N", "Y", "Y", "Y", "Y", "Y"),
  SAFFL = c("N", "N", "N", "N", "N", "N", "N", "N", "N", "N"),
  PKFL = c("N", "N", "N", "N", "N", "N", "N", "N", "N", "N")
)

lyt <- basic_table(round_type = "sas") |>
  split_cols_by("TRT01P") |>
  add_overall_col("Total") |>
  analyze(
    "FASFL",
    var_labels = "Analysis set:",
    afun = a_freq_j,
    extra_args = list(label = "Full", val = "Y"),
    show_labels = "visible"
  ) |>
  analyze(
    "SAFFL",
    var_labels = "Analysis set:",
    afun = a_freq_j,
    extra_args = list(label = "Safety", val = "Y"),
    show_labels = "visible"
  ) |>
  analyze(
    "PKFL",
    var_labels = "Analysis set:",
    afun = a_freq_j,
    extra_args = list(label = "PK", val = "Y"),
    show_labels = "visible"
  )

tt <- build_table(lyt, ADSL)

test_that("ttype_wrap_vec works as expected", {
  result <- ttype_wrap_vec(vec = c(1, 2, 3, 4) |> as.character(), fontspec = fontspec, width = 2)
  expected_result <- list(
    c("1"),
    c("2"),
    c("3"),
    c("4")
  )
  testthat::expect_equal(result, expected_result)
})

test_that("check_wrap_nobreak works as expected", {
  # Derive a safe width programmatically (max word length per column + a margin)
  largest_word <- function(x) {
    max(nchar(x), na.rm = TRUE)
  }
  mpf <- matrix_form(tt, fontspec = fontspec)
  strs <- mf_strings(mpf)
  max_per_col <- apply(strs, 2, largest_word)
  # add a small margin
  safe_widths <- max_per_col + 1L

  result <- check_wrap_nobreak(tt = tt, colwidths = safe_widths, fontspec = fontspec)
  testthat::expect_true(result)

  result <- check_wrap_nobreak(tt = tt, colwidths = max_per_col, fontspec = fontspec)
  testthat::expect_false(result)
})


test_that("def_colwidths works as expected", {
  anl <- ex_adsl
  anl <- anl[1:10, c("USUBJID", "ARM", "BMRKR1")]
  anl <- var_relabel(anl, USUBJID = "Unique\nSubject\nIdentifier", ARM = "Description\nOf\nPlanned Arm")

  suppressMessages(
    tt2 <- as_listing(anl, key_cols = c("USUBJID")) |>
      add_listing_col("ARM")
  )

  result <- def_colwidths(tt = tt2, fontspec = fontspec)
  expected_result <- c(67, 72, 84)
  # fixed expected_result to back us up from
  # the hardcoded colwidths changes in scda.test
  testthat::expect_equal(result, expected_result)
})

test_that("def_colwidths does not fail when a column label is too large", {
  library(dplyr)
  popfl <- "FASFL"
  trtvar <- "TRT01P"
  key_cols <- c("COL0", "COL1")
  disp_cols <- paste0("COL", 0:8)
  concat_sep <- " / "

  adsl <- pharmaverseadamjnj::adsl |>
    filter(!!rlang::sym(popfl) == "Y" & !(EOTSTT %in% c("COMPLETED", "ONGOING")))

  adsl_ds <- adsl
  adsl_ds$DSSCAT <- "TREATMENT"

  adexsum <- pharmaverseadamjnj::adexsum |>
    filter(PARAMCD == "CUMDOSE") |>
    select(STUDYID, USUBJID, PARAMCD, PARAM, AVAL)

  adsl_ds_adexsum <- left_join(
    adsl_ds,
    adexsum,
    by = c(
      "STUDYID" = "STUDYID",
      "USUBJID" = "USUBJID"
    )
  )

  lsting <- adsl_ds_adexsum |>
    mutate(
      AGE = explicit_na(as.character(AGE), ""),
      SEX = explicit_na(SEX, ""),
      RACE = explicit_na(RACE, ""),
      AVAL = explicit_na(as.character(AVAL), ""),
      AVALU = case_when(
        !is.na(AVAL) ~
          stringr::str_extract(PARAM, "(?<=\\()([^()]*?)(?=\\)[^()]*$)"),
        is.na(AVAL) ~ ""
      ),
      DCSREAS = explicit_na(DCSREAS, ""),
      DCTREASP = explicit_na(DCTREASP, ""),
      COL0 = explicit_na(.data[[trtvar]], ""),
      COL1 = explicit_na(USUBJID, ""),
      COL2 = paste(AGE, SEX, RACE, sep = concat_sep),
      COL3 = explicit_na(stringr::str_to_sentence(DSSCAT), ""),
      COL4 = explicit_na(LTVISIT, ""),
      COL5 = explicit_na(as.character(TRTEDY), ""),
      COL6 = paste0(AVAL, " ", AVALU),
      COL7 = ifelse(
        is.na(DCTDT),
        "",
        toupper(format(as.Date(DCTDT), format = "%d%b%Y"))
      ),
      COL8 = case_when(
        DCSREAS == "OTHER" ~
          paste0(DCSREAS, " (", stringr::str_to_sentence(DCTREASP), ")"),
        DCSREAS != "OTHER" ~ DCSREAS
      )
    ) |>
    arrange(COL0, COL1, COL2, COL3)

  lsting <- lsting |>
    mutate(COL7 = ifelse(is.na(DCTADY), COL7, sprintf("%s (%s)", COL7, DCTADY)))

  lsting <- var_relabel(
    lsting,
    COL0 = "Treatment Group",
    COL1 = "Subject ID",
    COL2 = paste("Age (years)", "Sex", "Race", sep = concat_sep),
    COL3 = "Study Agent Discontinued",
    COL4 = "Last Visit~[super a]",
    COL5 = "Date of Last Study Agent Administered (Study Day~[super a])",
    COL6 = "Cumulative Dose (unit)",
    COL7 = "Date of Discontinuation (Study Day~[super b])",
    COL8 = "Primary Reason for Discontinuation"
  )

  result <- rlistings::as_listing(
    df = lsting,
    key_cols = key_cols,
    disp_cols = disp_cols
  )

  fontspec <- font_spec("Times", 9L, 1.2)
  col_gap <- 7L
  label_width_ins <- 2

  testthat::expect_no_error(
    colwidths <- def_colwidths(
      result,
      fontspec,
      col_gap = col_gap,
      label_width_ins = label_width_ins
    )
  )
  expected_result <- c(34, 26, 36, 36, 31, 36, 33, 40, 40)
  testthat::expect_equal(colwidths, expected_result)
})

test_that("listing_column_widths works as expected", {
  anl <- ex_adsl
  anl <- anl[1:10, c("USUBJID", "ARM", "BMRKR1")]
  anl <- var_relabel(anl, USUBJID = "Unique\nSubject\nIdentifier", ARM = "Description\nOf\nPlanned Arm")
  suppressMessages(
    tt3 <- as_listing(anl, key_cols = c("USUBJID")) |>
      add_listing_col("ARM")
  )

  mpf <- rlistings::matrix_form(tt3)
  suppressMessages(testthat::expect_message(result <- listing_column_widths(mpf, verbose = TRUE)))
  expected_result <- c(67, 72, 84)
  # fixed expected_result to back us up from
  # the hardcoded colwidths changes in scda.test
  testthat::expect_equal(result, expected_result)
})

test_that("find_free_colspc works as expected", {
  anl <- ex_adsl
  anl <- anl[1:10, c("USUBJID", "ARM", "BMRKR1")]
  anl <- var_relabel(anl, USUBJID = "Unique\nSubject\nIdentifier", ARM = "Description\nOf\nPlanned Arm")
  suppressMessages(
    tt4 <- as_listing(anl, key_cols = c("USUBJID")) |>
      add_listing_col("ARM")
  )
  mpf <- rlistings::matrix_form(tt4)

  possdf <- make_poss_wdf(mpf)

  odf <- order(possdf$col_num, possdf$colwidth)
  possdf <- possdf[odf, ]
  full_possdf <- possdf
  ## already ordered by colnum then width so this the first of each colwidth is the min width for that col
  dups <- duplicated(possdf$col_num)
  curdf <- possdf[!dups, ]
  possdf <- possdf[dups, ] ## without rows for ones in curdf

  suppressWarnings(result <- find_free_colspc(curposs = curdf, fullposs = full_possdf))

  expect_snapshot(cran = TRUE, result)
})

test_that("smart_colwidths_1page works as expected", {
  expected_smart <- (function(
      tt,
      fontspec,
      col_gap = 6L,
      rowlabel_width_ins = 2,
      print_width_ins = 8.5 - 2.12,
      landscape = FALSE,
      lastcol_gap = TRUE) {
    # Derive all intermediate values exactly
    rowlabel_width <- inches_to_spaces(rowlabel_width_ins, fontspec)
    total_cpp <- floor(inches_to_spaces(ifelse(landscape, 11, 8.5) - 2.12, fontspec = fontspec, raw = TRUE))
    nc <- ncol(tt)
    remain <- total_cpp - rowlabel_width - col_gap * (nc - !lastcol_gap)
    c(rowlabel_width - col_gap, formatters:::spread_integer(remain, nc))
  })(tt, fontspec)

  result <- smart_colwidths_1page(tt = tt, fontspec = fontspec)
  expect_equal(result, expected_smart)
  expect_equal(length(result), ncol(tt) + 1)

  # Gap sensitivity check
  result2 <- smart_colwidths_1page(tt = tt, fontspec = fontspec, col_gap = 0)
  result3 <- smart_colwidths_1page(tt = tt, fontspec = fontspec, col_gap = 2)
  expect_equal(result2 - 2, result3)
})

test_that("spaces_to_inches works as expected", {
  testthat::expect_equal(spaces_to_inches(spcs = 2, fontspec = fontspec), 2)
})
