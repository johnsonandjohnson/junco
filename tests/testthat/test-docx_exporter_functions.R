library(dplyr)
library(tern)
library(rtables)
library(ggplot2)



skip_on_cran()

adsl <- ex_adsl
adae <- ex_adae
extra_args_1 <- list(
  .stats = c("count_unique_denom_fraction")
)
lyt1 <- basic_table(show_colcounts = TRUE) |>
  split_cols_by("ARM") |>
  analyze(
    vars = "COUNTRY",
    afun = a_freq_j,
    extra_args = extra_args_1
  )

tbl1 <- build_table(lyt1, adsl)

tab_titles <- list(
  "title" = "This is the main Title",
  "subtitles" = NULL,
  "main_footer" = c(
    "footer 1",
    "footer 2"
  ),
  "prov_footer" = NULL
)
tbl1b <- set_titles(tbl1, tab_titles)

tbl1c <- tbl1
tab_titles <- list(
  "title" = "This is the main Title",
  "subtitles" = NULL,
  "main_footer" = c(
    "~{super a}Canada",
    "This ~{optional text should} stay the same"
  ),
  "prov_footer" = NULL
)
tbl1c <- set_titles(tbl1c, tab_titles)

rtables::label_at_path(tbl1c, c("COUNTRY", "count_unique_denom_fraction.CAN")) <-
  "CAN~[super a]"
rtables::label_at_path(tbl1c, c("COUNTRY", "count_unique_denom_fraction.JPN")) <-
  "JPN >="

snapshot_test_docx <- function(doc) {
  if (Sys.info()[["sysname"]] == "Windows") {
    testthat::expect_snapshot(doc$doc_obj$get() |> xml2::xml_child(1) |> as.character())
  }
}

snapshot_test_flextable <- function(res) {
  testthat::expect_true(inherits(res, "flextable"))
  if (Sys.info()[["sysname"]] == "Windows") {
    testthat::expect_snapshot(res$header)
    testthat::expect_snapshot(res$body)
    testthat::expect_snapshot(res$footer)
    testthat::expect_snapshot(res$col_keys)
    testthat::expect_snapshot(res$caption)
    testthat::expect_snapshot(res$blanks)
    testthat::expect_snapshot(res$properties)

    doc <- officer::read_docx()
    doc <- flextable::body_add_flextable(doc, res, align = "center")
    snapshot_test_docx(doc)
  }
}


testthat::test_that("tt_to_flextable_j() works fine with Tables", {
  testthat::expect_error(
    tt_to_flextable_j(tt = adsl, tblid = "output ID"),
    "Input object is not an rtables' or rlistings' object."
  )

  # basic example
  options(docx.add_datetime = FALSE)
  testthat::expect_no_error(
    res <- tt_to_flextable_j(tt = tbl1, tblid = "output ID")
  )
  options(docx.add_datetime = TRUE)
  snapshot_test_flextable(res)


  # example with titles and footers
  options(docx.add_datetime = FALSE)
  testthat::expect_no_error(
    res <- tt_to_flextable_j(tt = tbl1b, tblid = "output ID")
  )
  options(docx.add_datetime = TRUE)
  snapshot_test_flextable(res)


  # example with superscript and >=
  options(docx.add_datetime = FALSE)
  testthat::expect_no_error(
    res <- tt_to_flextable_j(tt = tbl1c, tblid = "output ID")
  )
  options(docx.add_datetime = TRUE)
  snapshot_test_flextable(res)


  # example with alignments
  alignments <- list(
    list(row = 2, col = 2:4, value = "right"),
    list(row = 6:7, col = 2, value = "left"),
    list(row = 8, col = 1, value = "right")
  )
  options(docx.add_datetime = FALSE)
  testthat::expect_no_error(
    res <- tt_to_flextable_j(tt = tbl1, tblid = "output ID", alignments = alignments)
  )
  options(docx.add_datetime = TRUE)
  snapshot_test_flextable(res)
})

testthat::test_that("tt_to_flextable_j() works fine with border_mat", {
  adsl2 <- adsl |> dplyr::mutate(colspan_trt = ifelse(ARM == "B: Placebo", " ", "Active Study Agent"))

  colspan_trt_map <- create_colspan_map(
    adsl2,
    non_active_grp = "B: Placebo",
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = "ARM"
  )

  lyt2 <- basic_table(
    top_level_section_div = " ",
    show_colcounts = TRUE,
    colcount_format = "N=xx"
  ) |>
    split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map)) |>
    split_cols_by("ARM") |>
    analyze(
      vars = "COUNTRY",
      afun = a_freq_j,
      extra_args = extra_args_1
    )

  tbl2 <- build_table(lyt2, adsl2)

  border_mat <- make_header_bordmat(tbl2)
  border_mat[2, 4] <- 1

  options(docx.add_datetime = FALSE)
  flx1 <- tt_to_flextable_j(tt = tbl2, tblid = "output ID")
  flx2 <- tt_to_flextable_j(tt = tbl2, tblid = "output ID", border_mat = border_mat)
  options(docx.add_datetime = TRUE)

  testthat::expect_equal(flx1$header$styles$cells$border.width.bottom$data[2, 4], c(V4 = 0))
  testthat::expect_equal(flx2$header$styles$cells$border.width.bottom$data[2, 4], c(V4 = 0.75))

  snapshot_test_flextable(flx2)
})

testthat::test_that("tt_to_flextable_j() works fine with round_type", {
  lsting <- adae |>
    dplyr::select(USUBJID, AGE, SEX, RACE, ARM, BMRKR1) |>
    dplyr::distinct() |>
    dplyr::group_by(ARM) |>
    dplyr::slice_head(n = 2) |>
    dplyr::ungroup()

  lsting[1, "BMRKR1"] <- 1.865
  lsting[2, "BMRKR1"] <- 2.985
  lsting[3, "BMRKR1"] <- -0.001

  lsting <- lsting |>
    dplyr::mutate(
      AGE = tern::explicit_na(as.character(AGE), ""),
      SEX = tern::explicit_na(SEX, ""),
      RACE = explicit_na(RACE, ""),
      COL0 = explicit_na(.data[["ARM"]], ""),
      COL1 = explicit_na(USUBJID, ""),
      COL2 = paste(AGE, SEX, RACE, sep = " / "),
      COL3 = BMRKR1
    ) |>
    arrange(COL0, COL1)

  lsting <- formatters::var_relabel(
    lsting,
    COL0 = "Treatment Group",
    COL1 = "Subject ID",
    COL2 = paste("Age (years)", "Sex", "Race", sep = " / "),
    COL3 = "Biomarker 1"
  )

  ls1 <- rlistings::as_listing(
    df = lsting,
    key_cols = c("COL0", "COL1"),
    disp_cols = c("COL0", "COL1", "COL2", "COL3"),
    col_formatting = list(COL3 = formatters::fmt_config(format = "xx.xx"))
  )

  options(docx.add_datetime = FALSE)
  testthat::expect_no_error(
    res <- tt_to_flextable_j(tt = ls1, tblid = "output ID", orientation = "landscape", round_type = "iec")
  )
  testthat::expect_equal(res$body$dataset[1, "COL3"], "1.86")
  testthat::expect_equal(res$body$dataset[2, "COL3"], "2.98")
  testthat::expect_equal(res$body$dataset[3, "COL3"], "-0.00")
  testthat::expect_no_error(
    res <- tt_to_flextable_j(tt = ls1, tblid = "output ID", orientation = "landscape", round_type = "sas")
  )
  testthat::expect_equal(res$body$dataset[1, "COL3"], "1.87")
  testthat::expect_equal(res$body$dataset[2, "COL3"], "2.99")
  testthat::expect_equal(res$body$dataset[3, "COL3"], "0.00")
  options(docx.add_datetime = TRUE)
})

testthat::test_that("tt_to_flextable_j() works fine with Listings", {
  lsting <- adae |>
    dplyr::select(USUBJID, AGE, SEX, RACE, ARM) |>
    dplyr::distinct() |>
    dplyr::group_by(ARM) |>
    dplyr::slice_head(n = 10) |>
    dplyr::ungroup()


  lsting <- lsting |>
    dplyr::mutate(
      AGE = tern::explicit_na(as.character(AGE), ""),
      SEX = tern::explicit_na(SEX, ""),
      RACE = explicit_na(RACE, ""),
      COL0 = explicit_na(.data[["ARM"]], ""),
      COL1 = explicit_na(USUBJID, ""),
      COL2 = paste(AGE, SEX, RACE, sep = " / ")
    ) |>
    arrange(COL0, COL1)

  lsting <- formatters::var_relabel(lsting,
    COL0 = "Treatment Group",
    COL1 = "Subject ID",
    COL2 = paste("Age (years)", "Sex", "Race", sep = " / ")
  )

  ls1 <- rlistings::as_listing(
    df = lsting,
    key_cols = c("COL0", "COL1"),
    disp_cols = c("COL0", "COL1", "COL2")
  )

  # basic example
  options(docx.add_datetime = FALSE)
  testthat::expect_no_error(
    res <- tt_to_flextable_j(tt = ls1, tblid = "output ID", orientation = "landscape")
  )
  options(docx.add_datetime = TRUE)
  snapshot_test_flextable(res)

  # example with titles and footers
  tab_titles <- list(
    "title" = "This is the main Title",
    "subtitles" = NULL,
    "main_footer" = c(
      "footer 1",
      "footer 2"
    ),
    "prov_footer" = NULL
  )
  ls1b <- set_titles(ls1, tab_titles)
  options(docx.add_datetime = FALSE)
  testthat::expect_no_error(
    res <- tt_to_flextable_j(tt = ls1b, tblid = "output ID", orientation = "landscape")
  )
  options(docx.add_datetime = TRUE)
  snapshot_test_flextable(res)

  # example with superscript and >=
  lsting <- adae |>
    dplyr::select(USUBJID, AGE, SEX, RACE, ARM) |>
    dplyr::distinct() |>
    dplyr::group_by(ARM) |>
    dplyr::slice_head(n = 10) |>
    dplyr::ungroup()

  lsting$ARM <- sub(pattern = "Drug X", replacement = "Drug X~[super b]", x = lsting$ARM)

  lsting <- lsting |>
    dplyr::mutate(
      AGE = tern::explicit_na(as.character(AGE), ""),
      SEX = tern::explicit_na(SEX, ""),
      RACE = explicit_na(RACE, ""),
      COL0 = explicit_na(.data[["ARM"]], ""),
      COL1 = explicit_na(USUBJID, ""),
      COL2 = paste(AGE, SEX, RACE, sep = " / ")
    ) |>
    arrange(COL0, COL1)

  lsting <- formatters::var_relabel(lsting,
    COL0 = "Treatment Group",
    COL1 = "Subject ID",
    COL2 = paste("Age~[super a]", "Sex", "Race", sep = " / ")
  )

  ls1c <- rlistings::as_listing(
    df = lsting,
    key_cols = c("COL0", "COL1"),
    disp_cols = c("COL0", "COL1", "COL2")
  )

  tab_titles <- list(
    "title" = "This is the main Title",
    "subtitles" = NULL,
    "main_footer" = c(
      "~{super a}Age in years",
      "~{super b}Xanomeline with dose >= 20mg",
      "This ~{optional text should} stay the same"
    ),
    "prov_footer" = NULL
  )
  ls1c <- set_titles(ls1c, tab_titles)

  options(docx.add_datetime = FALSE)
  testthat::expect_no_error(
    res <- tt_to_flextable_j(tt = ls1c, tblid = "output ID", orientation = "landscape")
  )
  options(docx.add_datetime = TRUE)
  snapshot_test_flextable(res)
})

# to compare 2 docx
# - open the docx in R and get the XML
# - treat the XML content as string
# - use snapshot testing against the string
testthat::test_that("remove_security_popup_page_numbers() removes dirty='true'", {
  doc <- officer::read_docx()
  section_properties <- officer::prop_section(
    page_size = officer::page_size(width = 11, height = 8.5, orient = "landscape"),
    page_margins = officer::page_mar(bottom = 1, top = 1, right = 1, left = 1, gutter = 0, footer = 1, header = 1)
  )

  # add the page numbers
  formatted_par <-
    officer::fpar("Listing Page ",
      officer::run_word_field("Page",
        prop = officer::fp_text(font.size = 8, font.family = "Times New Roman")
      ),
      " of ",
      officer::run_word_field("NumPages",
        prop = officer::fp_text(font.size = 8, font.family = "Times New Roman")
      ),
      fp_p = officer::fp_par(text.align = "right", padding.top = 12),
      fp_t = officer::fp_text(font.size = 8, font.family = "Times New Roman")
    )
  footer_default <- officer::block_list(formatted_par)
  section_properties$footer_default <- footer_default
  doc <- officer::body_set_default_section(doc, section_properties)

  l_1 <- xml2::xml_find_all(doc$doc_obj$get(), ".//w:instrText[@w:dirty='true'] | .//w:fldChar[@w:dirty='true']")

  junco:::remove_security_popup_page_numbers(doc = doc, tlgtype = "something different than Listing")
  l_2 <- xml2::xml_find_all(doc$doc_obj$get(), ".//w:instrText[@w:dirty='true'] | .//w:fldChar[@w:dirty='true']")

  junco:::remove_security_popup_page_numbers(doc = doc, tlgtype = "Listing")
  l_3 <- xml2::xml_find_all(doc$doc_obj$get(), ".//w:instrText[@w:dirty='true'] | .//w:fldChar[@w:dirty='true']")

  testthat::expect_equal(length(l_1), 6)
  testthat::expect_equal(length(l_2), 6)
  testthat::expect_equal(length(l_3), 0)
})

testthat::test_that("add_title_style_caption() adds a new XML node w:pStyle w:val='Caption'", {
  options(docx.add_datetime = FALSE)
  flx <- tt_to_flextable_j(tt = tbl1, tblid = "output ID")
  options(docx.add_datetime = TRUE)

  # nolint start
  flx <- junco:::insert_title_hanging_indent_v3(flx, "output id:this is a veeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeery long title")
  # nolint end
  flx <- flx |> flextable::set_table_properties(layout = "autofit")
  doc <- officer::read_docx(system.file("template_file.docx", package = "junco"))
  doc <- flextable::body_add_flextable(doc, flx, align = "center")

  l_x_before <- xml2::xml_find_all(doc$doc_obj$get(), ".//w:pStyle[@w:val='Caption']")

  string_to_look_for <- sub(pattern = ":\t.*", replacement = ":", flx$header$dataset[1, 1])
  add_title_style_caption(doc, string_to_look_for)

  # this print() is needed to update the XML and be able to retrieve the newly inserted node
  # with style Caption
  temp_file <- tempfile(fileext = ".docx")
  print(doc, target = temp_file)


  # w:pStyle w:val="Caption"
  l_x_after <- xml2::xml_find_all(doc$doc_obj$get(), ".//w:pStyle[@w:val='Caption']")

  testthat::expect_equal(length(l_x_before), 0)
  testthat::expect_equal(length(l_x_after), 1)
})

testthat::test_that("my_pg_width_by_orient() returns what it should", {
  res <- junco:::my_pg_width_by_orient(orientation = "portrait")
  testthat::expect_equal(res, 6.38)
  res <- junco:::my_pg_width_by_orient(orientation = "landscape")
  testthat::expect_equal(res, 8.88)
})

testthat::test_that("insert_footer_text() adds a footer line to a flextable", {
  options(docx.add_datetime = FALSE)
  flx <- tt_to_flextable_j(tt = tbl1c, tblid = "output ID")
  options(docx.add_datetime = TRUE)
  n_footer_lines_1 <- flx |> flextable::nrow_part(part = "footer")
  footer_lines_1 <- flx$footer$dataset

  options(docx.add_datetime = FALSE)
  flx <- insert_footer_text(flx, "output ID")
  options(docx.add_datetime = TRUE)
  n_footer_lines_2 <- flx |> flextable::nrow_part(part = "footer")
  footer_lines_2 <- flx$footer$dataset

  flx <- insert_footer_text(flx, "output ID")
  n_footer_lines_3 <- flx |> flextable::nrow_part(part = "footer")
  footer_lines_3 <- flx$footer$dataset

  testthat::expect_equal(n_footer_lines_1, n_footer_lines_2)
  testthat::expect_true(all.equal(footer_lines_1, footer_lines_2))

  testthat::expect_equal(n_footer_lines_1 + 1, n_footer_lines_3)
  testthat::expect_true(all.equal(footer_lines_1, footer_lines_3 |> head(n_footer_lines_1)))
})

testthat::test_that("interpret_cell_content() returns what it should", {
  res <- junco:::interpret_cell_content("Any AE~[super a]~[sub bds]")
  expected_res <- "flextable::as_paragraph('Any AE', flextable::as_sup('a'), '', flextable::as_sub('bds'))"
  testthat::expect_equal(res, expected_res)

  res <- junco:::interpret_cell_content("Any AE~{super a}~[sub bds]other ~{super b}b")
  # nolint start
  expected_res <- "flextable::as_paragraph('Any AE', flextable::as_sup('a'), '', flextable::as_sub('bds'), 'other ', flextable::as_sup('b'), 'b')"
  # nolint end
  testthat::expect_equal(res, expected_res)

  res <-
    junco:::interpret_cell_content("~{super a} The event experienced by the subject with the worst severity is used.")
  # nolint start
  expected_res <- "flextable::as_paragraph('', flextable::as_sup('a'), ' The event experienced by the subject with the worst severity is used.')"
  # nolint end
  testthat::expect_equal(res, expected_res)

  # nolint start
  res <- junco:::interpret_cell_content("Note: Adverse events are coded using MedDRA version 26.0.~{optional ; toxicity grade is evaluated according to NCI-CTCAE version &ctcae.}.")
  expected_res <- "flextable::as_paragraph('Note: Adverse events are coded using MedDRA version 26.0.', '; toxicity grade is evaluated according to NCI-CTCAE version &ctcae.', '.')"
  # nolint end
  testthat::expect_equal(res, expected_res)
})

testthat::test_that("interpret_all_cell_content() is interpreting markups correctly", {
  options(docx.add_datetime = FALSE)
  flx <- tt_to_flextable_j(tt = tbl1, tblid = "output ID")
  options(docx.add_datetime = TRUE)

  flx <- insert_title_hanging_indent_v3(flx,
    title = "This is the main Ttl~[super a]"
  )

  flx <- flx |>
    flextable::append_chunks(part = "header", i = 2, j = 2, flextable::as_chunk("~[super b]")) |>
    flextable::append_chunks(part = "body", i = 2, j = 1, flextable::as_chunk("~[sub c]"))

  flx <- flx |>
    flextable::add_footer_lines("~[super a]Title") |>
    flextable::add_footer_lines("~{super b}Drug = Xanomeline") |>
    flextable::add_footer_lines("~[super c]United States of America")

  flx <- flx |>
    flextable::align(part = "footer", align = "left") |>
    flextable::fontsize(part = "footer", size = 8) |>
    flextable::padding(part = "footer", padding = 0)

  res <- interpret_all_cell_content(flx)

  snapshot_test_flextable(res)
})

testthat::test_that("insert_title_hanging_indent_v3() adds the title correctly", {
  options(docx.add_datetime = FALSE)
  flx <- tt_to_flextable_j(tt = tbl1, tblid = "output ID")
  options(docx.add_datetime = TRUE)

  res <- insert_title_hanging_indent_v3(flx, "output id:this is a test title")
  snapshot_test_flextable(res)
})


testthat::test_that("add_hanging_indent_first_column() works correctly", {
  options(docx.add_datetime = FALSE)
  flx <- tt_to_flextable_j(tt = tbl1, tblid = "output ID")
  options(docx.add_datetime = TRUE)

  flx$body$dataset[1, 1] <- "Republic of China"
  flx$body$dataset[2, 1] <- "United States of America"

  res <- add_hanging_indent_first_column(flx, 0.7)
  snapshot_test_flextable(res)
})

testthat::test_that("wrap_string_with_indent() works correctly", {
  res <- junco:::wrap_string_with_indent("this is a veeeeeeeeeeeeeeery long string", max_width_inch = 1)
  expected_res <- "this is a\n\tveeeeeeeeeeeeeeery\n\tlong string"
  testthat::expect_equal(res, expected_res)

  res <- junco:::wrap_string_with_indent("Study agent permanently discontinued",
    max_width_inch = 1.99 - 0.375, dpi = 78
  )
  expected_res <- "Study agent permanently\n\tdiscontinued"
  testthat::expect_equal(res, expected_res)

  res <- junco:::wrap_string_with_indent("Resulting in persistent or significant disability/incapacity",
    max_width_inch = 1.98 - 0.125, dpi = 78
  )
  expected_res <- "Resulting in persistent or significant\n\tdisability/incapacity"
  testthat::expect_equal(res, expected_res)
})


testthat::test_that("add_little_gap_bottom_borders_spanning_headers() works correctly", {
  options(docx.add_datetime = FALSE)
  flx <- tt_to_flextable_j(tt = tbl1, tblid = "output ID")
  options(docx.add_datetime = TRUE)


  flx <- flx |>
    flextable::add_header_row(
      values = c("spanning header 1", "spanning header 2"),
      colwidths = c(2, 2)
    )
  flx <- flx |> flextable::align(part = "header", i = 1, align = "center")

  testthat::expect_no_error(
    res <- add_little_gap_bottom_borders_spanning_headers(flx)
  )
  snapshot_test_flextable(res)
})


testthat::test_that("export_as_docx_j() works with pagination", {
  # create a TableTree with a few pages
  colspan_trt_map <- data.frame(
    colspan_trt = c("Active Study Agent", "Active Study Agent", " "),
    ARM = c("A: Drug X", "C: Combination", "B: Placebo")
  )
  df <- ex_adlb |> dplyr::mutate(colspan_trt = ifelse(ARM == "B: Placebo", " ", "Active Study Agent"))
  df$colspan_trt <- factor(df$colspan_trt, levels = c("Active Study Agent", " "))
  .trtvar <- "ARM"
  df <- df |> dplyr::mutate(AGEGRP = ifelse(AGE >= 35, ">= 35", "< 35"))
  df$AGEGRP <- factor(df$AGEGRP, levels = c("< 35", ">= 35"))
  .subgrpvar <- "AGEGRP"
  .subgrplbl <- "Age: %s years"
  multivars <- c("AVAL", "AVAL", "CHG")
  .ctrl_grp <- "B: Placebo"
  .ref_path <- c("colspan_trt", " ", .trtvar, .ctrl_grp)
  extra_args_3col <- list(
    format_na_str = rep("NA", 3),
    d = "decimal",
    ref_path = .ref_path,
    ancova = FALSE,
    comp_btw_group = TRUE,
    indatavar = "inlbdata",
    multivars = multivars
  )
  df$rrisk_header <- "Difference in Mean Change (95% CI)"
  df$rrisk_label <- paste(df[[.trtvar]], paste("vs", .ctrl_grp))
  df$STUDYID <- df$STUDYID |> as.factor()
  df <- df |> filter(!is.na(CHG), LBCAT == "CHEMISTRY")
  df$inlbdata <- "Y"

  df_alt <- ex_adsl |> dplyr::mutate(colspan_trt = ifelse(ARM == "B: Placebo", " ", "Active Study Agent"))
  df_alt$colspan_trt <- factor(df_alt$colspan_trt, levels = c("Active Study Agent", " "))
  df_alt$rrisk_header <- "Difference in Mean Change (95% CI)"
  df_alt$STUDYID <- df_alt$STUDYID |> as.factor()

  lyt <- basic_table(show_colcounts = FALSE, colcount_format = "N=xx") |>
    ### first columns
    split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map)) |>
    split_cols_by(.trtvar, show_colcounts = TRUE, colcount_format = "N=xx") |>
    split_rows_by(.subgrpvar,
      label_pos = "hidden", section_div = " ",
      split_fun = drop_split_levels, page_by = TRUE
    ) |>
    ### just show number of subjects in current level of subgrpvar
    ### only show this number in the first AVAL column
    summarize_row_groups(
      var = .subgrpvar, cfun = a_freq_j,
      extra_args = list(
        label_fstr = .subgrplbl,
        extrablankline = TRUE,
        restr_columns = "AVAL",
        .stats = c("n_altdf"),
        riskdiff = FALSE, denom_by = .subgrpvar
      )
    ) |>
    split_rows_by("PARAM",
      label_pos = "topleft", split_label = "Laboratory Test",
      section_div = " ", split_fun = drop_split_levels
    ) |>
    ## note the child_labels = hidden for AVISIT, these labels will be taken care off by
    ## applying function summarize_aval_chg_diff further in the layout
    split_rows_by("AVISIT",
      label_pos = "topleft", split_label = "Study Visit",
      split_fun = drop_split_levels, child_labels = "hidden"
    ) |>
    ## set up a 3 column split
    split_cols_by_multivar(multivars,
      varlabels = c("n/N (%)", "Mean (95% CI)", "Mean Change From Baseline (95% CI)")
    ) |>
    ### restart for the rrisk_header columns - note the nested = FALSE option
    ### also note the child_labels = "hidden" in both PARAM and AVISIT
    split_cols_by("rrisk_header", nested = FALSE) |>
    split_cols_by(
      .trtvar,
      split_fun = remove_split_levels(.ctrl_grp),
      labels_var = "rrisk_label",
      show_colcounts = TRUE,
      colcount_format = "N=xx"
    ) |>
    ### difference columns : just 1 column & analysis needs to be done on change
    split_cols_by_multivar(multivars[3], varlabels = c(" ")) |>
    ### the variable passed here in analyze is not used (STUDYID), it is a dummy var passing,
    ### the function summarize_aval_chg_diff grabs the required vars from cols_by_multivar calls
    analyze("STUDYID", afun = a_summarize_aval_chg_diff_j, extra_args = extra_args_3col)
  suppressMessages(
    result <- build_table(lyt, df, alt_counts_df = df_alt)
  )
  tab_titles <- list(
    "title" = "This is the main Title",
    "subtitles" = NULL,
    "main_footer" = c(
      "footer 1",
      "footer 2"
    ),
    "prov_footer" = NULL
  )
  result <- set_titles(result, tab_titles)


  # export it as docx
  output_dir <- tempdir()
  options(docx.add_datetime = FALSE)
  export_as_docx_j(
    result,
    output_dir = output_dir,
    orientation = "landscape",
    tblid = "test1234",
    nosplitin = list(cols = c(.trtvar, "rrisk_header")),
    paginate = TRUE,
    add_page_break = TRUE,
    combined_docx = TRUE
  )
  options(docx.add_datetime = TRUE)

  # check that the files exist, including the allparts
  testthat::expect_true(file.exists(paste0(output_dir, "/test1234part1of2.docx")))
  testthat::expect_true(file.exists(paste0(output_dir, "/test1234part2of2.docx")))
  testthat::expect_true(file.exists(paste0(output_dir, "/test1234allparts.docx")))
  testthat::expect_true(file.exists(paste0(output_dir, "/test1234part1of2.csv")))
  testthat::expect_true(file.exists(paste0(output_dir, "/test1234part2of2.csv")))

  # open the files and check the XML
  doc <- officer::read_docx(paste0(output_dir, "/test1234part1of2.docx"))
  snapshot_test_docx(doc)

  doc <- officer::read_docx(paste0(output_dir, "/test1234part2of2.docx"))
  snapshot_test_docx(doc)

  doc <- officer::read_docx(paste0(output_dir, "/test1234allparts.docx"))
  snapshot_test_docx(doc)

  file.remove(c(
    paste0(output_dir, "/test1234part1of2.docx"),
    paste0(output_dir, "/test1234part2of2.docx"),
    paste0(output_dir, "/test1234allparts.docx"),
    paste0(output_dir, "/test1234part1of2.csv"),
    paste0(output_dir, "/test1234part2of2.csv")
  ))

  # do not save csv
  options(docx.add_datetime = FALSE)
  export_as_docx_j(
    result,
    output_dir = output_dir,
    orientation = "landscape",
    tblid = "test1234",
    nosplitin = list(cols = c(.trtvar, "rrisk_header")),
    paginate = TRUE,
    add_page_break = TRUE,
    combined_docx = TRUE,
    export_csv = FALSE
  )
  options(docx.add_datetime = TRUE)

  # check that the files exist, including the allparts
  testthat::expect_true(file.exists(paste0(output_dir, "/test1234part1of2.docx")))
  testthat::expect_true(file.exists(paste0(output_dir, "/test1234part2of2.docx")))
  testthat::expect_true(file.exists(paste0(output_dir, "/test1234allparts.docx")))
  testthat::expect_false(file.exists(paste0(output_dir, "/test1234part1of2.csv")))
  testthat::expect_false(file.exists(paste0(output_dir, "/test1234part2of2.csv")))
  file.remove(c(
    paste0(output_dir, "/test1234part1of2.docx"),
    paste0(output_dir, "/test1234part2of2.docx"),
    paste0(output_dir, "/test1234allparts.docx")
  ))

  # save csv in a different location
  output_csv_directory <- tempfile()
  dir.create(output_csv_directory, showWarnings = FALSE, recursive = TRUE)
  options(docx.add_datetime = FALSE)
  printed_messages <- testthat::capture_messages(
    export_as_docx_j(result,
                     output_dir = output_dir,
                     orientation = "landscape",
                     tblid = "test1234",
                     nosplitin = list(cols = c(.trtvar, "rrisk_header")),
                     paginate = TRUE,
                     add_page_break = TRUE,
                     combined_docx = TRUE,
                     output_csv_directory = output_csv_directory)
  )
  options(docx.add_datetime = TRUE)

  # check that the files exist, including the allparts
  testthat::expect_true(file.exists(paste0(output_dir, "/test1234part1of2.docx")))
  testthat::expect_true(file.exists(paste0(output_dir, "/test1234part2of2.docx")))
  testthat::expect_true(file.exists(paste0(output_dir, "/test1234allparts.docx")))
  testthat::expect_false(file.exists(paste0(output_dir, "/test1234part1of2.csv")))
  testthat::expect_false(file.exists(paste0(output_dir, "/test1234part2of2.csv")))
  testthat::expect_true(file.exists(paste0(output_csv_directory, "/test1234part1of2.csv")))
  testthat::expect_true(file.exists(paste0(output_csv_directory, "/test1234part2of2.csv")))
  testthat::expect_true(all.equal(printed_messages,
    c(paste0("Saving csv as ", output_csv_directory, "/test1234part1of2.csv\n"),
      paste0("Saving csv as ", output_csv_directory, "/test1234part2of2.csv\n"))
  ))

  file.remove(c(
    paste0(output_dir, "/test1234part1of2.docx"),
    paste0(output_dir, "/test1234part2of2.docx"),
    paste0(output_dir, "/test1234allparts.docx")
  ))
  unlink(output_csv_directory, recursive = TRUE)
})


testthat::test_that("export_graph_as_docx() works with basic example", {
  # create a few ggplots
  cbbPalette <- c("#000000", "#E69F00", "#0072B2")

  g_facet <- function(df) {
    # bar plot in facet -------------------------------------
    plot1 <- df |>
      ggplot(aes(x = .data$trt_abb, y = .data$pern, fill = .data$TRT01A)) +
      geom_col(position = position_dodge(0.5)) +
      geom_text(aes(label = perc), position = position_dodge(0.5), vjust = -0.5) +
      facet_wrap(~ .data$cat, nrow = 1, strip.position = "bottom") +

      # assign colorblind friendly palette
      scale_fill_manual(values = cbbPalette) +
      labs(x = " ", y = "Percentage of Subjects") +
      theme_bw() +
      theme(
        text = element_text(size = 9, color = "black"),
        strip.background = element_rect(fill = NA, color = "black"),
        strip.placement = "outside",
        strip.text = element_text(size = 9),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.size = unit(5, "mm"),
        legend.box.background = element_rect(colour = "black", linewidth = 0.5),
        legend.text = element_text(size = 9),
        panel.spacing.x = unit(0, "line"),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.text.x = element_text(angle = 320, vjust = -1),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 9)
      )
  }

  df1 <- data.frame(
    cat = factor(c(
      rep("0 to <3 months", 3),
      rep("3 to <6 months", 3),
      rep("6 to <9 months", 3),
      rep("9 to <12 months", 3),
      rep("12 to <15 months", 3),
      rep("15 to <18 months", 3),
      rep("18 to <21 months", 3)
    ), levels = c(
      "0 to <3 months", "3 to <6 months", "6 to <9 months",
      "9 to <12 months", "12 to <15 months", "15 to <18 months", "18 to <21 months"
    )),
    TRT01A = factor(c("Xanomeline (High)", "Xanomeline (Low)", "Placebo (PBO)"),
      levels = c("Xanomeline (High)", "Xanomeline (Low)", "Placebo (PBO)")
    ),
    trt_abb = c("Xan Low", "Xan High", "PBO"),
    n = c(7, 4, 2, 5, 5, 5, 3, 3, 7, 4, 2, 7, 6, 8, 5, 3, 10, 3, 5, 4, 5),
    total = c(
      53, 73, 59, 53, 73, 59, 53, 73, 59, 53, 73, 59, 53, 73, 59, 53,
      73, 59, 53, 73, 59
    ),
    pern = c(
      13.2, 5.5, 3.4, 9.4, 6.8, 8.5, 5.7, 4.1, 11.9, 7.5, 2.7,
      11.9, 11.3, 11.0, 8.5, 5.7, 13.7, 5.1, 9.4, 5.5, 8.5
    )
  )
  df1$perc <- sprintf("%.1f", df1$pern)

  df2 <- data.frame(
    cat = factor(c(
      rep("21 to <24 months", 3),
      rep("24 to <27 months", 3),
      rep("27 to <30 months", 3),
      rep("30 to <33 months", 3),
      rep("33 to <36 months", 3),
      rep("36 to <39 months", 3)
    ), levels = c(
      "21 to <24 months", "24 to <27 months", "27 to <30 months",
      "30 to <33 months", "33 to <36 months", "36 to <39 months"
    )),
    TRT01A = factor(c("Xanomeline (High)", "Xanomeline (Low)", "Placebo (PBO)"),
      levels = c("Xanomeline (High)", "Xanomeline (Low)", "Placebo (PBO)")
    ),
    trt_abb = c("Xan Low", "Xan High", "PBO"),
    n = c(1, 7, 5, 3, 6, 6, 6, 9, 2, 5, 4, 3, 5, 9, 5, 0, 2, 4),
    total = c(53, 73, 59, 53, 73, 59, 53, 73, 59, 53, 73, 59, 53, 73, 59, 53, 73, 59),
    pern = c(1.9, 9.6, 8.5, 5.7, 8.2, 10.2, 11.3, 12.3, 3.4, 9.4, 5.5, 5.1, 9.4, 12.3, 8.5, 0.0, 2.7, 6.8)
  )
  df2$perc <- sprintf("%.1f", df2$pern)

  p1 <- g_facet(df1)
  p2 <- g_facet(df2)

  # save the ggplots as png
  output_dir <- tempdir()
  pn1 <- paste0(output_dir, "/", "temp_1.png")
  png(pn1,
    width  = 22,
    height = 14,
    units  = "cm",
    res    = 300,
    type   = "cairo"
  )
  print(p1)
  dev.off()

  pn2 <- paste0(output_dir, "/", "temp_2.png")
  png(pn2,
    width  = 22,
    height = 14,
    units  = "cm",
    res    = 300,
    type   = "cairo"
  )
  print(p2)
  dev.off()


  # export them as docx
  options(docx.add_datetime = FALSE)
  testthat::expect_no_error(
    export_graph_as_docx(
      plotnames = list(pn1, pn2),
      tblid = "testgraph1234",
      output_dir = output_dir,
      orientation = "landscape",
      title = "Duration of Treatment; Safety Analysis Set (Study jjcs - core)",
      footers = NULL
    )
  )
  options(docx.add_datetime = TRUE)
  output_docx <- paste0(output_dir, "/testgraph1234.docx")

  # check that the file exist
  testthat::expect_true(file.exists(output_docx))

  # open the file and check the XML
  doc <- officer::read_docx(output_docx)
  snapshot_test_docx(doc)

  file.remove(c(pn1, pn2, output_docx))
})
