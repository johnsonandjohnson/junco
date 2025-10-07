library(testthat)
library(rtables)
library(dplyr)

# Create test data
adsl <- ex_adsl
adae <- ex_adae
adae$TRTEMFL <- "Y"

test_that("h_update_factor works correctly", {
  # Create test data
  df <- data.frame(
    ID = 1:5,
    Category = factor(
      c("A", "B", "C", "A", "B"),
      levels = c("A", "B", "C", "D")
    )
  )

  # Test keeping only specific values
  result1 <- h_update_factor(df, .var = "Category", val = c("A", "B"))
  expect_equal(levels(result1$Category), c("A", "B"))

  # Test excluding specific levels
  result2 <- h_update_factor(df, .var = "Category", excl_levels = "C")
  expect_equal(levels(result2$Category), c("A", "B", "D"))

  # Test with no data to report
  df_empty <- data.frame(
    ID = 1:3,
    Category = factor(c("D", "D", "D"), levels = c("A", "B", "C", "D"))
  )
  result3 <- h_update_factor(
    df_empty,
    .var = "Category",
    val = c("A", "B", "C")
  )
  # TODO: fix this equal(levels(result3$Category), "No data to report")

  # Test with non-factor
  df_non_factor <- data.frame(
    ID = 1:3,
    Category = c("A", "B", "C")
  )
  result4 <- h_update_factor(
    df_non_factor,
    .var = "Category",
    val = c("A", "B")
  )
  expect_equal(df_non_factor, result4)
})

test_that("h_df_add_newlevels works correctly", {
  # Create test data
  df <- data.frame(
    ID = 1:5,
    Category = factor(c("A", "B", "C", "A", "B"), levels = c("A", "B", "C"))
  )

  # Test adding new levels after existing levels
  new_levels <- list(c("All A and B"), list(c("A", "B")))
  result1 <- h_df_add_newlevels(
    df,
    .var = "Category",
    new_levels = new_levels,
    new_levels_after = TRUE
  )
  expect_equal(levels(result1$Category), c("A", "B", "All A and B", "C"))
  expect_equal(nrow(result1), 9)

  # Test adding new levels before existing levels
  result2 <- h_df_add_newlevels(
    df,
    .var = "Category",
    new_levels = new_levels,
    new_levels_after = FALSE
  )
  expect_equal(levels(result2$Category), c("All A and B", "A", "B", "C"))
  expect_equal(nrow(result2), 9)

  # Test with string added to levels
  result3 <- h_df_add_newlevels(
    df,
    .var = "Category",
    new_levels = new_levels,
    addstr2levs = " (n)",
    new_levels_after = TRUE
  )
  expect_equal(
    levels(result3$Category),
    c("A (n)", "B (n)", "All A and B (n)", "C (n)")
  )
})

test_that("h_colexpr_substr works correctly", {
  # Test with a valid column expression
  col_expr <- "(!is.na(ARM) & ARM %in% c('A: Drug X', 'B: Placebo')) & (!is.na(SEX) & SEX %in% c('F', 'M'))"
  result1 <- h_colexpr_substr("ARM", col_expr)
  expect_equal(result1, "(!is.na(ARM) & ARM %in% c('A: Drug X', 'B: Placebo')")

  # Test with a variable not in the expression
  result2 <- h_colexpr_substr("AGE", col_expr)
  expect_null(result2)

  # Test with a simple expression
  simple_expr <- "(!is.na(COUNTRY) & COUNTRY %in% c('CHN'))"
  result3 <- h_colexpr_substr("COUNTRY", simple_expr)
  expect_equal(result3, "(!is.na(COUNTRY) & COUNTRY %in% c('CHN'))")
})

test_that("h_get_label_map works correctly", {
  # Create test data
  labels <- c("A", "B", "C")
  label_map <- data.frame(
    value = c("A", "B", "C"),
    label = c("Label A", "Label B", "Label C")
  )

  # Test basic mapping
  result1 <- h_get_label_map(
    labels,
    label_map,
    .var = "Category",
    split_info = list(split = "root", value = "root")
  )
  expect_equal(result1, c("Label A", "Label B", "Label C"))

  # Test with var column in label_map
  label_map_with_var <- data.frame(
    var = c("Category", "Category", "OtherVar"),
    value = c("A", "B", "C"),
    label = c("Cat A", "Cat B", "Other C")
  )

  # Test with split variables
  label_map_with_split <- data.frame(
    SEX = c("F", "F", "M"),
    value = c("A", "B", "A"),
    label = c("Female A", "Female B", "Male A")
  )
  split_info <- list(split = c("SEX"), value = c("F"))
  result3 <- h_get_label_map(
    c("A", "B"),
    label_map_with_split,
    .var = "Category",
    split_info = split_info
  )
  expect_equal(result3, c("Female A", "Female B"))
})

test_that("h_subset_combo works correctly", {
  # Create test data
  df <- data.frame(
    ID = 1:4,
    Treatment = c("Drug A", "Drug B", "Drug A", "Drug B"),
    Period = c("Week 1", "Week 2", "Week 3", "Week 1"),
    Flag = c("Y", "N", "Y", "N")
  )

  combosdf <- data.frame(
    valname = c("Week 1", "Week 2", "Week 3"),
    label = c("Week 1", "Week 2", "Week 3")
  )

  # Test with flag filtering
  result1 <- h_subset_combo(
    df,
    combosdf,
    do_not_filter = NULL,
    filter_var = "Period",
    flag_var = "Flag",
    colid = "Treatment.Drug A.Week 1"
  )
  expect_equal(nrow(result1), 1)
  expect_equal(result1$ID, 1)

  # Test without flag filtering
  result2 <- h_subset_combo(
    df,
    combosdf,
    do_not_filter = NULL,
    filter_var = "Period",
    flag_var = NULL,
    colid = "Treatment.Drug A.Week 2"
  )
  expect_equal(nrow(result2), 1)
  expect_equal(result2$ID, 2)

  # Test with do_not_filter
  result3 <- h_subset_combo(
    df,
    combosdf,
    do_not_filter = "Week 1",
    filter_var = "Period",
    flag_var = "Flag",
    colid = "Treatment.Drug A.Week 1"
  )
  expect_equal(nrow(result3), 2) # All rows with Flag="Y" and Treatment="Drug A"
})

# TODO: fix this test_that("h_create_altdf works correctly")
