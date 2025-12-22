# Table & Listing Customizations

## Introduction

Within clinical trial reporting, there is often the need to incorporate
custom components to the tables and listings being created. Common
custom components may include the following:

- Page Orientation (Portrait vs. Landscape)
- Adjusting the Font Size of a Table
- Adding a Spanning Column Header to a Table
- Adding a Combined Treatment Column to a Table
- Inserting a New Line within Table & Listing Text
- Custom Table Column Header Border Matrix
- Addition of Superscript or Other Symbol
- Grouping of Columns for Tables Containing Many Columns
- Inserting Page Breaks in Tables
- Manually Splitting Large Table & Listing Files into Multiple Smaller
  Files

The purpose of this vignette is to provide clear, concise examples of
how to achieve each custom component.

> **Note:** The visual output shown in this HTML vignette may not
> exactly match what the .docx or .rtf exporters will actually produce
> due to differences between HTML and those formats. The HTML outputs
> displayed here are for demonstration purposes only. Users should run
> the code examples themselves to see the exact output in their
> preferred format (.docx or .rtf).

## Page Orientation (Portrait vs. Landscape)

Sometimes when creating a table, there are many columns that need to be
presented. In this scenario, it might be beneficial for the resulting
output file to have an orientation of landscape. As a result, the page
is wider, allowing the information to be more readable.

### Table .rtf File

In the .rtf exporter function,
[`junco::tt_to_tlgrtf`](https://johnsonandjohnson.github.io/junco/reference/tt_to_tlgrtf.md),
by default, the page orientation is set to portrait. If landscape
orientation is required, then the orientation can be adjusted by
including the `orientation` argument and specifying a value of
“landscape”.

``` r
library(junco)

adsl <- data.frame(TRT01A = factor(c("Example Drug 5 mg",
                                     "Example Drug 10 mg",
                                     "Example Drug 20 mg",
                                     "Placebo",
                                     "Example Drug 5 mg",
                                     "Example Drug 10 mg",
                                     "Example Drug 20 mg",
                                     "Placebo"),
                                   levels = c("Example Drug 5 mg",
                                              "Example Drug 10 mg",
                                              "Example Drug 20 mg",
                                              "Placebo"
                                              )
                              ),
                   USUBJID = c("1",
                               "2",
                               "3",
                               "4",
                               "5",
                               "6",
                               "7",
                               "8"),
                   RESPONSE = factor(c("Yes",
                                       "No",
                                       "Yes",
                                       "Yes",
                                       "Yes",
                                       "No",
                                       "Yes",
                                       "No"),
                                     levels = c("Yes",
                                                "No")
                              ),
                   SEX = factor(c("Male",
                                  "Female",
                                  "Male",
                                  "Female",
                                  "Female",
                                  "Male",
                                  "Female",
                                  "Male"),
                                levels = c("Male",
                                           "Female")
                           )
) |>
  var_relabel(RESPONSE = "Response")

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) |>
  ### first columns
  split_cols_by("TRT01A",
                show_colcounts = FALSE
  ) |>
  ### create table body row sections based on SEX
  split_rows_by("SEX",
                section_div = c(" "),
                page_by = TRUE,
                split_fun = drop_split_levels
                ) |>
  summarize_row_groups("SEX",
                       cfun =a_freq_j,
                       extra_args = list(denom = "n_df",
                                         denom_by = "SEX",
                                         riskdiff = FALSE,
                                         extrablankline = TRUE,
                                         .stats = c("count_unique")
                                         )
                       ) |>
  ### analyze height
  analyze("RESPONSE",
          var_labels = c("Response"),
          show_labels = "visible",
          afun = a_freq_j,
          extra_args = list(denom = "n_df",
                            denom_by = "SEX",
                            riskdiff = FALSE,
                            .stats = c("count_unique_fraction")
                            )
          )

result <- build_table(lyt, adsl)
result <- set_titles(result, titles)
tt_to_tlgrtf(result,
             file = paste0(out_dir, "/exampleorientation"),
             orientation = "landscape")
#> [[1]]
#> NULL
```

### Table .docx File

To create a .docx file with landscape orientation, the
[`junco::export_as_docx_j`](https://johnsonandjohnson.github.io/junco/reference/export_as_docx_j.md)
function can be called with the `orientation` argument set to
“landscape”.

``` r
export_as_docx_j(result,
                tblid = "exampleorientation",
                output_dir = out_dir,
                orientation = "landscape")
```

### Visual Output (HTML Representation)

| exampleorientation:                                       |                   |                    |                    |            |
|-----------------------------------------------------------|-------------------|--------------------|--------------------|------------|
|                                                           | Example Drug 5 mg | Example Drug 10 mg | Example Drug 20 mg | Placebo    |
|                                                           | N=2               | N=2                | N=2                | N=2        |
| Male                                                      | 1                 | 1                  | 1                  | 1          |
|                                                           |                   |                    |                    |            |
| Response                                                  |                   |                    |                    |            |
| Yes                                                       | 1 (100.0%)        | 0                  | 1 (100.0%)         | 0          |
| No                                                        | 0                 | 1 (100.0%)         | 0                  | 1 (100.0%) |
| Female                                                    | 1                 | 1                  | 1                  | 1          |
|                                                           |                   |                    |                    |            |
| Response                                                  |                   |                    |                    |            |
| Yes                                                       | 1 (100.0%)        | 0                  | 1 (100.0%)         | 1 (100.0%) |
| No                                                        | 0                 | 1 (100.0%)         | 0                  | 0          |
|                                                           |                   |                    |                    |            |
| Dummy Note: On-treatment is defined as treatment-emergent |                   |                    |                    |            |

## Adjusting the Font Size of a Table or Listing

By default, the font type and size for the body of tables is Times New
Roman 9-pt. If a font size of 8-pt is required for a table, then the
font size can be adjusted as necessary. Other font sizes are not
currently supported for tables. For listings, the default font size is
8-pt. Other font sizes are not currently supported for listings.

### Table .rtf File

To display the body of tables in Times New Roman 8-pt for .rtf files,
the
[`junco::tt_to_tlgrtf`](https://johnsonandjohnson.github.io/junco/reference/tt_to_tlgrtf.md)
function can be called with the `fontspec` argument included. The
`fontspec` argument will specify a
[`formatters::font_spec`](https://insightsengineering.github.io/formatters/latest-tag/reference/font_spec.html)
function call.

Example of Table with Font Size of 8-pt

``` r
library(junco)

adsl <- data.frame(TRT01A = factor(c("Example Drug 5 mg",
                                     "Example Drug 10 mg",
                                     "Example Drug 20 mg",
                                     "Placebo",
                                     "Example Drug 5 mg",
                                     "Example Drug 10 mg",
                                     "Example Drug 20 mg",
                                     "Placebo"),
                                   levels = c("Example Drug 5 mg",
                                              "Example Drug 10 mg",
                                              "Example Drug 20 mg",
                                              "Placebo"
                                              )
                              ),
                   USUBJID = c("1",
                               "2",
                               "3",
                               "4",
                               "5",
                               "6",
                               "7",
                               "8"),
                   RESPONSE = factor(c("Yes",
                                       "No",
                                       "Yes",
                                       "Yes",
                                       "Yes",
                                       "No",
                                       "Yes",
                                       "No"),
                                     levels = c("Yes",
                                                "No")
                              ),
                   SEX = factor(c("Male",
                                  "Female",
                                  "Male",
                                  "Female",
                                  "Female",
                                  "Male",
                                  "Female",
                                  "Male"),
                                levels = c("Male",
                                           "Female")
                           )
) |>
  var_relabel(RESPONSE = "Response")

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) |>
  ### first columns
  split_cols_by("TRT01A",
                show_colcounts = FALSE
  ) |>
  ### create table body row sections based on SEX
  split_rows_by("SEX",
                section_div = c(" "),
                page_by = TRUE,
                split_fun = drop_split_levels
                ) |>
  summarize_row_groups("SEX",
                       cfun =a_freq_j,
                       extra_args = list(denom = "n_df",
                                         denom_by = "SEX",
                                         riskdiff = FALSE,
                                         extrablankline = TRUE,
                                         .stats = c("count_unique")
                                         )
                       ) |>
  ### analyze height
  analyze("RESPONSE",
          var_labels = c("Response"),
          show_labels = "visible",
          afun = a_freq_j,
          extra_args = list(denom = "n_df",
                            denom_by = "SEX",
                            riskdiff = FALSE,
                            .stats = c("count_unique_fraction")
                            )
          )

result <- build_table(lyt, adsl)
result <- set_titles(result, titles)
tt_to_tlgrtf(result,
             file = paste0(out_dir, "/examplefontsize"),
             fontspec = formatters::font_spec("Times", 8, 1.2))
#> [[1]]
#> NULL
```

### Table .docx File

To display the body of tables in Times New Roman 8-pt for .docx files,
the junco::export_as_docx_j function can be called with the theme
argument included. The theme argument will specify a
junco::theme_docx_default_j function call.

``` r
export_as_docx_j(result,
                tblid = "examplefontsize",
                output_dir = out_dir,
                theme = theme_docx_default_j(font = "Times New Roman", font_size = 8L))
```

### Visual Output (HTML Representation)

| examplefontsize:                                          |                   |                    |                    |            |
|-----------------------------------------------------------|-------------------|--------------------|--------------------|------------|
|                                                           | Example Drug 5 mg | Example Drug 10 mg | Example Drug 20 mg | Placebo    |
|                                                           | N=2               | N=2                | N=2                | N=2        |
| Male                                                      | 1                 | 1                  | 1                  | 1          |
|                                                           |                   |                    |                    |            |
| Response                                                  |                   |                    |                    |            |
| Yes                                                       | 1 (100.0%)        | 0                  | 1 (100.0%)         | 0          |
| No                                                        | 0                 | 1 (100.0%)         | 0                  | 1 (100.0%) |
| Female                                                    | 1                 | 1                  | 1                  | 1          |
|                                                           |                   |                    |                    |            |
| Response                                                  |                   |                    |                    |            |
| Yes                                                       | 1 (100.0%)        | 0                  | 1 (100.0%)         | 1 (100.0%) |
| No                                                        | 0                 | 1 (100.0%)         | 0                  | 0          |
|                                                           |                   |                    |                    |            |
| Dummy Note: On-treatment is defined as treatment-emergent |                   |                    |                    |            |

## Adding a Spanning Column Header to a Table

Sometimes, it is necessary to include a spanning header in the column
header space. This is common when there is a need to group specific
columns together under a common category text. A common example of this
is when a spanning header such as “Active Study Agent” is to be
displayed above all active treatments for a given study. To display the
spanning header, a variable needs to be included in the data to
represent the spanning header. Subsequently, a column header treatment
map needs to be created to identify which spanning headers and columns
are to be present in the resulting table output. For simple studies, the
[`junco::create_colspan_map`](https://johnsonandjohnson.github.io/junco/reference/colspan_map.md)
function can be leveraged to achieve the creation of the treatment map.

In the code below, the variable `colspan_trt` is being created to
identify the spanning header text. In this example, the “Example Drug 5
mg”, “Example Drug 10 mg”, and “Example Drug 20 mg” columns are to be
displayed beneath a spanning header of “Active Study Agent”. Then, the
treatment map is being created by calling
[`junco::create_colspan_map`](https://johnsonandjohnson.github.io/junco/reference/colspan_map.md).

``` r
library(junco)

adsl <- data.frame(TRT01A = factor(c("Example Drug 5 mg",
                                     "Example Drug 10 mg",
                                     "Example Drug 20 mg",
                                     "Placebo",
                                     "Example Drug 5 mg",
                                     "Example Drug 10 mg",
                                     "Example Drug 20 mg",
                                     "Placebo"),
                                   levels = c("Example Drug 5 mg",
                                              "Example Drug 10 mg",
                                              "Example Drug 20 mg",
                                              "Placebo"
                                              )
                              ),
                   colspan_trt = factor(c("Active Study Agent",
                                          "Active Study Agent",
                                          "Active Study Agent",
                                          " ",
                                          "Active Study Agent",
                                          "Active Study Agent",
                                          "Active Study Agent",
                                          " "),
                                        levels = c(
                                          "Active Study Agent",
                                          " "
                                        )
                                        ),
                   USUBJID = c("1",
                               "2",
                               "3",
                               "4",
                               "5",
                               "6",
                               "7",
                               "8"),
                   RESPONSE = factor(c("Yes",
                                       "No",
                                       "Yes",
                                       "Yes",
                                       "Yes",
                                       "No",
                                       "Yes",
                                       "No"),
                                     levels = c("Yes",
                                                "No")
                              ),
                   SEX = factor(c("Male",
                                  "Female",
                                  "Male",
                                  "Female",
                                  "Female",
                                  "Male",
                                  "Female",
                                  "Male"),
                                levels = c("Male",
                                           "Female")
                           )
) |>
  var_relabel(RESPONSE = "Response")

adsl
#>               TRT01A        colspan_trt USUBJID RESPONSE    SEX
#> 1  Example Drug 5 mg Active Study Agent       1      Yes   Male
#> 2 Example Drug 10 mg Active Study Agent       2       No Female
#> 3 Example Drug 20 mg Active Study Agent       3      Yes   Male
#> 4            Placebo                          4      Yes Female
#> 5  Example Drug 5 mg Active Study Agent       5      Yes Female
#> 6 Example Drug 10 mg Active Study Agent       6       No   Male
#> 7 Example Drug 20 mg Active Study Agent       7      Yes Female
#> 8            Placebo                          8       No   Male

  colspan_trt_map <- create_colspan_map(adsl,
                                      non_active_grp = "Placebo",
                                      non_active_grp_span_lbl = " ",
                                      active_grp_span_lbl = "Active Study Agent",
                                      colspan_var = "colspan_trt",
                                      trt_var = "TRT01A")

  colspan_trt_map
#>          colspan_trt             TRT01A
#> 1 Active Study Agent  Example Drug 5 mg
#> 2 Active Study Agent Example Drug 10 mg
#> 3 Active Study Agent Example Drug 20 mg
#> 4                               Placebo
```

The full code to generate the output with the spanning column header is
as follows.

``` r
library(junco)

adsl <- data.frame(TRT01A = factor(c("Example Drug 5 mg",
                                     "Example Drug 10 mg",
                                     "Example Drug 20 mg",
                                     "Placebo",
                                     "Example Drug 5 mg",
                                     "Example Drug 10 mg",
                                     "Example Drug 20 mg",
                                     "Placebo"),
                                   levels = c("Example Drug 5 mg",
                                              "Example Drug 10 mg",
                                              "Example Drug 20 mg",
                                              "Placebo"
                                              )
                              ),
                   colspan_trt = factor(c("Active Study Agent",
                                          "Active Study Agent",
                                          "Active Study Agent",
                                          " ",
                                          "Active Study Agent",
                                          "Active Study Agent",
                                          "Active Study Agent",
                                          " "),
                                        levels = c(
                                          "Active Study Agent",
                                          " "
                                        )
                                        ),
                   USUBJID = c("1",
                               "2",
                               "3",
                               "4",
                               "5",
                               "6",
                               "7",
                               "8"),
                   RESPONSE = factor(c("Yes",
                                       "No",
                                       "Yes",
                                       "Yes",
                                       "Yes",
                                       "No",
                                       "Yes",
                                       "No"),
                                     levels = c("Yes",
                                                "No")
                              ),
                   SEX = factor(c("Male",
                                  "Female",
                                  "Male",
                                  "Female",
                                  "Female",
                                  "Male",
                                  "Female",
                                  "Male"),
                                levels = c("Male",
                                           "Female")
                           )
) |>
  var_relabel(RESPONSE = "Response")

  # No combined column needed for spanning header example
  mysplit <- make_split_fun()

  colspan_trt_map <- create_colspan_map(adsl,
                                      non_active_grp = "Placebo",
                                      non_active_grp_span_lbl = " ",
                                      active_grp_span_lbl = "Active Study Agent",
                                      colspan_var = "colspan_trt",
                                      trt_var = "TRT01A")

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) |>
  split_cols_by("colspan_trt",
                split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) |>
  split_cols_by("TRT01A",
                show_colcounts = FALSE,
                split_fun = mysplit
  ) |>
  ### create table body row sections based on SEX
  split_rows_by("SEX",
                section_div = c(" "),
                page_by = TRUE,
                split_fun = drop_split_levels
                ) |>
  summarize_row_groups("SEX",
                       cfun =a_freq_j,
                       extra_args = list(denom = "n_df",
                                         denom_by = "SEX",
                                         riskdiff = FALSE,
                                         extrablankline = TRUE,
                                         .stats = c("count_unique")
                                         )
                       ) |>
  ### analyze height
  analyze("RESPONSE",
          var_labels = c("Response"),
          show_labels = "visible",
          afun = a_freq_j,
          extra_args = list(denom = "n_df",
                            denom_by = "SEX",
                            riskdiff = FALSE,
                            .stats = c("count_unique_fraction")
                            )
          )

result <- build_table(lyt, adsl)
result <- set_titles(result, titles)
tt_to_tlgrtf(result,
             file = paste0(out_dir, "/examplespanningheader1"),
             orientation = "landscape")
#> [[1]]
#> NULL
```

### Visual Output (HTML Representation)

| examplespanningheader1:                                   |                    |                    |                    |            |
|-----------------------------------------------------------|--------------------|--------------------|--------------------|------------|
|                                                           | Active Study Agent |                    |                    |            |
|                                                           | Example Drug 5 mg  | Example Drug 10 mg | Example Drug 20 mg | Placebo    |
|                                                           | N=2                | N=2                | N=2                | N=2        |
| Male                                                      | 1                  | 1                  | 1                  | 1          |
|                                                           |                    |                    |                    |            |
| Response                                                  |                    |                    |                    |            |
| Yes                                                       | 1 (100.0%)         | 0                  | 1 (100.0%)         | 0          |
| No                                                        | 0                  | 1 (100.0%)         | 0                  | 1 (100.0%) |
| Female                                                    | 1                  | 1                  | 1                  | 1          |
|                                                           |                    |                    |                    |            |
| Response                                                  |                    |                    |                    |            |
| Yes                                                       | 1 (100.0%)         | 0                  | 1 (100.0%)         | 1 (100.0%) |
| No                                                        | 0                  | 1 (100.0%)         | 0                  | 0          |
|                                                           |                    |                    |                    |            |
| Dummy Note: On-treatment is defined as treatment-emergent |                    |                    |                    |            |

## Adding a Combined Column to a Table

Sometimes it is necessary to add a combined column to a table, which
contains aggregated data from multiple columns.

The creation of a combined column can be achieved by leveraging
[`rtables::add_combo_facet`](https://insightsengineering.github.io/rtables/latest-tag/reference/add_combo_facet.html).
In the example below, we want to add a column named “Combined”, that
aggregates the data from the “Example Drug 5 mg”, “Example Drug 10 mg”,
and “Example Drug 20 mg” columns. This can be achieved by creating an
object named “add_combo”, that calls
[`rtables::add_combo_facet`](https://insightsengineering.github.io/rtables/latest-tag/reference/add_combo_facet.html),
which creates a new column from other columns in the table. A split
function can then be constructed using
[`rtables::make_split_fun`](https://insightsengineering.github.io/rtables/latest-tag/reference/make_split_fun.html),
where the “post” argument contains the “add_combo” object, which calls
[`rtables::add_combo_facet`](https://insightsengineering.github.io/rtables/latest-tag/reference/add_combo_facet.html).
The split function is then included in the
[`rtables::split_cols_by`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_cols_by.html)
function call for the TRT01A variable.

The snippet of code of interest is as follows.

``` r
add_combo <- add_combo_facet("Combined",
    label = "Combined",
    levels = c("Example Drug 5 mg", "Example Drug 10 mg", "Example Drug 20 mg")
  )

  mysplit <- make_split_fun(post = list(add_combo))
```

The full code to generate the output with the new “Combined” column is
as follows.

``` r
library(junco)

adsl <- data.frame(TRT01A = factor(c("Example Drug 5 mg",
                                     "Example Drug 10 mg",
                                     "Example Drug 20 mg",
                                     "Placebo",
                                     "Example Drug 5 mg",
                                     "Example Drug 10 mg",
                                     "Example Drug 20 mg",
                                     "Placebo"),
                                   levels = c("Example Drug 5 mg",
                                              "Example Drug 10 mg",
                                              "Example Drug 20 mg",
                                              "Placebo"
                                              )
                              ),
                   colspan_trt = factor(c("Active Study Agent",
                                          "Active Study Agent",
                                          "Active Study Agent",
                                          " ",
                                          "Active Study Agent",
                                          "Active Study Agent",
                                          "Active Study Agent",
                                          " "),
                                        levels = c(
                                          "Active Study Agent",
                                          " "
                                        )
                                        ),
                   USUBJID = c("1",
                               "2",
                               "3",
                               "4",
                               "5",
                               "6",
                               "7",
                               "8"),
                   RESPONSE = factor(c("Yes",
                                       "No",
                                       "Yes",
                                       "Yes",
                                       "Yes",
                                       "No",
                                       "Yes",
                                       "No"),
                                     levels = c("Yes",
                                                "No")
                              ),
                   SEX = factor(c("Male",
                                  "Female",
                                  "Male",
                                  "Female",
                                  "Female",
                                  "Male",
                                  "Female",
                                  "Male"),
                                levels = c("Male",
                                           "Female")
                           )
) |>
  var_relabel(RESPONSE = "Response")

  # Set up levels and label for the required combined column
  add_combo <- add_combo_facet("Combined",
    label = "Combined",
    levels = c("Example Drug 5 mg", "Example Drug 10 mg", "Example Drug 20 mg")
  )

  mysplit <- make_split_fun(post = list(add_combo))

  colspan_trt_map <- create_colspan_map(adsl,
                                      non_active_grp = "Placebo",
                                      non_active_grp_span_lbl = " ",
                                      active_grp_span_lbl = "Active Study Agent",
                                      colspan_var = "colspan_trt",
                                      trt_var = "TRT01A")

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) |>
  split_cols_by("colspan_trt",
                split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) |>
  split_cols_by("TRT01A",
                show_colcounts = FALSE,
                split_fun = mysplit
  ) |>
  ### create table body row sections based on SEX
  split_rows_by("SEX",
                section_div = c(" "),
                page_by = TRUE,
                split_fun = drop_split_levels
                ) |>
  summarize_row_groups("SEX",
                       cfun =a_freq_j,
                       extra_args = list(denom = "n_df",
                                         denom_by = "SEX",
                                         riskdiff = FALSE,
                                         extrablankline = TRUE,
                                         .stats = c("count_unique")
                                         )
                       ) |>
  ### analyze height
  analyze("RESPONSE",
          var_labels = c("Response"),
          show_labels = "visible",
          afun = a_freq_j,
          extra_args = list(denom = "n_df",
                            denom_by = "SEX",
                            riskdiff = FALSE,
                            .stats = c("count_unique_fraction")
                            )
          )

result <- build_table(lyt, adsl)
result <- set_titles(result, titles)
tt_to_tlgrtf(result,
             file = paste0(out_dir, "/examplecombinedcolumn1"),
             orientation = "landscape")
#> [[1]]
#> NULL
```

### Visual Output (HTML Representation)

| examplecombinedcolumn1:                                   |                    |                    |                    |           |            |          |
|-----------------------------------------------------------|--------------------|--------------------|--------------------|-----------|------------|----------|
|                                                           | Active Study Agent |                    |                    |           |            |          |
|                                                           | Example Drug 5 mg  | Example Drug 10 mg | Example Drug 20 mg | Combined  | Placebo    | Combined |
|                                                           | N=2                | N=2                | N=2                | N=6       | N=2        | N=0      |
| Male                                                      | 1                  | 1                  | 1                  | 3         | 1          | 0        |
|                                                           |                    |                    |                    |           |            |          |
| Response                                                  |                    |                    |                    |           |            |          |
| Yes                                                       | 1 (100.0%)         | 0                  | 1 (100.0%)         | 2 (66.7%) | 0          | -        |
| No                                                        | 0                  | 1 (100.0%)         | 0                  | 1 (33.3%) | 1 (100.0%) | -        |
| Female                                                    | 1                  | 1                  | 1                  | 3         | 1          | 0        |
|                                                           |                    |                    |                    |           |            |          |
| Response                                                  |                    |                    |                    |           |            |          |
| Yes                                                       | 1 (100.0%)         | 0                  | 1 (100.0%)         | 2 (66.7%) | 1 (100.0%) | -        |
| No                                                        | 0                  | 1 (100.0%)         | 0                  | 1 (33.3%) | 0          | -        |
|                                                           |                    |                    |                    |           |            |          |
| Dummy Note: On-treatment is defined as treatment-emergent |                    |                    |                    |           |            |          |

The code above creates the desired column, however, the issue is that
the resulting output produces a “Combined” column under each spanning
header, and as a result, 2 “Combined” columns are created. The
“Combined” column of interest is the one appearing beneath the “Active
Study Agent” spanning header. We need additional code to remove the
“Combined” column that lives in the other spanning header value of ” “.
This can be achieved by creating an object that calls
[`junco::cond_rm_facets`](https://johnsonandjohnson.github.io/junco/reference/cond_rm_facets.md),
which removes user-specified columns from facets. A split function can
then be constructed using
[`rtables::make_split_fun`](https://insightsengineering.github.io/rtables/latest-tag/reference/make_split_fun.html),
where the”post” argument contains the “add_combo” object, which calls
[`rtables::add_combo_facet`](https://insightsengineering.github.io/rtables/latest-tag/reference/add_combo_facet.html),
and the “rm_combo_from_placebo” object, which calls
[`junco::cond_rm_facets`](https://johnsonandjohnson.github.io/junco/reference/cond_rm_facets.md).
Similar to above, the split function is then included in the
[`rtables::split_cols_by`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_cols_by.html)
function call for the TRT01A variable.

The additional code required for to produce the desired result is as
follows.

``` r
  # choose if any facets need to be removed - e.g remove the combined column for placebo
  rm_combo_from_placebo <- cond_rm_facets(
    facets = "Combined",
    ancestor_pos = NA,
    value = " ",
    split = "colspan_trt"
  )

  mysplit <- make_split_fun(post = list(add_combo, rm_combo_from_placebo))
```

The full code to generate the output with the desired “Combined” column
is as follows.

``` r
library(junco)

adsl <- data.frame(TRT01A = factor(c("Example Drug 5 mg",
                                     "Example Drug 10 mg",
                                     "Example Drug 20 mg",
                                     "Placebo",
                                     "Example Drug 5 mg",
                                     "Example Drug 10 mg",
                                     "Example Drug 20 mg",
                                     "Placebo"),
                                   levels = c("Example Drug 5 mg",
                                              "Example Drug 10 mg",
                                              "Example Drug 20 mg",
                                              "Placebo"
                                              )
                              ),
                   colspan_trt = factor(c("Active Study Agent",
                                          "Active Study Agent",
                                          "Active Study Agent",
                                          " ",
                                          "Active Study Agent",
                                          "Active Study Agent",
                                          "Active Study Agent",
                                          " "),
                                        levels = c(
                                          "Active Study Agent",
                                          " "
                                        )
                                        ),
                   USUBJID = c("1",
                               "2",
                               "3",
                               "4",
                               "5",
                               "6",
                               "7",
                               "8"),
                   RESPONSE = factor(c("Yes",
                                       "No",
                                       "Yes",
                                       "Yes",
                                       "Yes",
                                       "No",
                                       "Yes",
                                       "No"),
                                     levels = c("Yes",
                                                "No")
                              ),
                   SEX = factor(c("Male",
                                  "Female",
                                  "Male",
                                  "Female",
                                  "Female",
                                  "Male",
                                  "Female",
                                  "Male"),
                                levels = c("Male",
                                           "Female")
                           )
) |>
  var_relabel(RESPONSE = "Response")

  # Set up levels and label for the required combined column
  add_combo <- add_combo_facet("Combined",
    label = "Combined",
    levels = c("Example Drug 5 mg", "Example Drug 10 mg", "Example Drug 20 mg")
  )

  # choose if any facets need to be removed - e.g remove the combined column for placebo
  rm_combo_from_placebo <- cond_rm_facets(
    facets = "Combined",
    ancestor_pos = NA,
    value = " ",
    split = "colspan_trt"
  )

  mysplit <- make_split_fun(post = list(add_combo, rm_combo_from_placebo))

  colspan_trt_map <- create_colspan_map(adsl,
                                      non_active_grp = "Placebo",
                                      non_active_grp_span_lbl = " ",
                                      active_grp_span_lbl = "Active Study Agent",
                                      colspan_var = "colspan_trt",
                                      trt_var = "TRT01A")

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) |>
  split_cols_by("colspan_trt",
                split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) |>
  split_cols_by("TRT01A",
                show_colcounts = FALSE,
                split_fun = mysplit
  ) |>
  ### create table body row sections based on SEX
  split_rows_by("SEX",
                section_div = c(" "),
                page_by = TRUE,
                split_fun = drop_split_levels
                ) |>
  summarize_row_groups("SEX",
                       cfun =a_freq_j,
                       extra_args = list(denom = "n_df",
                                         denom_by = "SEX",
                                         riskdiff = FALSE,
                                         extrablankline = TRUE,
                                         .stats = c("count_unique")
                                         )
                       ) |>
  ### analyze height
  analyze("RESPONSE",
          var_labels = c("Response"),
          show_labels = "visible",
          afun = a_freq_j,
          extra_args = list(denom = "n_df",
                            denom_by = "SEX",
                            riskdiff = FALSE,
                            .stats = c("count_unique_fraction")
                            )
          )

result <- build_table(lyt, adsl)
result <- set_titles(result, titles)
tt_to_tlgrtf(result,
             file = paste0(out_dir, "/examplecombinedcolumn2"),
             orientation = "landscape")
#> [[1]]
#> NULL
```

### Visual Output (HTML Representation)

| examplecombinedcolumn2:                                   |                    |                    |                    |           |            |
|-----------------------------------------------------------|--------------------|--------------------|--------------------|-----------|------------|
|                                                           | Active Study Agent |                    |                    |           |            |
|                                                           | Example Drug 5 mg  | Example Drug 10 mg | Example Drug 20 mg | Combined  | Placebo    |
|                                                           | N=2                | N=2                | N=2                | N=6       | N=2        |
| Male                                                      | 1                  | 1                  | 1                  | 3         | 1          |
|                                                           |                    |                    |                    |           |            |
| Response                                                  |                    |                    |                    |           |            |
| Yes                                                       | 1 (100.0%)         | 0                  | 1 (100.0%)         | 2 (66.7%) | 0          |
| No                                                        | 0                  | 1 (100.0%)         | 0                  | 1 (33.3%) | 1 (100.0%) |
| Female                                                    | 1                  | 1                  | 1                  | 3         | 1          |
|                                                           |                    |                    |                    |           |            |
| Response                                                  |                    |                    |                    |           |            |
| Yes                                                       | 1 (100.0%)         | 0                  | 1 (100.0%)         | 2 (66.7%) | 1 (100.0%) |
| No                                                        | 0                  | 1 (100.0%)         | 0                  | 1 (33.3%) | 0          |
|                                                           |                    |                    |                    |           |            |
| Dummy Note: On-treatment is defined as treatment-emergent |                    |                    |                    |           |            |

## Inserting a New Line within Table & Listing Text

Sometimes it is necessary to insert a new line within text so that the
information is presented in the exact format provided in the table or
listing specification.

In the following example data, there is a desire to insert a new line
between the drug name and the dose level for the “Example Drug” drug. In
this case, the text “Example Drug” is to appear on a different line than
the dose levels (i.e. “5 mg”, “10 mg”, “20 mg”).

``` r
adsl <- data.frame(TRT01A = c("Example Drug 5 mg",
                              "Example Drug 10 mg",
                              "Example Drug 20 mg",
                              "Placebo"),
                   SUBJECT = c("1",
                               "2",
                               "3",
                               "4"),
                   HEIGHT = c(70,
                              74,
                              60,
                              64
                   )
)

adsl
#>               TRT01A SUBJECT HEIGHT
#> 1  Example Drug 5 mg       1     70
#> 2 Example Drug 10 mg       2     74
#> 3 Example Drug 20 mg       3     60
#> 4            Placebo       4     64
```

### Table .rtf File

In order to get the dose level to appear on a new line, then the TRT01A
variable values will need to be updated to include the text “\\line”
between the drug name and dose level.

``` r
library(junco)
library(rtables)

adsl <- data.frame(TRT01A = c("Example Drug\\line 5 mg",
                              "Example Drug\\line 10 mg",
                              "Example Drug\\line 20 mg",
                              "Placebo"),
                   SUBJECT = c("1",
                               "2",
                               "3",
                               "4"),
                   HEIGHT = c(70,
                              74,
                              60,
                              64
                   )
)

lyt <- basic_table(
  show_colcounts = FALSE,
  colcount_format = "N=xx"
) |>
  ### first columns
  split_cols_by("TRT01A",,
                show_colcounts = FALSE
  ) |>
  ### analyze height
  analyze("HEIGHT", afun = list_wrap_x(summary), format = "xx.xx")

result <- build_table(lyt, adsl)
result <- set_titles(result, titles)
tt_to_tlgrtf(result,
             file = paste0(out_dir, "/newlinetable"),
             orientation = "landscape")
#> [[1]]
#> NULL
```

### Table .docx File

In order to get the dose level to appear on a new line, then the TRT01A
variable values will need to be updated to include the text “\n” between
the drug name and dose level.

``` r
library(junco)
library(rtables)

adsl <- data.frame(TRT01A = c("Example Drug\n 5 mg",
                              "Example Drug\n 10 mg",
                              "Example Drug\n 20 mg",
                              "Placebo"),
                   SUBJECT = c("1",
                               "2",
                               "3",
                               "4"),
                   HEIGHT = c(70,
                              74,
                              60,
                              64
                   )
)

lyt <- basic_table(
  show_colcounts = FALSE,
  colcount_format = "N=xx"
) |>
  ### first columns
  split_cols_by("TRT01A",,
                show_colcounts = FALSE
  ) |>
  ### analyze height
  analyze("HEIGHT", afun = list_wrap_x(summary), format = "xx.xx")

result <- build_table(lyt, adsl)
result <- set_titles(result, titles)

export_as_docx_j(result,
                tblid = "examplenewline",
                output_dir = out_dir,
                orientation = "landscape")
```

### Visual Output (HTML Representation)

| newlinetable:                                             |              |              |              |         |
|-----------------------------------------------------------|--------------|--------------|--------------|---------|
|                                                           | Example Drug | Example Drug | Example Drug |         |
|                                                           |  5 mg        |  10 mg       |  20 mg       | Placebo |
| Min.                                                      | 70.00        | 74.00        | 60.00        | 64.00   |
| 1st Qu.                                                   | 70.00        | 74.00        | 60.00        | 64.00   |
| Median                                                    | 70.00        | 74.00        | 60.00        | 64.00   |
| Mean                                                      | 70.00        | 74.00        | 60.00        | 64.00   |
| 3rd Qu.                                                   | 70.00        | 74.00        | 60.00        | 64.00   |
| Max.                                                      | 70.00        | 74.00        | 60.00        | 64.00   |
|                                                           |              |              |              |         |
| Dummy Note: On-treatment is defined as treatment-emergent |              |              |              |         |

### Listing .rtf File

If a .rtf file is to be created for the listing, new lines can be
inserted into the column header by updating the variable label that
corresponds with the column of interest.

By adding in “\\line” to the TRT01A variable label, a new line is
inserted after “Actual Treatment”.

``` r
library(junco)
library(rlistings)

adsl <- data.frame(TRT01A = c("Example Drug 5 mg",
                              "Example Drug 10 mg",
                              "Example Drug 20 mg",
                              "Placebo"),
                   SUBJECT = c("1",
                               "2",
                               "3",
                               "4"),
                   HEIGHT = c(70,
                              74,
                              60,
                              64
                   )
) |>
  formatters::var_relabel(TRT01A = "Actual Treatment for\\line Period 01") |>
  formatters::var_relabel(SUBJECT = "Subject") |>
  formatters::var_relabel(HEIGHT = "Height (in)")

result <- rlistings::as_listing(
  df = adsl,
  key_cols = c("TRT01A", "SUBJECT"),
  disp_cols = "HEIGHT"
)
result <- set_titles(result, titles)
tt_to_tlgrtf(result,
             file = paste0(out_dir, "/examplelistingnewline2"),
             orientation = "landscape")
```

### Listing .docx File

If a .docx file is to be created for the listing, new lines can be
inserted into the column header by updating the variable label that
corresponds with the column of interest.

By adding in “\n” to the TRT01A variable label, a new line is inserted
after “Actual Treatment”.

``` r
library(junco)
library(rlistings)

adsl <- data.frame(TRT01A = c("Example Drug 5 mg",
                              "Example Drug 10 mg",
                              "Example Drug 20 mg",
                              "Placebo"),
                   SUBJECT = c("1",
                               "2",
                               "3",
                               "4"),
                   HEIGHT = c(70,
                              74,
                              60,
                              64
                   )
) |>
  formatters::var_relabel(TRT01A = "Actual Treatment for\n Period 01") |>
  formatters::var_relabel(SUBJECT = "Subject") |>
  formatters::var_relabel(HEIGHT = "Height (in)")

result <- rlistings::as_listing(
  df = adsl,
  key_cols = c("TRT01A", "SUBJECT"),
  disp_cols = "HEIGHT"
)
result <- set_titles(result, titles)


export_as_docx_j(result,
                tblid = "examplelistingnewline2",
                output_dir = out_dir,
                orientation = "landscape")
```

### Visual Output (HTML Representation)

| examplelistingnewline2:                                   |         |             |
|-----------------------------------------------------------|---------|-------------|
| Actual Treatment for                                      |         |             |
| Period 01                                                 | Subject | Height (in) |
| Example Drug 10 mg                                        | 2       | 74          |
| Example Drug 20 mg                                        | 3       | 60          |
| Example Drug 5 mg                                         | 1       | 70          |
| Placebo                                                   | 4       | 64          |
|                                                           |         |             |
| Dummy Note: On-treatment is defined as treatment-emergent |         |             |

## Custom Table Column Header Border Matrix

When creating a table using, there is a set of default assumptions in
place that determine how the borders are crafted within the table column
header space. There are times when the table specification calls for a
border matrix that is not based on the default assumptions. Below is an
example of how to create a border matrix that produces the user required
table.

### Table .rtf File

The following is an example of a table created from
[`junco::tt_to_tlgrtf`](https://johnsonandjohnson.github.io/junco/reference/tt_to_tlgrtf.md).

``` r
library(junco)

adsl <- data.frame(span = c("Example Drug Low Dose",
                            "Example Drug Low Dose",
                            "Example Drug High Dose",
                            " "),
                   TRT01A = c("Example Drug 5 mg",
                              "Example Drug 10 mg",
                              "Example Drug 20 mg",
                              "Placebo"),
                   SUBJECT = c("1",
                               "2",
                               "3",
                               "4"),
                   HEIGHT = c(70,
                              74,
                              60,
                              64
                   )
)

lyt <- basic_table(
  show_colcounts = FALSE,
  colcount_format = "N=xx"
) |>
  ### first level in column header
  split_cols_by("span",,
                show_colcounts = FALSE
  ) |>
  ### second level in column header
  split_cols_by("TRT01A",,
                show_colcounts = FALSE
  ) |>
  ### analyze height
  analyze("HEIGHT", afun = list_wrap_x(summary), format = "xx.xx")

result <- build_table(lyt, adsl)
result <- set_titles(result, titles)
tt_to_tlgrtf(result,
             file = paste0(out_dir, "/bordertable"),
             orientation = "landscape")
#> [[1]]
#> NULL
```

### Visual Output (HTML Representation)

| bordertable:                                              |                       |                    |                        |         |
|-----------------------------------------------------------|-----------------------|--------------------|------------------------|---------|
|                                                           | Example Drug Low Dose |                    | Example Drug High Dose |         |
|                                                           | Example Drug 5 mg     | Example Drug 10 mg | Example Drug 20 mg     | Placebo |
| Min.                                                      | 70.00                 | 74.00              | 60.00                  | 64.00   |
| 1st Qu.                                                   | 70.00                 | 74.00              | 60.00                  | 64.00   |
| Median                                                    | 70.00                 | 74.00              | 60.00                  | 64.00   |
| Mean                                                      | 70.00                 | 74.00              | 60.00                  | 64.00   |
| 3rd Qu.                                                   | 70.00                 | 74.00              | 60.00                  | 64.00   |
| Max.                                                      | 70.00                 | 74.00              | 60.00                  | 64.00   |
|                                                           |                       |                    |                        |         |
| Dummy Note: On-treatment is defined as treatment-emergent |                       |                    |                        |         |

In the resulting output, the “Example Drug High Dose” spanning header
does not include an underline beneath the text at the bottom of the
cell. It’s important for us to understand what the default border matrix
looks like. This can be achieved by calling the
`junco:::make_header_bordmat` function.

``` r
header_border <- junco:::make_header_bordmat(obj = result)
header_border
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    0    2    2    0    0
#> [2,]    1    1    1    1    1
```

It can be observed that the first row, and fourth column in the border
matrix contains a value of 0. In order to have a bottom border present
under “Example Drug High Dose”, a value other than 0 or 2 must be
specified. This is because “Example Drug High Dose” is expected to have
it’s own spanning header, and the value specified in the border matrix
must be unique to the cell. The border can be updated as follows.

``` r
header_border[1, 4] <- 4
header_border
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    0    2    2    4    0
#> [2,]    1    1    1    1    1
```

Now, if the
[`junco::tt_to_tlgrtf`](https://johnsonandjohnson.github.io/junco/reference/tt_to_tlgrtf.md)
function is called with the “border_mat” argument included, the new
user-defined border matrix can be provided as the argument value to
specify the desired column header borders.

``` r
tt_to_tlgrtf(result,
             file = paste0(out_dir, "/custombordertable"),
             orientation = "landscape",
             border_mat = header_border)
#> [[1]]
#> NULL
```

### Table .docx File

To export a table with a custom border matrix to a .docx file, the
[`junco::export_as_docx_j`](https://johnsonandjohnson.github.io/junco/reference/export_as_docx_j.md)
function can be called with the `border_mat` argument:

``` r
export_as_docx_j(result,
                tblid = "custombordertable",
                output_dir = out_dir,
                orientation = "landscape",
                border_mat = header_border)
```

### Visual Output (HTML Representation)

> **Note:** In this HTML output the spanning line is a single line,
> instead of 2 lines. However, if you output the .docx or .rtf file
> using the same TableTree as input, it will be displayed correctly.

| custombordertable:                                        |                       |                    |                        |         |
|-----------------------------------------------------------|-----------------------|--------------------|------------------------|---------|
|                                                           | Example Drug Low Dose |                    | Example Drug High Dose |         |
|                                                           | Example Drug 5 mg     | Example Drug 10 mg | Example Drug 20 mg     | Placebo |
| Min.                                                      | 70.00                 | 74.00              | 60.00                  | 64.00   |
| 1st Qu.                                                   | 70.00                 | 74.00              | 60.00                  | 64.00   |
| Median                                                    | 70.00                 | 74.00              | 60.00                  | 64.00   |
| Mean                                                      | 70.00                 | 74.00              | 60.00                  | 64.00   |
| 3rd Qu.                                                   | 70.00                 | 74.00              | 60.00                  | 64.00   |
| Max.                                                      | 70.00                 | 74.00              | 60.00                  | 64.00   |
|                                                           |                       |                    |                        |         |
| Dummy Note: On-treatment is defined as treatment-emergent |                       |                    |                        |         |

## Addition of Superscript or Other Symbol

Sometimes it is necessary to insert a superscript or other symbol within
text so that the information is presented according to the table or
listing specification. This can be achieved by specifying a markup data
frame in the export function call.

In the `junco` package, there is a default markup data frame for each
exporter function, which contains common markup that translates to
symbols/characters that may be desired in an output. The default markup
data frame contains markup to insert superscripts, insert subscripts,
and can remove optional text.

The default markup file for
[`junco::tt_to_tlgrtf`](https://johnsonandjohnson.github.io/junco/reference/tt_to_tlgrtf.md)
to produce an .rtf file is as follows.

``` r
library(tibble)

dps_markup_df <- tibble::tribble(
  ~keyword,
  ~rtfstart,
  ~rtfend,
  "super",
  "\\super",
  "\\nosupersub",
  "sub",
  "\\sub",
  "\\nosupersub",
  "optional",
  "",
  ""
)

dps_markup_df
#> # A tibble: 3 × 3
#>   keyword  rtfstart  rtfend        
#>   <chr>    <chr>     <chr>         
#> 1 super    "\\super" "\\nosupersub"
#> 2 sub      "\\sub"   "\\nosupersub"
#> 3 optional ""        ""
```

If additional markup is to be considered, then a custom markup data
frame can be created and specified in the exporter function call.

In the following example data, there is a desire to insert a superscript
“a” at the end the drug name for the “Example Drug” drug in reference to
a footnote present at the end of the table.

``` r
adsl <- data.frame(TRT01A = c("Example Drug 5 mg",
                              "Example Drug 10 mg",
                              "Example Drug 20 mg",
                              "Placebo"),
                   SUBJECT = c("1",
                               "2",
                               "3",
                               "4"),
                   HEIGHT = c(70,
                              74,
                              60,
                              64
                   )
)

adsl
#>               TRT01A SUBJECT HEIGHT
#> 1  Example Drug 5 mg       1     70
#> 2 Example Drug 10 mg       2     74
#> 3 Example Drug 20 mg       3     60
#> 4            Placebo       4     64
```

Depending on the file format required (i.e. .rtf or .docx) the data will
require different updates, according to the markup that is required.

### Table .rtf File

If a .rtf file is to be created for the table, the
[`junco::tt_to_tlgrtf`](https://johnsonandjohnson.github.io/junco/reference/tt_to_tlgrtf.md)
is to be called. In this example, when calling
[`junco::tt_to_tlgrtf`](https://johnsonandjohnson.github.io/junco/reference/tt_to_tlgrtf.md),
the “markup_df” argument has the default “dps_markup_df” object
specified.

In order to get the superscript “a” to appear, the TRT01A variable
values will need to be updated to include the text “~\[super a\]”
between the drug name and dose level.

``` r
library(junco)

adsl <- data.frame(TRT01A = c("Example Drug~[super a] 5 mg",
                              "Example Drug~[super a] 10 mg",
                              "Example Drug~[super a] 20 mg",
                              "Placebo"),
                   SUBJECT = c("1",
                               "2",
                               "3",
                               "4"),
                   HEIGHT = c(70,
                              74,
                              60,
                              64
                   )
)

lyt <- basic_table(
  show_colcounts = FALSE,
  colcount_format = "N=xx"
) |>
  ### first columns
  split_cols_by("TRT01A",,
                show_colcounts = FALSE
  ) |>
  ### analyze height
  analyze("HEIGHT", afun = list_wrap_x(summary), format = "xx.xx")

result <- build_table(lyt, adsl)
result <- set_titles(result, titles)
tt_to_tlgrtf(result,
             file = paste0(out_dir, "/superscripttable"),
             orientation = "landscape",
             markup_df = dps_markup_df)
#> [[1]]
#> NULL
```

### Table .docx File

The default markup file for
[`junco::export_as_docx_j`](https://johnsonandjohnson.github.io/junco/reference/export_as_docx_j.md)
to produce a .docx file is as follows.

``` r
library(tibble)

docx_markup_df <- tibble::tribble(
  ~keyword,
  ~replace_by,
  "super",
  "flextable::as_sup",
  "sub",
  "flextable::as_sub",
  "optional",
  ""
)

docx_markup_df
#> # A tibble: 3 × 2
#>   keyword  replace_by         
#>   <chr>    <chr>              
#> 1 super    "flextable::as_sup"
#> 2 sub      "flextable::as_sub"
#> 3 optional ""
```

If a .docx file is to be created for the table, the
[`junco::export_as_docx_j`](https://johnsonandjohnson.github.io/junco/reference/export_as_docx_j.md)
is to be called. For .docx files, the markup is different from .rtf
files. In this example, when calling
[`junco::export_as_docx_j`](https://johnsonandjohnson.github.io/junco/reference/export_as_docx_j.md),
the “markup_df” argument has the default “docx_markup_df” object
specified.

``` r
library(junco)

adsl <- data.frame(TRT01A = c("Example Drug~[super a] 5 mg",
                              "Example Drug~[super a] 10 mg",
                              "Example Drug~[super a] 20 mg",
                              "Placebo"),
                   SUBJECT = c("1",
                               "2",
                               "3",
                               "4"),
                   HEIGHT = c(70,
                              74,
                              60,
                              64
                   )
)

lyt <- basic_table(
  show_colcounts = FALSE,
  colcount_format = "N=xx"
) |>
  ### first columns
  split_cols_by("TRT01A",,
                show_colcounts = FALSE
  ) |>
  ### analyze height
  analyze("HEIGHT", afun = list_wrap_x(summary), format = "xx.xx")

result <- build_table(lyt, adsl)
result <- set_titles(result, titles)

export_as_docx_j(result,
                tblid = "superscripttable",
                output_dir = out_dir,
                orientation = "landscape",
                markup_df = docx_markup_df)
```

### Visual Output (HTML Representation)

| superscripttable:                                         |                    |                     |                     |         |
|-----------------------------------------------------------|--------------------|---------------------|---------------------|---------|
|                                                           | Example Druga 5 mg | Example Druga 10 mg | Example Druga 20 mg | Placebo |
| Min.                                                      | 70.00              | 74.00               | 60.00               | 64.00   |
| 1st Qu.                                                   | 70.00              | 74.00               | 60.00               | 64.00   |
| Median                                                    | 70.00              | 74.00               | 60.00               | 64.00   |
| Mean                                                      | 70.00              | 74.00               | 60.00               | 64.00   |
| 3rd Qu.                                                   | 70.00              | 74.00               | 60.00               | 64.00   |
| Max.                                                      | 70.00              | 74.00               | 60.00               | 64.00   |
|                                                           |                    |                     |                     |         |
| Dummy Note: On-treatment is defined as treatment-emergent |                    |                     |                     |         |

### Listing .rtf File

If a .rtf file is to be created for the listing, a superscript “a” can
be inserted into the column header by updating the variable label that
corresponds with the column of interest.

By adding in “~\[super a\]” to the TRT01A variable label, a superscript
“a” is added to the column label.

``` r
library(junco)
library(rlistings)

adsl <- data.frame(TRT01A = c("Example Drug 5 mg",
                              "Example Drug 10 mg",
                              "Example Drug 20 mg",
                              "Placebo"),
                   SUBJECT = c("1",
                               "2",
                               "3",
                               "4"),
                   HEIGHT = c(70,
                              74,
                              60,
                              64
                   )
) |>
  formatters::var_relabel(TRT01A = "Actual Treatment for Period 01~[super a]") |>
  formatters::var_relabel(SUBJECT = "Subject") |>
  formatters::var_relabel(HEIGHT = "Height (in)")

result <- rlistings::as_listing(
  df = adsl,
  key_cols = c("TRT01A", "SUBJECT"),
  disp_cols = "HEIGHT"
)
result <- set_titles(result, titles)
tt_to_tlgrtf(result,
             file = paste0(out_dir, "/examplelistingsuperscript2"),
             orientation = "landscape")
```

### Listing .docx File

The default markup file for
[`junco::export_as_docx_j`](https://johnsonandjohnson.github.io/junco/reference/export_as_docx_j.md)
to produce a .docx file is as follows.

``` r
library(tibble)

docx_markup_df <- tibble::tribble(
  ~keyword,
  ~replace_by,
  "super",
  "flextable::as_sup",
  "sub",
  "flextable::as_sub",
  "optional",
  ""
)

docx_markup_df
#> # A tibble: 3 × 2
#>   keyword  replace_by         
#>   <chr>    <chr>              
#> 1 super    "flextable::as_sup"
#> 2 sub      "flextable::as_sub"
#> 3 optional ""
```

If a .docx file is to be created for the table, the
[`junco::export_as_docx_j`](https://johnsonandjohnson.github.io/junco/reference/export_as_docx_j.md)
is to be called. For .docx files, the markup is different from .rtf
files. In this example, when calling
[`junco::export_as_docx_j`](https://johnsonandjohnson.github.io/junco/reference/export_as_docx_j.md),
the “markup_df” argument has the default “docx_markup_df” object
specified.

``` r

library(junco)
library(rlistings)

adsl <- data.frame(TRT01A = c("Example Drug 5 mg",
                              "Example Drug 10 mg",
                              "Example Drug 20 mg",
                              "Placebo"),
                   SUBJECT = c("1",
                               "2",
                               "3",
                               "4"),
                   HEIGHT = c(70,
                              74,
                              60,
                              64
                   )
) |>
  formatters::var_relabel(TRT01A = "Actual Treatment for Period 01~[super a]") |>
  formatters::var_relabel(SUBJECT = "Subject") |>
  formatters::var_relabel(HEIGHT = "Height (in)")

result <- rlistings::as_listing(
  df = adsl,
  key_cols = c("TRT01A", "SUBJECT"),
  disp_cols = "HEIGHT"
)
result <- set_titles(result, titles)

export_as_docx_j(result,
                tblid = "examplelistingsuperscript2",
                output_dir = out_dir,
                orientation = "landscape",
                markup_df = docx_markup_df)
```

### Visual Output (HTML Representation)

| examplelistingsuperscript2:                               |         |             |
|-----------------------------------------------------------|---------|-------------|
| Actual Treatment for Period 01a                           | Subject | Height (in) |
| Example Drug 10 mg                                        | 2       | 74          |
| Example Drug 20 mg                                        | 3       | 60          |
| Example Drug 5 mg                                         | 1       | 70          |
| Placebo                                                   | 4       | 64          |
|                                                           |         |             |
| Dummy Note: On-treatment is defined as treatment-emergent |         |             |

## Grouping of Columns for Tables Containing Many Columns

In some cases, there are tables that are required that contain many
columns. As a result, it’s possible that the table might need to be
split into multiple files so that the information can be presented in a
readable manner. Oftentimes, it’s important to keep certain information
grouped together in the same file. The following is an example of this
scenario.

Data Preparation for example

``` r
library(tern)
library(dplyr)
library(rtables)
library(junco)

################################################################################
# Define script level parameters:
################################################################################

string_map <- default_str_map
trtvar <- "TRT01A"
popfl <- "SAFFL"

################################################################################
# Process Data:
################################################################################

adsl <- pharmaverseadamjnj::adsl %>%
  filter(!!rlang::sym(popfl) == "Y") %>%
  select(STUDYID, USUBJID, all_of(trtvar), all_of(popfl), RACE)

adae <- pharmaverseadamjnj::adae %>%
  filter(TRTEMFL == "Y" & AEBODSYS == "Skin and subcutaneous tissue disorders") %>%
  select(USUBJID, TRTEMFL, AEBODSYS, AEDECOD, RACE)

adsl$colspan_trt <- factor(ifelse(adsl[[trtvar]] == "Placebo", " ", "Active Study Agent"),
                           levels = c("Active Study Agent", " ")
)

colspan_trt_map <- create_colspan_map(adsl,
                                      non_active_grp = "Placebo",
                                      non_active_grp_span_lbl = " ",
                                      active_grp_span_lbl = "Active Study Agent",
                                      colspan_var = "colspan_trt",
                                      trt_var = trtvar
)

# Add total for Race - adsl
totalrace1 <- adsl %>%
  filter(RACE != "UNKNOWN" & !is.na(RACE)) %>%
  mutate(RACE = "Total")

adsl <- bind_rows(totalrace1, adsl)

adsl <- adsl %>%
  mutate(RACEcat = case_when(
    RACE == "Total" ~ "Total",
    RACE == "WHITE" ~ "White",
    RACE == "BLACK OR AFRICAN AMERICAN" ~ "Black",
    RACE == "ASIAN" ~ "Asian",
    RACE != "UNKNOWN" & !is.na(RACE) ~ "Other"
  )) %>%
  filter(RACEcat %in% c("Total", "White", "Black", "Asian", "Other")) %>%
  select(-RACE)

adsl$spanheader <- factor(ifelse(adsl$RACEcat == "Total", " ", "Race"),
                          levels = c(" ", "Race")
)

adsl$RACEcat <- factor(adsl$RACEcat, levels = c("Total", "White", "Black", "Asian", "Other"))

# Add total for Race - adae
totalrace2 <- adae %>%
  filter(RACE != "UNKNOWN" & !is.na(RACE)) %>%
  mutate(RACE = "Total")

adae <- bind_rows(totalrace2, adae)

adae <- adae %>%
  mutate(RACEcat = case_when(
    RACE == "Total" ~ "Total",
    RACE == "WHITE" ~ "White",
    RACE == "BLACK OR AFRICAN AMERICAN" ~ "Black",
    RACE == "ASIAN" ~ "Asian",
    RACE != "UNKNOWN" & !is.na(RACE) ~ "Other"
  )) %>%
  filter(RACEcat %in% c("Total", "White", "Black", "Asian", "Other")) %>%
  select(-RACE)

adae$RACEcat <- factor(adae$RACEcat, levels = c("Total", "White", "Black", "Asian", "Other"))

# join data together
ae <- left_join(adsl, adae, by = c("USUBJID", "RACEcat"))

################################################################################
# Define layout and build table:
################################################################################

extra_args_1 <- list(denom = "n_altdf",
                     .stats = c("count_unique_fraction")
)


extra_args_2 <- list(denom = "n_altdf",
                     .stats = c("count_unique")
)



lyt <- basic_table(
  top_level_section_div = " ",
  show_colcounts = FALSE
) %>%
  split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map))

lyt <- lyt %>%
  split_cols_by(trtvar)

lyt <- lyt %>%
  split_cols_by("spanheader", split_fun = trim_levels_in_group("RACEcat")) %>%
  split_cols_by("RACEcat") %>%
  analyze(popfl,
          afun = a_freq_j,
          show_labels = "hidden",
          section_div = c(" "),
          extra_args = append(extra_args_2,
                              list(
                                label = "Analysis set: Safety",
                                val = "Y",
                                section_div = c(" ")))
  ) %>%
  analyze("TRTEMFL",
          afun = a_freq_j,
          show_labels = "hidden",
          extra_args = append(extra_args_1,
                              list(label = "Subjects with >=1 AE",
                                   val = "Y",
                                   section_div = c(" ")))
  ) %>%
  split_rows_by("AEBODSYS",
                split_label = "System Organ Class",
                split_fun = trim_levels_in_group("AEDECOD"),
                label_pos = "topleft",
                section_div = c(" "),
                nested = FALSE
  ) %>%
  summarize_row_groups("AEBODSYS",
                       cfun = a_freq_j,
                       extra_args = extra_args_1
  ) %>%
  analyze("AEDECOD",
          afun = a_freq_j,
          extra_args = extra_args_1
  ) %>%
  append_topleft("  Preferred Term, n (%)")

result <- build_table(lyt, ae, alt_counts_df = adsl)
result <- set_titles(result, titles)
#########################################################################################
# Post-Processing step to sort by descending count on chosen active treatment columns.
# For this table we can use a defined colpath so it takes the appropriate sub-column ("Total")
# for the last active treatment group/combined and use this for its sort order.
# If you only have 1 active treatment arm, consider using jj_complex_scorefun(spanningheadercolvar = NA, usefirstcol = TRUE)
# See function documentation for jj_complex_scorefun should your require a different sorting behavior.
#########################################################################################

# col_paths_summary(result)

if (length(adae$TRTEMFL) != 0) {
  result <- sort_at_path(result, c("root", "AEBODSYS"),
                         scorefun = jj_complex_scorefun(colpath = c("colspan_trt", "Active Study Agent", trtvar, "Xanomeline High Dose", "spanheader", " ", "RACEcat", "Total"))
  )
  result <- sort_at_path(result, c("root", "AEBODSYS", "*", "AEDECOD"),
                         scorefun = jj_complex_scorefun(colpath = c("colspan_trt", "Active Study Agent", trtvar, "Xanomeline High Dose", "spanheader", " ", "RACEcat", "Total"))
  )
}
```

### Table .rtf File

By default, the
[`junco::tt_to_tlgrtf`](https://johnsonandjohnson.github.io/junco/reference/tt_to_tlgrtf.md)
function determines the appropriate place to break the columns so that
the created files contain an appropriate number of columns that contain
readable data. The default logic might not be ideal, as sometimes
columns that make sense to be grouped together to appear within the same
file, are not. This can be seen in the example call below, where the
race information for the “Placebo” treatment group is split across
files.

``` r
################################################################################
# Convert to tbl file and output table
################################################################################

tt_to_tlgrtf(string_map = string_map, tt = result,
             file = paste0(out_dir, "/aetablemultipledocs1"), orientation = "landscape"
)
#> [[1]]
#> NULL
#> 
#> [[2]]
#> NULL
```

### Visual Output (HTML Representation)

| aetablemultipledocs1part1of2:                             |                      |           |           |           |            |                     |           |           |           |
|-----------------------------------------------------------|----------------------|-----------|-----------|-----------|------------|---------------------|-----------|-----------|-----------|
|                                                           | Active Study Agent   |           |           |           |            |                     |           |           |           |
|                                                           | Xanomeline High Dose |           |           |           |            | Xanomeline Low Dose |           |           |           |
| System Organ Class                                        |                      | Race      |           |           |            |                     | Race      |           |           |
| Preferred Term, n (%)                                     | Total                | White     | Black     | Asian     | Other      | Total               | White     | Black     | Asian     |
| Analysis set: Safety                                      | 47                   | 8         | 3         | 10        | 26         | 68                  | 8         | 11        | 9         |
|                                                           |                      |           |           |           |            |                     |           |           |           |
| Subjects with ≥1 AE                                       | 32 (68.1%)           | 7 (87.5%) | 1 (33.3%) | 8 (80.0%) | 16 (61.5%) | 26 (38.2%)          | 4 (50.0%) | 4 (36.4%) | 4 (44.4%) |
|                                                           |                      |           |           |           |            |                     |           |           |           |
| Skin and subcutaneous tissue disorders                    | 32 (68.1%)           | 7 (87.5%) | 1 (33.3%) | 8 (80.0%) | 16 (61.5%) | 26 (38.2%)          | 4 (50.0%) | 4 (36.4%) | 4 (44.4%) |
| PRURITUS                                                  | 18 (38.3%)           | 3 (37.5%) | 1 (33.3%) | 4 (40.0%) | 10 (38.5%) | 14 (20.6%)          | 3 (37.5%) | 1 (9.1%)  | 2 (22.2%) |
| ERYTHEMA                                                  | 10 (21.3%)           | 3 (37.5%) | 1 (33.3%) | 0         | 6 (23.1%)  | 8 (11.8%)           | 1 (12.5%) | 1 (9.1%)  | 0         |
| HYPERHIDROSIS                                             | 7 (14.9%)            | 2 (25.0%) | 0         | 3 (30.0%) | 2 (7.7%)   | 3 (4.4%)            | 0         | 0         | 2 (22.2%) |
| RASH                                                      | 5 (10.6%)            | 0         | 0         | 2 (20.0%) | 3 (11.5%)  | 8 (11.8%)           | 3 (37.5%) | 2 (18.2%) | 1 (11.1%) |
| SKIN IRRITATION                                           | 4 (8.5%)             | 1 (12.5%) | 0         | 2 (20.0%) | 1 (3.8%)   | 4 (5.9%)            | 0         | 0         | 1 (11.1%) |
| RASH PRURITIC                                             | 2 (4.3%)             | 1 (12.5%) | 0         | 0         | 1 (3.8%)   | 1 (1.5%)            | 0         | 1 (9.1%)  | 0         |
| ACTINIC KERATOSIS                                         | 1 (2.1%)             | 0         | 0         | 0         | 1 (3.8%)   | 0                   | 0         | 0         | 0         |
| BLISTER                                                   | 1 (2.1%)             | 0         | 0         | 0         | 1 (3.8%)   | 2 (2.9%)            | 0         | 0         | 0         |
| PRURITUS GENERALISED                                      | 1 (2.1%)             | 0         | 0         | 1 (10.0%) | 0          | 0                   | 0         | 0         | 0         |
| RASH MACULO-PAPULAR                                       | 1 (2.1%)             | 0         | 0         | 0         | 1 (3.8%)   | 0                   | 0         | 0         | 0         |
| SKIN ODOUR ABNORMAL                                       | 1 (2.1%)             | 1 (12.5%) | 0         | 0         | 0          | 0                   | 0         | 0         | 0         |
| URTICARIA                                                 | 1 (2.1%)             | 0         | 0         | 0         | 1 (3.8%)   | 1 (1.5%)            | 1 (12.5%) | 0         | 0         |
| ALOPECIA                                                  | 0                    | 0         | 0         | 0         | 0          | 0                   | 0         | 0         | 0         |
| DRUG ERUPTION                                             | 0                    | 0         | 0         | 0         | 0          | 0                   | 0         | 0         | 0         |
| RASH ERYTHEMATOUS                                         | 0                    | 0         | 0         | 0         | 0          | 1 (1.5%)            | 0         | 0         | 0         |
| SKIN ULCER                                                | 0                    | 0         | 0         | 0         | 0          | 0                   | 0         | 0         | 0         |
|                                                           |                      |           |           |           |            |                     |           |           |           |
| Dummy Note: On-treatment is defined as treatment-emergent |                      |           |           |           |            |                     |           |           |           |

  
  

| aetablemultipledocs1part2of2:                             |                     |            |           |           |           |           |
|-----------------------------------------------------------|---------------------|------------|-----------|-----------|-----------|-----------|
|                                                           | Active Study Agent  |            |           |           |           |           |
|                                                           | Xanomeline Low Dose | Placebo    |           |           |           |           |
| System Organ Class                                        | Race                |            | Race      |           |           |           |
| Preferred Term, n (%)                                     | Other               | Total      | White     | Black     | Asian     | Other     |
| Analysis set: Safety                                      | 40                  | 51         | 7         | 7         | 5         | 32        |
|                                                           |                     |            |           |           |           |           |
| Subjects with ≥1 AE                                       | 14 (35.0%)          | 12 (23.5%) | 2 (28.6%) | 2 (28.6%) | 1 (20.0%) | 7 (21.9%) |
|                                                           |                     |            |           |           |           |           |
| Skin and subcutaneous tissue disorders                    | 14 (35.0%)          | 12 (23.5%) | 2 (28.6%) | 2 (28.6%) | 1 (20.0%) | 7 (21.9%) |
| PRURITUS                                                  | 8 (20.0%)           | 4 (7.8%)   | 1 (14.3%) | 1 (14.3%) | 0         | 2 (6.2%)  |
| ERYTHEMA                                                  | 6 (15.0%)           | 4 (7.8%)   | 0         | 2 (28.6%) | 0         | 2 (6.2%)  |
| HYPERHIDROSIS                                             | 1 (2.5%)            | 0          | 0         | 0         | 0         | 0         |
| RASH                                                      | 2 (5.0%)            | 3 (5.9%)   | 1 (14.3%) | 0         | 1 (20.0%) | 1 (3.1%)  |
| SKIN IRRITATION                                           | 3 (7.5%)            | 3 (5.9%)   | 0         | 1 (14.3%) | 0         | 2 (6.2%)  |
| RASH PRURITIC                                             | 0                   | 0          | 0         | 0         | 0         | 0         |
| ACTINIC KERATOSIS                                         | 0                   | 0          | 0         | 0         | 0         | 0         |
| BLISTER                                                   | 2 (5.0%)            | 0          | 0         | 0         | 0         | 0         |
| PRURITUS GENERALISED                                      | 0                   | 0          | 0         | 0         | 0         | 0         |
| RASH MACULO-PAPULAR                                       | 0                   | 0          | 0         | 0         | 0         | 0         |
| SKIN ODOUR ABNORMAL                                       | 0                   | 0          | 0         | 0         | 0         | 0         |
| URTICARIA                                                 | 0                   | 0          | 0         | 0         | 0         | 0         |
| ALOPECIA                                                  | 0                   | 1 (2.0%)   | 0         | 0         | 0         | 1 (3.1%)  |
| DRUG ERUPTION                                             | 0                   | 1 (2.0%)   | 0         | 0         | 1 (20.0%) | 0         |
| RASH ERYTHEMATOUS                                         | 1 (2.5%)            | 0          | 0         | 0         | 0         | 0         |
| SKIN ULCER                                                | 0                   | 1 (2.0%)   | 0         | 0         | 0         | 1 (3.1%)  |
|                                                           |                     |            |           |           |           |           |
| Dummy Note: On-treatment is defined as treatment-emergent |                     |            |           |           |           |           |

  
  

When calling the
[`junco::tt_to_tlgrtf`](https://johnsonandjohnson.github.io/junco/reference/tt_to_tlgrtf.md)
function, the `nosplitin` argument can be included, and the user can
specify a variable in which the associated columns should be grouped
together in the same file. Now, all the sub columns for the “Placebo”
treatment group (e.g. each unique TRT01A value), are kept together
within the same file.

``` r
################################################################################
# Convert to tbl file and output table
################################################################################

tt_to_tlgrtf(string_map = string_map, tt = result,
             file = paste0(out_dir, "/aetablemultipledocs2"), orientation = "landscape",
             nosplitin = list(cols = c(trtvar))
)
#> [[1]]
#> NULL
#> 
#> [[2]]
#> NULL
#> 
#> [[3]]
#> NULL
```

### Visual Output (HTML Representation)

[TABLE]

  
  

[TABLE]

  
  

| aetablemultipledocs2part3of3:                             |            |           |           |           |           |
|-----------------------------------------------------------|------------|-----------|-----------|-----------|-----------|
|                                                           |            |           |           |           |           |
|                                                           | Placebo    |           |           |           |           |
| System Organ Class                                        |            | Race      |           |           |           |
| Preferred Term, n (%)                                     | Total      | White     | Black     | Asian     | Other     |
| Analysis set: Safety                                      | 51         | 7         | 7         | 5         | 32        |
|                                                           |            |           |           |           |           |
| Subjects with ≥1 AE                                       | 12 (23.5%) | 2 (28.6%) | 2 (28.6%) | 1 (20.0%) | 7 (21.9%) |
|                                                           |            |           |           |           |           |
| Skin and subcutaneous tissue disorders                    | 12 (23.5%) | 2 (28.6%) | 2 (28.6%) | 1 (20.0%) | 7 (21.9%) |
| PRURITUS                                                  | 4 (7.8%)   | 1 (14.3%) | 1 (14.3%) | 0         | 2 (6.2%)  |
| ERYTHEMA                                                  | 4 (7.8%)   | 0         | 2 (28.6%) | 0         | 2 (6.2%)  |
| HYPERHIDROSIS                                             | 0          | 0         | 0         | 0         | 0         |
| RASH                                                      | 3 (5.9%)   | 1 (14.3%) | 0         | 1 (20.0%) | 1 (3.1%)  |
| SKIN IRRITATION                                           | 3 (5.9%)   | 0         | 1 (14.3%) | 0         | 2 (6.2%)  |
| RASH PRURITIC                                             | 0          | 0         | 0         | 0         | 0         |
| ACTINIC KERATOSIS                                         | 0          | 0         | 0         | 0         | 0         |
| BLISTER                                                   | 0          | 0         | 0         | 0         | 0         |
| PRURITUS GENERALISED                                      | 0          | 0         | 0         | 0         | 0         |
| RASH MACULO-PAPULAR                                       | 0          | 0         | 0         | 0         | 0         |
| SKIN ODOUR ABNORMAL                                       | 0          | 0         | 0         | 0         | 0         |
| URTICARIA                                                 | 0          | 0         | 0         | 0         | 0         |
| ALOPECIA                                                  | 1 (2.0%)   | 0         | 0         | 0         | 1 (3.1%)  |
| DRUG ERUPTION                                             | 1 (2.0%)   | 0         | 0         | 1 (20.0%) | 0         |
| RASH ERYTHEMATOUS                                         | 0          | 0         | 0         | 0         | 0         |
| SKIN ULCER                                                | 1 (2.0%)   | 0         | 0         | 0         | 1 (3.1%)  |
|                                                           |            |           |           |           |           |
| Dummy Note: On-treatment is defined as treatment-emergent |            |           |           |           |           |

  
  

If there is a need to provide a single .rtf file that contains the
multiple “part” .rtf files, then the
[`junco::tt_to_tlgrtf`](https://johnsonandjohnson.github.io/junco/reference/tt_to_tlgrtf.md)
function can be called with the “combined_rtf” argument included with a
value of “TRUE”. In this case, both the multiple “part” .rtf files and
the single .rtf file with all “parts” appended will be created.

``` r
################################################################################
# Convert to tbl file and output table
################################################################################

tt_to_tlgrtf(string_map = string_map, tt = result,
             file = paste0(out_dir, "/aetablemultipledocs3"), orientation = "landscape",
             nosplitin = list(cols = c(trtvar)),
             combined_rtf = TRUE
)
#> [[1]]
#> NULL
#> 
#> [[2]]
#> NULL
#> 
#> [[3]]
#> NULL
```

### Visual Output (HTML Representation)

[TABLE]

  
  

[TABLE]

  
  

| aetablemultipledocs3part3of3:                             |            |           |           |           |           |
|-----------------------------------------------------------|------------|-----------|-----------|-----------|-----------|
|                                                           |            |           |           |           |           |
|                                                           | Placebo    |           |           |           |           |
| System Organ Class                                        |            | Race      |           |           |           |
| Preferred Term, n (%)                                     | Total      | White     | Black     | Asian     | Other     |
| Analysis set: Safety                                      | 51         | 7         | 7         | 5         | 32        |
|                                                           |            |           |           |           |           |
| Subjects with ≥1 AE                                       | 12 (23.5%) | 2 (28.6%) | 2 (28.6%) | 1 (20.0%) | 7 (21.9%) |
|                                                           |            |           |           |           |           |
| Skin and subcutaneous tissue disorders                    | 12 (23.5%) | 2 (28.6%) | 2 (28.6%) | 1 (20.0%) | 7 (21.9%) |
| PRURITUS                                                  | 4 (7.8%)   | 1 (14.3%) | 1 (14.3%) | 0         | 2 (6.2%)  |
| ERYTHEMA                                                  | 4 (7.8%)   | 0         | 2 (28.6%) | 0         | 2 (6.2%)  |
| HYPERHIDROSIS                                             | 0          | 0         | 0         | 0         | 0         |
| RASH                                                      | 3 (5.9%)   | 1 (14.3%) | 0         | 1 (20.0%) | 1 (3.1%)  |
| SKIN IRRITATION                                           | 3 (5.9%)   | 0         | 1 (14.3%) | 0         | 2 (6.2%)  |
| RASH PRURITIC                                             | 0          | 0         | 0         | 0         | 0         |
| ACTINIC KERATOSIS                                         | 0          | 0         | 0         | 0         | 0         |
| BLISTER                                                   | 0          | 0         | 0         | 0         | 0         |
| PRURITUS GENERALISED                                      | 0          | 0         | 0         | 0         | 0         |
| RASH MACULO-PAPULAR                                       | 0          | 0         | 0         | 0         | 0         |
| SKIN ODOUR ABNORMAL                                       | 0          | 0         | 0         | 0         | 0         |
| URTICARIA                                                 | 0          | 0         | 0         | 0         | 0         |
| ALOPECIA                                                  | 1 (2.0%)   | 0         | 0         | 0         | 1 (3.1%)  |
| DRUG ERUPTION                                             | 1 (2.0%)   | 0         | 0         | 1 (20.0%) | 0         |
| RASH ERYTHEMATOUS                                         | 0          | 0         | 0         | 0         | 0         |
| SKIN ULCER                                                | 1 (2.0%)   | 0         | 0         | 0         | 1 (3.1%)  |
|                                                           |            |           |           |           |           |
| Dummy Note: On-treatment is defined as treatment-emergent |            |           |           |           |           |

  
  

### Table .docx File

## Inserting Page Breaks in Tables

It is sometimes necessary to insert page breaks into tables, to indicate
that a new grouping of rows is occurring. This can be achieved by
including `rtables::split_rows_by within` the layout definition and
specifying the `page_by` argument with a value of “TRUE”. In the
following example, the table is being split by the SEX variable. When
specifying `page_by` = “TRUE”, each new observed value of SEX (and the
subsequent rows generated form the
[`rtables::analyze`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html)
function call) will be separated by a page break.

``` r
library(junco)

adsl <- data.frame(TRT01A = factor(c("Example Drug 5 mg",
                                     "Example Drug 10 mg",
                                     "Example Drug 20 mg",
                                     "Placebo",
                                     "Example Drug 5 mg",
                                     "Example Drug 10 mg",
                                     "Example Drug 20 mg",
                                     "Placebo"),
                                   levels = c("Example Drug 5 mg",
                                              "Example Drug 10 mg",
                                              "Example Drug 20 mg",
                                              "Placebo"
                                              )
                              ),
                   USUBJID = c("1",
                               "2",
                               "3",
                               "4",
                               "5",
                               "6",
                               "7",
                               "8"),
                   RESPONSE = factor(c("Yes",
                                       "No",
                                       "Yes",
                                       "Yes",
                                       "Yes",
                                       "No",
                                       "Yes",
                                       "No"),
                                     levels = c("Yes",
                                                "No")
                              ),
                   SEX = factor(c("Male",
                                  "Female",
                                  "Male",
                                  "Female",
                                  "Female",
                                  "Male",
                                  "Female",
                                  "Male"),
                                levels = c("Male",
                                           "Female")
                           )
) |>
  var_relabel(RESPONSE = "Response")

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) |>
  ### first columns
  split_cols_by("TRT01A",
                show_colcounts = FALSE
  ) |>
  ### create table body row sections based on SEX
  split_rows_by("SEX",
                section_div = c(" "),
                page_by = TRUE,
                split_fun = drop_split_levels
                ) |>
  summarize_row_groups("SEX",
                       cfun =a_freq_j,
                       extra_args = list(denom = "n_df",
                                         denom_by = "SEX",
                                         riskdiff = FALSE,
                                         extrablankline = TRUE,
                                         .stats = c("count_unique")
                                         )
                       ) |>
  ### analyze height
  analyze("RESPONSE",
          var_labels = c("Response"),
          show_labels = "visible",
          afun = a_freq_j,
          extra_args = list(denom = "n_df",
                            denom_by = "SEX",
                            riskdiff = FALSE,
                            .stats = c("count_unique_fraction")
                            )
          )

result <- build_table(lyt, adsl)
result <- set_titles(result, titles)
tt_to_tlgrtf(result,
             file = paste0(out_dir, "/examplepagebreak"))
#> [[1]]
#> NULL
```

### Visual Output (HTML Representation)

> **Note:** HTML output notoriously does not handle well page break, so
> here it is represented in two parts to show the point.

| examplepagebreak:   |                   |                    |                    |            |
|---------------------|-------------------|--------------------|--------------------|------------|
|                     | Example Drug 5 mg | Example Drug 10 mg | Example Drug 20 mg | Placebo    |
|                     | N=2               | N=2                | N=2                | N=2        |
| Male                | 1                 | 1                  | 1                  | 1          |
|                     |                   |                    |                    |            |
| Response            |                   |                    |                    |            |
| Yes                 | 1 (100.0%)        | 0                  | 1 (100.0%)         | 0          |
| No                  | 0                 | 1 (100.0%)         | 0                  | 1 (100.0%) |

  
  

| examplepagebreak:                                         |                   |                    |                    |            |
|-----------------------------------------------------------|-------------------|--------------------|--------------------|------------|
|                                                           | Example Drug 5 mg | Example Drug 10 mg | Example Drug 20 mg | Placebo    |
|                                                           | N=2               | N=2                | N=2                | N=2        |
| Female                                                    | 1                 | 1                  | 1                  | 1          |
|                                                           |                   |                    |                    |            |
| Response                                                  |                   |                    |                    |            |
| Yes                                                       | 1 (100.0%)        | 0                  | 1 (100.0%)         | 1 (100.0%) |
| No                                                        | 0                 | 1 (100.0%)         | 0                  | 0          |
|                                                           |                   |                    |                    |            |
| Dummy Note: On-treatment is defined as treatment-emergent |                   |                    |                    |            |

## Manually Splitting Large Listing Files into Multiple Smaller Files

In rare cases, the listing files produced are large in size (e.g. \>20
MB). There can be multiple reasons why this occurs, but a common example
is that there is a need to present longitudinal data for a large trial.
In this scenario, it might be beneficial to create multiple smaller
files instead of a single large file. This can be achieved by subsetting
the data and then calling the necessary exporter function multiple
times.

### Listing .rtf File

The
[`junco::tt_to_tlgrtf`](https://johnsonandjohnson.github.io/junco/reference/tt_to_tlgrtf.md)
function can be called multiple times to produce multiple .rtf files.

``` r
library(junco)
library(rlistings)

adsl <- data.frame(TRT01A = c("Example Drug 5 mg",
                              "Example Drug 10 mg",
                              "Example Drug 20 mg",
                              "Placebo"),
                   SUBJECT = c("1",
                               "2",
                               "3",
                               "4"),
                   HEIGHT = c(70,
                              74,
                              60,
                              64
                   )
) |>
  formatters::var_relabel(TRT01A = "Actual Treatment for Period 01~[super a]") |>
  formatters::var_relabel(SUBJECT = "Subject") |>
  formatters::var_relabel(HEIGHT = "Height (in)")

result <- rlistings::as_listing(
  df = adsl,
  key_cols = c("TRT01A", "SUBJECT"),
  disp_cols = "HEIGHT"
)
result <- set_titles(result, titles)
keep <- result$TRT01A %in% c("Example Drug 5 mg", "Example Drug 10 mg")

result1 <- result[keep, ]

tt_to_tlgrtf(result1,
  file = paste0(out_dir, "/examplelistingmultiplefilesPART1OF2"),
  orientation = "landscape"
)
```

### Listing .docx File

The
[`junco::export_as_docx_j`](https://johnsonandjohnson.github.io/junco/reference/export_as_docx_j.md)
function can be called multiple times to produce multiple .docx files,
similar to the approach used for .rtf files:

``` r

export_as_docx_j(result1,
  tblid = "examplelistingmultiplefilesPART1OF2",
  output_dir = out_dir,
  orientation = "landscape"
)
```

### Visual Output (HTML Representation)

| examplelistingmultiplefilesPART1OF2:                      |         |             |
|-----------------------------------------------------------|---------|-------------|
| Actual Treatment for Period 01a                           | Subject | Height (in) |
| Example Drug 10 mg                                        | 2       | 74          |
| Example Drug 5 mg                                         | 1       | 70          |
|                                                           |         |             |
| Dummy Note: On-treatment is defined as treatment-emergent |         |             |

### Listing .rtf File 2

``` r

keep <- result$TRT01A %in% c("Example Drug 20 mg", "Placebo")

result2 <- result[keep, ]

tt_to_tlgrtf(result2,
  file = paste0(out_dir, "/examplelistingmultiplefilesPART2OF2"),
  orientation = "landscape"
)
```

### Listing .docx File 2

``` r

export_as_docx_j(result2,
  tblid = "examplelistingmultiplefilesPART2OF2",
  output_dir = out_dir,
  orientation = "landscape"
)
```

### Visual Output (HTML Representation)

| examplelistingmultiplefilesPART2OF2:                      |         |             |
|-----------------------------------------------------------|---------|-------------|
| Actual Treatment for Period 01a                           | Subject | Height (in) |
| Example Drug 20 mg                                        | 3       | 60          |
| Placebo                                                   | 4       | 64          |
|                                                           |         |             |
| Dummy Note: On-treatment is defined as treatment-emergent |         |             |
