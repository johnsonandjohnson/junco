% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/a_freq_j.R
\name{a_freq_j}
\alias{a_freq_j}
\alias{s_freq_j}
\title{Analysis/statistical function for count and percentage in core columns
+ (optional) relative risk columns}
\usage{
s_freq_j(
  df,
  .var,
  .df_row,
  val = NULL,
  drop_levels = FALSE,
  excl_levels = NULL,
  alt_df,
  parent_df,
  id = "USUBJID",
  denom = c("n_df", "n_altdf", "N_col", "n_rowdf", "n_parentdf"),
  .N_col,
  countsource = c("df", "altdf")
)

a_freq_j(
  df,
  labelstr = NULL,
  .var = NA,
  val = NULL,
  drop_levels = FALSE,
  excl_levels = NULL,
  new_levels = NULL,
  new_levels_after = FALSE,
  addstr2levs = NULL,
  .df_row,
  .spl_context,
  .N_col,
  id = "USUBJID",
  denom = c("N_col", "n_df", "n_altdf", "N_colgroup", "n_rowdf", "n_parentdf"),
  riskdiff = TRUE,
  ref_path = NULL,
  variables = list(strata = NULL),
  conf_level = 0.95,
  method = c("wald", "waldcc", "cmh", "ha", "newcombe", "newcombecc", "strat_newcombe",
    "strat_newcombecc"),
  weights_method = "cmh",
  label = NULL,
  label_fstr = NULL,
  label_map = NULL,
  .alt_df_full = NULL,
  denom_by = NULL,
  .stats = c("count_unique_denom_fraction"),
  .formats = NULL,
  .indent_mods = NULL,
  na_str = rep(NA, 3),
  .labels_n = NULL,
  extrablankline = FALSE,
  extrablanklineafter = NULL,
  restr_columns = NULL,
  colgroup = NULL,
  countsource = c("df", "altdf")
)
}
\arguments{
\item{df}{(`data.frame`)\cr data set containing all analysis variables.}

\item{.var}{(`string`)\cr single variable name that is passed by `rtables` when requested
by a statistics function.}

\item{.df_row}{(`data.frame`)\cr data frame across all of the columns for the given row split.}

\item{val}{(`character` or NULL)\cr
When NULL, all levels of the incoming variable (variable used in the `analyze` call)
will be considered.\cr
When a single `string`, only that current level/value of the incoming variable
will be considered.\cr
When multiple levels, only those levels/values of the incoming variable
will be considered.\cr
When no values are observed (eg zero row input df),
a row with row-label `No data reported` will be included in the table.}

\item{drop_levels}{(`logical`)\cr If `TRUE` non-observed levels
(based upon .df_row) will not be included.\cr
Cannot be used together with `val`.}

\item{excl_levels}{(`character` or NULL)\cr
When NULL, no levels of the incoming variable (variable used in the `analyze` call)
will be excluded.\cr
When multiple levels, those levels/values of the incoming variable
will be excluded.\cr
Cannot be used together with `val`.}

\item{alt_df}{(`dataframe`)\cr Will be derived based upon alt_df_full and denom_by within a_freq_j.}

\item{parent_df}{(`dataframe`)\cr Will be derived within a_freq_j based
upon the input dataframe that goes into build_table (df) and denom_by.\cr
It is a data frame in the higher row-space than the current input df
(which underwent row-splitting by the rtables splitting machinery).}

\item{id}{(`string`)\cr subject variable name.}

\item{denom}{(`string`)\cr
One of \cr
\itemize{
\item \strong{N_col} Column count, \cr
\item \strong{n_df} Number of patients (based upon the main input dataframe `df`),\cr
\item \strong{n_altdf} Number of patients from the secondary dataframe (`.alt_df_full`),\cr
Note that argument `denom_by` will perform a row-split on the `.alt_df_full` dataframe.\cr
It is a requirement that variables specified in `denom_by` are part of the row split specifications. \cr
\item \strong{N_colgroup} Number of patients from the column group variable
(note that this is based upon the input .alt_df_full dataframe).\cr
Note that the argument `colgroup` (column variable) needs to be provided,
as it cannot be retrieved directly from the column layout definition.
\item \strong{n_rowdf} Number of patients from the current row-level dataframe
(`.row_df` from the rtables splitting machinery).\cr
\item \strong{n_parentdf} Number of patients from a higher row-level split than the current split.\cr
This higher row-level split is specified in the argument `denom_by`.\cr
}}

\item{.N_col}{(`integer`)\cr column-wise N (column count) for the full column being analyzed that is typically
passed by `rtables`.}

\item{countsource}{Either `df` or `alt_df`.\cr
When `alt_df` the counts will be based upon the alternative dataframe `alt_df`.\cr
This is useful for subgroup processing,
to present counts of subjects in a subgroup from the alternative dataframe.}

\item{labelstr}{An argument to ensure this function can be used
as a `cfun` in a `summarize_row_groups` call.\cr
It is recommended not to utilize this argument for other purposes.\cr
The label argument could be used instead (if `val` is a single string)\cr
An another approach could be to utilize the `label_map` argument
to control the row labels of the incoming analysis variable.}

\item{new_levels}{(list(2) or NULL)\cr List of length 2.\cr
First element : names of the new levels\cr
Second element: list with values of the new levels.\cr}

\item{new_levels_after}{(`logical`)\cr If `TRUE` new levels will be added after last level.}

\item{addstr2levs}{string, if not NULL will be appended to the rowlabel for that level,
eg to add ",n (percent)" at the end of the rowlabels}

\item{.spl_context}{(`data.frame`)\cr gives information about ancestor split states
that is passed by `rtables`.}

\item{riskdiff}{(`logical`)\cr
When `TRUE`, risk difference calculations will be performed and
presented (if required risk difference column splits are included).\cr
When `FALSE`, risk difference columns will remain blank
(if required risk difference column splits are included).}

\item{ref_path}{(`string`)\cr Column path specifications for
the control group for the relative risk derivation.}

\item{variables}{Will be passed onto the relative risk function
(internal function s_rel_risk_val_j), which is based upon [tern::s_proportion_diff()].\cr
See `?tern::s_proportion_diff` for details.}

\item{conf_level}{(`proportion`)\cr confidence level of the interval.}

\item{method}{Will be passed onto the relative risk function (internal function s_rel_risk_val_j).\cr}

\item{weights_method}{Will be passed onto the relative risk function (internal function s_rel_risk_val_j).\cr}

\item{label}{(`string`)\cr
When `val`is a single `string`,
the row label to be shown on the output can be specified using this argument.\cr
When `val` is a `character vector`, the `label_map` argument can be specified
to control the row-labels.}

\item{label_fstr}{(`string`)\cr
a sprintf style format string.
It can contain up to one "\%s" which takes the current split value and
generates the row/column label.\cr
It will be combined with the `labelstr` argument,
when utilizing this function as
a `cfun` in a `summarize_row_groups` call.\cr
It is recommended not to utilize this argument for other purposes.
The label argument could be used instead (if `val` is a single string)\cr}

\item{label_map}{(`tibble`)\cr
A mapping tibble to translate levels from the incoming variable into
a different row label to be presented on the table.\cr}

\item{.alt_df_full}{(`dataframe`)\cr Denominator dataset
for fraction and relative risk calculations.\cr
.alt_df_full is a crucial parameter for the relative risk calculations
if this parameter is not set to utilize `alt_counts_df`,
then the values in the relative risk columns might not be correct.\cr
Once the rtables PR is integrated, this argument gets populated by the rtables
split machinery (see [rtables::additional_fun_params]).}

\item{denom_by}{(`character`)\cr Variables from row-split
to be used in the denominator derivation.\cr
This controls both `denom = "n_parentdf"` and `denom = "n_altdf"`.\cr
When `denom = "n_altdf"`, the denominator is derived from `.alt_df_full`
in combination with `denom_by` argument}

\item{.stats}{(`character`)\cr statistics to select for the table.
See Value for list of available statistics.}

\item{.formats}{(named 'character' or 'list')\cr
formats for the statistics.}

\item{.indent_mods}{(named `integer`)\cr indent modifiers for the labels. Defaults to 0, which corresponds to the
unmodified default behavior. Can be negative.}

\item{na_str}{(`string`)\cr string used to replace all `NA` or empty values in the output.}

\item{.labels_n}{(named `character`)\cr
String to control row labels for the 'n'-statistics.\cr
Only useful when more than one 'n'-statistic is requested (rare situations only).}

\item{extrablankline}{(`logical`)\cr
When `TRUE`, an extra blank line will be added after the last value.\cr
Avoid using this in template scripts, use section_div = " " instead (once PR for rtables is available)\cr}

\item{extrablanklineafter}{(`string`)\cr
When the row-label matches the string, an extra blank line will be added after
that value.}

\item{restr_columns}{`character`\cr
If not NULL, columns not defined in `restr_columns` will be blanked out.}

\item{colgroup}{The name of the column group variable that is used as source
for denominator calculation.\cr
Required to be specified when `denom = "N_colgroup"`.}
}
\value{
* `s_freq_j`: returns a list of following statistics\cr
\itemize{
\item n_df
\item n_rowdf
\item n_parentdf
\item n_altdf
\item denom
\item count
\item count_unique
\item count_unique_fraction
\item count_unique_denom_fraction
}

* `a_freq_j`: returns a list of requested statistics with formatted `rtables::CellValue()`.\cr
Within the relative risk difference columns, the following stats are blanked out:
\itemize{
\item any of the n-statistics (n_df, n_altdf, n_parentdf, n_rowdf, denom)
\item count
\item count_unique
}
For the others (count_unique_fraction, count_unique_denom_fraction),
the statistic is replaced by the relative risk difference + confidence interval.
}
\description{
Analysis/statistical function for count and percentage in core columns
+ (optional) relative risk columns
}
\examples{
library(dplyr)

adsl <- ex_adsl |> select("USUBJID", "SEX", "ARM")
adae <- ex_adae |> select("USUBJID", "AEBODSYS", "AEDECOD")
adae[["TRTEMFL"]] <- "Y"

trtvar <- "ARM"
ctrl_grp <- "B: Placebo"
adsl$colspan_trt <- factor(ifelse(adsl[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

adsl$rrisk_header <- "Risk Difference (\%) (95\% CI)"
adsl$rrisk_label <- paste(adsl[[trtvar]], paste("vs", ctrl_grp))

# join data together
adae <- adae |> left_join(adsl)

colspan_trt_map <- create_colspan_map(adsl,
  non_active_grp = "B: Placebo",
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

ref_path <- c("colspan_trt", " ", trtvar, ctrl_grp)

lyt <- basic_table(show_colcounts = TRUE) |>
  split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map)) |>
  split_cols_by(trtvar) |>
  split_cols_by("rrisk_header", nested = FALSE) |>
  split_cols_by(trtvar, labels_var = "rrisk_label", split_fun = remove_split_levels(ctrl_grp))

lyt1 <- lyt |>
  analyze("TRTEMFL",
    show_labels = "hidden",
    afun = a_freq_j,
    extra_args = list(
      method = "wald",
      .stats = c("count_unique_denom_fraction"),
      ref_path = ref_path
    )
  )

result1 <- build_table(lyt1, adae, alt_counts_df = adsl)

result1

# quick check for risk difference results using tern function stat_propdiff_ci
# For Drug X vs Placebo
x_drug_x <- list(length(unique(subset(adae, adae[[trtvar]] == "A: Drug X")[["USUBJID"]])))
N_x_drug_x <- length(unique(subset(adsl, adsl[[trtvar]] == "A: Drug X")[["USUBJID"]]))
y_placebo <- list(length(unique(subset(adae, adae[[trtvar]] == ctrl_grp)[["USUBJID"]])))
N_y_placebo <- length(unique(subset(adsl, adsl[[trtvar]] == ctrl_grp)[["USUBJID"]]))

tern::stat_propdiff_ci(
  x = x_drug_x,
  N_x = N_x_drug_x,
  y = y_placebo,
  N_y = N_y_placebo
)

# For Combination vs Placebo
x_combo <- list(length(unique(subset(adae, adae[[trtvar]] == "C: Combination")[["USUBJID"]])))
N_x_combo <- length(unique(subset(adsl, adsl[[trtvar]] == "C: Combination")[["USUBJID"]]))

tern::stat_propdiff_ci(
  x = x_combo,
  N_x = N_x_combo,
  y = y_placebo,
  N_y = N_y_placebo
)


# example for subgroup AE table

extra_args_rr <- list(
  denom = "n_altdf",
  denom_by = "SEX",
  riskdiff = FALSE,
  .stats = c("count_unique")
)

extra_args_rr2 <- list(
  denom = "n_altdf",
  denom_by = "SEX",
  riskdiff = TRUE,
  ref_path = ref_path,
  method = "wald",
  .stats = c("count_unique_denom_fraction"),
  na_str = rep(NA, 3)
)

lyt2 <- basic_table(
  top_level_section_div = " ",
  colcount_format = "N=xx"
) |>
  ## main columns
  split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map)) |>
  split_cols_by(trtvar, show_colcounts = TRUE) |>
  ## risk diff columns, note nested = FALSE
  split_cols_by("rrisk_header", nested = FALSE) |>
  split_cols_by(trtvar,
    labels_var = "rrisk_label", split_fun = remove_split_levels("B: Placebo"),
    show_colcounts = FALSE
  ) |>
  split_rows_by("SEX", split_fun = drop_split_levels) |>
  summarize_row_groups("SEX",
    cfun = a_freq_j,
    extra_args = append(extra_args_rr, list(label_fstr = "Gender: \%s"))
  ) |>
  split_rows_by("TRTEMFL",
    split_fun = keep_split_levels("Y"),
    indent_mod = -1L,
    section_div = c(" ")
  ) |>
  summarize_row_groups("TRTEMFL",
    cfun = a_freq_j,
    extra_args = append(extra_args_rr2, list(
      label =
        "Subjects with >=1 AE", extrablankline = TRUE
    ))
  ) |>
  split_rows_by("AEBODSYS",
    split_label = "System Organ Class",
    split_fun = trim_levels_in_group("AEDECOD"),
    label_pos = "topleft",
    section_div = c(" "),
    nested = TRUE
  ) |>
  summarize_row_groups("AEBODSYS",
    cfun = a_freq_j,
    extra_args = extra_args_rr2
  ) |>
  analyze("AEDECOD",
    afun = a_freq_j,
    extra_args = extra_args_rr2
  )

result2 <- build_table(lyt2, adae, alt_counts_df = adsl)

}
