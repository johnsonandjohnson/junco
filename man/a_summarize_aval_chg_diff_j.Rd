% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/a_summarize_aval_chg_diff.R
\name{a_summarize_aval_chg_diff_j}
\alias{a_summarize_aval_chg_diff_j}
\title{Analysis function 3-column presentation}
\usage{
a_summarize_aval_chg_diff_j(
  df,
  .df_row,
  .spl_context,
  ancova = FALSE,
  comp_btw_group = TRUE,
  ref_path = NULL,
  .N_col,
  denom = c("N", ".N_col"),
  indatavar = NULL,
  d = 0,
  id = "USUBJID",
  interaction_y = FALSE,
  interaction_item = NULL,
  conf_level = 0.95,
  variables = list(arm = "TRT01A", covariates = NULL),
  format_na_str = "",
  .stats = list(col1 = "count_denom_frac", col23 = "mean_ci_3d", coldiff =
    "meandiff_ci_3d"),
  .formats = list(col1 = NULL, col23 = "xx.dx (xx.dx, xx.dx)", coldiff =
    "xx.dx (xx.dx, xx.dx)"),
  .formats_fun = list(col1 = jjcsformat_count_denom_fraction, col23 = jjcsformat_xx,
    coldiff = jjcsformat_xx),
  multivars = c("AVAL", "AVAL", "CHG")
)
}
\arguments{
\item{df}{(`data.frame`)\cr data set containing all analysis variables.}

\item{.df_row}{(`data.frame`)\cr data frame across all of the columns for the given row split.}

\item{.spl_context}{(`data.frame`)\cr gives information about ancestor split states
that is passed by `rtables`.}

\item{ancova}{(`logical`)\cr If FALSE, only descriptive methods will be used. \cr
If TRUE Ancova methods will be used for each of the columns : AVAL, CHG, DIFF. \cr}

\item{comp_btw_group}{(`logical`)\cr If TRUE,
\cr When ancova = FALSE, the estimate of between group difference (on CHG)
will be based upon a two-sample t-test. \cr
\cr When ancova = TRUE, the same ancova model will be used for the estimate of between group difference (on CHG).}

\item{ref_path}{(`character`)\cr global reference group specification, see [get_ref_info()].}

\item{.N_col}{(`integer`)\cr column-wise N (column count) for the full column being analyzed that is typically
passed by `rtables`.}

\item{denom}{(`string`)\cr choice of denominator for proportions. Options are:
  * `N`: number of records in this column/row split.
\cr There is no check in place that the current split only has one record per subject.
Users should be careful with this.
  * `.N_col`: number of records in this column intersection (based on alt_counts_df dataset)
  \cr (when alt_counts_df is a single record per subjects, this will match number of subjects)}

\item{indatavar}{(`string`)\cr If not null, variable name to extra subset
incoming df to non-missing values of this variable.}

\item{d}{(default = 1) \cr choice of Decimal precision.
Note that one extra precision will be added, as means are presented.
 \cr Options are:
  * numerical(1)
  * variable name containing information on the precision, this variable
  should be available on input dataset. The content of this variable should
  then be an integer.}

\item{id}{(`string`)\cr subject variable name.}

\item{interaction_y}{(`character`)\cr Will be passed onto the `tern` function `s_ancova`, when ancova = TRUE.}

\item{interaction_item}{(`character`)\cr Will be passed onto the `tern` function `s_ancova`, when ancova = TRUE.}

\item{conf_level}{(`proportion`)\cr Confidence level of the interval}

\item{variables}{(named list of strings)\cr
list of additional analysis variables, with expected elements:
   * arm (string)\cr
group variable, for which the covariate adjusted means of multiple groups will be summarized.
Specifically, the first level of arm variable is taken as the reference group.
   * covariates (character)\cr
a vector that can contain single variable names (such as 'X1'), and/or interaction terms indicated by 'X1 * X2'.}

\item{format_na_str}{(`string`)\cr}

\item{.stats}{(named `list`)\cr column statistics to select for the table.
The following column names are to be used: `col1`, `col23`, `coldiff`.\cr
For `col1`, the following stats can be specified.\cr
For `col23`, only `mean_ci_3d` is available. When ancova=`TRUE` these are LS Means, otherwise, arithmetic means.\cr
For `coldiff`, only `meandiff_ci_3d` is available. When ancova=`TRUE` these
are LS difference in means, otherwise, difference in means based upon 2-sample t-test.\cr}

\item{.formats}{(named `list`)\cr formats for the column statistics. `xx.d` style formats can be used.}

\item{.formats_fun}{(named `list`)\cr formatting functions for the column
statistics, to be applied after the conversion of `xx.d` style to the
appropriate precision.}

\item{multivars}{(`string(3)`)\cr Variables names to use in 3-col layout.}
}
\value{
A function that can be used in an analyze function call
}
\description{
Analysis functions to produce a 1-row summary presented in
a 3-column layout in the columns: column 1: N, column 2: Value, column 3: change\cr
In the difference columns, only 1 column will be presented : difference + CI\cr
When ancova = `TRUE`, the presented statistics will be based on ANCOVA method (`s_summarize_ancova_j`).\cr
mean and ci (both for Value (column 2) and Chg (column 3)) using statistic `lsmean_ci`\cr
mean and ci for the difference column are based on same ANCOVA model using statistic `lsmean_diffci`\cr
When ancova = `FALSE`, descriptive statistics will be used instead.\cr
In the difference column, the 2-sample t-test will be used.
}
\details{
See Description
}
\examples{

library(dplyr)

ADEG <- data.frame(
  STUDYID = c(
    'DUMMY', 'DUMMY', 'DUMMY', 'DUMMY', 'DUMMY',
    'DUMMY', 'DUMMY', 'DUMMY', 'DUMMY', 'DUMMY'
  ),
  USUBJID = c(
    'XXXXX01', 'XXXXX02', 'XXXXX03', 'XXXXX04', 'XXXXX05',
    'XXXXX06', 'XXXXX07', 'XXXXX08', 'XXXXX09', 'XXXXX10'
  ),
  TRT01A = c(
    'ARMA', 'ARMA', 'ARMA', 'ARMA', 'ARMA', 'Placebo',
    'Placebo', 'Placebo', 'ARMA', 'ARMA'
  ),
  PARAM = c('BP', 'BP', 'BP', 'BP', 'BP', 'BP', 'BP', 'BP', 'BP', 'BP'),
  AVISIT = c(
    'Visit 1', 'Visit 1', 'Visit 1', 'Visit 1', 'Visit 1',
    'Visit 1', 'Visit 1', 'Visit 1', 'Visit 1', 'Visit 1'
  ),
  AVAL = c(56, 78, 67, 87, 88, 93, 39, 87, 65, 55),
  CHG = c(2, 3, -1, 9, -2, 0, 6, -2, 5, 2)
)

ADEG <- ADEG |>
  mutate(
    TRT01A = as.factor(TRT01A),
    STUDYID = as.factor(STUDYID)
  )

ADEG$colspan_trt <- factor(ifelse(ADEG$TRT01A == 'Placebo', ' ', 'Active Study Agent'),
  levels = c('Active Study Agent', ' ')
)
ADEG$rrisk_header <- 'Risk Difference (\%) (95\% CI)'
ADEG$rrisk_label <- paste(ADEG$TRT01A, paste('vs', 'Placebo'))

colspan_trt_map <- create_colspan_map(ADEG,
  non_active_grp = 'Placebo',
  non_active_grp_span_lbl = ' ',
  active_grp_span_lbl = 'Active Study Agent',
  colspan_var = 'colspan_trt',
  trt_var = 'TRT01A'
)
ref_path <- c('colspan_trt', ' ', 'TRT01A', 'Placebo')

lyt <- basic_table() |>
  ### first columns
  split_cols_by(
    'colspan_trt',
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) |>
  split_cols_by('TRT01A') |>
  split_rows_by(
    'PARAM',
    label_pos = 'topleft',
    split_label = 'Blood Pressure',
    section_div = ' ',
    split_fun = drop_split_levels
  ) |>
  split_rows_by(
    'AVISIT',
    label_pos = 'topleft',
    split_label = 'Study Visit',
    split_fun = drop_split_levels,
    child_labels = 'hidden'
  ) |>
  ## set up a 3 column split
  split_cols_by_multivar(
    c('AVAL', 'AVAL', 'CHG'),
    varlabels = c('n/N (\%)', 'Mean (CI)', 'CFB (CI)')
  ) |>
  split_cols_by('rrisk_header', nested = FALSE) |>
  split_cols_by(
    'TRT01A',
    split_fun = remove_split_levels('Placebo'),
    labels_var = 'rrisk_label'
  ) |>
  ### difference columns : just 1 column & analysis needs to be done on change
  split_cols_by_multivar(c('CHG'), varlabels = c(' ')) |>
  # the variable passed here in analyze is not used (STUDYID),
  # it is a dummy var passing, the function a_summarize_aval_chg_diff_j
  # grabs the required vars from cols_by_multivar calls
  analyze('STUDYID',
    afun = a_summarize_aval_chg_diff_j,
    extra_args = list(
      format_na_str = '-', d = 0,
      ref_path = ref_path, variables = list(arm = 'TRT01A', covariates = NULL)
    )
  )

result <- build_table(lyt, ADEG)

result
}
\seealso{
s_summarize_ancova_j

Other Inclusion of ANCOVA Functions: 
\code{\link{a_summarize_ancova_j}()},
\code{\link{s_ancova_j}()}
}
\concept{Inclusion of ANCOVA Functions}
