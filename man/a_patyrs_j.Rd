% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/a_eair_j.R
\name{a_patyrs_j}
\alias{a_patyrs_j}
\alias{s_patyrs_j}
\title{Patient years exposure}
\usage{
s_patyrs_j(
  df,
  .var,
  id = "USUBJID",
  .alt_df_full,
  source = c("alt_df", "df"),
  inriskdiffcol = FALSE
)

a_patyrs_j(
  df,
  .var,
  .df_row,
  id = "USUBJID",
  .alt_df_full = NULL,
  .formats = NULL,
  .labels = NULL,
  source = c("alt_df", "df"),
  .spl_context,
  .stats = "patyrs"
)
}
\arguments{
\item{df}{(\code{data.frame})\cr data set containing all analysis variables.}

\item{.var}{(\code{string})\cr variable name containing the patient years data.}

\item{id}{(\code{string})\cr subject variable name.}

\item{.alt_df_full}{(\code{dataframe})\cr alternative dataset for calculations.}

\item{source}{(\code{string})\cr source of data, either "alt_df" or "df".}

\item{inriskdiffcol}{(\code{logical})\cr flag indicating if the function is called within a risk difference column.}

\item{.df_row}{(\code{data.frame})\cr data frame across all of the columns for the given row split.}

\item{.formats}{(named 'character' or 'list')\cr formats for the statistics.}

\item{.labels}{(named 'character')\cr labels for the statistics.}

\item{.spl_context}{(\code{data.frame})\cr gives information about ancestor split states.}

\item{.stats}{(\code{character})\cr statistics to select for the table.}
}
\value{
\itemize{
\item \code{s_patyrs_j()} return x a list containing the patient years statistics.
The list of available statistics for can be viewed by running \code{junco_get_stats("a_patyrs_j")},
currently this is just a single statistic \code{patyrs}, patient years of exposure.
}

\itemize{
\item \code{a_patyrs_j} returns the corresponding list with formatted \code{\link[rtables:CellValue]{rtables::CellValue()}}.
}
}
\description{
Statistical/Analysis Function for presenting Patient years exposure summary data
}
\section{Functions}{
\itemize{
\item \code{s_patyrs_j()}: Statistical Function for Patient years exposure summary data

\item \code{a_patyrs_j()}: Formatted analysis function for patient years summary which is used
as \code{afun} in \code{analyze} or \code{cfun} in \code{summarize_row_groups}.

}}
\examples{
library(tern)
library(dplyr)
trtvar <- "ARM"
ctrl_grp <- "B: Placebo"
cutoffd <- as.Date("2023-09-24")


adexsum <- ex_adsl \%>\%
  create_colspan_var(
    non_active_grp          = ctrl_grp,
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl     = "Active Study Agent",
    colspan_var             = "colspan_trt",
    trt_var                 = trtvar
  ) \%>\%
  mutate(
    rrisk_header = "Risk Difference (95\% CI)",
    rrisk_label = paste(!!rlang::sym(trtvar), "vs", ctrl_grp),
    TRTDURY = case_when(
      !is.na(EOSDY) ~ EOSDY,
      TRUE ~ as.integer(cutoffd - as.Date(TRTSDTM) + 1)
    )
  ) \%>\%
  select(USUBJID, !!rlang::sym(trtvar), colspan_trt, rrisk_header, rrisk_label, TRTDURY)

adae <- ex_adae \%>\%
  group_by(USUBJID, AEDECOD) \%>\%
  select(USUBJID, AEDECOD, ASTDY) \%>\%
  mutate(rwnum = row_number()) \%>\%
  mutate(AOCCPFL = case_when(
    rwnum == 1 ~ "Y",
    TRUE ~ NA
  )) \%>\%
  filter(AOCCPFL == "Y")

aefup <- left_join(adae, adexsum, by = "USUBJID")

colspan_trt_map <- create_colspan_map(adexsum,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

ref_path <- c("colspan_trt", " ", trtvar, ctrl_grp)

lyt <- basic_table(show_colcounts = TRUE, colcount_format = "N=xx", top_level_section_div = " ") \%>\%
  split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map)) \%>\%
  split_cols_by(trtvar) \%>\%
  split_cols_by("rrisk_header", nested = FALSE) \%>\%
  split_cols_by(trtvar, labels_var = "rrisk_label", split_fun = remove_split_levels(ctrl_grp)) \%>\%
  analyze("TRTDURY",
    nested = FALSE,
    show_labels = "hidden",
    afun = a_patyrs_j
  )
result <- build_table(lyt, aefup, alt_counts_df = adexsum)
result

}
\keyword{internal}
