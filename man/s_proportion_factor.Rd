% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/proportions.R
\name{s_proportion_factor}
\alias{s_proportion_factor}
\title{s_function for proportion of factor levels}
\usage{
s_proportion_factor(
  x,
  .alt_df,
  use_alt_counts = TRUE,
  show_total = c("none", "top", "bottom"),
  total_label = "Total"
)
}
\arguments{
\item{x}{(`factor`)\cr categorical variable we want to analyze.}

\item{.alt_df}{(`data.frame`)\cr alternative data frame used for denominator calculation.}

\item{use_alt_counts}{(`flag`)\cr whether the `.alt_df` should be used for the total, i.e. the denominator.
If not, then the number of non-missing values in `x` is used.}

\item{show_total}{(`string`)\cr show the total level optionally on the top or in the bottom
of the factor levels.}

\item{total_label}{(`string`)\cr which label to use for the optional total level.}
}
\value{
The [rtables::in_rows()] result with the proportion statistics.
}
\description{
A simple statistics function which prepares the numbers with percentages
  in the required format. The denominator here is from the alternative counts data set
  in the given row and column split.

  If a total row is shown, then here just the total number is shown (without 100%).
}
\seealso{
[s_proportion_logical()] for tabulating logical `x`.
}
