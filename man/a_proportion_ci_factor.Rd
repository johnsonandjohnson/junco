% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/proportions.R
\name{a_proportion_ci_factor}
\alias{a_proportion_ci_factor}
\title{Formatted Analysis Function For Proportion Confidence Interval for Factor}
\usage{
a_proportion_ci_factor(df, .var, ...)
}
\arguments{
\item{df}{(\code{data.frame})\cr including factor \code{.var}.}

\item{.var}{(\code{string})\cr name of the factor variable.}

\item{\dots}{see \code{\link[=a_proportion_ci_logical]{a_proportion_ci_logical()}} for additionally required
arguments.}
}
\value{
The \code{\link[rtables:rcell]{rtables::rcell()}} result.
}
\description{
Formatted Analysis Function For Proportion Confidence Interval for Factor
}
\examples{
a_proportion_ci_factor(
  df = DM,
  .var = "SEX",
  .alt_df = DM,
  conf_level = 0.95,
  formats = list(prop_ci = jjcsformat_xx("xx.x\%, xx.x\%")),
  method = "clopper-pearson"
)
}
