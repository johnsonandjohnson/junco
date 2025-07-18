% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarize_mmrm.R
\name{h_summarize_mmrm}
\alias{h_summarize_mmrm}
\title{Helper Function to Fit the MMRM and Return LS Mean Estimates and Contrasts}
\usage{
h_summarize_mmrm(
  .var,
  df_parent,
  variables,
  ref_arm_level,
  ref_visit_levels,
  ...
)
}
\arguments{
\item{.var}{(\code{string})\cr single variable name that is passed by \code{rtables} when requested
by a statistics function.}

\item{df_parent}{(\code{data.frame})\cr data set containing all analysis variables
from all visits and arms.}

\item{variables}{(named \code{list} of \code{string})\cr list of additional analysis variables.}

\item{ref_arm_level}{(\code{string})\cr the reference arm which should be compared
against.}

\item{ref_visit_levels}{(\code{character})\cr the reference visits which should not
be included in the model fit.}

\item{...}{additional options passed to \code{\link[=fit_mmrm_j]{fit_mmrm_j()}}.}
}
\value{
The resulting estimates and contrasts LS means as returned by
\code{\link[=tidy.tern_model]{tidy.tern_model()}}.
}
\description{
Helper Function to Fit the MMRM and Return LS Mean Estimates and Contrasts
}
\keyword{internal}
