% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyze_values.R
\name{analyze_values}
\alias{analyze_values}
\title{Shortcut Layout Function for Standard Continuous Variable Analysis}
\usage{
analyze_values(lyt, vars, ..., formats)
}
\arguments{
\item{lyt}{(`layout`)\cr input layout where analyses will be added to.}

\item{vars}{(`character`)\cr variable names for the primary analysis variable to be iterated over.}

\item{...}{additional arguments for the lower level functions.}

\item{formats}{(`list`)\cr formats including `mean_sd`, `median` and `range`
specifications.}
}
\value{
Modified layout.
}
\description{
Shortcut Layout Function for Standard Continuous Variable Analysis
}
\note{
This is used in `tefmad01` and `tefmad03a` e.g.
}
