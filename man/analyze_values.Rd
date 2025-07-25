% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyze_values.R
\name{analyze_values}
\alias{analyze_values}
\title{Shortcut Layout Function for Standard Continuous Variable Analysis}
\usage{
analyze_values(lyt, vars, ..., formats)
}
\arguments{
\item{lyt}{(\code{layout})\cr input layout where analyses will be added to.}

\item{vars}{(\code{character})\cr variable names for the primary analysis variable to be iterated over.}

\item{...}{additional arguments for the lower level functions.}

\item{formats}{(\code{list})\cr formats including \code{mean_sd}, \code{median} and \code{range}
specifications.}
}
\value{
Modified layout.
}
\description{
Shortcut Layout Function for Standard Continuous Variable Analysis
}
\note{
This is used in \code{tefmad01} and \code{tefmad03a} e.g.
}
