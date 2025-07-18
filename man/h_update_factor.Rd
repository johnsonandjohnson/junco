% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/h_freq_funs.R
\name{h_update_factor}
\alias{h_update_factor}
\title{Update Factor}
\usage{
h_update_factor(df, .var, val = NULL, excl_levels = NULL)
}
\arguments{
\item{df}{Data frame containing the variable to update.}

\item{.var}{Variable name to update.}

\item{val}{Values to keep.}

\item{excl_levels}{Levels to exclude from the factor.}
}
\value{
Updated data frame.
}
\description{
Updates a factor variable in a data frame based on specified values.
}
