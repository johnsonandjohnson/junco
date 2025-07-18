% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tern_utils_default_stats_formats_labels.R
\docType{data}
\name{tern_default_stats_formats_labels}
\alias{tern_default_stats_formats_labels}
\alias{tern_get_stats}
\alias{tern_get_formats_from_stats}
\alias{tern_get_labels_from_stats}
\alias{tern_get_indents_from_stats}
\alias{tern_default_labels}
\title{Get default statistical methods and their associated formats, labels, and indent modifiers}
\format{
An object of class \code{character} of length 40.
}
\usage{
tern_get_stats(
  method_groups = "analyze_vars_numeric",
  stats_in = NULL,
  custom_stats_in = NULL,
  add_pval = FALSE,
  tern_defaults = tern_default_stats
)

tern_get_formats_from_stats(
  stats,
  formats_in = NULL,
  levels_per_stats = NULL,
  tern_defaults = tern_default_formats
)

tern_get_labels_from_stats(
  stats,
  labels_in = NULL,
  levels_per_stats = NULL,
  label_attr_from_stats = NULL,
  tern_defaults = tern_default_labels
)

tern_get_indents_from_stats(
  stats,
  indents_in = NULL,
  levels_per_stats = NULL,
  tern_defaults = stats::setNames(as.list(rep(0L, length(stats))), stats)
)

tern_default_labels
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
}
\section{Functions}{
\itemize{
\item \code{tern_get_stats()}: Get statistics available for a given method
group (analyze function).

\item \code{tern_get_formats_from_stats()}: Get formats corresponding to a list of statistics.

\item \code{tern_get_labels_from_stats()}: Get labels corresponding to a list of statistics.

\item \code{tern_get_indents_from_stats()}: Get row indent modifiers corresponding to a list of statistics/rows.

\item \code{tern_default_labels}: Named \code{character} vector of default labels for \code{tern}.
This is only copied here from the latest GitHub version, because otherwise a tern test fails.

}}
\note{
These functions have been copied from the \code{tern} package file
\code{utils_default_stats_formats_labels.R} from GitHub development version 0.9.7.9017.
Slight modifications have been applied to enhance functionality:
\itemize{
\item \code{tern_get_stats} added the \code{tern_stats} argument to avoid hardcoding within the function's body.
\item \code{tern_get_labels_from_stats} is more careful when receiving partial \code{labels_in}
and partial \code{label_attr_from_stats}.
}

Once these features are included in the \code{tern} package, this file could be removed from
the \code{junco} package, and the functions could be used from the \code{tern} namespace directly.
}
\keyword{internal}
