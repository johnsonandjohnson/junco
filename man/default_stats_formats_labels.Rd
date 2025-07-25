% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/junco_utils_default_stats_formats_labels.R
\docType{data}
\name{default_stats_formats_labels}
\alias{default_stats_formats_labels}
\alias{junco_get_stats}
\alias{junco_get_formats_from_stats}
\alias{junco_get_labels_from_stats}
\alias{get_label_attr_from_stats}
\alias{junco_get_indents_from_stats}
\alias{format_stats}
\alias{junco_default_stats}
\alias{junco_default_formats}
\alias{junco_default_labels}
\alias{junco_default_indents}
\title{Get default statistical methods and their associated formats, labels, and indent modifiers}
\format{
\itemize{
\item \code{junco_default_stats} is a named list of available statistics, with each element
named for their corresponding statistical method group.
}

\itemize{
\item \code{junco_default_formats} is a named vector of available default formats, with each element
named for their corresponding statistic.
}

\itemize{
\item \code{junco_default_labels} is a named \code{character} vector of available default labels, with each element
named for their corresponding statistic.
}

\itemize{
\item \code{junco_default_indents} is a named \code{integer} vector of available default indents, with each element
named for their corresponding statistic. Only indentations different from zero need to be
recorded here.
}
}
\usage{
junco_get_stats(
  method_groups = "analyze_vars_numeric",
  stats_in = NULL,
  custom_stats_in = NULL,
  add_pval = FALSE
)

junco_get_formats_from_stats(stats, formats_in = NULL, levels_per_stats = NULL)

junco_get_labels_from_stats(
  stats,
  labels_in = NULL,
  levels_per_stats = NULL,
  label_attr_from_stats = NULL
)

get_label_attr_from_stats(x_stats)

junco_get_indents_from_stats(stats, indents_in = NULL, levels_per_stats = NULL)

format_stats(
  x_stats,
  method_groups,
  stats_in,
  formats_in,
  labels_in,
  indents_in
)

junco_default_stats

junco_default_formats

junco_default_labels

junco_default_indents
}
\arguments{
\item{method_groups}{(\code{character})\cr indicates the statistical method group (\code{junco} analyze function)
to retrieve default statistics for. A character vector can be used to specify more than one statistical
method group.}

\item{stats_in}{(\code{character})\cr statistics to retrieve for the selected method group. If custom statistical
functions are used, \code{stats_in} needs to have them in too.}

\item{custom_stats_in}{(\code{character})\cr custom statistics to add to the default statistics.}

\item{add_pval}{(\code{flag})\cr should \code{'pval'} (or \code{'pval_counts'} if \code{method_groups} contains
\code{'analyze_vars_counts'}) be added to the statistical methods?}

\item{stats}{(\code{character})\cr statistical methods to return defaults for.}

\item{formats_in}{(named \code{vector})\cr custom formats to use instead of defaults. Can be a character vector with
values from \code{\link[formatters:list_formats]{formatters::list_valid_format_labels()}} or custom format functions. Defaults to \code{NULL} for any rows
with no value is provided.}

\item{levels_per_stats}{(named \code{list} of \code{character} or \code{NULL})\cr named list where the name of each element is a
statistic from \code{stats} and each element is the levels of a \code{factor} or \code{character} variable (or variable name),
each corresponding to a single row, for which the named statistic should be calculated for. If a statistic is only
calculated once (one row), the element can be either \code{NULL} or the name of the statistic. Each list element will be
flattened such that the names of the list elements returned by the function have the format \code{statistic.level} (or
just \code{statistic} for statistics calculated for a single row). Defaults to \code{NULL}.}

\item{labels_in}{(named \code{character})\cr custom labels to use instead of defaults. If no value is provided, the
variable level (if rows correspond to levels of a variable) or statistic name will be used as label.}

\item{label_attr_from_stats}{(named \code{list})\cr if \code{labels_in = NULL}, then this will be used instead. It is a list
of values defined in statistical functions as default labels. Values are ignored if \code{labels_in} is provided or \code{''}
values are provided.}

\item{x_stats}{(\code{list})\cr with the statistics results.}

\item{indents_in}{(named \code{integer})\cr custom row indent modifiers to use instead of defaults. Defaults to \code{0L} for
all values.}
}
\value{
\itemize{
\item \code{junco_get_stats()} returns a \code{character} vector of statistical methods.
}

\itemize{
\item \code{junco_get_formats_from_stats()} returns a named list of formats as strings or functions.
}

\itemize{
\item \code{junco_get_labels_from_stats()} returns a named list of labels as strings.
}

\itemize{
\item \code{junco_get_indents_from_stats()} returns a named list of indentation modifiers as integers. By default all of the
indentations will be zero.
}

\itemize{
\item \code{format_stats()} returns the correspondingly formatted \code{\link[rtables:in_rows]{rtables::in_rows()}} result.
}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

Utility functions to get valid statistic methods for different method groups
(\code{.stats}) and their associated formats (\code{.formats}), labels (\code{.labels}), and indent modifiers
(\code{.indent_mods}). This utility is used across \code{junco}, but some of its working principles can be
seen in \code{\link[tern:analyze_variables]{tern::analyze_vars()}}. See notes to understand why this is experimental.
}
\details{
Current choices for \code{type} are \code{counts} and \code{numeric} for \code{\link[tern:analyze_variables]{tern::analyze_vars()}} and affect \code{junco_get_stats()}.
}
\section{Functions}{
\itemize{
\item \code{junco_get_stats()}: Get statistics available for a given method
group (analyze function). To check available defaults see \code{junco_default_stats} list.

\item \code{junco_get_formats_from_stats()}: Get formats corresponding to a list of statistics.
To check available defaults see list \code{junco_default_formats}.

\item \code{junco_get_labels_from_stats()}: Get labels corresponding to a list of statistics.
To check for available defaults see list \code{junco_default_labels}.

\item \code{get_label_attr_from_stats()}: Get label attributes from statistics list.

\item \code{junco_get_indents_from_stats()}: Get row indent modifiers corresponding to a list of statistics/rows.

\item \code{format_stats()}: Format statistics results according to format specifications.

\item \code{junco_default_stats}: Named list of available statistics by method group for \code{junco}.

\item \code{junco_default_formats}: Named vector of default formats for \code{junco}.

\item \code{junco_default_labels}: Named \code{character} vector of default labels for \code{junco}.

\item \code{junco_default_indents}: Named \code{integer} vector of default indents for \code{junco}.

}}
\note{
These defaults are experimental because we use the names of functions to retrieve the default
statistics. This should be generalized in groups of methods according to more reasonable groupings.

These functions have been modified from the \code{tern} file \code{utils_default_stats_formats_labels.R}.
This file contains \code{junco} specific wrappers of functions called within the \code{afun} functions,
in order to point to \code{junco} specific default statistics, formats and labels.

Formats in \code{tern} or \code{junco} and \code{rtables} can be functions that take in the table cell value and
return a string. This is well documented in \code{vignette('custom_appearance', package = 'rtables')}.
}
\keyword{datasets}
\keyword{internal}
