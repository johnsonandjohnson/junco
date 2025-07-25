% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/odds_ratio.R
\name{h_odds_ratio}
\alias{h_odds_ratio}
\alias{or_glm_j}
\alias{or_clogit_j}
\alias{or_cmh}
\title{Helper functions for odds ratio estimation}
\usage{
or_glm_j(data, conf_level)

or_clogit_j(data, conf_level, method = "exact")

or_cmh(data, conf_level)
}
\arguments{
\item{data}{(\code{data.frame})\cr data frame containing at least the variables \code{rsp} and \code{grp}, and optionally
\code{strata} for \code{\link[=or_clogit_j]{or_clogit_j()}}.}

\item{conf_level}{(\code{numeric})\cr confidence level for the confidence interval.}

\item{method}{(\code{string})\cr whether to use the correct (\code{'exact'}) calculation in the conditional likelihood or one
of the approximations, or the CMH method. See \code{\link[survival:clogit]{survival::clogit()}} for details.}
}
\value{
A named \code{list} of elements \code{or_ci}, \code{n_tot} and \code{pval}.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Functions to calculate odds ratios in \code{\link[=s_odds_ratio_j]{s_odds_ratio_j()}}.
}
\section{Functions}{
\itemize{
\item \code{or_glm_j()}: Estimates the odds ratio based on \code{\link[stats:glm]{stats::glm()}}. Note that there must be
exactly 2 groups in \code{data} as specified by the \code{grp} variable.

\item \code{or_clogit_j()}: Estimates the odds ratio based on \code{\link[survival:clogit]{survival::clogit()}}. This is done for
the whole data set including all groups, since the results are not the same as when doing
pairwise comparisons between the groups.

\item \code{or_cmh()}: Estimates the odds ratio based on CMH. Note that there must be
exactly 2 groups in \code{data} as specified by the \code{grp} variable.

}}
\examples{
data <- data.frame(
  rsp = as.logical(c(1, 1, 0, 1, 0, 0, 1, 1)),
  grp = letters[c(1, 1, 1, 2, 2, 2, 1, 2)],
  strata = letters[c(1, 2, 1, 2, 2, 2, 1, 2)],
  stringsAsFactors = TRUE
)

or_glm_j(data, conf_level = 0.95)

data <- data.frame(
  rsp = as.logical(c(1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0)),
  grp = letters[c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3)],
  strata = LETTERS[c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)],
  stringsAsFactors = TRUE
)

or_clogit_j(data, conf_level = 0.95)

set.seed(123)
data <- data.frame(
  rsp = as.logical(rbinom(n = 40, size = 1, prob = 0.5)),
  grp = letters[sample(1:2, size = 40, replace = TRUE)],
  strata = LETTERS[sample(1:2, size = 40, replace = TRUE)],
  stringsAsFactors = TRUE
)

or_cmh(data, conf_level = 0.95)

}
\seealso{
\link{odds_ratio}
}
