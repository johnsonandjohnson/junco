% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resp01_functions.R
\name{resp01_acfun}
\alias{resp01_acfun}
\title{Formatted Analysis and Content Summary Function for Response Tables (RESP01)}
\usage{
resp01_acfun(
  df,
  labelstr = NULL,
  label = NULL,
  .var,
  .spl_context,
  include_comp,
  .alt_df,
  conf_level,
  arm,
  strata,
  formats,
  methods
)
}
\arguments{
\item{df}{(\code{data.frame})\cr data set containing all analysis variables.}

\item{labelstr}{(\code{character})\cr label of the level of the parent split currently being summarized
(must be present as second argument in Content Row Functions). See \code{\link[rtables:summarize_row_groups]{rtables::summarize_row_groups()}}
for more information.}

\item{label}{(\code{string})\cr only for logicals, which label to use. (For factors, the
labels are the factor levels.)}

\item{.var}{(\code{string})\cr single variable name that is passed by \code{rtables} when requested
by a statistics function.}

\item{.spl_context}{(\code{data.frame})\cr gives information about ancestor split states
that is passed by \code{rtables}.}

\item{include_comp}{(\code{character} or \code{flag})\cr whether to include comparative
statistic results, either \code{character} for factors or \code{flag} for logicals.}

\item{.alt_df}{(\code{data.frame})\cr alternative data frame used for denominator calculation.}

\item{conf_level}{(\code{proportion})\cr confidence level of the interval.}

\item{arm}{(\code{string})\cr column name in the data frame that identifies the treatment arms.}

\item{strata}{(\code{character} or \code{NULL})\cr variable names indicating stratification factors.}

\item{formats}{(\code{list})\cr containing formats for \code{prop_ci}, \code{comp_stat_ci}
and \code{pval}.}

\item{methods}{(\code{list})\cr containing methods for comparative statistics. The element \code{comp_stat_ci} can be
'rr' (relative risk), 'or_cmh' (odds ratio with CMH estimation and p-value) or 'or_logistic' (odds ratio
estimated by conditional or standard logistic regression). The element \code{pval} can be 'fisher' (Fisher's
exact test) or 'chisq' (chi-square test), only used when using unstratified analyses with 'or_logistic'.
The element \code{prop_ci} specifies the method for proportion confidence interval calculation.}
}
\value{
The formatted result as \code{\link[rtables:in_rows]{rtables::in_rows()}} result.
}
\description{
This function applies to both \code{factor} and \code{logical} columns called
\code{.var} from \code{df}. Depending on the position in the split, it returns the
right formatted results for the RESP01 and related layouts.
}
\examples{
fake_spl_context <- data.frame(
  cur_col_split_val = I(list(c(ARM = "A: Drug X", count_prop = "count_prop")))
)
dm <- droplevels(subset(DM, SEX \%in\% c("F", "M")))
resp01_acfun(
  dm,
  .alt_df = dm,
  .var = "COUNTRY",
  .spl_context = fake_spl_context,
  conf_level = 0.9,
  include_comp = c("USA", "CHN"),
  arm = "SEX",
  strata = "RACE",
  methods = list(
    comp_stat_ci = "or_cmh",
    pval = "",
    prop_ci = "wald"
  ),
  formats = list(
    prop_ci = jjcsformat_xx("xx.\% - xx.\%"),
    comp_stat_ci = jjcsformat_xx("xx.xx (xx.xx - xx.xx)"),
    pval = jjcsformat_pval_fct(0.05)
  )
)
fake_spl_context2 <- data.frame(
  cur_col_split_val = I(list(c(ARM = "Overall", comp_stat_ci = "comp_stat_ci")))
)
resp01_acfun(
  dm,
  .alt_df = dm,
  .var = "COUNTRY",
  .spl_context = fake_spl_context2,
  conf_level = 0.9,
  include_comp = c("USA", "CHN"),
  arm = "SEX",
  strata = "RACE",
  methods = list(
    comp_stat_ci = "or_cmh",
    pval = "",
    prop_ci = "wald"
  ),
  formats = list(
    prop_ci = jjcsformat_xx("xx.\% - xx.\%"),
    comp_stat_ci = jjcsformat_xx("xx.xx (xx.xx - xx.xx)"),
    pval = jjcsformat_pval_fct(0.05)
  )
)
}
