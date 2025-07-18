% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/h_freq_funs.R
\name{h_colexpr_substr}
\alias{h_colexpr_substr}
\title{Extract Substring from Column Expression}
\usage{
h_colexpr_substr(var, col_expr)
}
\arguments{
\item{var}{Variable to extract from the expression.}

\item{col_expr}{Column expression string.}
}
\value{
Substring corresponding to the variable.
}
\description{
Retrieves the substring from a column expression related to a variable component.
}
\details{
get substring from col_expr related to var component
intended usage is on strings coming from .spl_context$cur_col_expr
these strings are of type '!(is.na(var) & var \%in\% 'xxx') & !(is.na(var2) & var2 \%in\% 'xxx')'
}
