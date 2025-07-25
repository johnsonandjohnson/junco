% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/span_var_map.R
\name{create_colspan_var}
\alias{create_colspan_var}
\title{Creation of Column Spanning Variables}
\usage{
create_colspan_var(
  df,
  non_active_grp = c("Placebo"),
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = "TRT01A"
)
}
\arguments{
\item{df}{The name of the data frame in which the spanning variable is to be appended to}

\item{non_active_grp}{The value(s) of the treatments that represent the non-active or comparator
treatment groups
default value = c('Placebo')}

\item{non_active_grp_span_lbl}{The assigned value of the spanning variable for the non-active or comparator
treatment groups
default value = ''}

\item{active_grp_span_lbl}{The assigned value of the spanning variable for the active treatment group(s)
default value = 'Active Study Agent'}

\item{colspan_var}{The desired name of the newly created spanning variable
default value = 'colspan_trt'}

\item{trt_var}{The name of the treatment variable that is used to determine which
spanning treatment group value to apply.
default value = 'TRT01A'}
}
\value{
a data frame that contains the new variable as specified in colspan_var
}
\description{
A function used for creating a spanning variable for treatment groups
}
\details{
This function creates a spanning variable for treatment groups that is intended to
be used within the column space.
}
\examples{

library(tibble)

df <- tribble(
  ~TRT01A,
  "Placebo",
  "Active 1",
  "Active 2"
)

df$TRT01A <- factor(df$TRT01A, levels = c("Placebo", "Active 1", "Active 2"))

colspan_var <- create_colspan_var(
  df = df,
  non_active_grp = c("Placebo"),
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Treatment",
  colspan_var = "colspan_trt",
  trt_var = "TRT01A"
)

colspan_var
}
