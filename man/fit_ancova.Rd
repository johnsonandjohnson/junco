% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ancova.R
\name{fit_ancova}
\alias{fit_ancova}
\title{`ANCOVA` Analysis}
\usage{
fit_ancova(
  vars = list(response = "AVAL", covariates = c(), arm = "ARM", visit = "AVISIT", id =
    "USUBJID"),
  data,
  conf_level = 0.95,
  weights_emmeans = "proportional"
)
}
\arguments{
\item{vars}{(named `list` of `string` or `character`)\cr specifying the variables in the `ANCOVA` analysis.
  The following elements need to be included as character vectors and match corresponding columns
  in `data`:

  - `response`: the response variable.
  - `covariates`: the additional covariate terms (might also include interactions).
  - `id`: the subject ID variable (not really needed for the computations but for internal logistics).
  - `arm`: the treatment group variable (factor).
  - `visit`: the visit variable (factor).

  Note that the `arm` variable is by default included in the model, thus should not be part of `covariates`.}

\item{data}{(`data.frame`)\cr with all the variables specified in
`vars`. Records with missing values in any independent variables
will be excluded.}

\item{conf_level}{(`proportion`)\cr confidence level of the interval.}

\item{weights_emmeans}{(`string`)\cr argument from [emmeans::emmeans()], `'counterfactual'` by default.}
}
\value{
A `tern_model` object which is a list with model results:

  - `fit`: A list with a fitted [stats::lm()] result for each visit.
  - `mse`: Mean squared error, i.e. variance estimate, for each visit.
  - `df`: Degrees of freedom for the variance estimate for each visit.
  - `lsmeans`: This is a list with data frames `estimates` and `contrasts`.
       The attribute `weights` savse the settings used (`weights_emmeans`).
  - `vars`: The variable list.
  - `labels`: Corresponding list with variable labels extracted from `data`.
  - `ref_level`: The reference level for the arm variable, which is always the first level.
  - `treatment_levels`: The treatment levels for the arm variable.
  - `conf_level`: The confidence level which was used to construct the `lsmeans` confidence intervals.
}
\description{
Does the `ANCOVA` analysis, separately for each visit.
}
\examples{
library(mmrm)

fit <- fit_ancova(
  vars = list(
    response = 'FEV1',
    covariates = c('RACE', 'SEX'),
    arm = 'ARMCD',
    id = 'USUBJID',
    visit = 'AVISIT'
  ),
  data = fev_data,
  conf_level = 0.9,
  weights_emmeans = 'equal'
)

}
