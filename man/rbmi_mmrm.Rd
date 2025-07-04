% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mmrm_rbmi.R
\name{rbmi_mmrm}
\alias{rbmi_mmrm}
\title{MMRM Analysis for Imputed Datasets}
\usage{
rbmi_mmrm(
  data,
  vars,
  cov_struct = c("us", "toep", "cs", "ar1"),
  visits = NULL,
  weights = c("counterfactual", "equal"),
  ...
)
}
\arguments{
\item{data}{(`data.frame`)\cr containing the data to be used in the model.}

\item{vars}{(`vars`)\cr list as generated by [set_vars()]. Only the `subjid`, `group`,
`visit`, `outcome` and `covariates` elements are required. See details.}

\item{cov_struct}{(`string`)\cr the covariance structure to use. Note that the same
covariance structure is assumed for all treatment groups.}

\item{visits}{(`NULL` or `character`)\cr An optional character vector specifying
which visits to fit the MMRM at. If `NULL`, the MMRM model will be fit to
the whole dataset.}

\item{weights}{(`string`)\cr the weighting strategy to be used when calculating the
least square means, either `'counterfactual'` or `'equal'`.}

\item{...}{additional arguments passed to [mmrm::mmrm()], in particular
`method` and `vcov` to control the degrees of freedom and variance-covariance
adjustment methods as well as `reml` decide between REML and ML estimation.}
}
\description{
Performs an MMRM for two or more groups returning the estimated
'treatment effect' (i.e. the contrast between treatment groups and the control
group) and the least square means estimates in each group.
}
\details{
The function works as follows:

1. Optionally select the subset of the `data` corresponding to `visits.
2. Fit an MMRM as `vars$outcome ~ vars$group + vars$visit + vars$covariates`
   with the specified covariance structure for visits within subjects.
3. Extract the 'treatment effect' & least square means for each treatment group
   vs the control group.

In order to meet the formatting standards set by [analyse()] the results will be collapsed
into a single list suffixed by the visit name, e.g.:
```
list(
   var_B_visit_1 = list(est = ...),
   trt_B_visit_1 = list(est = ...),
   lsm_A_visit_1 = list(est = ...),
   lsm_B_visit_1 = list(est = ...),
   var_B_visit_2 = list(est = ...),
   trt_B_visit_2 = list(est = ...),
   lsm_A_visit_2 = list(est = ...),
   lsm_B_visit_2 = list(est = ...),
   ...
)
```
Please note that 'trt' refers to the treatment effects, and 'lsm' refers to the least
square mean results. In the above example `vars$group` has two factor levels A and B.
The new 'var' refers to the model estimated variance of the residuals at the given
visit, together with the degrees of freedom (which is treatment group specific).

If you want to include additional interaction terms in your model this can be done
by providing them to the `covariates` argument of [set_vars()]
e.g. `set_vars(covariates = c('sex*age'))`.
}
\note{
The `group` and `visit` interaction `group:visit` is not included by
default in the model, therefore please add that to `covariates` manually if
you want to include it. This will make sense in most cases.
}
\seealso{
[rbmi_analyse()]

[mmrm::mmrm()]

[set_vars()]
}
