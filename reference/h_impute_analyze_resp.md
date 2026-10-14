# Impute missing binary responses and analyze each imputed data set

Performs independent Bernoulli imputations for missing binary responses
in the control and treatment groups, then calculates a CMH risk
difference and p-value for each imputed data set. Only observations in
`ctrlab` and `trtlab` are included in the analysis.

## Usage

``` r
h_impute_analyze_resp(
  dat,
  p_ctrl,
  p_trt,
  trtvar,
  ctrlab,
  trtlab,
  respvar,
  stratvar,
  n_imputations
)
```

## Arguments

- dat:

  (`data.frame`) Analysis data containing treatment, response, and
  stratification variables.

- p_ctrl:

  (`numeric(1)`) Probability of response used to impute missing
  control-group responses.

- p_trt:

  (`numeric(1)`) Probability of response used to impute missing
  treatment-group responses.

- trtvar:

  (`string`) Name of the treatment variable in `dat`.

- ctrlab:

  (`string`) Value of `trtvar` identifying the control group.

- trtlab:

  (`string`) Value of `trtvar` identifying the treatment group.

- respvar:

  (`string`) Name of a logical binary response variable in `dat`.
  Missing values are imputed; observed values are retained.

- stratvar:

  (`character`) Names of one or more stratification variables in `dat`.
  Stratification variables must not be missing in the analysis groups.

- n_imputations:

  (`count`) Number of independently imputed data sets to analyze.

## Value

A tibble with one row per imputation and the following columns:

- `rd_est`: CMH risk-difference estimate.

- `rd_se`: standard error of the risk-difference estimate.

- `rd_var`: variance of the risk-difference estimate.

- `p_cmh`: CMH p-value using the Wilson-Hilferty transformation.

- `z_stat`: Wilson-Hilferty z statistic used to calculate `p_cmh`.
