#' @describeIn junco_varying_decimal_precision Named list of junco default formats for various junco/tern based statistics (mainly for `tern::s_summary` and `ancova` methods).
#'
#' @format
#' * `junco_default_formats_d` is a named vector of available default formats, with each element
#'   named for their corresponding statistic. Both `xx.x` and `xx.d` style notation can be used, as well as formatting functions.
#'
#' @export
#' @order 3
junco_default_formats_d <- c(
  n = "xx.",
  sum = "xx.d",
  mean = "xx.dx",
  sd = "xx.dxx",
  se = "xx.dxx",
  mean_sd = "xx.dx (xx.dxx)",
  mean_se = "xx.dx (xx.dxx)",
  mean_ci = "(xx.dx, xx.dx)",
  mean_sei = "(xx.dx, xx.dx)", 
  mean_sdi = "(xx.dx, xx.dx)", 
  mean_pval = jjcsformat_pval_fct(0),
  median = "xx.dx", 
  mad = "xx.dx",
  median_ci = "(xx.dx, xx.dx)",
  quantiles = "xx.dx - xx.dx",
  iqr = "xx.dx",
  range = "xx.d - xx.d",
  min = "xx.d",
  max = "xx.d",
  median_range = "xx.dx (xx.dx - xx.dx)",
  cv = "xx.dx",
  geom_mean = "xx.dx",
  geom_mean_ci = "(xx.dx, xx.dx)",
  geom_cv = "xx.dx",
  
  lsmean = "xx.dx",
  lsmean_ci = "xx.dx (xx.dx, xx.dx)",
  lsmean_diff = "xx.dx",
  lsmean_diffci = "xx.dx (xx.dx, xx.dx)",
  lsmean_diff_ci = "(xx.dx, xx.dx)",
  lsmean_se = "xx.dx (xx.dxx)",
  pval = jjcsformat_pval_fct(0),
  
  "count" = "xx",
  "count_fraction" = jjcsformat_count_fraction,
  "count_fraction_fixed_dp" = jjcsformat_count_fraction
)
