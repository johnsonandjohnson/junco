#' Boxplot statistics with configurable quantile type
#'
#' `StatBoxplotQuantile` computes boxplot statistics using a configurable
#' quantile definition. By default, quartiles use `quantile(type = 2)` to follow
#' `PCTLDEF = 5` (SAS default). It computes quartiles, whiskers, and outliers.
#'
#' Required aesthetics: `x`, `y`.
#'
#' Computed variables (per group):
#' - `ymin`: lower whisker (min value within lower fence)
#' - `lower`: first quartile (Q1)
#' - `middle`: median (Q2)
#' - `upper`: third quartile (Q3)
#' - `ymax`: upper whisker (max value within upper fence)
#' - `outliers`: list-column of values outside whiskers
#' - `x`: x position (first value in group)
#' - `width`: box width used by `GeomBoxplot`
#'
#' @section Quantile details:
#' By default, quartiles are computed with `quantile(type = 2)` to follow
#' `PCTLDEF = 5` (SAS default). You can change this via the `quantile_type` argument. Fences
#' are defined as `Q1 - coef * IQR` and `Q3 + coef * IQR` where `IQR = Q3 - Q1` and `coef`
#' defaults to `1.5`.
#'
#' @keywords internal
#' @importFrom ggplot2 ggproto Stat
#' @importFrom stats quantile
StatBoxplotQuantile <- ggproto("StatBoxplotQuantile", Stat,
                               required_aes = c("x", "y"),
                               dropped_aes = c("x", "y", "weight"),
                               compute_group = function(data, scales, width = NULL, na.rm = FALSE, coef = 1.5,
                           quantile_type = 2) {
    vec <- data$y
    if (length(vec) == 0) {
      return(data.frame())
    }

    # Quantiles controlled by quantile_type (default 2 = PCTLDEF=5)
    qs <- quantile(vec, probs = c(0, 0.25, 0.5, 0.75, 1), type = quantile_type, na.rm = na.rm)
    iqr <- qs[4] - qs[2]

    lower_fence <- qs[2] - (coef * iqr)
    upper_fence <- qs[4] + (coef * iqr)

    ymin <- min(vec[vec >= lower_fence], na.rm = na.rm)
    ymax <- max(vec[vec <= upper_fence], na.rm = na.rm)

    outliers <- vec[vec < ymin | vec > ymax]

    data.frame(
      ymin = ymin,
      lower = qs[2],
      middle = qs[3],
      upper = qs[4],
      ymax = ymax,
      outliers = I(list(outliers)),
      x = data$x[1],
      width = if (is.null(width)) 0.75 else width
    )
  }
)

#'  Draws boxplots with configurable quantile type
#'
#' `geom_boxplot_j()` draws boxplots whose statistics can follow different
#'  percentile definitions for quartiles. By default it uses
#'  `quantile(type = 2)` to follow `PCTLDEF = 5` (SAS default). You can override
#'  the quantile definition via `quantile_type`. It uses `StatBoxplotQuantile`
#'  then renders with `GeomBoxplot`.
#'
#' @param mapping,data,position,... Standard ggplot2 layer arguments passed to
#'   `layer()`/`GeomBoxplot`.
#' @param coef Numeric multiplier for the IQR to compute fences. Defaults to 1.5
#'   (Tukey).
#' @param quantile_type Integer in 1:9 passed to `stats::quantile(type = ...)` to
#'   compute quartiles. Defaults to 2 (follows PCTLDEF = 5).
#' @param na.rm Logical; if `TRUE`, silently removes `NA` values.
#' @param show.legend,inherit.aes See `ggplot2::layer()`.
#'
#' @return A ggplot2 layer that produces boxplots using the chosen quantile type.
#'
#' @examples
#' library(ggplot2)
#' library(pharmaverseadamjnj)
#' ggplot(advs, aes(AVISIT, AVAL, fill = TRT01A)) +
#'   geom_boxplot_j(position = position_dodge2(preserve = "single"))
#'
#' @export
#' @importFrom ggplot2 layer GeomBoxplot
geom_boxplot_j <- function(mapping = NULL, data = NULL, position = "dodge2",
                             ..., coef = 1.5, quantile_type = 2, na.rm = FALSE,
                             show.legend = NA, inherit.aes = TRUE) {
  layer(
    data = data, mapping = mapping, stat = StatBoxplotQuantile, geom = GeomBoxplot,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, coef = coef, quantile_type = quantile_type, ...)
  )
}