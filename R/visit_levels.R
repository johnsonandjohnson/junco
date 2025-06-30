#' Get Visit Levels in Order Defined by Numeric Version
#'
#' @param visit_cat (`character`)\cr the categorical version.
#' @param visit_n (`numeric`)\cr the numeric version.
#'
#' @return The unique visit levels in the order defined by the numeric version.
#' @export
#'
#' @examples
#' get_visit_levels(
#'   visit_cat = c('Week 1', 'Week 11', 'Week 2'),
#'   visit_n = c(1, 5, 2)
#' )
get_visit_levels <- function(visit_cat, visit_n) {
  checkmate::assert_character(visit_cat)
  checkmate::assert_numeric(visit_n)

  nvar <- "n"
  visit_levels_df <- unique(data.frame(cat = visit_cat, n = visit_n))
  visit_levels_df <- visit_levels_df[order(visit_levels_df$n), ]
  visit_levels <- visit_levels_df$cat

  checkmate::assert_character(visit_levels, unique = TRUE)

  visit_levels
}
