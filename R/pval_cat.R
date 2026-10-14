#' Helper function to normalize p-value categories
#'
#' @description Converts a named list of p-value category bounds into a matrix
#'   and retains the category labels in their supplied order.
#'
#' @param pvalcat (`named list`)\cr A non-empty named list. Each element must be
#'   a numeric vector of length two specifying the lower and upper bounds of a
#'   p-value category.
#'
#' @return A named `list` with the following elements:
#'
#' * `bounds`: a two-column numeric matrix of lower and upper category bounds.
#' * `cats`: a character vector of category labels.
#'
#' @keywords internal
h_normalize_pvalcat <- function(pvalcat) {
  checkmate::assert_list(pvalcat, min.len = 1, names = "unique")
  checkmate::assert_character(names(pvalcat), any.missing = FALSE, unique = TRUE)
  checkmate::assert_true(all(nzchar(names(pvalcat))))

  bounds <- lapply(pvalcat, function(x) {
    checkmate::assert_numeric(x, len = 2, any.missing = FALSE, finite = TRUE)
    checkmate::assert_true(x[1] <= x[2])
    x
  })

  list(
    bounds = matrix(unlist(bounds, use.names = FALSE), ncol = 2, byrow = TRUE),
    cats = names(pvalcat)
  )
}

#' Categorize p-values
#'
#' @description Assigns each p-value to a category defined by a named list of
#'   lower and upper bounds.
#'
#' @details Categories are evaluated in the order supplied by `pvalcat`. Bounds
#'   are lower-inclusive and upper-exclusive, except that the upper bound of
#'   the final category is inclusive. P-values that do not fall in a category,
#'   including missing values, are returned as `NA_character_`.
#'
#' @param p (`numeric`)\cr A vector of p-values to categorize. Missing values are
#'   permitted.
#' @param pvalcat (`named list`)\cr A non-empty named list of p-value category
#'   bounds. See [h_normalize_pvalcat()] for the required structure.
#'
#' @return A character vector of category labels, with one element per value in
#'   `p`.
#'
#' @examples
#' pvalcat <- list(
#'   "<0.001" = c(0, 0.001),
#'   "0.001 to <0.05" = c(0.001, 0.05),
#'   ">=0.05" = c(0.05, 1)
#' )
#'
#' categorize_pval(c(0, 0.001, 0.049, 0.05, 1, NA), pvalcat)
#' @export
categorize_pval <- function(p, pvalcat) {
  checkmate::assert_numeric(p, any.missing = TRUE)
  checkmate::assert_list(pvalcat, min.len = 1, names = "unique")

  info <- h_normalize_pvalcat(pvalcat)
  bounds <- info$bounds
  cats <- info$cats
  last_row <- nrow(bounds)

  vapply(
    p,
    function(x) {
      if (is.na(x)) {
        return(NA_character_)
      }

      idx <- which(
        bounds[, 1] <= x &
          (x < bounds[, 2] | (seq_len(last_row) == last_row & x <= bounds[, 2]))
      )
      if (length(idx) == 0) {
        return(NA_character_)
      }
      cats[idx[1]]
    },
    FUN.VALUE = character(1)
  )
}
