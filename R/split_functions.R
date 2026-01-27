#' rm_other_facets_fact
#' @param nm character. names of facets to keep. all other facets will be
#' removed
#' @returns a function suitable for use within the `post` portion make_split_fun

rm_other_facets_fact <- function(nm) {
  function(ret, spl, .spl_context, fulldf) {
    keep <- which(names(ret$values) %in% nm)
    stopifnot(length(keep) > 0)
    # values already have subsetting expressions on them
    make_split_result(ret$values[keep], ret$datasplit[keep], ret$labels[keep])
  }
}

#' @name real_add_overall_facet
#'
#' @title Add Overall Facet
#'
#' @description
#' A function to help add an overall facet to your tables
#' @param name character(1). Name/virtual 'value' for the new facet
#' @param label character(1). label for the new facet
#' @note current add_overall_facet is bugged, can use that directly after it's fixed
#' https://github.com/insightsengineering/rtables/issues/768
#' @examples
#'
#' splfun <- make_split_fun(post = list(real_add_overall_facet('Total', 'Total')))
#' @export
#' @returns function usable directly as a split function.
#'
real_add_overall_facet <- function(name, label) {
  function(ret, spl, .spl_context, fulldf) {
    add_to_split_result(
      ret,
      values = name,
      datasplit = stats::setNames(list(fulldf), name),
      labels = stats::setNames(label, name),
      subset_exprs = quote(TRUE)
    )
  }
}

#' @name make_combo_splitfun
#'
#' @title Split Function Helper
#'
#' @description
#' A function which aids the construction for users to create their own split function for combined columns
#' @param nm character(1). Name/virtual 'value' for the new facet
#' @param label character(1). label for the new facet
#' @param levels character or NULL. The levels to combine into the new facet,
#' or NULL, indicating the facet should include all incoming data.
#' @param rm_other_facets logical(1). Should facets other than the newly
#' created one be removed. Defaults to `TRUE`
#' @export
#' @returns function usable directly as a split function.
#' @examples
#' aesevall_spf <- make_combo_splitfun(nm = 'AESEV_ALL', label  = 'Any AE', levels = NULL)
#'
make_combo_splitfun <- function(nm, label = nm, levels = NULL, rm_other_facets = TRUE) {
  if (is.null(levels)) {
    fn <- real_add_overall_facet(name = nm, label = label)
  } else {
    fn <- add_combo_facet(name = nm, label = label, levels = levels)
  }
  if (rm_other_facets) {
    rmfun <- rm_other_facets_fact(nm)
  } else {
    rmfun <- NULL
  }
  make_split_fun(post = c(list(fn), if (rm_other_facets) list(rmfun)))
}

blank_regex <- "^[[:space:]]*$"

combine_nonblank <- function(name, label) {
  function(ret, spl, .spl_context, fulldf) {
    df <- fulldf[!grepl(blank_regex, fulldf[[spl_variable(spl)]]), ]
    add_to_split_result(
      ret,
      values = name,
      datasplit = stats::setNames(list(df), name),
      labels = stats::setNames(label, name)
    )
  }
}

rm_blank_levels <- function(df, spl, ...) {
  var <- spl_variable(spl)
  varvec <- df[[var]]
  oldlevs <- levels(varvec)
  newlevs <- oldlevs[!grepl(blank_regex, oldlevs)]
  df <- df[!grepl(blank_regex, varvec), ]
  df[[var]] <- factor(df[[var]], levels = newlevs)
  df
}


find_torm <- function(spl_ret, torm, torm_regex, keep_matches) {
  if (!is.null(torm)) {
    ans_lgl <- names(spl_ret$datasplit) %in% torm
  } else {
    ans_lgl <- grepl(torm_regex, names(spl_ret$datasplit))
  }
  if (keep_matches) {
    ans_lgl <- !ans_lgl
  }
  which(ans_lgl)
}

.check_rem_cond <- function(cond_str, cond_regex, spl_ctx, pos, type) {
  if (is.null(cond_str) && is.null(cond_regex)) {
    return(TRUE)
  }
  if (!is.null(cond_str) && !is.null(cond_regex)) {
    stop(
      "Got both ",
      paste(paste0(type, c("", "_regex")), collapse = " and "),
      ". Please specify at most one of these."
    )
  }
  ctx_data <- spl_ctx[[type]][pos]
  if (!is.null(cond_str)) {
    any(cond_str %in% ctx_data)
  } else {
    any(grepl(cond_regex, ctx_data))
  }
}

## handle support for NA and negative positions only called once but code is nicer with it factored out here
resolve_ancestor_pos <- function(anc_pos, numrows) {
  if (is.na(anc_pos)) {
    anc_pos <- seq_len(numrows)
  } else if (any(anc_pos < 0)) {
    if (!all(anc_pos < 0)) {
      stop("Got mix of negative and non-negative values for ancestor_pos; ", "this is not supported.")
    }
    ## direct parent is actually NROW(.spl_context) so avoid off-by-1 error with the + 1 here
    anc_pos <- numrows - (anc_pos + 1)
  }
  anc_pos
}

#' @name cond_rm_facets
#' @title Conditional Removal of Facets
#' @param facets character or NULL. Vector of facet names to be removed
#' if condition(s) are met
#' @param facets_regex character(1). Regular expression to identify facet
#' names to be removed if condition(s) are met.
#' @param ancestor_pos numeric(1). Row in spl_context to check the condition
#' within. E.g., 1 represents the first split, 2 represents the second split
#' nested within the first, etc. NA specifies that the conditions
#' should be checked at all split levels. Negative integers indicate position
#' counting back from the current one, e.g., -1 indicates the direct parent
#' (most recent split before this one). Negative and positive/NA positions
#' cannot be mixed.
#' @param split character(1) or NULL. If specified, name of the split
#' at position `ancestor_pos` must be identical to this value for
#' the removal condition to be met.
#' @param split_regex character(1) or NULL. If specified, a regular expression
#' the name of the split at position `ancestor_pos` must match for
#' the removal condition to be met. Cannot be specified at the same time
#' as `split`.
#' @param value character(1) or NULL. If specified, split (facet) value
#' at position `ancestor_pos` must be identical to this value for
#' removal condition to be met.
#' @param value_regex character(1) or NULL. If specified, a regular expression
#' the value of the split at position `ancestor_pos` must match for
#' the removal condition to be met. Cannot be specified at the same time
#' as `value`.
#' @param keep_matches logical(1). Given the specified condition is met,
#' should the facets removed be those matching `facets`/`facets_regex`
#' (`FALSE`, the default), or those *not* matching (`TRUE`).
#'
#' @details Facet removal occurs when the specified condition(s)
#' on the split(s) and or value(s) are met within at least one
#' of the split_context rows indicated by `ancestor_pos`; otherwise
#' the set of facets is returned unchanged.
#'
#' If facet removal is performed, either *all* facets which match `facets` (or
#' `facets_regex` will be removed ( the default `keep_matches == FALSE`
#'  case), or all *non-matching* facets will be removed (when
#'   `keep_matches_only == TRUE`).
#'
#' @note A degenerate table is likely to be returned if all facets
#' are removed.
#'
#' @returns a function suitable for use in `make_split_fun`'s
#' `post` argument which encodes the specified condition.
#' @export
#' @examples
#'
#' rm_a_from_placebo <- cond_rm_facets(
#'   facets = "A",
#'   ancestor_pos = NA,
#'   value_regex = "Placeb",
#'   split = "ARM"
#' )
#' mysplit <- make_split_fun(post = list(rm_a_from_placebo))
#'
#' lyt <- basic_table() |>
#'   split_cols_by("ARM") |>
#'   split_cols_by("STRATA1", split_fun = mysplit) |>
#'   analyze("AGE", mean, format = "xx.x")
#' build_table(lyt, ex_adsl)
#'
#' rm_bc_from_combo <- cond_rm_facets(
#'   facets = c("B", "C"),
#'   ancestor_pos = -1,
#'   value_regex = "Combi"
#' )
#' mysplit2 <- make_split_fun(post = list(rm_bc_from_combo))
#'
#' lyt2 <- basic_table() |>
#'   split_cols_by("ARM") |>
#'   split_cols_by("STRATA1", split_fun = mysplit2) |>
#'   analyze("AGE", mean, format = "xx.x")
#' tbl2 <- build_table(lyt2, ex_adsl)
#' tbl2
#'
#' rm_bc_from_combo2 <- cond_rm_facets(
#'   facets_regex = "^A$",
#'   ancestor_pos = -1,
#'   value_regex = "Combi",
#'   keep_matches = TRUE
#' )
#' mysplit3 <- make_split_fun(post = list(rm_bc_from_combo2))
#'
#' lyt3 <- basic_table() |>
#'   split_cols_by("ARM") |>
#'   split_cols_by("STRATA1", split_fun = mysplit3) |>
#'   analyze("AGE", mean, format = "xx.x")
#' tbl3 <- build_table(lyt3, ex_adsl)
#'
#' stopifnot(identical(cell_values(tbl2), cell_values(tbl3)))
cond_rm_facets <- function(
    facets = NULL,
    facets_regex = NULL,
    ancestor_pos = 1,
    split = NULL,
    split_regex = NULL,
    value = NULL,
    value_regex = NULL,
    keep_matches = FALSE) {
  ## detect errors/miscalling which don't even require us to have the facets
  if (is.null(split) && is.null(split_regex) && is.null(value) && is.null(value_regex)) {
    stop(
      "Must specify condition in terms of at least one of ",
      "split name (split or split_regex) or ",
      "facet value (value or value_regex)."
    )
  }
  if (is.null(facets) && is.null(facets_regex)) {
    stop("Must specify facets either facets or facets_regex, got neither.")
  } else if (!is.null(facets) && !is.null(facets_regex)) {
    stop("Got both facets and facets_regex, this is not supported, please specify only one.")
  }
  function(ret, spl, .spl_context, fulldf) {
    torm_ind <- c()
    ancestor_pos <- resolve_ancestor_pos(ancestor_pos, NROW(.spl_context))
    torm_ind <- find_torm(ret, facets, facets_regex, keep_matches = keep_matches)
    fct_abbrev <- ifelse(is.null(facets_regex), paste(facets, collapse = ", "), facets_regex)
    if (length(torm_ind) == 0) {
      # nocov start
      warning(
        "No facets matched removal criteria [",
        fct_abbrev,
        "] ",
        "in function created with cond_rm_facets.\n",
        "Occured at path: ",
        spl_context_to_disp_path(.spl_context),
        call. = FALSE
      )
      # nocov end
    } else if (length(torm_ind) == length(ret$datasplit)) {
      # nocov start
      warning(
        "All facets matched removal criteria [",
        fct_abbrev,
        "] in function created with cond_rm_facets. ",
        "This will result in a degenerate table (if the condition ",
        "is met) within row splitting and in table-creation failing ",
        "entirely in column splitting.\n",
        "Occured at path: ",
        spl_context_to_disp_path(.spl_context),
        call. = FALSE
      )
      # nocov end
    }
    if (
      .check_rem_cond(split, split_regex, .spl_context, ancestor_pos, type = "split") &&
        .check_rem_cond(value, value_regex, .spl_context, ancestor_pos, type = "value")
    ) {
      ## find_torm handles the keep matching case, so by this point torm_ind is always the ones to remove
      ret <- lapply(ret, function(part) part[-torm_ind])
    }

    ret
  }
}


#' @name rm_levels
#'
#' @title Removal of Levels
#'
#' @description
#' custom function for removing level inside pre step in make_split_fun.
#'
#' @param excl Choose which level(s) to remove
#' @return a function implementing pre-processing split behavior (for use in
#'   `make_split_fun(pre = )` which removes the levels in `excl` from the data
#'   before facets are generated.
#' @export
#'
rm_levels <- function(excl) {
  function(df, spl, ...) {
    var <- spl_variable(spl)
    varvec <- df[[var]]
    oldlevs <- levels(varvec)

    exclevs <- oldlevs %in% excl
    newlevs <- oldlevs[!exclevs]
    df[[var]] <- factor(df[[var]], levels = newlevs)
    df
  }
}

#' Shortcut for Creating Custom Column Splits
#'
#' This is a short cut for a common use of [rtables::make_split_result()] where you need to create
#' custom column splits with different labels but using the same full dataset for each column.
#' It automatically sets up the values, datasplit (using the same full dataset for each column),
#' and subset_exprs (using TRUE for all subsets) parameters for make_split_result().
#'
#' @param ... sequence of named labels for the columns.
#' @param fulldf (`data.frame`)\cr the `fulldf` which will be used for each column.
#' @return The result from [rtables::make_split_result()].
#'
#' @keywords internal
short_split_result <- function(..., fulldf) {
  labels <- c(...)
  values <- stats::setNames(names(labels), names(labels))
  datasplit <- stats::setNames(replicate(n = length(labels), list(fulldf)), names(labels))
  subset_exprs <- replicate(n = length(labels), list(expression(TRUE)))
  make_split_result(values = values, labels = labels, datasplit = datasplit, subset_exprs = subset_exprs)
}
