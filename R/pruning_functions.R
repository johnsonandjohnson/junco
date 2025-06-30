#' @name safe_prune_table
#'
#' @title Safely Prune Table With Empty Table Message If Needed
#'
#' @inheritParams rtables::prune_table
#'
#' @param empty_msg character(1). The message to place in the table
#' if no rows were left after pruning
#'
#' @param spancols logical(1). Should `empty_msg` be spanned
#' across the table's columns (`TRUE`) or placed in the
#' rows row label (`FALSE`). Defaults to `FALSE` currently.
#'
#' @rdname safe_prune_table
#' @return `tt` pruned based on the arguments, or, if
#' pruning would remove all rows, a TableTree with the
#' same column structure, and one row containing the
#' empty message spanning all columns
#'
#' @export
#' @examples
#' # always prune
#' prfun <- function(tt) TRUE
#'
#' lyt <- basic_table() |>
#'   split_cols_by('ARM') |>
#'   split_cols_by('STRATA1') |>
#'   split_rows_by('SEX') |>
#'   analyze('AGE')
#' tbl <- build_table(lyt, ex_adsl)
#'
#' safe_prune_table(tbl, prfun)
safe_prune_table <- function(
  tt,
  prune_func = prune_empty_level,
  stop_depth = NA,
  empty_msg = " - No Data To Display - ",
  spancols = FALSE
) {
  ret <- prune_table(tt = tt, prune_func = prune_func, stop_depth = stop_depth, depth = 0)
  if (is.null(ret)) {
    ret <- tt[integer(), , keep_titles = TRUE, keep_topleft = TRUE, keep_footers = TRUE]

    if (spancols) {
      ## this will eventually be the only option...
      ret <- sanitize_table_struct(ret, empty_msg = empty_msg)
    } else {
      tree_children(ret) <- list(rrowl(empty_msg, rep(" ", ncol(tt))))
    }
  }
  ret
}

#' @name count_pruner
#'
#' @title Count Pruner
#'
#' @description
#' This is a pruning constructor function which identifies records to be pruned
#' based on the count (assumed to be the first statistic displayed when a compound
#' statistic (e.g., ## / ## (XX.X percent) is presented).
#'
#' @param  count   count threshold.  Function will keep all records strictly greater
#'                 than this threshold.
#' @param  cols    column path (character or integer (column indices))
#' @param  cat_include     Category to be considered for pruning
#' @param  cat_exclude logical Category to be excluded from pruning
#' @export
#'
#'
#' @examples
#'
#' ADSL <- data.frame(
#'   USUBJID = c(
#'     'XXXXX01', 'XXXXX02', 'XXXXX03', 'XXXXX04', 'XXXXX05',
#'     'XXXXX06', 'XXXXX07', 'XXXXX08', 'XXXXX09', 'XXXXX10'
#'   ),
#'   TRT01P = factor(
#'     c(
#'       'ARMA', 'ARMB', 'ARMA', 'ARMB', 'ARMB',
#'       'Placebo', 'Placebo', 'Placebo', 'ARMA', 'ARMB'
#'      )
#'   ),
#'   FASFL = c('Y', 'Y', 'Y', 'Y', 'N', 'Y', 'Y', 'Y', 'Y', 'Y'),
#'   SAFFL = c('N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N'),
#'   PKFL = c('N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N')
#' )
#'
#' lyt <- basic_table() |>
#'   split_cols_by('TRT01P') |>
#'   add_overall_col('Total') |>
#'   analyze('FASFL',
#'     var_labels = 'Analysis set:',
#'     afun = a_freq_j,
#'     extra_args = list(label = 'Full', val = 'Y'),
#'     show_labels = 'visible'
#'   ) |>
#'   analyze('SAFFL',
#'     var_labels = 'Analysis set:',
#'     afun = a_freq_j,
#'     extra_args = list(label = 'Safety', val = 'Y'),
#'     show_labels = 'visible'
#'   ) |>
#'   analyze('PKFL',
#'     var_labels = 'Analysis set:',
#'     afun = a_freq_j,
#'     extra_args = list(label = 'PK', val = 'Y'),
#'     show_labels = 'visible'
#'   )
#'
#' result <- build_table(lyt, ADSL)
#'
#' result
#'
#' # use pruning function to prune where total column has >0 count,
#' # but exclude pruning safety set row
#' result <- prune_table(
#'   result,
#'   prune_func = count_pruner(cat_exclude = c('Safety'), cols = 'Total')
#' )
#'
#' result
#'
#' @rdname count_pruner
#' @returns  function that can be utilized as pruning function in prune_table
#'
count_pruner <- function(count = 0, cat_include = NULL, cat_exclude = NULL, cols = c("TRT01A")) {
  function(tt) {
    # Do not ever prune the following rows.  a row that should be kept in the table will get the value of FALSE

    if (
      !methods::is(tt, "TableRow") ||
        methods::is(tt, "LabelRow") ||
        obj_label(tt) == " " ||
        (!is.null(cat_include) &&
           !obj_label(tt) %in% cat_include) ||
        (!is.null(cat_exclude) && obj_label(tt) %in% cat_exclude)
    ) {
      return(FALSE)
    }

    # Check the remaining rows to see if any meet the specified threshold.

    if (!is.null(cols)) {
      tt <- subset_cols(tt, cols)
    }

    # init return value to FALSE (not remove row)
    remove <- FALSE

    colpaths <- col_paths(tt)
    # identify non relative risk columns as in rrisk columns first element is not a count, but diff percentage with
    # small total columns and count > 0 (eg count = 1, n per group = 10), you can run into count1 = 1, countpbo =
    # 0, diffpct = 10, this row should not be considered though
    cp_nonrelrisk <- vapply(
      colpaths,
      function(pth) {
        !any(grepl("rrisk", tolower(pth)))
      },
      FUN.VALUE = TRUE
    )

    ## only continue if at least one non relative risk column selected
    if (any(cp_nonrelrisk)) {
      # get cell_values of non rel risk columns
      cell_vals <- cell_values(tt)[cp_nonrelrisk]

      len_cell_vals <- lapply(cell_vals, function(x) {
        length(x)
      })

      if (any(len_cell_vals == 0)) {
        stop("column cell values has not appropriate structure (a column with NULL value).")
      }

      # get count (first element)
      counts <- unlist(
        lapply(cell_vals, function(x) {
          x[[1]]
        }),
        use.names = FALSE
      )

      # check that we do have counts only
      checkmate::assert_integerish(counts)

      keep <- counts > count
      # the row should be kept if at least one column has count above threshold
      keep <- any(keep)

      # pruning function should return TRUE if row has to be removed, ie opposite of keep
      remove <- !keep
    }

    return(remove)
  }
}


#' @name bspt_pruner
#'
#' @title Pruning Function for pruning based on a fraction and/or a difference from the control arm
#'
#' @description
#' This is a pruning constructor function which identifies records to be pruned
#' based on the the fraction from the percentages. In addition to just looking at a fraction within an arm
#' this function also allows further flexibility to also prune based on a comparison versus the control arm.
#' @param  fraction   fraction threshold.  Function will keep all records strictly greater
#'                 than this threshold.
#' @param  cols    column path.
#' @param  keeprowtext     Row to be excluded from pruning.
#' @param reg_expr Apply keeprowtext as a regular expression (grepl with fixed = TRUE)
#' @param  control        Control Group
#' @param  diff_from_control  Difference from control threshold.
#' @param  only_more_often    TRUE: Only consider when column pct is more often
#' than control. FALSE: Also select a row where column pct is less often than
#' control and abs(diff) above threshold
#'
#' @export
#'
#'
#' @examples
#' ADSL <- data.frame(
#'   USUBJID = c(
#'     'XXXXX01', 'XXXXX02', 'XXXXX03', 'XXXXX04', 'XXXXX05',
#'     'XXXXX06', 'XXXXX07', 'XXXXX08', 'XXXXX09', 'XXXXX10'
#'   ),
#'   TRT01P = c(
#'     'ARMA', 'ARMB', 'ARMA', 'ARMB', 'ARMB',
#'     'Placebo', 'Placebo', 'Placebo', 'ARMA', 'ARMB'
#'   ),
#'   FASFL = c('Y', 'Y', 'Y', 'Y', 'N', 'Y', 'Y', 'Y', 'Y', 'Y'),
#'   SAFFL = c('N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N'),
#'   PKFL = c('N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N')
#' )
#'
#' ADSL <- ADSL |>
#'   dplyr::mutate(TRT01P = as.factor(TRT01P)) |>
#'   dplyr::mutate(SAFFL = factor(SAFFL, c("Y", "N"))) |>
#'   dplyr::mutate(PKFL = factor(PKFL, c("Y", "N")))
#'
#' lyt <- basic_table() |>
#'   split_cols_by('TRT01P') |>
#'   add_overall_col('Total') |>
#'
#'   split_rows_by(
#'     "FASFL",
#'     split_fun = drop_and_remove_levels("N"),
#'     child_labels = "hidden"
#'   ) |>
#'   analyze('FASFL',
#'     var_labels = 'Analysis set:',
#'     afun = a_freq_j,
#'     show_labels = 'visible',
#'     extra_args = list(label = 'Full', .stats = "count_unique_fraction")
#'   ) |>
#'
#'   split_rows_by(
#'     "SAFFL",
#'     split_fun = remove_split_levels("N"),
#'     child_labels = "hidden"
#'   ) |>
#'   analyze('SAFFL',
#'     var_labels = 'Analysis set:',
#'     afun = a_freq_j,
#'     show_labels = 'visible',
#'     extra_args = list(label = 'Safety', .stats = "count_unique_fraction")
#'   ) |>
#'
#'   split_rows_by(
#'     "PKFL",
#'     split_fun = remove_split_levels("N"),
#'     child_labels = "hidden"
#'   ) |>
#'   analyze('PKFL',
#'     var_labels = 'Analysis set:',
#'     afun = a_freq_j,
#'     show_labels = 'visible',
#'     extra_args = list(label = 'PK', .stats = "count_unique_fraction")
#'   )
#'
#' result <- build_table(lyt, ADSL)
#'
#' result
#'
#' # use pruning function to prune rows where it doesn't meet 5% criteria
#' # in the total column but it will not
#' # prune the Safety row regardless
#' result <- prune_table(
#'   result,
#'   prune_func = bspt_pruner(
#'     fraction = 0.05,
#'     keeprowtext = 'Safety',
#'     cols = c('Total')
#'   )
#' )
#'
#' result
#' @rdname bspt_pruner
#' @returns  function that can be utilized as pruning function in prune_table
#'
bspt_pruner <- function(
  fraction = 0.05,
  keeprowtext = "Analysis set: Safety",
  reg_expr = FALSE,
  control = NULL,
  diff_from_control = NULL,
  only_more_often = TRUE,
  cols = c("TRT01A")
) {
  if (is.null(fraction) && is.null(diff_from_control)) {
    stop("At least one of fraction or diff_from_control must be non-NULL.")
  }
  if (!is.null(diff_from_control) && is.null(control)) {
    stop("control must be specified when diff_from_control is not NULL.")
  }

  function(tt) {
    # Do not ever prune the following rows.
    if (!methods::is(tt, "TableRow") || methods::is(tt, "LabelRow")) {
      return(FALSE)
    }

    if (
      reg_expr &&
        any(sapply(keeprowtext, function(x) {
          grepl(x, obj_label(tt), fixed = TRUE)
        }))
    ) {
      return(FALSE)
    }
    if (!reg_expr && obj_label(tt) %in% keeprowtext) {
      return(FALSE)
    }

    # init return value to FALSE (not remove row)
    remove <- FALSE

    # needed later for control column
    tt_all_cols <- tt

    if (!is.null(cols)) {
      tt <- subset_cols(tt, cols)
    }

    colpaths <- col_paths(tt)
    # identify non relative risk columns
    cp_nonrelrisk <- vapply(
      colpaths,
      function(pth) {
        !any(grepl("rrisk", tolower(pth)))
      },
      FUN.VALUE = TRUE
    )

    ## only continue if at least one non relative risk column selected
    if (any(cp_nonrelrisk)) {
      # get rid of rel risk columns (if present)
      cell_vals <- cell_values(tt)[cp_nonrelrisk]

      len_cell_vals <- lapply(cell_vals, function(x) {
        length(x)
      })

      if (any(len_cell_vals < 2)) {
        stop("column cell values has not appropriate structure (less than 2 values in cell).")
      }

      # get count and percentage columns counts in first col pcts in second col
      counts <- unname(lst_slicer(cell_vals, 1, numeric(1)))
      pcts <- unname(lst_slicer(cell_vals, 2, numeric(1)))

      checkmate::check_integerish(counts)
      # similarly check that pcts is indeed a percentage
      if (!all(dplyr::between(pcts, 0, 1))) {
        stop("second value column cell is not a percentage.")
      }

      if (!is.null(fraction)) {
        # avoid problems with FALSE for 0.05 >= 0.05
        check_pcts <- pcts >= fraction |
          vapply(
            pcts,
            function(x) {
              isTRUE(all.equal(x, fraction))
            },
            TRUE
          )
      }

      if (!is.null(diff_from_control)) {
        # get control group from all columns tt (in case the columns selected would not include the control
        # group)
        colpaths0 <- col_paths(subset_cols(tt_all_cols, c(control)))
        cp0_nonrelrisk <- vapply(
          colpaths0,
          function(pth) {
            !any(grepl("rrisk", tolower(pth)))
          },
          FUN.VALUE = TRUE
        )
        colpaths0 <- colpaths0[cp0_nonrelrisk]

        if (length(colpaths0) != 1) {
          stop("control group spec does not result in single column.")
        } else {
          colpaths0 <- colpaths0[[1]]
          # get percent from control group
          pct0 <- cell_values(tt_all_cols, colpath = c(control))[[1]][2]

          if (!dplyr::between(pct0, 0, 1)) {
            stop("second value of control column cell is not a percentage.")
          }
        }

        # check if control group column was also present in main column selector
        colid0 <- which(sapply(colpaths, function(x) {
          all(x == colpaths0)
        }))

        pctdiffs <- pcts - pct0

        # exclude control column from this (would only be relevant if we want to check criteria on all columns
        # rather than any column)
        if (!identical(colid0, integer(0))) pctdiffs <- pctdiffs[-colid0]

        # if not only_more_often be equally strict on worse from control, better as control ie also select rows
        # where column is less often than control pct = 0.07 pct0 = 0.10 (diff = -0.03 )
        if (!only_more_often) pctdiffs <- abs(pctdiffs)

        # avoid problems with FALSE for 0.02 >= 0.02
        check_diffpcts <- pctdiffs >= diff_from_control |
          vapply(
            pctdiffs,
            function(x) {
              isTRUE(all.equal(x, diff_from_control))
            },
            TRUE
          )
      }

      # final step to check if row should be kept in table
      if (!is.null(fraction) && is.null(diff_from_control)) {
        # keep row in table if any column pct above threshold
        keep <- any(check_pcts)
      } else if ((!is.null(fraction) && !is.null(diff_from_control))) {
        # if any column pct above threshold AND any diff pct above threshold note any is used in both
        # expressions separately situation pct = 0.04 pct0 = 0.06 should be kept (fraction = 0.05, abs(diff
        # control) = 0.02) this problem only arises in the non-default situation when only_more_often is FALSE
        # (ie when column pct is less often than control) this would not be the case if we would any(check_pcts
        # & check_diffpcts) as 0.04 is below fraction
        keep <- any(check_pcts) && any(check_diffpcts)
      } else if ((is.null(fraction) && !is.null(diff_from_control))) {
        keep <- any(check_diffpcts)
      }

      # pruning function should return TRUE if row has to be removed, ie opposite of keep
      remove <- !keep
    }

    return(remove)
  }
}

lst_slicer <- function(lst, ind, type) {
  vapply(lst, `[[`, i = ind, type)
}

#' @name remove_rows
#'
#' @title
#' Pruning function to remove specific rows of a table regardless of counts
#' @description
#' This function will remove all rows of a table based on the row text
#' provided by the user.
#' @param  removerowtext  define a text string for which any row with row text will be removed.
#' @param reg_expr Apply removerowtext as a regular expression (grepl with fixed = TRUE)
#' @export
#' @rdname remove_rows
#'
#'
#' @examples
#' ADSL <- data.frame(
#'   USUBJID = c(
#'     'XXXXX01', 'XXXXX02', 'XXXXX03', 'XXXXX04', 'XXXXX05',
#'     'XXXXX06', 'XXXXX07', 'XXXXX08', 'XXXXX09', 'XXXXX10'
#'   ),
#'   TRT01P = c(
#'     'ARMA', 'ARMB', 'ARMA', 'ARMB', 'ARMB', 'Placebo',
#'     'Placebo', 'Placebo', 'ARMA', 'ARMB'
#'   ),
#'   Category = c(
#'     'Cat 1', 'Cat 2', 'Cat 1', 'Unknown', 'Cat 2',
#'     'Cat 1', 'Unknown', 'Cat 1', 'Cat 2', 'Cat 1'
#'   ),
#'   SAFFL = c('N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N'),
#'   PKFL = c('N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N')
#' )
#'
#' ADSL <- ADSL |>
#'   dplyr::mutate(TRT01P = as.factor(TRT01P))
#'
#' lyt <- basic_table() |>
#'   split_cols_by("TRT01P") |>
#'
#'   analyze(
#'     "Category",
#'     afun = a_freq_j,
#'     extra_args = list(.stats = "count_unique_fraction")
#'   )
#'
#' result <- build_table(lyt, ADSL)
#'
#' result
#'
#' # use pruning function to prune rows where category is Unknown
#' result <- prune_table(result, prune_func = remove_rows(removerowtext = 'Unknown'))
#'
#' result
#' @aliases remove_rows
#' @returns function that can be utilized as pruning function in prune_table
#'
remove_rows <- function(removerowtext = NULL, reg_expr = FALSE) {
  function(tt) {
    if (!methods::is(tt, "TableRow") || methods::is(tt, "LabelRow")) {
      return(FALSE)
    }

    ret <- FALSE
    if (!is.null(removerowtext)) {
      if (!reg_expr) {
        ret <- obj_label(tt) %in% removerowtext
      } else {
        ret <- any(sapply(removerowtext, function(x) {
          grepl(x, obj_label(tt), fixed = TRUE)
        }))
      }
    }

    ret
  }
}

#' @name keep_non_null_rows
#'
#' @title Pruning Function to accommodate removal of completely NULL rows within a table
#'
#' @description
#' Condition function on individual analysis rows. Flag as FALSE when all
#' columns are NULL, as then the row should not be kept. To be utilized as a
#' row_condition in function tern::keep_rows
#'
#' @param  tr      table tree object
#' @export
#'
#'
#' @examples
#'
#' library(dplyr)
#'
#' ADSL <- data.frame(
#'   USUBJID = c(
#'     'XXXXX01', 'XXXXX02', 'XXXXX03', 'XXXXX04', 'XXXXX05',
#'     'XXXXX06', 'XXXXX07', 'XXXXX08', 'XXXXX09', 'XXXXX10'
#'   ),
#'   TRT01P = c(
#'     'ARMA', 'ARMB', 'ARMA', 'ARMB', 'ARMB', 'Placebo',
#'     'Placebo', 'Placebo', 'ARMA', 'ARMB'
#'   ),
#'   AGE = c(34, 56, 75, 81, 45, 75, 48, 19, 32, 31),
#'   SAFFL = c('N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N'),
#'   PKFL = c('N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N')
#' )
#'
#' ADSL <- ADSL |>
#'   mutate(TRT01P = as.factor(TRT01P))
#'
#' create_blank_line <- function(x) {
#'   list(
#'     'Mean' = rcell(mean(x), format = 'xx.x'),
#'     ' ' = rcell(NULL),
#'     'Max' = rcell(max(x))
#'   )
#' }
#'
#' lyt <- basic_table() |>
#'   split_cols_by('TRT01P') |>
#'   analyze('AGE', afun = create_blank_line)
#'
#' result <- build_table(lyt, ADSL)
#'
#' result
#' # use pruning function to prune rows where category is Unknown
#' result <- prune_table(result, prune_func = tern::keep_rows(keep_non_null_rows))
#'
#' result
#' @rdname keep_non_null_rows
#' @returns a function that can be utilized as a row_condition in the tern::keep_rows function
#'
keep_non_null_rows <- function(tr) {
  if (methods::is(tr, "DataRow")) {
    r_cellvalue_null <- unlist(lapply(cell_values(tr), is.null))
    check <- all(r_cellvalue_null)

    ### if check TRUE (all cell values NULL -- need to return FALSE as need to remove)
    ret <- !check
  } else {
    ret <- TRUE
  }
  ret
}
