## contribute to formatters
#' @name inches_to_spaces
#' @title Conversion of inches to spaces.
#' @param ins (`numeric`)\cr Vector of widths in inches.
#' @param fontspec (`font_spec`)\cr The font specification to use.
#' @param raw (`logical`)\cr Should the answer be returned unrounded
#'  (`TRUE`), or rounded to the nearest reasonable value (`FALSE`,
#'  the default).
#' @param tol (`numeric`)\cr The numeric tolerance. Values
#'  between an integer `n`, and `n+tol` will be returned
#'  as `n`, rather than `n+1`, if `raw == FALSE`. Ignored
#'  when `raw` is `TRUE`.
#' @return The number of either fractional (`raw = TRUE`) or whole (`raw = FALSE`)
#'   spaces that will fit within `ins` inches in the specified font.
#' @export
inches_to_spaces <- function(ins, fontspec, raw = FALSE, tol = sqrt(.Machine$double.eps)) {
  newdev <- open_font_dev(fontspec)
  if (newdev) {
    on.exit(close_font_dev())
  }
  spcw <- grid::convertWidth(grid::unit(1, "strwidth", " "), "inches", valueOnly = TRUE)
  ans <- ins / spcw
  if (!raw) {
    win_tol <- ans - floor(ans) < tol
    ans[win_tol] <- floor(ans)
    ans[!win_tol] <- ceiling(ans)
  }
  ans
}

#' @importFrom formatters wrap_string_ttype
ttype_wrap_vec <- function(vec, fontspec, width, wordbreak_ok = TRUE, ...) {
  lapply(
    vec,
    wrap_string_ttype,
    width_spc = width,
    fontspec = fontspec,
    wordbreak_ok = wordbreak_ok,
    ...
  )
}

#' @name check_wrap_nobreak
#' @title Check Word Wrapping
#' @description Check a set of column widths for word-breaking wrap behavior.
#' @param tt (`TableTree`)\cr TableTree object
#' @param colwidths (`numeric`)\cr Column widths (in numbers of spaces under `fontspec`)
#' @param fontspec (`font_spec`)\cr Font specification object
#'
#' @return `TRUE` if the wrap is able to be done without breaking words,
#' `FALSE` if wordbreaking is required to apply `colwidths`.
#' @rdname check_wrap_nobreak
#' @export
check_wrap_nobreak <- function(tt, colwidths, fontspec) {
  newdev <- open_font_dev(fontspec)
  if (newdev) {
    on.exit(close_font_dev())
  }
  mpf <- matrix_form(tt, fontspec = fontspec)
  strs <- mf_strings(mpf)
  colok <- vapply(
    seq_len(ncol(strs)),
    function(i) {
      res <- tryCatch(
        ttype_wrap_vec(strs[, i, drop = TRUE], colwidths[i], fontspec = fontspec, wordbreak_ok = FALSE),
        error = function(e) e
      )
      !methods::is(res, "error")
    },
    TRUE
  )
  all(colok)
}

#' Colwidths for all columns to be forced on one page.
#'
#' @param tt (`TableTree`)\cr TableTree object to calculate column widths for
#' @param fontspec (`font_spec`)\cr Font specification object
#' @param col_gap (`numeric`)\cr Column gap in spaces
#' @param rowlabel_width (`numeric`)\cr Width of row labels in spaces
#' @param print_width_ins (`numeric`)\cr Print width in inches
#' @param landscape (`logical`)\cr Whether the output is in landscape orientation
#' @param lastcol_gap (`logical`)\cr Whether to include a gap after the last column
#' @keywords internal
smart_colwidths_1page <- function(
    tt,
    fontspec,
    col_gap = 6L,
    rowlabel_width = inches_to_spaces(2, fontspec),
    print_width_ins = ifelse(landscape, 11, 8.5) - 2.12,
    landscape = FALSE,
    lastcol_gap = TRUE) {
  total_cpp <- floor(inches_to_spaces(print_width_ins, fontspec = fontspec, raw = TRUE))

  nc <- ncol(tt)
  remain <- total_cpp - rowlabel_width - col_gap * (nc - !lastcol_gap)

  c(rowlabel_width - col_gap, spread_integer(remain, nc))
}

spaces_to_inches <- function(spcs, fontspec) {
  nchar_ttype(" ", fontspec, raw = TRUE) * spcs
}

no_cellwrap_colwidths <- function(tt, fontspec, col_gap = 4L, label_width_ins = 2) {
  if (is.null(col_gap)) {
    col_gap <- 4L
  }
  mpf <- matrix_form(tt, TRUE, FALSE, fontspec = fontspec, col_gap = col_gap)
  strmat <- mf_strings(mpf)
  label_width_max <- inches_to_spaces(label_width_ins, fontspec)

  nchar_mat <- nchar_ttype(strmat[-seq_len(mf_nlheader(mpf)), , drop = FALSE],
    fontspec = fontspec
  )

  label_width <- min(
    label_width_max,
    max(nchar_mat[, 1, drop = TRUE])
  )
  col_maxes <- apply(nchar_mat[, -1, drop = FALSE], 2, max)
  c(label_width, col_maxes + col_gap)
}

pack_into_lines2 <- function(strs, wrdwidths = nchar_ttype(strs), colwidth, fontspec) {
  if (length(wrdwidths) == 0 || (length(wrdwidths) == 1 && wrdwidths <= colwidth)) {
    return(1)
  }
  newdev <- open_font_dev(fontspec)
  if (newdev) {
    on.exit(close_font_dev())
  }

  csums <- cumsum(wrdwidths + c(0, rep(1, length(wrdwidths) - 1)))
  oneline_wdth <- sum(wrdwidths) + length(wrdwidths) - 1
  if (colwidth >= ceiling(oneline_wdth)) {
    return(1)
  }
  lines <- 0
  wrdsused <- 0
  widthused <- 0
  index_seq <- seq_along(wrdwidths)
  totwrds <- length(wrdwidths)
  adj <- 0
  ## finite precision arithmetic is a dreamscape of infinite wonder
  tol <- sqrt(.Machine$double.eps)
  while (wrdsused < length(wrdwidths)) {
    csums_i <- csums - widthused

    fit <- which(index_seq > wrdsused & csums_i <= colwidth + tol)
    widthused <- sum(widthused, wrdwidths[fit], length(fit))
    wrdsused <- wrdsused + length(fit)
    lines <- lines + 1
  }
  lines
}

recursive_add_poss <- function(
    wlst,
    cur_lst,
    ubnd_width,
    lbnd_width,
    ubnd_lines = calc_total_lns(wlst, fontspec = fontspec, colwidth = ubnd_width)$lines,
    lbnd_lines = calc_total_lns(wlst, fontspec = fontspec, colwidth = lbnd_width)$lines,
    fontspec) {
  if (ubnd_width <= lbnd_width + 1 + sqrt(.Machine$double.eps)) {
    return(cur_lst)
  }
  curw <- floor((ubnd_width + lbnd_width) / 2)
  curlnsdf <- calc_total_lns(wlst, fontspec = fontspec, colwidth = curw)
  to_add <- list(curlnsdf)

  if (curlnsdf$lines != lbnd_lines) {
    to_add <- c(
      to_add,
      recursive_add_poss(
        wlst = wlst,
        fontspec = fontspec,
        ubnd_width = curw,
        ubnd_lines = curlnsdf$lines,
        lbnd_width = lbnd_width,
        lbnd_lines = lbnd_lines,
        cur_lst = list()
      )
    )
  }
  if (curlnsdf$lines != ubnd_lines) {
    to_add <- c(
      to_add,
      recursive_add_poss(
        wlst = wlst,
        fontspec = fontspec,
        ubnd_width = ubnd_width,
        ubnd_lines = ubnd_lines,
        lbnd_width = curw,
        lbnd_lines = curlnsdf$lines,
        cur_lst = list()
      )
    )
  }

  c(cur_lst, to_add)
}

calc_total_lns <- function(wlst, colwidth, fontspec, lns_per_pg = 50) {
  lns <- vapply(
    seq_along(wlst),
    function(i) {
      pack_into_lines2(wrdwidths = wlst[[i]], colwidth = colwidth, fontspec = fontspec)
    },
    1
  )
  nonblnks <- !vapply(wlst, function(x) length(x) == 0, TRUE)
  celllns <- lns[-1]
  cllsum <- sum(celllns)
  hdr <- lns[1]
  data.frame(
    colwidth = colwidth,
    lines = cllsum + ceiling(cllsum / lns_per_pg) * hdr,
    cell_lines = cllsum,
    lbl_lines = hdr,
    min_cell_lines = min(1, celllns[nonblnks], na.rm = TRUE),
    max_cell_lines = max(celllns)
  )
}


calc_poss_lines <- function(wlst, lbound, avail_spc, fontspec) {
  minposs <- calc_total_lns(wlst, lbound + avail_spc, fontspec)
  maxposs <- calc_total_lns(wlst, lbound, fontspec)

  retlst <- list(minposs, maxposs)

  retlst <- recursive_add_poss(
    wlst = wlst,
    ubnd_width = lbound + avail_spc,
    lbnd_width = lbound,
    cur_lst = retlst,
    fontspec = fontspec
  )
  do.call(rbind, retlst)
}


make_poss_wdf <- function(
    mpf,
    incl_header = FALSE,
    col_gap = 3,
    pg_width_ins = 8.88,
    fontspec = font_spec("Times", 9, 1.2)) {
  newdev <- open_font_dev(fontspec)
  if (newdev) {
    on.exit(close_font_dev())
  }
  if (!methods::is(mpf, "MatrixPrintForm")) {
    mpf <- matrix_form(mpf, fontspec = fontspec, col_gap = col_gap)
  }
  strs <- mf_strings(mpf)
  if (!incl_header) {
    strs <- strs[-seq_len(mf_nlheader(mpf)), ]
  }
  nc <- ncol(strs)
  nr <- nrow(strs)
  ## strip out markup so we are not over countin (by as much)
  strs <- matrix(gsub("~[{[][[:alpha:]]+ ([^]}]+)[]}]", "\\1", strs), ncol = nc, nrow = nr)

  possrows <- lapply(seq_len(ncol(strs)), function(ii) {
    res <- one_col_strs(strs[, ii, drop = TRUE], fontspec = fontspec, col_gap = col_gap)
    res$col_num <- ii
    res
  })

  possdf <- do.call(rbind, possrows)
  o <- order(possdf$col_num, possdf$colwidth)
  possdf <- possdf[o, ]
  possdf
}

#'
#' @param mpf (`listing_df` or `MatrixPrintForm` derived thereof)\cr The listing
#' calculate column widths for.
#' @param incl_header (`logical(1)`)\cr Should the constraint to not break up
#' individual words be extended to words in the column labels? Defaults to `TRUE`
#' @param col_gap (`numeric(1)`)\cr Amount of extra space (in spaces) to
#' assume between columns. Defaults to `0.5`
#' @param pg_width_ins (`numeric(1)`)\cr Number of inches in width for
#'  *the portion of the page the listing will be printed to*. Defaults to `8.88`
#'  which corresponds to landscape orientation on a standard page after margins.
#' @param fontspec (`font_spec`)\cr Defaults to Times New Roman 8pt font with 1.2 line
#' height.
#' @param verbose (`logical(1)`)\cr Should additional information messages be
#' displayed during the calculation of the column widths? Defaults to `FALSE`.
#' @return
#' * `listing_column_widths`: a vector of column widths suitable to use in `tt_to_tlgrtf` and
#' other exporters.
#' @rdname def_colwidths
#' @export
listing_column_widths <- function(
    mpf,
    incl_header = TRUE,
    col_gap = 0.5,
    pg_width_ins = 8.88,
    fontspec = font_spec("Times", 8, 1.2),
    verbose = FALSE) {
  newdev <- open_font_dev(fontspec)
  if (newdev) {
    on.exit(close_font_dev())
  }
  possdf <- make_poss_wdf(
    mpf = mpf,
    incl_header = incl_header,
    col_gap = col_gap,
    fontspec = fontspec,
    pg_width_ins = pg_width_ins
  )
  optimal <- optimal_widths(
    possdf = possdf,
    tot_spaces = inches_to_spaces(pg_width_ins, fontspec = fontspec),
    verbose = verbose
  )
  optimal$colwidth
}

find_free_colspc <- function(curposs, fullposs, thresh = 0.99, skip = integer(), verbose = FALSE) {
  orig_curposs <- curposs
  maxlns_ind <- which.max(curposs$cell_lines)
  longestcol <- curposs$col_num[maxlns_ind]
  maxlns <- curposs$cell_lines[maxlns_ind]
  adjrow_inds <- setdiff(which(curposs$cell_lines < thresh * maxlns), skip)
  for (arowi in adjrow_inds) {
    col <- curposs$col_num[arowi]
    colwidthii <- curposs$colwidth[arowi]
    fp_ind <- with(fullposs, min(which(col_num == col & cell_lines <= maxlns & colwidth < colwidthii)))
    if (is.finite(fp_ind)) {
      if (verbose) {
        oldwdth <- curposs$colwidth[arowi]
        newwdth <- fullposs$colwidth[fp_ind]
        msg <- sprintf("adjusting column %d width from %d to %d", col, oldwdth, newwdth)
        message(msg)
      }
      curposs[arowi, ] <- fullposs[fp_ind, ]
    }
  }
  curposs
}

constrict_lbl_lns <- function(curdf, possdf, avail_spc, verbose = TRUE) {
  old_lbl_lns <- max(curdf$lbl_lines)
  cols_to_pack <- which(curdf$lbl_lns == old_lbl_lns)
  olddf <- curdf
  success <- TRUE
  for (ii in cols_to_pack) {
    colii <- curdf$col_num[ii]
    cwidthii <- curdf$colwidth[ii]
    possdfii <- possdf[possdf$col_num == colii & possdf$lbl_lines < old_lbl_lns, ]
    if (nrow(possdfii) == 0) {
      success <- FALSE
      break
    }
    newrow <- possdfii[ii, ]
    if (newrow$colwidth - cwidthii > avail_spc) {
      success <- FALSE
      break
    }
    ## assuming sorted
    curdf[ii, ] <- newrow
    avail_spc <- avail_spc + cwidthii - newrow$colwidth
  }

  if (verbose) {
    if (success) {
      msg <- paste(
        "overall number of label rows successfully reduced. Cols affected: ",
        paste(curdf$col_num[cols_to_pack], collapse = ", ")
      )
    } else {
      msg <- paste("Unable to reduce label rows required.")
    }
  }

  if (!success) {
    curdf <- olddf
  }
  curdf
}

optimal_widths <- function(possdf, tot_spaces = 320, max_lbl_lines = 3, verbose = FALSE) {
  odf <- order(possdf$col_num, possdf$colwidth)
  possdf <- possdf[odf, ]
  badlbl <- which(possdf$lbl_lines > max_lbl_lines)
  if (length(badlbl > 0)) {
    if (verbose) {
      message("Excluding ", length(badlbl), " column widths for labels taking over ", max_lbl_lines, " lines.")
    }
    possdf <- possdf[-badlbl, ]
    possdf <- possdf[possdf$lbl_lines <= max_lbl_lines, , drop = FALSE]
  }
  full_possdf <- possdf
  ## already ordered by colnum then width so this the first of each colwidth is the min width for that col
  dups <- duplicated(possdf$col_num)
  curdf <- possdf[!dups, ]
  possdf <- possdf[dups, ] ## without rows for ones in curdf
  spcleft <- tot_spaces - sum(curdf$colwidth)
  if (verbose) {
    message(
      "Optimizng Column Widths\n",
      "Initial lines required: ",
      max(curdf$lines),
      "\n",
      "Available adjustment: ",
      spcleft,
      " spaces\n"
    )
  }
  done <- FALSE
  while (!done) {
    oldwdths <- curdf$colwidth
    curdf <- constrict_lbl_lns(curdf, possdf, verbose = verbose)
    if (all.equal(curdf$colwidth, oldwdths)) {
      done <- TRUE
    }
  }
  change <- TRUE
  while (spcleft > 0 && change && nrow(possdf) > 0) {
    change <- FALSE
    ii <- which.max(curdf$cell_lines)
    spcleft <- tot_spaces - sum(curdf$colwidth)
    colii <- curdf$col_num[ii]
    bef_lns <- curdf$cell_lines[ii]
    bef_width <- curdf$colwidth[ii]
    nextlns <- max(curdf$cell_lines[-ii])
    cand_row_cond <- possdf$col_num == colii & possdf$cell_lines < bef_lns & possdf$colwidth - bef_width <= spcleft
    canddf <- possdf[cand_row_cond, , drop = FALSE]
    if (nrow(canddf) > 0) {
      more_than_next <- canddf$cell_lines >= nextlns
      if (any(more_than_next)) {
        candrow <- canddf[max(which(more_than_next)), ]
      } else {
        candrow <- canddf[nrow(canddf), ]
      }

      if (verbose) {
        message(
          "COL ",
          colii,
          " width: ",
          bef_width,
          "->",
          candrow$colwidth,
          " lines req: ",
          bef_lns,
          "->",
          candrow$cell_lines
        )
      }
      change <- TRUE
      curdf[ii, ] <- candrow
    }
  }
  curdf
}


one_col_strs <- function(strcol, col_gap = 2, fontspec) {
  strspls <- strsplit(strcol, split = "(-| (?=[^/]))", perl = TRUE)
  strspl_widths <- lapply(strspls, nchar_ttype, fontspec = fontspec, raw = TRUE)
  lbound_raw <- max(unlist(strspl_widths))
  lbound <- ceiling(lbound_raw + 2 * col_gap)
  ret <- calc_poss_lines(strspl_widths, lbound, 50, fontspec = fontspec)
  ret
}

## we have permission from the formatters maintainer to use this
## unexported function
j_mf_col_widths <- utils::getFromNamespace("mf_col_widths", "formatters")

#' @name def_colwidths
#'
#' @title Define Column Widths
#'
#' @description
#' `def_colwidths` uses heuristics to determine suitable column widths given a
#' table or listing, and a font.
#'
#' @param tt input TableTree
#' @param fontspec Font specification
#' @param label_width_ins Label Width in Inches.
#' @param col_gap Column gap in spaces. Defaults to `.5` for listings and `3`
#'   for tables.
#' @param type Type of the TableTree, used to determine column width calculation method.
#'
#' @details Listings are assumed to be rendered landscape on standard A1 paper,
#'   such that all columns are rendered on one page. Tables are allowed to
#'   be horizontally paginated, and column widths are determined based only on
#'   required word wrapping. See the `Automatic Column Widths` vignette for
#'   a detailed discussion of the algorithms used.
#' @return
#' * `def_colwidths`: a vector of column widths (including the label row pseudo-column in the table
#'   case) suitable for use rendering `tt` in the specified font.
#' @export
def_colwidths <- function(tt,
                          fontspec,
                          label_width_ins = 2,
                          col_gap = ifelse(type == "Listing", .5, 3),
                          type = tlg_type(tt)) {
  if (type == "Figure") {
    ret <- NULL
  } else if (type == "Table") {
    if (
      is.list(tt) &&
        !methods::is(tt, "MatrixPrintForm") &&
        !is.null(j_mf_col_widths(tt[[1]]))
    ) {
      ret <- j_mf_col_widths(tt[[1]])
    } else {
      ret <- no_cellwrap_colwidths(tt, fontspec, col_gap = col_gap, label_width_ins = label_width_ins)
    }
  } else {
    ret <- listing_column_widths(tt, fontspec = fontspec, col_gap = col_gap)
  }
  ret
}
