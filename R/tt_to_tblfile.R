#' @title Create TableTree as DataFrame via gentlg
#'
#' @param tt TableTree object to convert to a data frame
#' @param fontspec Font specification object
#' @param string_map Unicode mapping for special characters
#' @param markup_df Data frame containing markup information
#' @return `tt` represented as a "tbl" data.frame suitable for passing
#'   to [tidytlg::gentlg] via the `huxme` argument.
tt_to_tbldf <- function(
    tt,
    fontspec = font_spec("Times", 9L, 1),
    string_map = default_str_map,
    markup_df = dps_markup_df) {
  if (!validate_table_struct(tt)) {
    stop(
      "invalid table structure. summarize_row_groups without ",
      "analyze below it in layout structure?"
    )
  }
  mpf <- matrix_form(
    tt,
    indent_rownames = FALSE,
    expand_newlines = FALSE,
    fontspec = fontspec
  )

  strmat <- mf_strings(mpf)
  strmat[, 1] <- gsub("^[[:space:]]+", "", strmat[, 1])
  nhl <- mf_nlheader(mpf)
  strbody <- strmat[-(1:nhl), , drop = FALSE]
  row_type <- apply(
    strbody,
    1,
    function(x) {
      nonempty <- nzchar(x)
      if (all(!nonempty)) {
        ## shouldn't ever happen
        "EMPTY"
      } else if (nonempty[1] && all(!nonempty[-1])) {
        # only 1st cell nonempty
        "HEADER"
      } else {
        if (x[1] == "N") {
          "N"
        } else {
          "VALUE"
        }
      }
    }
  )
  rowdf <- mf_rinfo(mpf)
  rinds <- mf_lgrouping(mpf)[-(1:nhl)] -
    mf_nrheader(mpf)
  indentme <- rowdf$indent[rinds]
  anbr <- cumsum(!is.na(c(0, utils::head(rowdf$trailing_sep, -1))))[rinds] + 1 ## so it starts at 1
  roworder <- seq_len(NROW(rowdf)) - (anbr - 1)
  newrows <- c(0, ifelse(utils::tail(anbr, -1) == utils::head(anbr, -1), 0, 1))

  strbody <- prep_strs_for_rtf(strbody, string_map, markup_df)
  tbldf <- as.data.frame(strbody)

  names(tbldf) <- c("label", paste("col", seq_len(ncol(tbldf) - 1)))
  emptycols <- tbldf[1, ]
  emptycols[1, ] <- ""

  tbldf <- cbind(
    tbldf,
    data.frame(
      row_type = row_type,
      anbr = anbr,
      indentme = indentme,
      roworder = roworder,
      newrows = newrows,
      stringsAsFactors = FALSE
    )
  )
  tbldf
}

brackets_to_rtf <- function(strs) {
  gsub("\\[\\[([^]]+)\\]\\]", "\\\\{\\1}", strs)
}

gutter_width <- .12 # inches

## this is for Times New Roman 9
mar_plus_gutters <- 2 + gutter_width

pg_width_by_orient <- function(landscape = FALSE) {
  fullpg <- ifelse(landscape, 11, 8.5)
  fullpg - mar_plus_gutters
}

tlg_type <- function(tt) {
  if (methods::is(tt, "list") && !methods::is(tt, "listing_df")) {
    tt <- tt[[1]]
  }
  if (methods::is(tt, "ggplot")) {
    ret <- "Figure"
  } else if (methods::is(tt, "VTableTree")) {
    ret <- "Table"
  } else if (methods::is(tt, "listing_df")) {
    ret <- "Listing"
  } else {
    stop(
      "unable to determine tlg type for object of class: ",
      paste(class(tt), collapse = ",")
    )
  }
  ret
}

mpf_to_colspan <- function(
    mpf,
    string_map = default_str_map,
    markup_df = dps_markup_df) {
  if (!methods::is(mpf, "MatrixPrintForm")) {
    stop("figure out how to make this an mpf (MatrixPrintForm) first.")
  }
  strmat <- mf_strings(mpf)
  ## spaces in column header will get underlines, but we dont' want them to
  nhl <- mf_nlheader(mpf)
  strmat[seq_len(nhl - 1), ] <- gsub(
    "^[[:space:]]+$",
    "",
    strmat[seq_len(nhl - 1), ]
  )
  strmat[nhl, ] <- ifelse(nzchar(strmat[nhl, ]), strmat[nhl, ], " ")
  strmat <- prep_strs_for_rtf(strmat, string_map, markup_df)
  nspancols <- nhl - 1L
  if (nspancols > 0) {
    csph <- lapply(seq_len(nspancols), function(ii) {
      spns <- mf_spans(mpf)[ii, ]
      vals <- strmat[ii, ]
      jj <- 1 ## could start at 2 cause 1 is row label/topleft space but why complicate thigns
      ind <- 1
      myrle <- rle(vals)
      while (jj < length(vals)) {
        if (myrle$lengths[ind] == 1) {
          jj <- jj + 1
          ind <- ind + 1
        } else {
          sq <- seq(jj, jj + myrle$lengths[ind] - 1)
          valspns <- spns[sq]
          if (valspns[1] != length(sq)) {
            vals[sq] <- split_val_spans(vals[jj], valspns)
          }
          jj <- tail(sq, 1) + 1
          ind <- ind + 1
        }
      }
      vals
    })
  } else {
    csph <- NULL
  }

  list(colspan = csph, colheader = strmat[nhl, ])
}

split_val_spans <- function(val, spns) {
  if (val == "") {
    return(rep(val, length(spns)))
  }
  padvec <- rep("", length(spns))
  j <- 1
  padnum <- 0
  while (j < length(spns)) {
    onespn <- spns[j]
    if (padnum > 0) {
      padvec[j:(j + onespn - 1)] <- strrep(" ", padnum)
    }
    padnum <- padnum + 1
    j <- j + onespn
  }
  paste0(padvec, val, padvec)
}

partmpf_to_colinds <- function(fullmpf, partmpf) {
  stopifnot(
    mf_nlheader(fullmpf) == mf_nlheader(partmpf),
    mf_ncol(fullmpf) >= mf_ncol(partmpf)
  )
  nlh <- mf_nlheader(fullmpf)
  fullhdr <- mf_strings(fullmpf)[1:nlh, , drop = FALSE]
  parthdr <- mf_strings(partmpf)[1:nlh, , drop = FALSE]
  fullkeys <- apply(fullhdr, 2, paste, collapse = "", simplify = TRUE)
  ## hack to force first key to match uniquely
  fullkeys[1] <- paste(c("xxx", fullkeys[-1]), collapse = "")
  stopifnot(length(fullkeys) == length(unique(fullkeys)))
  partkeys <- apply(parthdr, 2, paste, collapse = "", simplify = TRUE)
  partkeys[1] <- fullkeys[1]
  match(partkeys, fullkeys)
}

subset_border_mat <- function(full_brdr, full_mpf, part_mpf) {
  if (is.null(full_brdr)) {
    return(NULL)
  }
  inds <- partmpf_to_colinds(full_mpf, part_mpf)
  full_brdr[, inds, drop = FALSE]
}


get_ncol <- function(tt) {
  if (is(tt, "listing_df") || is(tt, "VTableTree")) {
    ncol(tt)
  } else if (is(tt, "MatrixPrintForm")) {
    mf_ncol(tt)
  } else if (is.list(tt)) {
    if (is(tt[[1]], "MatrixPrintForm")) {
      mf_ncol(tt[[1]])
    } else {
      ncol(tt[[1]])
    }
  }
}

#' @name tt_to_tlgrtf
#' @title TableTree to .rtf Conversion
#' @description
#' A function to convert TableTree to .rtf
#' @details
#' This function aids in converting the rtables TableTree into the desired .rtf file.
#'
#' @param tt TableTree object to convert to RTF
#' @param file character(1). File to create, including path, but excluding
#' .rtf extension.
#' @param orientation Orientation of the output ("portrait" or "landscape")
#' @param colwidths Column widths for the table
#' @param label_width_ins Label width in inches
#' @param fontspec Font specification object
#' @param pg_width Page width in inches
#' @param margins Margins in inches (top, right, bottom, left)
#' @param paginate Whether to paginate the output
#' @param col_gap Column gap in spaces
#' @param verbose Whether to print verbose output
#' @param tlgtype Type of the output (Table, Listing, or Figure)
#' @param string_map Unicode mapping for special characters
#' @param markup_df Data frame containing markup information
#' @param ... Additional arguments passed to gentlg
#' @inheritParams tidytlg::gentlg
#' @param nosplitin list(row=, col=). Path elements whose children should not be paginated within
#' if it can be avoided. e.g., list(col="TRT01A") means don't split within treatment arms unless
#' all the associated columns don't fit on a single page.
#' @param combined_rtf logical(1). In the case where the result is broken up into multiple
#' parts due to width, should a combined rtf file also be created. Defaults to `FALSE`.
#' @param one_table logical(1). If `tt` is a (non-`MatrixPrintForm`) list,
#' should the parts be added to the rtf within a single table (`TRUE`, the
#' default) or as separate tables. End users will not generally need to set this.
#' @param border_mat matrix. A `m x k` matrix where m is the number of columns of `tt`
#'  and k is the number of lines the header takes up. See [tidytlg::add_bottom_borders]
#'  for what the matrix should contain. Users should only specify this when the
#'  default behavior does not meet their needs.
#' @import rlistings
#' @rdname tt_to_tlgrtf
#' @export
#' @seealso Used in all table and listing scripts
#' @note `file` should always include path. Path will be extracted
#' and passed separately to `gentlg`.
#' @note When `one_table` is `FALSE`, only the width of the row label
#'   pseudocolumn can be directly controlled due to a limitation in
#'   `tidytlg::gentlg`. The proportion of the full page that the first value
#'   in colwidths would take up is preserved and all other columns equally
#'   split the remaining available width. This will cause, e.g., the
#'   elements within the allparts rtf generated when `combined_rtf` is `TRUE`
#'   to differ visually from the content of the individual part rtfs.
#' @return If `file` is non-NULL, this is called for the side-effect of writing
#'   one or more RTF files. Otherwise, returns a list of `huxtable` objects.
tt_to_tlgrtf <- function(
    tt,
    file = NULL,
    orientation = c("portrait", "landscape"),
    colwidths = def_colwidths(
      tt,
      fontspec,
      col_gap = col_gap,
      label_width_ins = label_width_ins,
      type = tlgtype
    ),
    label_width_ins = 2,
    watermark = NULL,
    pagenum = ifelse(tlgtype == "Listing", TRUE, FALSE),
    fontspec = font_spec("Times", 9L, 1.2),
    pg_width = pg_width_by_orient(orientation == "landscape"),
    margins = c(0, 0, 0, 0),
    paginate = tlg_type(tt) == "Table",
    col_gap = ifelse(tlgtype == "Listing", .5, 3),
    nosplitin = list(
      row = character(),
      col = character()
    ),
    verbose = FALSE,
    tlgtype = tlg_type(tt),
    string_map = default_str_map,
    markup_df = dps_markup_df,
    combined_rtf = FALSE,
    one_table = TRUE,
    border_mat = make_header_bordmat(obj = tt),
    ...) {
  orientation <- match.arg(orientation)
  newdev <- open_font_dev(fontspec)
  if (newdev) {
    on.exit(close_font_dev())
  }

  if (tlgtype == "Listing" && nrow(tt) == 0) {
    dat <- as.list(c("No data to report", rep("", ncol(tt) - 1)))
    names(dat) <- names(tt)
    df <- as.data.frame(dat)
    var_labels(df) <- var_labels(tt)

    tt <- as_listing(
      df,
      key_cols = get_keycols(tt),
      disp_cols = listing_dispcols(tt)
    )
  }

  if (tlgtype == "Table" && fontspec$size == 8) {
    opts <- options(
      tidytlg.fontsize.table.footnote = 8,
      tidytlg.fontsize.table = 8
    )
    on.exit(options(opts), add = TRUE)
  }

  if (length(colwidths) == 1) {
    nc <- get_ncol(tt)
    tot_width <- page_lcpp(
      pg_width = pg_width,
      pg_height = 20, # don't care about this,
      font_family = fontspec$family,
      font_size = fontspec$size,
      lineheight = fontspec$lineheight,
      margins = c(0, 0, 0, 0),
      landscape = orientation == "landscape"
    )$cpp
    wdth_left <- tot_width - colwidths
    colwidths <- c(colwidths, rep(floor(wdth_left / nc), nc))
  }

  max_lwidth <- inches_to_spaces(label_width_ins, fontspec)
  if (colwidths[1] > max_lwidth) {
    colwidths[1] <- max_lwidth
  }

  if (!requireNamespace("tidytlg")) {
    stop("tidytlg not installed, cannot go out to rtf.")
  }

  if (paginate) {
    ## implies type Table
    if (tlgtype != "Table") {
      stop(
        "pagination is not currently supported for tlg types other than Table."
      )
    }
    if (methods::is(tt, "VTableTree")) {
      hdrmpf <- matrix_form(tt[1, ])
    } else if (methods::is(tt, "list") && methods::is(tt[[1]], "MatrixPrintForm")) {
      hdrmpf <- tt[[1]]
    } else {
      hrdmpf <- tt
    }
    pags <- paginate_to_mpfs(
      tt,
      fontspec = fontspec,
      landscape = orientation == "landscape",
      colwidths = colwidths,
      col_gap = col_gap,
      pg_width = pg_width,
      pg_height = NULL,
      margins = margins,
      lpp = NULL,
      nosplitin = nosplitin,
      verbose = verbose
    ) ##
    if (has_force_pag(tt)) {
      nslices <- which(
        cumsum(vapply(pags, mf_ncol, 1L)) == ncol(tt)
      )
      oldpags <- pags
      pags <- lapply(
        seq_len(nslices),
        function(ii) {
          inds <- seq(ii, by = nslices, length.out = length(oldpags) / nslices)
          oldpags[inds]
        }
      )
    }
    pag_bord_mats <- lapply(
      seq_along(pags),
      function(i) {
        if (methods::is(pags[[i]], "MatrixPrintForm")) {
          partmpf <- pags[[i]]
        } else {
          partmpf <- pags[[i]][[1]]
        }
        subset_border_mat(border_mat, hdrmpf, partmpf)
      }
    )
    ret <- lapply(
      seq_along(pags),
      function(i) {
        if (!is.null(file) && length(pags) > 1) {
          fmti <- paste0("%0", ceiling(log(length(pags), base = 10)), "d")
          fname <- paste0(file, "part", sprintf(fmti, i), "of", length(pags))
        } else {
          fname <- file
        }
        full_pag_i <- pags[[i]]
        if (
          is.list(full_pag_i) &&
            !methods::is(full_pag_i, "MatrixPrintForm")
        ) {
          pgi_for_cw <- full_pag_i[[1]]
        } else {
          pgi_for_cw <- full_pag_i
        }
        tt_to_tlgrtf(
          full_pag_i,
          file = fname,
          orientation = orientation,
          colwidths = j_mf_col_widths(pgi_for_cw),
          fontspec = fontspec,
          watermark = watermark,
          col_gap = col_gap,
          paginate = FALSE,
          tlgtype = tlgtype,
          string_map = string_map,
          markup_df = markup_df,
          border_mat = pag_bord_mats[[i]],
          ...
        )
      }
    )
    if (combined_rtf) {
      if (length(pags) > 1) {
        tt_to_tlgrtf(
          pags,
          file = paste0(file, "allparts"),
          orientation = orientation,
          fontspec = fontspec,
          watermark = watermark,
          col_gap = col_gap,
          paginate = FALSE,
          tlgtype = "Table",
          string_map = string_map,
          markup_df = markup_df,
          one_table = FALSE,
          ## gentlg isn't vectorized on column widths so we're SOL here...
          colwidths = colwidths, ## this is largely ignored see note in docs
          # colwidths are already on the pags since they are mpfs
          border_mat = pag_bord_mats,
          ...
        )
      } else if (!is.null(file)) { # only one page after pagination
        message(
          "Table ",
          basename(file),
          ": No combined RTF created, output fit within one part."
        )
      }
    }
    if (is.null(file) && length(pags) > 1) {
      ret <- unlist(ret, recursive = FALSE)
    }
    return(ret)
  }

  if (tlgtype == "Table") {
    if (is.list(tt) && !methods::is(tt, "MatrixPrintForm")) {
      df <- lapply(
        tt,
        tt_to_tbldf,
        fontspec = fontspec,
        string_map = string_map,
        markup_df = markup_df
      )
      if (one_table) {
        df <- do.call(
          rbind,
          lapply(
            seq_along(df),
            function(ii) {
              dfii <- df[[ii]]
              dfii$newpage <- 0
              if (ii > 1) {
                dfii$newpage[1] <- 1
              }
              dfii$indentme <- ifelse(dfii$indentme <= 1, 0, dfii$indentme - 1)
              dfii
            }
          )
        )
      }
    } else {
      df <- tt_to_tbldf(
        tt,
        fontspec = fontspec,
        string_map = string_map,
        markup_df = markup_df
      )
    }
  } else {
    df <- tt[, listing_dispcols(tt)]
  }

  ## we only care about the col labels here...
  if (tlgtype == "Table" && is.list(tt) && !methods::is(tt, "MatrixPrintForm")) {
    mpf <- tt[[1]]
    if (!one_table) {
      colinfo <- lapply(
        tt,
        mpf_to_colspan,
        markup_df = markup_df,
        string_map = string_map
      )
      csph <- lapply(colinfo, function(x) x$colspan)
      colheader <- lapply(colinfo, function(x) x$colheader)
    } else {
      colinfo <- mpf_to_colspan(
        mpf,
        markup_df = markup_df,
        string_map = string_map
      )
      csph <- colinfo$colspan
      colheader <- colinfo$colheader
    }
  } else if (methods::is(tt, "MatrixPrintForm")) {
    mpf <- tt
    colinfo <- mpf_to_colspan(
      mpf,
      markup_df = markup_df,
      string_map = string_map
    )
    csph <- colinfo$colspan
    colheader <- colinfo$colheader
  } else {
    mpf <- matrix_form(
      utils::head(tt, 1),
      indent_rownames = FALSE,
      expand_newlines = FALSE,
      fontspec = fontspec
    )
    colinfo <- mpf_to_colspan(
      mpf,
      markup_df = markup_df,
      string_map = string_map
    )
    csph <- colinfo$colspan
    colheader <- colinfo$colheader
  }

  if (is.null(file)) {
    fname <- NULL
    fpath <- tempdir()
  } else {
    fname <- basename(file)
    ## dirname on "table" returns "." so we're good using
    ## this unconditionally as opath
    fpath <- dirname(file)
  }

  if (tlgtype == "Table") {
    colwidths <- cwidths_final_adj(
      labwidth_ins = label_width_ins,
      total_width = pg_width,
      colwidths = colwidths[-1]
    )
  }
  colwidths <- colwidths / sum(colwidths)
  # finite precision arithmetic is a dreamscape of infinite wonder...
  ## sum(rep(1/18, 18)) <= 1 is FALSE...
  if (sum(colwidths) > 1) {
    colwidths <- colwidths - 0.00000000001 ## much smaller than a twip = 1/20 printing point
  }

  if (!one_table && # nolint start
    is.list(tt) && !is(tt, "MatrixPrintForm")) {
    ### gentlg is not vectorized on wcol.  x.x x.x x.x
    ### but it won't break if we only give it one number...
    ### Calling this an ugly hack is an insult to all the hard working hacks
    ### out there
    colwidths <- colwidths[1]
  } # nolint end

  footer_val <- prep_strs_for_rtf(
    c(
      main_footer(mpf),
      prov_footer(mpf)
    ),
    string_map,
    markup_df
  )
  if (length(footer_val) == 0) {
    footer_val <- NULL
  }

  if (!is.null(fname) && tlgtype == "Table" && is.data.frame(df)) {
    utils::write.csv(
      df,
      file = file.path(fpath, paste0(tolower(fname), ".csv")),
      row.names = FALSE
    )
  }

  tidytlg::gentlg(
    df,
    tlf = tlgtype,
    format = "rtf",
    idvars = if (tlgtype == "Listing") get_keycols(tt) else NULL,
    colspan = csph,
    file = fname,
    opath = fpath,
    colheader = colheader,
    title = prep_strs_for_rtf(
      main_title(mpf),
      string_map,
      markup_df
    ),
    footers = footer_val,
    orientation = orientation,
    wcol = colwidths,
    watermark = watermark,
    pagenum = pagenum,
    bottom_borders = border_mat,
    print.hux = !is.null(fname),
    ...
  )
}

## NB x/(x+sum(colwidths)) = labwidth_ins/total_width
cwidths_final_adj <- function(labwidth_ins, total_width, colwidths) {
  prop <- labwidth_ins / total_width
  lwidth <- floor(prop / (1 - prop) * sum(colwidths))
  c(lwidth, colwidths)
}

make_bordmat_row <- function(rowspns) {
  havespn <- rowspns > 1
  if (!any(havespn)) {
    return(rep(0, times = length(rowspns)))
  }

  pos <- 1
  ngrp <- 1
  ret <- numeric(length(rowspns))
  while (pos < length(rowspns)) {
    spnval <- rowspns[pos]
    if (spnval > 1) {
      multipos <- seq(pos, pos + spnval - 1)
      val <- ngrp
    } else {
      multipos <- pos
      val <- 0
    }
    ret[multipos] <- val
    pos <- pos + spnval
    ngrp <- ngrp + 1
  }
  ret
}

fixup_bord_mat <- function(brdmat, hstrs) {
  ## no lines between labels and their counts
  countcells <- matrix(
    grepl("N=", hstrs, fixed = TRUE),
    nrow = nrow(hstrs),
    ncol = ncol(hstrs)
  )

  countcoords <- which(countcells, arr.ind = TRUE)
  for (i in seq_len(nrow(countcoords))) {
    brdmat[countcoords[i, "row"] - 1, countcoords[i, "col"]] <- 0
  }

  brdmat[!nzchar(hstrs) | hstrs == " "] <- 0
  brdmat[nrow(brdmat), ] <- 1
  brdmat[seq_len(nrow(brdmat) - 1), 1] <- 0
  brdmat
}

.make_header_bordmat <- function(
    obj,
    mpf = matrix_form(utils::head(obj, 1), expand_newlines = FALSE)) {
  spns <- mf_spans(mpf)
  nlh <- mf_nlheader(mpf)
  nrh <- mf_nrheader(mpf)
  stopifnot(nlh == nrh)

  hstrs <- mf_strings(mpf)[seq_len(nrh), , drop = FALSE]
  spns <- mf_spans(mpf)[seq_len(nrh), , drop = FALSE]

  brdmat <- do.call(
    rbind,
    lapply(
      seq_len(nrh),
      function(i) make_bordmat_row(spns[i, ])
    )
  )

  brdmat <- fixup_bord_mat(brdmat, hstrs)
  brdmat
}

setGeneric(
  "make_header_bordmat",
  function(
      obj,
      mpf = matrix_form(utils::head(obj, 1), expand_newlines = FALSE)) {
    standardGeneric("make_header_bordmat")
  }
)

setMethod(
  "make_header_bordmat",
  c(obj = "ANY", mpf = "MatrixPrintForm"),
  .make_header_bordmat
)

setMethod(
  "make_header_bordmat",
  c(obj = "listing_df"),
  function(obj, mpf) matrix(1, nrow = 1, ncol = length(listing_dispcols(obj)))
)

setMethod(
  "make_header_bordmat",
  c(obj = "VTableTree", mpf = "missing"),
  function(obj, mpf) {
    make_header_bordmat(
      mpf = matrix_form(
        utils::head(obj, 1),
        expand_newlines = FALSE
      )
    )
  }
)
