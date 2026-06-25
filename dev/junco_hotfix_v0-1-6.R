# ==========================================
# Junco Hotfix Patch File
# ==========================================
# Summary of injected hotfixes:
# - Hotfix #375 `tt_to_tlgrtf()` sometimes missed the titles (issue #373) (introduced in junco v0.1.6)
# - Hotfix #395 `def_colwidths()` crashes when long column labels (issue #281) (introduced in junco v0.1.6)
# ==========================================


# ==========================================
# Surgical Patches
# ==========================================

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
    round_type = obj_round_type(tt),
    alignments = list(),
    validate = TRUE,
    export_csv = FALSE,
    output_csv_directory = NULL,
    ...
) {
  
  checkmate::assert_flag(export_csv)
  checkmate::assert_character(output_csv_directory, null.ok = TRUE, len = 1)
  
  if (validate && tlgtype == "Table" && methods::is(tt, "VTableTree")) {
    if (!rtables::validate_table_struct(tt)) {
      message(
        "Invalid table structure detected. This may cause issues in the output. ",
        "The validation process failed, proceed with caution."
      )
    }
  } else if (!validate && tlgtype == "Table" && methods::is(tt, "VTableTree")) {
    if (rtables::validate_table_struct(tt)) {
      message(
        "Table structure validation succeeded. You should not need to set validate=FALSE."
      )
    }
  }
  
  if (tlgtype != "Listing") {
    pagenum <- FALSE
  }
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
      disp_cols = listing_dispcols(tt),
      main_title = attr(tt, "main_title"),
      main_footer = attr(tt, "main_footer")
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
      hdrmpf <- matrix_form(tt[1, , keep_topleft = TRUE], round_type = round_type)
    } else if (methods::is(tt, "list") && methods::is(tt[[1]], "MatrixPrintForm")) {
      hdrmpf <- tt[[1]]
    } else {
      hdrmpf <- tt
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
      verbose = verbose,
      round_type = round_type
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
          round_type = round_type,
          alignments = alignments,
          export_csv = export_csv,
          output_csv_directory = output_csv_directory,
          label_width_ins = label_width_ins,
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
          round_type = round_type,
          alignments = alignments,
          export_csv = export_csv,
          output_csv_directory = output_csv_directory,
          label_width_ins = label_width_ins,
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
        markup_df = markup_df,
        round_type = round_type
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
        markup_df = markup_df,
        round_type = round_type
      )
    }
  } else {
    df <- tt[, listing_dispcols(tt)]
    # apply formats and round_type and return df as a dataframe to input in gentlg
    df <- listingdf_dataframe_formats(df, round_type = round_type)
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
      rtables::head(tt, 1), #### TODO: hotfix #375 `tt_to_tlgrtf()` sometimes missed the titles (issue #373)
      indent_rownames = FALSE,
      expand_newlines = FALSE,
      fontspec = fontspec,
      round_type = round_type
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
  
  if (!one_table && # nolint start
      is.list(tt) && !is(tt, "MatrixPrintForm")) {
    # this should be technically always 1 but just in case
    num_repeated_cols <- ncol(tt[[1]]$strings) - ncol(tt[[1]])
    # the following lines will listify the vector colwidths, this is, convert it
    # to a list of vectors (one vector per page)
    l_colwidths <- list()
    j <- num_repeated_cols + 1
    for (i in seq_along(tt)) {
      subt_col_idxs <- j - 1 + seq(ncol(tt[[i]]))
      colwidths_subt <- colwidths[c(1:num_repeated_cols, subt_col_idxs)]
      ## jump current col position to first column on next page
      j <- tail(subt_col_idxs, 1) + 1
      l_colwidths[[i]] <- get_colwidths_as_proportions(colwidths_subt,
                                                       tlgtype,
                                                       label_width_ins,
                                                       pg_width)
    }
    colwidths <- l_colwidths
  } else {  # nolint end
    colwidths <- get_colwidths_as_proportions(colwidths,
                                              tlgtype,
                                              label_width_ins,
                                              pg_width)
  }
  
  
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
  
  if (!is.null(fname) && tlgtype == "Table" && is.data.frame(df) && export_csv) {
    
    output_csv_filename <- get_output_csv_filename(output_csv_directory, fpath, fname)
    
    utils::write.csv(
      df,
      file = output_csv_filename,
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
    alignments = alignments,
    footers_one_row = TRUE,
    ...
  )
}

export_TLG_as_docx <- function(
    obj = NULL,
    tblid = NULL,
    output_dir = NULL,
    theme = theme_docx_default_j(
      font = "Times New Roman",
      font_size = 9L,
      bold = NULL
    ),
    add_page_break = FALSE,
    titles_as_header = TRUE,
    integrate_footers = TRUE,
    section_properties = officer::prop_section(
      page_size = officer::page_size(width = 11, height = 8.5, orient = orientation),
      page_margins = officer::page_mar(bottom = 1, top = 1, right = 1, left = 1, gutter = 0, footer = 1, header = 1)
    ),
    doc_metadata = NULL,
    template_file = NULL,
    orientation = "portrait",
    paginate = tlgtype == "Table",
    nosplitin = list(
      row = character(),
      col = character()
    ),
    string_map = default_str_map,
    markup_df_docx = dps_markup_df_docx,
    combined_docx = FALSE,
    tlgtype = ifelse(is.null(obj), "Figure", tlg_type(obj)),
    col_gap = ifelse(tlgtype == "Listing", .5, 3),
    pagenum = ifelse(tlgtype == "Listing", TRUE, FALSE),
    round_type = ifelse(tlgtype %in% c("Table", "Listing"),
                        formatters::obj_round_type(obj),
                        "iec"),
    alignments = list(),
    border = flextable::fp_border_default(width = 0.875, color = "black"),
    border_mat = NULL,
    export_csv = FALSE,
    output_csv_directory = NULL,
    markup_df = dps_markup_df,
    validate = TRUE,
    watermark = NULL,
    plotnames = NULL,
    title = NULL,
    footers = NULL,
    plotwidth = 8,
    plotheight = 5.51,
    units = c("in", "cm", "mm", "px")[1],
    ...
) {
  
  if (tlgtype %in% c("Table", "Listing") && is.null(border_mat)) {
    border_mat <- make_header_bordmat(obj = obj)
  } else if (tlgtype == "Figure") {
    border_mat <- matrix()
  }
  
  # if 'obj' is provided, it must be of type "TableTree", "listing_df" or list of "ggplot"
  if (!is.null(obj)) {
    checkmate::assert_true(inherits(obj, "VTableTree") || inherits(obj, "listing_df") ||
                             ggplot2::is.ggplot(obj) || is.list(obj))
  }
  checkmate::assert_character(tblid, null.ok = TRUE)
  checkmate::assert_character(output_dir, len = 1, null.ok = TRUE)
  if (!is.null(output_dir)) {
    checkmate::assert_directory_exists(output_dir)
  }
  checkmate::assert_function(theme)
  checkmate::assert_flag(add_page_break)
  checkmate::assert_flag(titles_as_header)
  checkmate::assert_flag(integrate_footers)
  checkmate::assert_class(section_properties, "prop_section")
  checkmate::assert_character(template_file, len = 1, null.ok = TRUE)
  if (!is.null(template_file)) {
    checkmate::assert_file_exists(template_file)
  }
  checkmate::assert_choice(orientation, choices = c("portrait", "landscape"))
  checkmate::assert_flag(paginate)
  checkmate::assert_list(nosplitin)
  checkmate::assert_tibble(string_map)
  checkmate::assert_true(all(colnames(string_map) == c("pattern", "value")))
  checkmate::assert_tibble(markup_df_docx)
  checkmate::assert_true(all(colnames(markup_df_docx) == c("keyword", "replace_by")))
  checkmate::assert_flag(combined_docx)
  checkmate::assert_character(tlgtype)
  checkmate::assert_numeric(col_gap)
  checkmate::assert_flag(pagenum)
  checkmate::assert_choice(round_type, choices = c("sas", "iec"))
  checkmate::assert_list(alignments)
  checkmate::assert_class(border, "fp_border")
  checkmate::assert_matrix(border_mat)
  checkmate::assert_flag(export_csv)
  checkmate::assert_character(output_csv_directory, null.ok = TRUE, len = 1)
  checkmate::assert_tibble(markup_df)
  checkmate::assert_true(all(colnames(markup_df) == c("keyword", "rtfstart", "rtfend")))
  checkmate::assert_flag(validate)
  checkmate::assert_character(watermark, len = 1, null.ok = TRUE)
  checkmate::assert_character(plotnames, null.ok = TRUE)
  checkmate::assert_character(title, null.ok = TRUE)
  checkmate::assert_character(footers, null.ok = TRUE)
  checkmate::assert_numeric(plotwidth)
  checkmate::assert_numeric(plotheight)
  checkmate::assert_choice(units, choices = c("in", "cm", "mm", "px"))
  
  if (tlgtype %in% c("Table", "Listing")) {
    export_as_docx_j(
      tt = obj, tblid = tblid, output_dir = output_dir,
      theme = theme, add_page_break = add_page_break,
      titles_as_header = titles_as_header,
      integrate_footers = integrate_footers,
      section_properties = section_properties, doc_metadata = doc_metadata,
      template_file = template_file, orientation = orientation,
      paginate = paginate, nosplitin = nosplitin,
      string_map = string_map, markup_df_docx = markup_df_docx,
      combined_docx = combined_docx, tlgtype = tlgtype,
      col_gap = col_gap, pagenum = pagenum, round_type = round_type,
      alignments = alignments, border = border,
      border_mat = border_mat, watermark = watermark,
      export_csv = export_csv, output_csv_directory = output_csv_directory,
      markup_df = markup_df,
      validate = validate,
      ...
    )
  } else {
    export_graph_as_docx(g = obj, plotnames = plotnames,
                         tblid = tblid, output_dir = output_dir,
                         title = title, footers = footers,
                         orientation = orientation, plotwidth = plotwidth,
                         plotheight = plotheight, units = units, border = border,
                         watermark = watermark)
  }
  
}

export_as_docx_j <- function(
    tt,
    tblid = NULL,
    output_dir = NULL,
    theme = theme_docx_default_j(
      font = "Times New Roman",
      font_size = 9L,
      bold = NULL
    ),
    add_page_break = FALSE,
    titles_as_header = TRUE,
    integrate_footers = TRUE,
    section_properties = officer::prop_section(
      page_size = officer::page_size(width = 11, height = 8.5, orient = orientation),
      page_margins = officer::page_mar(bottom = 1, top = 1, right = 1, left = 1, gutter = 0, footer = 1, header = 1)
    ),
    doc_metadata = NULL,
    template_file = NULL,
    orientation = "portrait",
    paginate = tlgtype == "Table",
    nosplitin = list(
      row = character(),
      col = character()
    ),
    string_map = default_str_map,
    markup_df_docx = dps_markup_df_docx,
    combined_docx = FALSE,
    tlgtype = tlg_type(tt),
    col_gap = ifelse(tlgtype == "Listing", .5, 3),
    pagenum = ifelse(tlgtype == "Listing", TRUE, FALSE),
    round_type = formatters::obj_round_type(tt),
    alignments = list(),
    border = flextable::fp_border_default(width = 0.875, color = "black"),
    border_mat = make_header_bordmat(obj = tt),
    watermark = NULL,
    export_csv = FALSE,
    output_csv_directory = NULL,
    markup_df = dps_markup_df,
    validate = TRUE,
    ...
) {
  
  checkmate::assert_true(inherits(tt, "VTableTree") || inherits(tt, "listing_df") ||
                           is.list(tt) || inherits(tt, "flextable"))
  checkmate::assert_character(tblid, null.ok = TRUE)
  checkmate::assert_character(output_dir, len = 1, null.ok = TRUE)
  if (!is.null(output_dir)) {
    checkmate::assert_directory_exists(output_dir)
  }
  checkmate::assert_function(theme)
  checkmate::assert_flag(add_page_break)
  checkmate::assert_flag(titles_as_header)
  checkmate::assert_flag(integrate_footers)
  checkmate::assert_class(section_properties, "prop_section")
  checkmate::assert_character(template_file, len = 1, null.ok = TRUE)
  if (!is.null(template_file)) {
    checkmate::assert_file_exists(template_file)
  }
  checkmate::assert_choice(orientation, choices = c("portrait", "landscape"))
  checkmate::assert_flag(paginate)
  checkmate::assert_list(nosplitin)
  checkmate::assert_tibble(string_map)
  checkmate::assert_true(all(colnames(string_map) == c("pattern", "value")))
  checkmate::assert_tibble(markup_df_docx)
  checkmate::assert_true(all(colnames(markup_df_docx) == c("keyword", "replace_by")))
  checkmate::assert_flag(combined_docx)
  checkmate::assert_character(tlgtype)
  checkmate::assert_numeric(col_gap)
  checkmate::assert_flag(pagenum)
  checkmate::assert_choice(round_type, choices = c("sas", "iec"))
  checkmate::assert_list(alignments)
  checkmate::assert_class(border, "fp_border")
  checkmate::assert_matrix(border_mat)
  checkmate::assert_tibble(markup_df)
  checkmate::assert_true(all(colnames(markup_df) == c("keyword", "rtfstart", "rtfend")))
  
  # Validate `alignments` here because of its complicated data structure
  stopifnot("`alignments` must be a list" = is.list(alignments))
  for (alignment in alignments) {
    stopifnot("Each item of `alignments` must be a list" = is.list(alignment))
  }
  
  checkmate::assert_flag(add_page_break)
  checkmate::assert_character(watermark, len = 1, null.ok = TRUE)
  checkmate::assert_flag(export_csv)
  checkmate::assert_character(output_csv_directory, null.ok = TRUE, len = 1)
  
  if (is.null(tblid) || is.null(output_dir)) {
    temp_file <- tempfile()
    tblid <- basename(temp_file)
    output_dir <- dirname(temp_file)
  }
  
  do_tt_error <- FALSE
  if (tlgtype != "Listing") {
    pagenum <- FALSE
  }
  
  if (inherits(tt, "VTableTree") || inherits(tt, "listing_df")) {
    tt <- tt_to_flextable_j(tt,
                            titles_as_header = titles_as_header,
                            integrate_footers = integrate_footers,
                            tblid = tblid,
                            orientation = orientation,
                            paginate = paginate,
                            nosplitin = nosplitin,
                            string_map = string_map,
                            markup_df_docx = markup_df_docx,
                            tlgtype = tlgtype,
                            col_gap = col_gap,
                            theme = theme,
                            round_type = round_type,
                            alignments = alignments,
                            border = border,
                            border_mat = border_mat,
                            export_csv = export_csv,
                            output_csv_directory = output_csv_directory,
                            markup_df = markup_df,
                            output_dir = output_dir, # this argument is needed to guess where to save the csv
                            validate = validate,
                            ...
    )
  }
  if (inherits(tt, "flextable")) {
    flex_tbl_list <- list(tt)
  } else if (inherits(tt, "list")) {
    if (inherits(tt[[1]], "VTableTree") || inherits(
      tt[[1]],
      "listing_df"
    )) {
      flex_tbl_list <- mapply(tt_to_flextable_j,
                              tt = tt,
                              MoreArgs =
                                list(
                                  titles_as_header = titles_as_header,
                                  integrate_footers = integrate_footers,
                                  tblid = tblid,
                                  orientation = orientation,
                                  paginate = paginate,
                                  nosplitin = nosplitin,
                                  string_map = string_map,
                                  markup_df_docx = markup_df_docx,
                                  tlgtype = tlgtype,
                                  col_gap = col_gap,
                                  theme = theme,
                                  round_type = round_type,
                                  alignments = alignments,
                                  border = border,
                                  border_mat = border_mat,
                                  export_csv = export_csv,
                                  output_csv_directory = output_csv_directory,
                                  markup_df = markup_df,
                                  output_dir = output_dir, # this argument is needed to guess where to save the csv
                                  validate = validate,
                                  ...
                                ),
                              SIMPLIFY = FALSE
      )
    } else if (inherits(tt[[1]], "flextable") ||
               (inherits(tt[[1]], "list") && inherits(tt[[1]][[1]], "flextable"))) {
      flex_tbl_list <- tt
    } else {
      do_tt_error <- TRUE
    }
  } else {
    do_tt_error <- TRUE
  }
  if (isTRUE(do_tt_error)) {
    stop("tt must be a TableTree/listing_df, a flextable, or a list of TableTree/listing_df or flextable objects.")
  }
  if (isFALSE(titles_as_header) || isFALSE(integrate_footers)) {
    .extract_font_and_size_from_flx <- utils::getFromNamespace(
      ".extract_font_and_size_from_flx",
      "rtables.officer"
    )
    if (inherits(flex_tbl_list[[1]], "list")) {
      flx_fpt <- .extract_font_and_size_from_flx(flex_tbl_list[[1]][[1]])
    } else {
      flx_fpt <- .extract_font_and_size_from_flx(flex_tbl_list[[1]])
    }
  }
  
  if (combined_docx) {
    if (length(flex_tbl_list) > 1) {
      export_as_docx_j(
        tt = flex_tbl_list,
        output_dir = output_dir,
        theme = theme,
        add_page_break = TRUE,
        titles_as_header = titles_as_header,
        integrate_footers = integrate_footers,
        section_properties = section_properties,
        doc_metadata = doc_metadata,
        template_file = template_file,
        orientation = orientation,
        tblid = paste0(tblid, "allparts"),
        paginate = FALSE,
        string_map = string_map,
        markup_df_docx = markup_df_docx,
        combined_docx = FALSE,
        tlgtype = "Table",
        col_gap = col_gap,
        pagenum = pagenum,
        round_type = round_type,
        alignments = alignments,
        border = border,
        border_mat = border_mat,
        watermark = watermark,
        export_csv = export_csv,
        output_csv_directory = output_csv_directory,
        markup_df = markup_df,
        validate = validate,
        ...
      )
    } else {
      message(
        "Table ", tblid, ": No combined DOCX created, output fit within one part."
      )
    }
  }
  
  if (paginate && length(flex_tbl_list) > 1) {
    # export individual subtables as separate docx files
    for (i in seq(1, length(flex_tbl_list))) {
      flex_tbl_i <- flex_tbl_list[[i]]
      fmti <- paste0("%0", ceiling(log(length(flex_tbl_list), base = 10)), "d")
      fname <- paste0(tolower(tblid), "part", sprintf(fmti, i), "of", length(flex_tbl_list))
      export_as_docx_j(
        tt = flex_tbl_i,
        output_dir = output_dir,
        theme = theme,
        add_page_break = add_page_break,
        titles_as_header = titles_as_header,
        integrate_footers = integrate_footers,
        section_properties = section_properties,
        doc_metadata = doc_metadata,
        template_file = template_file,
        orientation = orientation,
        tblid = fname,
        paginate = FALSE,
        nosplitin = nosplitin,
        string_map = string_map,
        markup_df_docx = markup_df_docx,
        combined_docx = FALSE,
        tlgtype = tlgtype,
        col_gap = col_gap,
        pagenum = pagenum,
        round_type = round_type,
        alignments = alignments,
        border = border,
        border_mat = border_mat,
        watermark = watermark,
        export_csv = export_csv,
        output_csv_directory = output_csv_directory,
        markup_df = markup_df,
        validate = validate,
        ...
      )
    }
  } else {
    template_file <- get_template_file()
    doc <- officer::read_docx(template_file)
    doc <- officer::body_remove(doc)
    
    # NOTE: the following block adds the page numbering
    if (tlgtype == "Listing" && pagenum) {
      # nolint start
      formatted_par <- officer::fpar(
        "Listing Page ",
        officer::run_word_field("Page", prop = officer::fp_text(font.size = 8, font.family = "Times New Roman")),
        " of ",
        officer::run_word_field("NumPages", prop = officer::fp_text(font.size = 8, font.family = "Times New Roman")),
        fp_p = officer::fp_par(text.align = "right", padding.top = 12),
        fp_t = officer::fp_text(font.size = 8, font.family = "Times New Roman")
      )
      # nolint end
      footer_default <- officer::block_list(formatted_par)
      section_properties$footer_default <- footer_default
    }
    # END
    doc <- officer::body_set_default_section(doc, section_properties)
    
    flex_tbl_list <- lapply(flex_tbl_list, function(flx) {
      if (flx$properties$layout != "autofit") {
        page_width <- pg_width_by_orient(section_properties$page_size$orient == "landscape")
        # NOTE: here, even though page width is 8.88 inches, table width has
        # to be 8.82 inches, so leave a gap of 0.03 inches on both sides
        if (orientation == "landscape") {
          page_width <- (page_width - 0.03 * 2)
        }
        dflx <- dim(flx)
        if (abs(sum(unname(dflx$widths)) - page_width) > 0.01) {
          warning(
            "The total table width does not match the page width. The column widths",
            " will be resized to fit the page. Please consider modifying the parameter",
            " total_page_width in tt_to_flextable_j()."
          )
          final_cwidths <- page_width * unname(dflx$widths) / sum(unname(dflx$widths))
          flx <- flextable::width(flx, width = final_cwidths)
        }
      }
      flx
    })
    for (ii in seq(1, length(flex_tbl_list))) {
      flex_tbl_i <- flex_tbl_list[[ii]]
      if (endsWith(tblid, "allparts")) {
        tmp <- gsub(pattern = ".*:\t", replacement = paste0(tblid, ":\t"), flex_tbl_i$header$dataset[1, 1])
        flex_tbl_i <- flextable::compose(flex_tbl_i, part = "header", i = 1, value = flextable::as_paragraph(tmp))
        n_footer_lines <- flextable::nrow_part(flex_tbl_i, part = "footer")
        tmp <- gsub(
          pattern = "^\\[.*\\.docx\\]",
          replacement = paste0("[", tolower(tblid), ".docx]"),
          flex_tbl_i$footer$dataset[n_footer_lines, 1]
        )
        flex_tbl_i <- flextable::compose(flex_tbl_i,
                                         part = "footer",
                                         i = n_footer_lines,
                                         value = flextable::as_paragraph(tmp)
        )
      }
      # NOTE: this align = "left" is the alignment of the entire table relative to the page,
      # not the content within the table
      # calculate vertical pagination, where to place the new page breaks
      doc <- flextable::body_add_flextable(doc, flex_tbl_i, align = "center")
      if (isTRUE(add_page_break) && ii < length(flex_tbl_list)) {
        doc <- officer::body_add_break(doc)
      }
    }
    if (isFALSE(integrate_footers) && inherits(tt, "VTableTree")) {
      matform <- rtables::matrix_form(tt, indent_rownames = TRUE, round_type = round_type)
      if (length(matform$ref_footnotes) > 0) {
        chr_v <- matform$ref_footnotes
        text_format <- flx_fpt$fpt_footer
        for (ii in seq_along(chr_v)) {
          cur_fp <- officer::fpar(officer::ftext(chr_v[ii], prop = text_format))
          doc <- officer::body_add_fpar(doc, cur_fp)
        }
      }
      if (length(formatters::all_footers(tt)) > 0) {
        chr_v <- formatters::all_footers(tt)
        text_format <- flx_fpt$fpt_footer
        for (ii in seq_along(chr_v)) {
          cur_fp <- officer::fpar(officer::ftext(chr_v[ii], prop = text_format))
          doc <- officer::body_add_fpar(doc, cur_fp)
        }
      }
    }
    if (!is.null(doc_metadata)) {
      doc <- do.call(officer::set_doc_properties, c(
        list(x = doc),
        doc_metadata
      ))
    }
    
    add_hanging_indent_in_title_XML(doc)
    if (tlgtype == "Table") {
      n_rows_footer <- flextable::nrow_part(flex_tbl_list[[1]], part = "footer")
      insert_hanging_indent_first_col_XML(doc, n_rows_footer = n_rows_footer)
    }
    add_vertical_pagination_XML(doc)
    remove_security_popup_page_numbers_XML(doc, tlgtype, pagenum)
    remove_trHeight_nodes_XML(doc)
    if (!is.null(watermark)) {
      insert_fake_watermark_XML(doc, watermark, orientation, tlgtype)
    }
    
    doc <- align_rows_with_rtf(doc)
    
    print(doc, target = paste0(output_dir, "/", tolower(tblid), ".docx"))
    invisible(TRUE)
  }
}


tt_to_flextable_j <- function(
    tt,
    tblid = NULL,
    theme = theme_docx_default_j(font = "Times New Roman", font_size = 9L, bold = NULL),
    border = flextable::fp_border_default(width = 0.875, color = "black"),
    titles_as_header = TRUE,
    bold_titles = TRUE,
    integrate_footers = TRUE,
    counts_in_newline = FALSE,
    paginate = tlg_type(tt) == "Table",
    fontspec = formatters::font_spec("Times", 9L, 1.2),
    colwidths = NULL,
    label_width_ins = 2,
    total_page_width = pg_width_by_orient(orientation == "landscape"),
    orientation = "portrait",
    nosplitin = list(
      row = character(),
      col = character()
    ),
    string_map = default_str_map,
    markup_df_docx = dps_markup_df_docx,
    reduce_first_col_indentation = FALSE,
    tlgtype = tlg_type(tt),
    col_gap = ifelse(tlgtype == "Listing", .5, 3),
    round_type = formatters::obj_round_type(tt),
    alignments = list(),
    border_mat = make_header_bordmat(obj = tt),
    validate = TRUE,
    ...) {
  if (inherits(tt, "list")) {
    stop("Please use paginate = TRUE or mapply() to create multiple outputs. export_as_docx accepts lists.")
  }
  if (!inherits(tt, "VTableTree") && !inherits(tt, "listing_df")) {
    stop("Input object is not an rtables' or rlistings' object.")
  }
  
  checkmate::assert_true(inherits(tt, "VTableTree") || inherits(tt, "listing_df"))
  checkmate::assert_character(tblid, null.ok = TRUE)
  checkmate::assert_function(theme)
  checkmate::assert_class(border, "fp_border")
  checkmate::assert_flag(titles_as_header)
  checkmate::assert_flag(bold_titles)
  checkmate::assert_flag(integrate_footers)
  checkmate::assert_flag(counts_in_newline)
  checkmate::assert_flag(paginate)
  checkmate::assert_class(fontspec, "font_spec")
  checkmate::assert_numeric(colwidths, lower = 0, len = ncol(tt) + 1, null.ok = TRUE)
  checkmate::assert_numeric(label_width_ins)
  checkmate::assert_number(total_page_width, lower = 1)
  checkmate::assert_choice(orientation, choices = c("portrait", "landscape"))
  checkmate::assert_list(nosplitin)
  checkmate::assert_multi_class(x = string_map,
                                classes = c("tbl_df", "data.frame"))
  checkmate::assert_multi_class(x = markup_df_docx,
                                classes = c("tbl_df", "data.frame"))
  checkmate::assert_flag(reduce_first_col_indentation)
  checkmate::assert_character(tlgtype)
  checkmate::assert_numeric(col_gap)
  checkmate::assert_choice(round_type, choices = c("sas", "iec"))
  checkmate::assert_list(alignments)
  checkmate::assert_matrix(border_mat)
  
  # Validate `alignments` here because of its complicated data structure
  stopifnot("`alignments` must be a list" = is.list(alignments))
  for (alignment in alignments) {
    stopifnot("Each item of `alignments` must be a list" = is.list(alignment))
  }
  
  if (is.null(tblid)) {
    temp_file <- tempfile()
    tblid <- basename(temp_file)
  }
  
  validate_tabletree(tt, validate, tlgtype)
  
  if (tlgtype == "Listing" && nrow(tt) == 0) {
    dat <- as.list(c("No data to report", rep("", ncol(tt) - 1)))
    names(dat) <- names(tt)
    df <- as.data.frame(dat)
    formatters::var_labels(df) <- formatters::var_labels(tt)
    
    titles <- list()
    titles$title <- main_title(tt)
    titles$subtitles <- subtitles(tt)
    titles$main_footer <- main_footer(tt)
    titles$prov_footer <- prov_footer(tt)
    
    tt <- rlistings::as_listing(
      df,
      key_cols = rlistings::get_keycols(tt),
      disp_cols = rlistings::listing_dispcols(tt)
    )
    tt <- set_titles(tt, titles)
  }
  
  if (is.null(colwidths)) {
    colwidths <- def_colwidths(
      tt,
      fontspec,
      col_gap = col_gap,
      label_width_ins = label_width_ins,
      type = tlgtype
    )
  }
  colwidths_2 <- colwidths
  if (tlgtype == "Table") {
    colwidths <- cwidths_final_adj(
      labwidth_ins = label_width_ins,
      total_width = total_page_width,
      colwidths = colwidths[-1]
    )
    colwidths <- (colwidths / sum(colwidths)) * total_page_width
  }
  
  
  if (paginate) {
    ## implies type Table
    if (tlgtype != "Table") {
      stop(
        "pagination is not currently supported for tlg types other than Table."
      )
    }
    if (methods::is(tt, "VTableTree")) {
      hdrmpf <- rtables::matrix_form(tt[1, , keep_topleft = TRUE], round_type = round_type)
    } else if (methods::is(tt, "list") && methods::is(tt[[1]], "MatrixPrintForm")) {
      hdrmpf <- tt[[1]]
    } else {
      hdrmpf <- tt
    }
    
    pags <- formatters::paginate_to_mpfs(
      tt,
      fontspec = fontspec,
      landscape = orientation == "landscape",
      colwidths = colwidths_2,
      col_gap = col_gap,
      pg_width = pg_width_by_orient(orientation == "landscape"),
      pg_height = NULL,
      margins = rep(0, 4),
      lpp = NULL,
      nosplitin = nosplitin,
      verbose = FALSE,
      round_type = round_type
    )
    if (rtables::has_force_pag(tt)) {
      nslices <- which(
        cumsum(vapply(pags, formatters::mf_ncol, 1L)) == ncol(tt)
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
        if (!is.null(tblid) && length(pags) > 1) {
          fmti <- paste0("%0", ceiling(log(length(pags), base = 10)), "d")
          fname <- paste0(tblid, "part", sprintf(fmti, i), "of", length(pags))
        } else {
          fname <- tblid
        }
        full_pag_i <- pags[[i]]
        # NOTE: if there is also vertical pagination, full_pag_i will have more than 1 page
        if (is.list(full_pag_i) && !methods::is(full_pag_i, "MatrixPrintForm")) {
          pgi_for_cw <- full_pag_i[[1]]
        } else {
          pgi_for_cw <- full_pag_i
        }
        
        # if there is no vertical pagination, make each horizontal pagination
        # a list of 1 page
        if (is.list(full_pag_i) && methods::is(full_pag_i, "MatrixPrintForm")) {
          full_pag_i <- list(full_pag_i)
        }
        
        # Note: slice the big table into multiple subtables, by rows and columns
        jj <- pgi_for_cw$col_info$label
        subt <- tt[, jj,
                   drop = FALSE, keep_titles = TRUE, keep_topleft = TRUE,
                   reindex_refs = FALSE
        ]
        
        # export csv
        args <- list(...)
        export_csv <- args$export_csv
        output_csv_directory <- args$output_csv_directory
        markup_df <- args$markup_df
        output_dir <- args$output_dir
        if (!isTRUE(export_csv)) {
          export_csv <- FALSE
        }
        export_as_csv(tlgtype = tlgtype,
                      export_csv = export_csv,
                      pags = full_pag_i,
                      fontspec = fontspec,
                      string_map = string_map,
                      markup_df = markup_df,
                      round_type = round_type,
                      output_csv_directory = output_csv_directory,
                      output_dir = output_dir,
                      fname = fname)
        
        # we want to decrease the indentation of each vertical pagination
        # only if for each vertical pagination only the first row has indentation == 0
        only_first_row_indent_zero <-
          all(lapply(full_pag_i, function(x) {
            tmp <- x$row_info
            tmp <- tmp[tmp$label != " " & tmp$indent == 0, ]
            return(nrow(tmp))
          }) == 1)
        
        sub_ft <- tt_to_flextable_j(
          tt = subt,
          theme = theme,
          border = border,
          titles_as_header = titles_as_header,
          bold_titles = bold_titles,
          integrate_footers = integrate_footers,
          counts_in_newline = counts_in_newline,
          paginate = FALSE,
          fontspec = fontspec,
          colwidths = colwidths_2[c(1, jj + 1)],
          total_page_width = total_page_width,
          orientation = orientation,
          tblid = fname,
          nosplitin = nosplitin,
          string_map = string_map,
          markup_df_docx = markup_df_docx,
          reduce_first_col_indentation = (length(full_pag_i) > 1 && only_first_row_indent_zero),
          tlgtype = tlgtype,
          col_gap = col_gap,
          round_type = round_type,
          alignments = alignments,
          border_mat = pag_bord_mats[[i]],
          export_csv = FALSE, # this is because we already exported the csv a few lines above,
          validate = validate,
          ...
        )
        
        return(sub_ft)
      }
    )
    
    if (is.null(file) && length(pags) > 1) {
      ret <- unlist(ret, recursive = FALSE)
    }
    
    if (length(ret) == 1) {
      ret <- ret[[1]]
    }
    return(ret)
  }
  
  
  matform <- rtables::matrix_form(
    tt,
    fontspec = fontspec,
    indent_rownames = FALSE,
    round_type = round_type
  )
  body <- formatters::mf_strings(matform)
  spans <- formatters::mf_spans(matform)
  mpf_aligns <- formatters::mf_aligns(matform)
  hnum <- formatters::mf_nlheader(matform)
  rdf <- rtables::make_row_df(tt)
  
  
  # NOTE: convert the '>=', '<=', etc symbols
  body <- strmodify(body, string_map)
  
  
  # NOTE:
  # calculate where to place the blank rows
  mpf <- matform
  rowdf <- formatters::mf_rinfo(mpf)
  nhl <- formatters::mf_nlheader(mpf)
  rinds <- formatters::mf_lgrouping(mpf)[-(1:nhl)] - formatters::mf_nrheader(mpf)
  anbr <- cumsum(!is.na(c(0, utils::head(rowdf$trailing_sep, -1))))[rinds] + 1 ## so it starts at 1
  newrows <- c(0, ifelse(utils::tail(anbr, -1) == utils::head(anbr, -1), 0, 1))
  # END
  
  if (any(grepl("dec", mpf_aligns))) {
    body <- formatters::decimal_align(body, mpf_aligns)
    mpf_aligns[mpf_aligns == "decimal"] <- "center"
    mpf_aligns[mpf_aligns == "dec_left"] <- "left"
    mpf_aligns[mpf_aligns == "dec_right"] <- "right"
  }
  if (tlgtype == "Listing") {
    # NOTE: left-align the first column for listings
    mpf_aligns[, 1] <- "left"
  }
  
  content <- as.data.frame(body[-seq_len(hnum), , drop = FALSE])
  content[content == ""] <- " "
  # NOTE:
  # insert blank lines previously calculated
  content <- tidytlg::insert_empty_rows(content, newrows)
  # update 'mpf_aligns'
  idx <- which(newrows == 1)
  # NOTE: here, it is important to traverse 'idx' in reverse order
  # to avoid shifting by 1 position every time we insert a row in mpf_aligns
  for (i in rev(idx)) {
    new_row <- matrix(data = mpf_aligns[i, ], nrow = 1)
    n <- nrow(mpf_aligns)
    new_mpf_aligns <- rbind(mpf_aligns[seq(1, i), ], new_row)
    new_mpf_aligns <- rbind(new_mpf_aligns, mpf_aligns[seq(i + 1, n), ])
    mpf_aligns <- new_mpf_aligns
  }
  # END
  
  flx <- flextable::qflextable(content) |>
    flextable::border_remove()
  
  hdr <- body[seq_len(hnum), , drop = FALSE]
  if (hnum > 1) {
    det_nclab <- apply(hdr, 2, grepl, pattern = "\\(N=[0-9]+\\)$")
    has_nclab <- apply(det_nclab, 1, any)
    whsnc <- which(has_nclab)
    if (any(has_nclab)) {
      for (i in seq_along(whsnc)) {
        wi <- whsnc[i]
        what_is_nclab <- det_nclab[wi, ]
        colcounts_split_chr <- if (isFALSE(counts_in_newline)) {
          " "
        } else {
          "\n"
        }
        hdr[wi, what_is_nclab] <- paste(hdr[wi - 1, what_is_nclab],
                                        hdr[wi, what_is_nclab],
                                        sep = colcounts_split_chr
        )
        hdr[wi - 1, what_is_nclab] <- ""
        row_to_pop <- wi - 1
        what_to_put_up <- hdr[row_to_pop, what_is_nclab,
                              drop = FALSE
        ]
        if (all(!nzchar(what_to_put_up)) && row_to_pop > 1) {
          reconstructed_hdr <-
            rbind(
              cbind(
                hdr[seq(row_to_pop), !what_is_nclab],
                rbind(what_to_put_up, hdr[seq(row_to_pop - 1), what_is_nclab])
              ),
              hdr[seq(row_to_pop + 1, nrow(hdr)), ]
            )
          row_to_pop <- 1
          hdr <- reconstructed_hdr
        }
        if (all(!nzchar(hdr[row_to_pop, ]))) {
          hdr <- hdr[-row_to_pop, , drop = FALSE]
          spans <- spans[-row_to_pop, , drop = FALSE]
          body <- body[-row_to_pop, , drop = FALSE]
          mpf_aligns <- mpf_aligns[-row_to_pop, , drop = FALSE]
          hnum <- hnum - 1
          whsnc <- whsnc - 1
          det_nclab <- det_nclab[-row_to_pop, , drop = FALSE]
          # also remove that row from 'border_mat'
          border_mat <- border_mat[-row_to_pop, , drop = FALSE]
        }
      }
    }
  }
  hdr[hdr == ""] <- " "
  flx <- flx |> flextable::set_header_labels(values = setNames(as.vector(hdr[hnum, , drop = TRUE]), names(content)))
  
  # NOTE: this block of code calculates where to put horizontal borders
  # within the Header
  l_pos <- list()
  for (i in seq(1, nrow(border_mat))) {
    for (j in seq(1, ncol(border_mat))) {
      if (border_mat[i, j] != 0) {
        l_pos <- append(l_pos, list(c(i, j)))
      }
    }
  }
  
  if (hnum > 1) {
    for (i in seq(hnum - 1, 1)) {
      sel <- formatters::spans_to_viscell(spans[i, ])
      flx <- flextable::add_header_row(flx,
                                       top = TRUE,
                                       values = as.vector(hdr[i, sel]),
                                       colwidths = as.integer(spans[i, sel])
      )
      
      col_widths_in_header <- as.integer(spans[i, sel])
      cnt <- 1
      for (j in seq(1, length(col_widths_in_header))) {
        cur_width <- col_widths_in_header[j]
        if (cur_width > 1 &&
            !grepl("^N=", as.vector(hdr[i + 1, sel])[j]) &&
            trimws(as.vector(hdr[i, sel])[j]) != "") {}
        cnt <- cnt + cur_width
      }
    }
  }
  nr_body <- flextable::nrow_part(flx, part = "body")
  nr_header <- flextable::nrow_part(flx, part = "header")
  flx <- flx |>
    flextable::border(part = "header", i = 1, border.top = border) |>
    flextable::border(part = "body", i = nr_body, border.bottom = border)
  
  for (ij in l_pos) {
    i <- ij[1]
    j <- ij[2]
    flx <- flx |> flextable::hline(part = "header", i = i, j = j, border = border)
  }
  # END
  
  
  # check if the user passed 'alignments'
  # if so, overwrite the default ones
  for (al in alignments) {
    mpf_aligns[al$row, al$col] <- al$value
  }
  if (length(alignments) == 0) {
    .apply_alignments <- utils::getFromNamespace(
      ".apply_alignments",
      "rtables.officer"
    )
    flx <- flx |>
      .apply_alignments(mpf_aligns[seq_len(hnum), , drop = FALSE], "header") |>
      .apply_alignments(mpf_aligns[-seq_len(hnum), , drop = FALSE], "body")
  } else {
    # iterate for each row in the header
    for (i in seq_len(hnum)) {
      flx <- flx |>
        flextable::align(
          part = "header", i = i,
          align = mpf_aligns[i, , drop = FALSE]
        )
    }
    # iterate for each column in the body
    for (j in seq_len(ncol(mpf_aligns))) {
      flx <- flx |>
        flextable::align(
          part = "body", j = j,
          align = mpf_aligns[-seq_len(hnum), j, drop = FALSE]
        )
    }
  }
  
  flx <- flextable::line_spacing(flx, space = 0, part = "body")
  
  
  # The following block of code adds indentation in the first column
  # NOTE: 3.175mm = 9 ms word points = 0.125 inches
  indent_size <- rtables.officer::word_mm_to_pt(3.175)
  # add indentation in Body
  updated_indents <- rdf$indent
  idx <- which(newrows == 1)
  # NOTE: here, it is important to traverse 'idx' in reverse order
  # to avoid shifting by 1 position every time we insert a 0
  for (i in rev(idx)) {
    updated_indents <- append(updated_indents, 0, after = i - 1)
  }
  
  # NOTE: this simulates the "indentme" part in tt_to_tlgrtf()
  # basically reduce by 1 the indentation in the first column, if we have vertical pagination
  if (reduce_first_col_indentation) {
    updated_indents <- updated_indents - 1
    updated_indents[which(updated_indents < 0)] <- 0
  }
  for (i in seq_len(nr_body)) {
    # NOTE: this line adds the left padding to each row in column 1 (Body only)
    flx <- flextable::padding(flx,
                              i = i, j = 1,
                              padding.left = indent_size * updated_indents[[i]],
                              padding.right = 0,
                              padding.top = 0,
                              padding.bottom = 0,
                              part = "body"
    )
  }
  # add indentation in Header
  for (i in seq_len(nr_header)) {
    leading_spaces_count <- nchar(hdr[i, 1]) -
      nchar(stringi::stri_replace(hdr[i, 1], regex = "^ +", ""))
    # interpret every 2 leading whitespaces as 1 indentation level
    header_indent_size <- (leading_spaces_count / 2) * indent_size
    hdr[i, 1] <- trimws(x = hdr[i, 1], which = "left")
    flx <- flextable::compose(flx,
                              part = "header",
                              i = i,
                              j = 1,
                              value = flextable::as_paragraph(hdr[i, 1]))
    
    # NOTE: this line adds the left padding to each row in column 1 (Header only)
    flx <- flextable::padding(flx,
                              part = "header",
                              i = i,
                              j = 1,
                              padding.left = header_indent_size,
                              padding.right = 0
    )
  }
  
  if (length(formatters::all_footers(tt)) > 0 && isTRUE(integrate_footers)) {
    footers <- formatters::all_footers(tt)
    footers <- strmodify(footers, string_map)
    footers_1row <- paste(footers, collapse = "\n")
    footers_1row <- paste0("\n", footers_1row)
    flx <- flextable::add_footer_lines(flx, values = footers_1row) |>
      flextable::border(part = "footer",
                        i = length(footers_1row),
                        border.bottom = border)
  }
  
  # NOTE: the following block adds the footer, this is, the last line below footnotes
  flx <- insert_footer_text(flx, tblid)
  
  # apply theme
  flx <- do.call(theme, list(flx, tbl_row_class = rdf$node_class))
  
  
  # NOTE: for Listings, vertical alignment is "top" for the whole body
  if (tlgtype == "Listing") {
    flx <- flx |> flextable::valign(part = "body", valign = "top")
    flx <- flx |> flextable::align(part = "body", j = 1, align = "left")
    flx <- flx |> flextable::align(part = "body", j = -1, align = "center")
  }
  # END
  
  if (is.null(fontspec)) {
    fontspec <- formatters::font_spec("Times", 9L, 1.2)
  }
  if (is.null(colwidths)) {
    colwidths <- formatters::propose_column_widths(matform,
                                                   fontspec = fontspec,
                                                   indent_size = indent_size
    )
  }
  if (titles_as_header &&
      length(formatters::all_titles(tt)) > 0 &&
      any(nzchar(formatters::all_titles(tt)))) {
    # NOTE: the following block appends the Table ID to the Title,
    # inserts the Title with the hanging indent as part of the Header,
    # and adds the borders above and below the Title
    ts_tbl <- formatters::all_titles(tt)
    ts_tbl <- paste0(tblid, ":", ts_tbl)
    
    flx <- insert_title_as_header(
      flx = flx,
      title = ts_tbl,
      border = border
    )
    # END
  }
  
  
  # NOTE: if it's a listing, set font size = 8 (instead of 9) everywhere
  # except the Titles, which remain font size = 10
  if (tlgtype == "Listing") {
    header_start_pos <- length(formatters::all_titles(tt)) + 1
    header_end_pos <- nrow(flx$header$dataset)
    flx <- flextable::fontsize(
      flx,
      part = "header",
      i = seq(header_start_pos, header_end_pos),
      size = 8
    )
    flx <- flextable::fontsize(flx, part = "body", size = 8)
    flx <- flextable::fontsize(flx, part = "footer", size = 8)
  }
  
  # NOTE: here, even though page width is 8.88 inches, table width has
  # to be 8.82 inches, so leave a gap of 0.03 inches on both sides
  if (orientation == "landscape") {
    final_cwidths <- (total_page_width - 0.03 * 2) * colwidths / sum(colwidths)
  } else {
    final_cwidths <- total_page_width * colwidths / sum(colwidths)
  }
  flx <- flextable::width(flx, width = final_cwidths)
  autofit_to_page <- FALSE
  flx <- flextable::set_table_properties(flx,
                                         layout = ifelse(autofit_to_page, "autofit", "fixed"),
                                         align = "left", opts_word = list(split = TRUE, keep_with_next = TRUE)
  )
  
  if (!all(is.na(matform$row_info$trailing_sep))) {
    # NOTE: this block adds indentation after some rows to simulate blank lines
    new_trailing_sep <- matform$row_info$trailing_sep
    idx <- which(newrows == 1)
    # here, it is important to traverse 'idx' in reverse order
    # to avoid shifting by 1 position every time we insert a 'NA'
    for (i in rev(idx)) {
      new_trailing_sep <- append(new_trailing_sep, NA, after = i - 2)
    }
    new_trailing_sep[length(new_trailing_sep)] <- NA
    flx <- rtables.officer::add_flextable_separators(flx, new_trailing_sep,
                                                     border = border,
                                                     padding = 0
    ) # NOTE: here, we used to have padding = 10
    # END
  }
  flx <- flextable::fix_border_issues(flx)
  
  # NOTE: add the vertical pagination break pages
  if (tlgtype == "Table" && nrow(tt) > 1) {
    pags <- formatters::paginate_to_mpfs(
      tt,
      fontspec = fontspec,
      landscape = orientation == "landscape",
      colwidths = colwidths_2,
      col_gap = col_gap,
      pg_width = pg_width_by_orient(orientation == "landscape"),
      pg_height = NULL,
      margins = rep(0, 4),
      lpp = NULL,
      nosplitin = nosplitin,
      verbose = FALSE,
      round_type = round_type
    )
    if (!is.null(names(pags))) {
      flx <- insert_keepNext_vertical_pagination(flx = flx,
                                                 pags = pags,
                                                 fontspec = fontspec,
                                                 string_map = string_map,
                                                 round_type = round_type)
    }
  }
  # END
  
  
  # NOTE:
  # the following block adds the Title with its own style
  if (isFALSE(titles_as_header)) {
    title <- formatters::all_titles(tt)
    title <- paste0(tblid, ":\t", title)
    
    w <- ifelse(orientation == "portrait", 155, 215)
    new_title <- formatters::wrap_string_ttype(
      str = title,
      width = w,
      fontspec = formatters::font_spec("Times", 10L, 1.2),
      collapse = "\n\t",
      wordbreak_ok = FALSE
    )
    new_title <- sub(": ", ":\t", new_title)
    
    p <- ifelse(orientation == "portrait", 4.32, 7.8)
    fpp <- officer::fp_par(
      text.align = "left",
      padding.left = p,
      padding.right = p,
      border.top = border
    )
    
    font_sz_body <- flx$header$styles$text$font.size$data[1, 1]
    font_fam <- flx$header$styles$text$font.family$data[1, 1]
    title_style <- officer::fp_text(font.family = font_fam, font.size = font_sz_body)
    title_style$font.size <- title_style$font.size + 1 # 10
    title_style$bold <- bold_titles
    flx <- flx |> flextable::set_caption(
      caption = flextable::as_paragraph(flextable::as_chunk(new_title, title_style)),
      word_stylename = "caption",
      fp_p = fpp,
      align_with_table = FALSE
    )
  }
  # END
  
  # NOTE: convert the super- and sub-scripts
  flx <- interpret_all_cell_content(flx, markup_df_docx)
  
  #  NOTE: add the little gap between the bottom borders of 2 spanning headers
  flx <- add_little_gap_bottom_borders_spanning_headers(flx, border)
  
  flx
}

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

constrict_lbl_lns <- function(curdf, possdf, avail_spc = 0, verbose = TRUE) {
  old_lbl_lns <- max(curdf$lbl_lines)
  cols_to_pack <- which(curdf$lbl_lines == old_lbl_lns)
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
    if (ii > nrow(possdfii)) { #### TODO: hotfix #395 `def_colwidths()` crashes when long column labels (issue #281)
      success <- FALSE
      break
    }
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



