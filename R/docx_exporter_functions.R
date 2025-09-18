
# based on rtables.officer::theme_docx_default
my_theme_docx_default <- function(font = "Arial",
                                  font_size = 9,
                                  cell_margins = c(word_mm_to_pt(1.9), word_mm_to_pt(1.9), 0, 0),
                                  bold = c("header", "content_rows", "label_rows", "top_left"),
                                  bold_manual = NULL, border = flextable::fp_border_default(width = 0.5)) {
  function(flx, ...) {
    if (!inherits(flx, "flextable")) {
      stop(sprintf("Function `%s` supports only flextable objects.", 
                   "theme_box()"))
    }
    checkmate::assert_int(font_size, lower = 6, upper = 12)
    checkmate::assert_string(font)
    checkmate::assert_subset(bold, eval(formals(theme_docx_default)$bold), 
                             empty.ok = TRUE)
    if (length(cell_margins) == 1) {
      cell_margins <- rep(cell_margins, 4)
    }
    checkmate::assert_numeric(cell_margins, lower = 0, len = 4)
    args <- list(...)
    tbl_row_class <- args$tbl_row_class
    tbl_ncol_body <- flextable::ncol_keys(flx)
    # NOTE: this is new
    flx <- flextable::fontsize(flx, size = font_size, part = "all") %>%
      flextable::fontsize(size = font_size, part = "header") %>%
      flextable::font(fontname = font, part = "all")
    # flx <- flextable::fontsize(flx, size = font_size, part = "all") %>% 
    #   flextable::fontsize(size = font_size - 1, part = "footer") %>% 
    #   flextable::font(fontname = font, part = "all")
    # flx <- .add_borders(flx, border = border, ncol = tbl_ncol_body)
    flx <- flx %>%
      flextable::valign(j = seq(2, tbl_ncol_body), valign = "bottom", part = "body") %>%
      flextable::valign(j = 1, valign = "bottom", part = "all") %>%
      flextable::valign(j = 1, valign = "bottom", part = "header") %>%
      flextable::valign(j = seq(2, tbl_ncol_body), valign = "bottom", part = "header")
    flx <- .apply_indentation_and_margin(flx, cell_margins = cell_margins, 
                                         tbl_row_class = tbl_row_class, tbl_ncol_body = tbl_ncol_body)
    if (any(tbl_row_class == "LabelRow")) {
      flx <- flextable::padding(flx, j = 1, i = which(tbl_row_class == "LabelRow"),
                                padding.top = 0,
                                padding.bottom = 0,
                                part = "body")
    }
    if (any(tbl_row_class == "ContentRow")) {
      flx <- flextable::padding(flx, i = which(tbl_row_class == "ContentRow"),
                                padding.top = 0,
                                padding.bottom = 0,
                                part = "body")
    }
    flx <- flextable::line_spacing(flx, space = 1, part = "all")
    # # NOTE: here an attempt to insert a blank line after each DataRow
    # # rtables::row_paths_summary(result)
    # if (any(tbl_row_class == "DataRow")) {
    #   flx <- flextable::padding(flx, i = which(tbl_row_class == "DataRow"),
    #                             padding.top = 0,
    #                             padding.bottom = 10,
    #                             part = "body")
    #   flx <- flextable::line_spacing(flx, space = 2, part = "body",
    #                                  i = which(tbl_row_class == "DataRow"))
    # }
    if (any(bold == "header")) {
      flx <- flextable::bold(flx, j = seq(2, tbl_ncol_body), 
                             part = "header")
    }
    if (any(bold == "content_rows")) {
      if (is.null(tbl_row_class)) {
        stop("bold = \"content_rows\" needs tbl_row_class = rtables::make_row_df(tt).")
      }
      flx <- flextable::bold(flx, j = 1, i = which(tbl_row_class == 
                                                     "ContentRow"), part = "body")
    }
    if (any(bold == "label_rows")) {
      if (is.null(tbl_row_class)) {
        stop("bold = \"content_rows\" needs tbl_row_class = rtables::make_row_df(tt).")
      }
      flx <- flextable::bold(flx, j = 1, i = which(tbl_row_class == 
                                                     "LabelRow"), part = "body")
    }
    if (any(bold == "top_left")) {
      flx <- flextable::bold(flx, j = 1, part = "header")
    }
    flx <- .apply_bold_manual(flx, bold_manual)
    flx
  }
}


# based on rtables.officer::tt_to_flextable
my_tt_to_flextable <- function(tt,
                               theme = theme_docx_default(), border = flextable::fp_border_default(width = 0.5), 
                               indent_size = NULL, titles_as_header = TRUE, bold_titles = TRUE, 
                               integrate_footers = TRUE, counts_in_newline = FALSE, paginate = FALSE, 
                               fontspec = NULL, lpp = NULL, cpp = NULL, ..., colwidths = NULL, 
                               tf_wrap = !is.null(cpp), max_width = cpp, total_page_height = 10, 
                               total_page_width = 10, autofit_to_page = TRUE) 
{
  
  # NOTE: calculate page width and column widths
  if (!exists("orientation")) {
    orientation <- "portrait"
  }
  # total_page_width <- 6.38
  total_page_width <- pg_width_by_orient(orientation == "landscape")
  
  fontspec <- font_spec("Times", 9L, 1.2)
  label_width_ins <- 2
  tlgtype <- tlg_type(tt)
  col_gap <- ifelse(tlgtype == "Listing", .5, 3)
  colwidths <- def_colwidths(
    tt,
    fontspec,
    col_gap = col_gap,
    label_width_ins = label_width_ins,
    type = tlgtype
  )
  # colwidths <- c(64, 21, 21, 21, 21)
  if (tlgtype == "Table") {
    colwidths <- cwidths_final_adj(
      labwidth_ins = label_width_ins,
      total_width = total_page_width,
      colwidths = colwidths[-1]
    )
  }
  colwidths <- (colwidths / sum(colwidths)) * total_page_width
  # colwidths <- c(1.99, 1.1, 1.1, 1.1, 1.1)
  # END
  
  if (inherits(tt, "list")) {
    stop("Please use paginate = TRUE or mapply() to create multiple outputs. export_as_docx accepts lists.")
  }
  if (!inherits(tt, "VTableTree") && !inherits(tt, "listing_df")) {
    stop("Input object is not an rtables' or rlistings' object.")
  }
  checkmate::assert_flag(titles_as_header)
  checkmate::assert_flag(integrate_footers)
  checkmate::assert_flag(counts_in_newline)
  checkmate::assert_flag(autofit_to_page)
  checkmate::assert_number(total_page_width, lower = 1)
  checkmate::assert_number(total_page_height, lower = 1)
  checkmate::assert_numeric(colwidths, lower = 0, len = ncol(tt) + 1, null.ok = TRUE)
  
  if (!is.null(colwidths)) {
    autofit_to_page <- FALSE
  }
  # NOTE
  # left_right_fixed_margins <- word_mm_to_pt(0)
  left_right_fixed_margins <- word_mm_to_pt(1.9)
  if (paginate) {
    args <- as.list(environment())
    args$paginate <- FALSE
    tmp_flx <- do.call(tt_to_flextable, args)
    row_heights <- dim(tmp_flx)$heights
    nr_header <- flextable::nrow_part(tmp_flx, part = "header")
    nr_body <- flextable::nrow_part(tmp_flx, part = "body")
    nr_footer <- flextable::nrow_part(tmp_flx, part = "footer")
    if (sum(nr_header, nr_body, nr_footer) != length(row_heights)) {
      stop("Something went wrong with the row heights. Maybe \\n? Contact maintener.")
    }
    rh_df <- data.frame(rh = row_heights, part = c(rep("header", 
                                                       nr_header), rep("body", nr_body), rep("footer", nr_footer)))
    needed_height_header_footer <- sum(rh_df$rh[rh_df$part %in% 
                                                  c("header", "footer")])
    starting_lpp <- nr_header + nr_footer
    cumsum_page_heights <- needed_height_header_footer + 
      cumsum(rh_df$rh[rh_df$part == "body"])
    expected_lpp <- starting_lpp + max(which(cumsum_page_heights < 
                                               total_page_height))
    if (!is.null(lpp) && starting_lpp + 1 > lpp) {
      stop("Header rows are more than selected lines per pages (lpp).")
    }
    tabs <- rtables::paginate_table(tt, fontspec = fontspec, 
                                    lpp = lpp, cpp = cpp, tf_wrap = tf_wrap, max_width = max_width, 
                                    ...)
    cinds <- lapply(tabs, function(tb) c(1, .figure_out_colinds(tb, 
                                                                tt) + 1L))
    args$colwidths <- NULL
    args$tt <- NULL
    cl <- if (!is.null(colwidths)) {
      lapply(cinds, function(ci) colwidths[ci])
    }
    else {
      lapply(cinds, function(ci) {
        NULL
      })
    }
    return(mapply(tt_to_flextable, tt = tabs, colwidths = cl, 
                  MoreArgs = args, SIMPLIFY = FALSE))
  }
  matform <- rtables::matrix_form(tt, fontspec = fontspec, 
                                  indent_rownames = FALSE)
  body <- formatters::mf_strings(matform)
  spans <- formatters::mf_spans(matform)
  mpf_aligns <- formatters::mf_aligns(matform)
  hnum <- formatters::mf_nlheader(matform)
  rdf <- rtables::make_row_df(tt)
  
  # NOTE:
  # calculate where to place the blank rows
  mpf <- matform
  rowdf <- mf_rinfo(mpf)
  nhl <- mf_nlheader(mpf)
  rinds <- mf_lgrouping(mpf)[-(1:nhl)] - mf_nrheader(mpf)
  anbr <- cumsum(!is.na(c(0, utils::head(rowdf$trailing_sep, -1))))[rinds] + 1 ## so it starts at 1
  newrows <- c(0, ifelse(utils::tail(anbr, -1) == utils::head(anbr, -1), 0, 1))
  # END
  
  if (any(grepl("dec", mpf_aligns))) {
    body <- formatters::decimal_align(body, mpf_aligns)
    mpf_aligns[mpf_aligns == "decimal"] <- "center"
    mpf_aligns[mpf_aligns == "dec_left"] <- "left"
    mpf_aligns[mpf_aligns == "dec_right"] <- "right"
  }
  content <- as.data.frame(body[-seq_len(hnum), , drop = FALSE])
  content[content == ""] <- " "
  # NOTE:
  # insert blank lines previously calculated
  content <- tidytlg:::insert_empty_rows(content, newrows)
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
  flx <- flextable::qflextable(content) %>% .remove_hborder(part = "body", 
                                                            w = "bottom")
  # NOTE:
  # this is to add a horizontal line above the Title
  # rtables.officer::add_flextable_separators()
  # END
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
        }
        else {
          "\n"
        }
        hdr[wi, what_is_nclab] <- paste(hdr[wi - 1, what_is_nclab], 
                                        hdr[wi, what_is_nclab], sep = colcounts_split_chr)
        hdr[wi - 1, what_is_nclab] <- ""
        row_to_pop <- wi - 1
        what_to_put_up <- hdr[row_to_pop, what_is_nclab, 
                              drop = FALSE]
        if (all(!nzchar(what_to_put_up)) && row_to_pop > 
            1) {
          reconstructed_hdr <- rbind(cbind(hdr[seq(row_to_pop), 
                                               !what_is_nclab], rbind(what_to_put_up, hdr[seq(row_to_pop - 
                                                                                                1), what_is_nclab])), hdr[seq(row_to_pop + 
                                                                                                                                1, nrow(hdr)), ])
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
        }
      }
    }
  }
  hdr[hdr == ""] <- " "
  flx <- flx %>% flextable::set_header_labels(
    values = setNames(as.vector(hdr[hnum, , drop = TRUE]), names(content)))
  
  if (hnum > 1) {
    for (i in seq(hnum - 1, 1)) {
      sel <- formatters::spans_to_viscell(spans[i, ])
      flx <- flextable::add_header_row(flx, top = TRUE, 
                                       values = as.vector(hdr[i, sel]),
                                       colwidths = as.integer(spans[i, sel]))
      
      # NOTE: this block of code calculates where to put horizontal borders
      # within the Header in the first row
      if (i == 1) {
        
        col_widths_in_header <- as.integer(spans[i, sel]) # c(1, 3, 1)
        l_pos <- c()
        cnt <- 1
        for (j in col_widths_in_header) {
          if (j > 1) {
            l_pos <- c(l_pos, cnt)
          }
          cnt <- cnt + j
        }
        
        # l_pos <- list()
        # cnt <- 1
        # for (j in seq(1, length(col_widths_in_header))) {
        #   cur_cw <- col_widths_in_header[j]
        #   if (cur_cw > 1) {
        #     start_pos <- cnt
        #     end_pos <- cnt + cur_cw - 1
        #     l_pos[[cnt]] <- c(start_pos, end_pos)
        #   }
        #   cnt <- cnt + cur_cw
        # }
        # keep <- !(lapply(l_pos, is.null) %>% unlist())
        # l_pos <- l_pos[keep]
        
      }
      # END
      
    }
  }
  nr_body <- flextable::nrow_part(flx, part = "body")
  nr_header <- flextable::nrow_part(flx, part = "header")
  flx <- flx %>% .remove_hborder(part = "header", w = "all") %>% 
    .add_hborder("header", ii = c(0, hnum), border = border)
  
  # NOTE: this line of code adds the border in the Header under "Active Study Agent"
  flx <- flx %>% flextable::hline(part = "header", i = 1, j = l_pos, border = border)
  
  flx <- flx %>%
    .apply_alignments(mpf_aligns[seq_len(hnum), , drop = FALSE], "header") %>%
    .apply_alignments(mpf_aligns[-seq_len(hnum), , drop = FALSE], "body")
  checkmate::check_number(indent_size, null.ok = TRUE)
  if (is.null(indent_size)) {
    indent_size <- matform$indent_size * word_mm_to_pt(1)
  }
  else {
    indent_size <- indent_size * word_mm_to_pt(1)
  }
  # NOTE:
  flx <- flextable::line_spacing(flx, space = 0, part = "body")
  indent_size <- word_mm_to_pt(3.175)
  left_right_fixed_margins <- 0
  updated_indents <- rdf$indent
  idx <- which(newrows == 1)
  # NOTE: here, it is important to traverse 'idx' in reverse order
  # to avoid shifting by 1 position every time we insert a 0
  for (i in rev(idx)) {
    updated_indents <- append(updated_indents, 0, after = i - 1)
  }
  for (i in seq_len(nr_body)) {
    flx <- flextable::padding(flx, i = i, j = 1,
                              # padding.left = indent_size * rdf$indent[[i]] + left_right_fixed_margins,
                              padding.left = indent_size * updated_indents[[i]] + left_right_fixed_margins, # NOTE:
                              padding.right = left_right_fixed_margins,
                              padding.top = 0, padding.bottom = 0,
                              part = "body")
  }
  for (i in seq_len(nr_header)) {
    leading_spaces_count <- nchar(hdr[i, 1]) - nchar(stringi::stri_replace(hdr[i, 
                                                                               1], regex = "^ +", ""))
    header_indent_size <- leading_spaces_count * word_mm_to_pt(1)
    hdr[i, 1] <- stringi::stri_replace(hdr[i, 1], regex = "^ +", 
                                       "")
    # NOTE: this line adds the left padding to each row column 1
    flx <- flextable::padding(flx, i = i, j = 1, padding.left = header_indent_size + 
                                left_right_fixed_margins, padding.right = left_right_fixed_margins, 
                              part = "header")
  }
  if (length(matform$ref_footnotes) > 0 && isTRUE(integrate_footers)) {
    # flx <- flextable::add_footer_lines(flx, values = matform$ref_footnotes) %>% 
    #   .add_hborder(part = "body", ii = nrow(tt), border = border)
    
    # NOTE: add 2 line above and below footnotes
    footers_with_blank_line <- c("", matform$ref_footnotes)
    flx <- flextable::add_footer_lines(flx, values = footers_with_blank_line) %>% 
      .add_hborder(part = "body", ii = nrow(content), border = border) %>% 
      .add_hborder(part = "footer", ii = length(footers_with_blank_line), border = border)
    
    flx <- flextable::padding(flx, 
                              padding = 0,
                              part = "footer")
  }
  if (length(formatters::all_footers(tt)) > 0 && isTRUE(integrate_footers)) {
    # flx <- flextable::add_footer_lines(flx, values = formatters::all_footers(tt)) %>% 
    #   .add_hborder(part = "body", border = border, ii = nrow(tt))
    
    # NOTE: add 2 line above and below footnotes
    footers_with_blank_line <- c("", formatters::all_footers(tt))
    flx <- flextable::add_footer_lines(flx, values = footers_with_blank_line) %>% 
      .add_hborder(part = "body", ii = nrow(content), border = border) %>% 
      .add_hborder(part = "footer", ii = length(footers_with_blank_line), border = border)
    
    flx <- flextable::padding(flx,
                              padding = 0,
                              part = "footer")
  }
  flx <- .apply_themes(flx, theme = theme, tbl_row_class = rtables::make_row_df(tt)$node_class)
  
  # NOTE: the following block adds the footer, this is, the last line below footnotes
  three_dots <- list(...)
  tblid <- three_dots$tblid
  footer_text <- paste0(
    "[", tolower(tblid), ".docx]",
    "[", tidytlg:::getFileName(), "]",
    toupper(format(Sys.time(), format = "%d%b%Y, %H:%M"))
  )
  n_footnotes <- length(footers_with_blank_line) + 1
  flx <- flextable::add_footer_lines(flx, values = footer_text) %>%
    flextable::fontsize(part = "footer", i = n_footnotes, size = 8) %>%
    flextable::align(part = "footer", i = n_footnotes, align = "right") %>%
    .remove_hborder(part = "footer", w = "bottom")
  # END
  
  
  
  if (is.null(fontspec)) {
    fontspec <- .extract_fontspec(flx)
  }
  if (is.null(colwidths)) {
    # fontspec <- font_spec("Times", 9L, 1.2)
    # tlgtype <- tlg_type(tt)
    # col_gap <- ifelse(tlgtype == "Listing", .5, 3)
    # colwidths <- def_colwidths(
    #   tt,
    #   fontspec,
    #   col_gap = col_gap,
    #   label_width_ins = 2,
    #   type = tlgtype
    # )
    colwidths <- formatters::propose_column_widths(matform,
                                                   fontspec = fontspec,
                                                   indent_size = indent_size)
  }
  if (titles_as_header &&
      length(formatters::all_titles(tt)) > 0 &&
      any(nzchar(formatters::all_titles(tt)))) {
    # NOTE: the following block adds the line on top of Title, and Table ID
    ts_tbl <- formatters::all_titles(tt)
    three_dots <- list(...)
    tblid <- three_dots$tblid
    ts_tbl <- paste0(tblid, ":\t", ts_tbl)
    
    flx_fpt <- .extract_font_and_size_from_flx(flx)
    title_style <- flx_fpt$fpt
    title_style$font.size <- title_style$font.size + 1 # 10
    title_style$bold <- bold_titles
    
    flx <- .add_titles_as_header(flx,
                                 # all_titles = formatters::all_titles(tt), 
                                 all_titles = ts_tbl, 
                                 bold = bold_titles) %>%
      flextable::border(part = "header",
                        i = length(formatters::all_titles(tt)),
                        border.bottom = border,
                        border.top = border # NOTE: added this line
                        ) %>% 
      flextable::fontsize(part = "header", i = 1, size = title_style$font.size)
    
    
    # flx %>% flextable::style(part = "header", i = 1,
    #                          pr_p = officer::fp_par(word_style = "centered"),
    #                          pr_t = officer::fp_text(color = "red"),
    #                          pr_c = officer::fp_cell(background.color = "blue"))
    # END
  }
  final_cwidths <- total_page_width * colwidths/sum(colwidths)
  flx <- flextable::width(flx, width = final_cwidths)
  flx <- flextable::set_table_properties(flx, layout = ifelse(autofit_to_page, 
                                                              "autofit", "fixed"), align = "left", opts_word = list(split = FALSE, 
                                                                                                                    keep_with_next = TRUE))
  if (!all(is.na(matform$row_info$trailing_sep))) {
    # flx <- add_flextable_separators(flx, matform$row_info$trailing_sep, 
    #                                 border = officer::fp_border(width = 1, color = "grey60"), 
    #                                 padding = 10)
    
    # NOTE: this block adds indentation after some rows to simulate blank lines
    new_trailing_sep <- matform$row_info$trailing_sep
    idx <- which(newrows == 1)
    # here, it is important to traverse 'idx' in reverse order
    # to avoid shifting by 1 position every time we insert a 'NA'
    for (i in rev(idx)) {
      new_trailing_sep <- append(new_trailing_sep, NA, after = i - 2)
    }
    new_trailing_sep[length(new_trailing_sep)] <- NA
    # new_trailing_sep <- c(rep(NA, 5), " ", rep(NA, 2), " ", rep(NA, 4), " ", rep(NA, 5), " ", rep(NA, 4), " ", rep(NA, 5))
    flx <- add_flextable_separators(flx, new_trailing_sep,
                                    border = officer::fp_border(width = 1, color = "grey60"),
                                    padding = 0) # NOTE: here, we used to have padding = 10
    # END
  }
  flx <- flextable::fix_border_issues(flx)
  
  
  # NOTE:
  # the following block adds the Title with its own style
  # rtables.officer::add_flextable_separators(ft = flx, trailing_sep = c("-", rep(NA, 29)), border = border)
  # flextable::hline(flx, i = NULL, j = NULL, border = border, part = "header")
  if (!titles_as_header) {
    ts_tbl <- formatters::all_titles(tt)
    three_dots <- list(...)
    tblid <- three_dots$tblid
    ts_tbl <- paste0(tblid, ":\t", ts_tbl)
    flx_fpt <- .extract_font_and_size_from_flx(flx)
    title_style <- flx_fpt$fpt
    title_style$font.size <- title_style$font.size + 1 # 10
    title_style$bold <- bold_titles
    flx <- flx %>% set_caption(as_paragraph(as_chunk(
      ts_tbl, title_style)),
      word_stylename = "Caption",
      style = "Caption",
      fp_p = officer::fp_par(text.align = "left"),
      align_with_table = F)
  }
  # END
  
  
  
  flx
}

# based on rtables.officer::export_as_docx
my_export_as_docx <- function(tt, file, add_page_break = FALSE, titles_as_header = TRUE, 
                              integrate_footers = TRUE, section_properties = section_properties_default(), 
                              doc_metadata = NULL, template_file = NULL, ...) 
{
  
  # main_title <- main_title(tt)
  # new_title <- paste0("TSIEX11", "\t", main_title)
  # main_title(tt) <- new_title
  
  # flextable::set_flextable_defaults(
  #   font.family = "Times New Roman",
  #   font.size = 9,
  #   padding = 2,
  #   # border.color = "#CCCCCC",
  #   line_spacing = 1
  # )
  
  # tt2 <- tt
  
  
  checkmate::assert_flag(add_page_break)
  do_tt_error <- FALSE
  if (inherits(tt, "VTableTree") || inherits(tt, "listing_df")) {
    tt <- tt_to_flextable(tt, titles_as_header = titles_as_header, 
                          integrate_footers = integrate_footers, ...)
    
    # NOTE: an attempt to left align only column headers and cell values
    # tt <- tt |>
    #   flextable::valign(valign = "bottom", part = "body") |>
    #   flextable::align(align = "center", part = "body")
    # tt <- tt |>
    #   flextable::mk_par(j = 2,
    #                     as_paragraph(
    #                       as_chunk(latin_name, 
    #                                props = fp_text_default(color = "#C32900", bold = TRUE)))
    #                     )
    
    # NOTE: an attempt to set line spacing = 0
    # tt %>% flextable::style(i = ~!is.na(variable),
    #                         pr_t = flextable::fp_text_default(bold = TRUE),
    #                         pr_p = officer::fp_par(text.align = "left", padding = 5, line_spacing = 1.5))
    # 
    # tt <- tt %>% flextable::style(pr_p = officer::fp_par(line_spacing = 1))
    
    
    
    
  }
  if (inherits(tt, "flextable")) {
    flex_tbl_list <- list(tt)
  }
  else if (inherits(tt, "list")) {
    if (inherits(tt[[1]], "VTableTree") || inherits(tt[[1]], 
                                                    "listing_df")) {
      flex_tbl_list <- mapply(tt_to_flextable, tt = tt, 
                              MoreArgs = list(titles_as_header = titles_as_header, 
                                              integrate_footers = integrate_footers, ...), 
                              SIMPLIFY = FALSE)
    }
    else if (inherits(tt[[1]], "flextable")) {
      flex_tbl_list <- tt
    }
    else {
      do_tt_error <- TRUE
    }
  }
  else {
    do_tt_error <- TRUE
  }
  if (isTRUE(do_tt_error)) {
    stop("tt must be a TableTree/listing_df, a flextable, or a list of TableTree/listing_df or flextable objects.")
  }
  if (isFALSE(titles_as_header) || isFALSE(integrate_footers)) {
    flx_fpt <- .extract_font_and_size_from_flx(flex_tbl_list[[1]])
  }
  if (!is.null(template_file) && !file.exists(template_file)) {
    template_file <- NULL
  }
  if (!is.null(template_file)) {
    doc <- officer::read_docx(template_file)
  }
  else {
    doc <- officer::read_docx()
  }
  doc <- officer::body_set_default_section(doc, section_properties)
  flex_tbl_list <- lapply(flex_tbl_list, function(flx) {
    if (flx$properties$layout != "autofit") {
      # page_width <- section_properties$page_size$width
      # NOTE: page_width = 6.38 inches, not 8.5 inches
      # page_width <- 6.38
      page_width <- pg_width_by_orient(section_properties$page_size$orient == "landscape")
      # END
      dflx <- dim(flx)
      if (abs(sum(unname(dflx$widths)) - page_width) > 0.01) {
        warning("The total table width does not match the page width. The column widths", 
                " will be resized to fit the page. Please consider modifying the parameter", 
                " total_page_width in tt_to_flextable().")
        final_cwidths <- page_width * unname(dflx$widths)/sum(unname(dflx$widths))
        flx <- flextable::width(flx, width = final_cwidths)
      }
    }
    flx
  })
  # NOTE:
  # the following block adds the Title
  # if (isFALSE(titles_as_header) && inherits(tt2, "VTableTree")) {
  #   ts_tbl <- formatters::all_titles(tt2)
  #   if (length(ts_tbl) > 0) {
  #     title_style <- flx_fpt$fpt
  #     title_style$font.size <- title_style$font.size + 1 # 10
  #     title_style$bold <- TRUE
  #     doc <- add_text_par(doc, ts_tbl, title_style)
  #   }
  # }
  # END
  for (flex_tbl_i in flex_tbl_list) {
    # NOTE: this align = "left" is the alignment of the entire table relative to the page,
    # not the content within the table
    doc <- flextable::body_add_flextable(doc, flex_tbl_i, align = "center") # NOTE: here it was align = "left" before
    if (isTRUE(add_page_break)) {
      doc <- body_add_break(doc)
    }
  }
  if (isFALSE(integrate_footers) && inherits(tt, "VTableTree")) {
    matform <- rtables::matrix_form(tt, indent_rownames = TRUE)
    if (length(matform$ref_footnotes) > 0) {
      doc <- add_text_par(doc, matform$ref_footnotes, flx_fpt$fpt_footer)
    }
    if (length(formatters::all_footers(tt)) > 0) {
      doc <- add_text_par(doc, formatters::all_footers(tt), 
                          flx_fpt$fpt_footer)
    }
  }
  if (!is.null(doc_metadata)) {
    doc <- do.call(officer::set_doc_properties, c(list(x = doc), 
                                                  doc_metadata))
  }
  print(doc, target = file)
  invisible(TRUE)
}


