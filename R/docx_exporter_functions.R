
add_title_style_caption <- function(doc) {
  # this function modifies the XML of the docx to add the "Caption" style to the Title
  
  pages_indexes <- doc$doc_obj$get() %>%
    xml2::xml_child(1) %>%
    xml2::xml_children()
  pages_indexes <- seq(1, length(pages_indexes), 2)
  
  for (page_idx in pages_indexes) {
    doc$doc_obj$get() %>%
      xml2::xml_child(1) %>%
      xml2::xml_child(page_idx) %>%
      xml2::xml_child(3) %>%
      xml2::xml_child(2) %>%
      xml2::xml_child(2) %>%
      xml2::xml_child(1) %>%
      xml2::xml_add_child(.value = 'w:pStyle w:val="Caption"')
  }
  
  # doc$doc_obj$get() %>%
  #   xml2::xml_child(1) %>%
  #   xml2::xml_child(1) %>%
  #   xml2::xml_child(3) %>%
  #   xml2::xml_child(2) %>%
  #   xml2::xml_child(2) %>%
  #   xml2::xml_child(1) %>%
  #   xml2::xml_add_child(.value = 'w:pStyle w:val="Caption"')
  
}

add_little_gap_bottom_borders_spanning_headers <- function(flx, border = flextable::fp_border_default(width = 0.75, color = "black")) {
  
  spanning_headers <- flx$header$spans$rows
  # if there is a row in 'spanning_headers' containing more than one number
  # that is > 1 (i.e. a spanning header), and there was a bottom border in those positions:
  # - get that row
  # - get the positions (columns)
  # - insert in that row and those columns the cell margins
  
  pos_little_gaps <- apply(spanning_headers, 1, function(row) {
    which(row > 1)
  })
  
  for (i in seq(1, length(pos_little_gaps))) {
    j <- pos_little_gaps[[i]]
    if (length(j) > 1 && 
        all(flx$header$styles$cells$border.width.bottom$data[i, j] > 0)) {
      flx <- flextable::style(
        x = flx,
        part = "header",
        i = i,
        j = j,
        pr_c = officer::fp_cell(margin.right = 3, margin.left = 3, vertical.align = "bottom"),
        pr_p = officer::fp_par(border.bottom = border, text.align = "center")
      )
    }
  }
  
  return(flx)
  
}


wrap_string_with_indent <- function(text,
                                    max_width_inch,
                                    font_family = "Times New Roman",
                                    font_size = 9,
                                    hanging_indent = 0.06,
                                    dpi = 96) {
  
  # NOTE: this function converts 'text' into another string with '\n\t' inserted
  # to simulate the hanging indent
  # This function is applied to the first column of the body
  # Steps: given 'max_width_inch' (available space in inches to show text
  #        in a particular cell):
  # - convert it to pixels
  # - split 'text' into words
  # - for each word, calculate the width in pixels with systemfonts::string_width(font)
  #     - if it fits in available space, concatenate it with existing line
  #     - if it doesn't fit, store the line in list and create a new line
  #     - if we are now in second line, decrease available space by 0.06 inches
  # 5. in the end, paste/merge all lines and return concatenated lines
  
  # Convert inches to pixels
  max_width_px <- max_width_inch * dpi
  
  # Split text into words
  words <- strsplit(text, " ")[[1]]
  
  current_line <- ""
  current_width <- 0
  lines <- c()
  we_are_in_second_line <- FALSE
  
  for (word in words) {
    test_line <- if (nchar(current_line) > 0) paste(current_line, word) else word
    test_width <- systemfonts::string_width(test_line,
                                            family = font_family,
                                            size = font_size)
    
    if (test_width <= max_width_px) {
      current_line <- test_line
    } else {
      lines <- c(lines, current_line)
      current_line <- paste0("\t", word)
      if (!we_are_in_second_line) {
        we_are_in_second_line <- TRUE
        # from the second line onwards, the available space is reduced
        # by the hanging indent (only once)
        max_width_inch <- max_width_inch - hanging_indent
        max_width_px <- max_width_inch * dpi
      }
    }
  }
  
  if (nchar(current_line) > 0) {
    lines <- c(lines, current_line)
  }
  
  return(paste(lines, collapse = "\n"))
}

add_hanging_indent_first_column <- function(flx, column_widths, hanging_indent = 0.06) {
  
  # will need:
  # - table of indentations -> flx$body$styles$pars$padding.left$data
  # - list of column widths -> flx$body$colwidths
  
  table_of_paddings <- flx$body$styles$pars$padding.left$data
  cw_in_inches <- column_widths[1]
  
  for (i in seq_len(nrow(table_of_paddings))) {
    
    # left_padding_in_inches <- (27/9)*0.125
    left_padding_in_inches <- (table_of_paddings[[i, 1]]/9)*0.125
    # this is the available space for the string to be displayed at a particular level,
    # which is equal to the width of the first column, minus the left padding at that level
    available_space_in_inches <- cw_in_inches - left_padding_in_inches
    
    # s <- "Study agent permanently discontinued"
    s <- flx$body$dataset[[i, 1]]
    # in 1.84 inches fits 38 characters approx
    # w <- (available_space_in_inches*38)/1.84
    # # 1.99 inches (column width) - 4 (level)
    # # w should be between 52-61
    # w <- 52
    
    # new_s <- 
    #   formatters::wrap_string_ttype(str = s,
    #                                 width = w,
    #                                 fontspec = formatters::font_spec("Times", 9L, 1.2),
    #                                 collapse = "\n\t", wordbreak_ok = FALSE)
    
    new_s <- wrap_string_with_indent(s,
                                     max_width_inch = available_space_in_inches,
                                     dpi = 78)
    
    if (grepl("\n\t", new_s)) {
      # insert new_s in row = i, j = 1
      flx <- flextable::compose(flx, part = "body", i = i, j = 1,
                                value = flextable::as_paragraph(new_s))
      # insert a tab stop at current indentation level + hanging indent
      flx <- flextable::tab_settings(flx, part = "body", i = i, j = 1,
                                     value = officer::fp_tabs(officer::fp_tab(pos = left_padding_in_inches + hanging_indent, style = "left")))
      
    }
  }
  
  return(flx)
}


insert_title_hanging_indent_v2 <- function(flx,
                                        title,
                                        orientation,
                                        border = flextable::fp_border_default(width = 0.75, color = "black"),
                                        bold_titles = TRUE,
                                        dpi = 72) {
  
  
  flx_fpt <- rtables.officer:::.extract_font_and_size_from_flx(flx)
  title_style <- flx_fpt$fpt
  title_font_size <- title_style$font.size + 1 # 10
  title_font_family <- title_style$font.family
  w <- ifelse(orientation == "portrait", 155, 215)
  new_title <-
    formatters::wrap_string_ttype(
      str = title,
      width =  w,
      # fontspec = formatters::font_spec("Times", 10L, 1.2),
      fontspec = formatters::font_spec(title_font_family, title_font_size, 1.2),
      collapse = "\t",
      wordbreak_ok = FALSE
    )
  
  new_title <- sub(":", ":\t", new_title)
  
  # NOTE: conversion ratio
  # 3.175 mm = 9 ms word points = 0.125 inches
  # multiply everything by 6.4 so we end up with 0.8 inches
  # 20.32 mm = 57.6 ms word points = 0.8 inches
  indent_size <- rtables.officer::word_mm_to_pt(20.32)

  flx <-
    rtables.officer:::.add_titles_as_header(flx, all_titles = new_title, bold = bold_titles) %>% 
    flextable::padding(part = "header", i = 1, padding.left = 0) %>% 
    flextable::fontsize(part = "header", i = 1, size = title_font_size) %>% 
    flextable::border(part = "header", i = 1, border.top = border, border.bottom = border)
  
  # NOTE: if output ID + ":" is less than 0.8 inches,
  # set tab stop "left" at pos = 0.8
  # else, set tab stop "decimal" at pos = 0.5
  
  temp <- sub(":.*", ":", new_title)
  width_pixels <- systemfonts::string_width(temp,
                                            family = title_font_family,
                                            size = title_font_size,
                                            bold = bold_titles)
  # Convert pixels to inches
  # max_width_px <- max_width_inch * dpi
  width_inches <- width_pixels / dpi
  
  # if (width_inches < 0.8) {
  #   flx <- flx %>% 
  #     flextable::tab_settings(i = 1, j = 1, part = "header",
  #                             value = officer::fp_tabs(officer::fp_tab(pos = 0.8, style = "left")))
  # } else {
  #   flx <- flx %>% 
  #     flextable::tab_settings(i = 1, j = 1, part = "header",
  #                             value = officer::fp_tabs(officer::fp_tab(pos = 0.5)))
  # }
  flx <- flx %>% 
    flextable::tab_settings(i = 1, j = 1, part = "header",
                            value = officer::fp_tabs(officer::fp_tab(pos = 0.8, style = "left")))
  
  
  return(flx)
}

insert_title_hanging_indent <- function(flx,
                                        title,
                                        orientation,
                                        border = flextable::fp_border_default(width = 0.75, color = "black"),
                                        bold_titles = TRUE,
                                        dpi = 72) {
  
  w <- ifelse(orientation == "portrait", 155, 215)
  new_title <- 
    formatters::wrap_string_ttype(str = title,
                                  width =  w, 
                                  fontspec = formatters::font_spec("Times", 10L, 1.2),
                                  collapse = "\n", wordbreak_ok = FALSE)
  res <- strsplit(new_title, "\n", fixed = TRUE) %>% unlist()
  if (length(res) >= 2) {
    res <- c(res[1], paste(res[2:length(res)], collapse = " "))
  } else {
    res <- c(res[1])
  }
  res[1] <- sub(":", ":\t", res[1])
  
  flx_fpt <- rtables.officer:::.extract_font_and_size_from_flx(flx)
  title_style <- flx_fpt$fpt
  title_font_size <- title_style$font.size + 1 # 10
  
  # NOTE: conversion ratio
  # 3.175 mm = 9 ms word points = 0.125 inches
  # multiply everything by 6.4 so we end up with 0.8 inches
  # 20.32 mm = 57.6 ms word points = 0.8 inches
  indent_size <- rtables.officer::word_mm_to_pt(20.32)
  
  if (length(res) == 2) {
    flx <- 
      rtables.officer:::.add_titles_as_header(flx, all_titles = res[2], bold = bold_titles) %>% 
      flextable::padding(part = "header", i = 1, padding.left = indent_size) %>% 
      flextable::fontsize(part = "header", i = 1, size = title_font_size)
  }
  flx <-
    rtables.officer:::.add_titles_as_header(flx, all_titles = res[1], bold = bold_titles) %>% 
    flextable::padding(part = "header", i = 1, padding.left = 0) %>% 
    flextable::fontsize(part = "header", i = 1, size = title_font_size) %>% 
    flextable::border(part = "header", i = 1, border.top = border)
  
  # NOTE: if output ID + ":" is less than 0.8 inches,
  # set tab stop "left" at pos = 0.8
  # else, set tab stop "decimal" at pos = 0.5
  
  temp <- res[1]
  temp <- sub(":.*", ":", temp)
  font_family <- title_style$font.family
  width_pixels <- systemfonts::string_width(temp,
                                            family = font_family,
                                            size = title_font_size,
                                            bold = bold_titles)
  # Convert pixels to inches
  # max_width_px <- max_width_inch * dpi
  width_inches <- width_pixels / dpi
  
  if (width_inches < 0.8) {
    flx <- flx %>% 
      flextable::tab_settings(i = 1, j = 1, part = "header",
                              value = officer::fp_tabs(officer::fp_tab(pos = 0.8, style = "left")))
  } else {
    flx <- flx %>% 
      flextable::tab_settings(i = 1, j = 1, part = "header",
                              value = officer::fp_tabs(officer::fp_tab(pos = 0.5)))
  }
  
  # add the line that separates the Title from the Header
  flx <- flx %>% flextable::border(part = "header", i = length(res), border.bottom = border)
  
  
  return(flx)
}

interpret_cell_content <- function(s, sep = "{") {
  
  closing_sep <- ifelse(sep == "{", "}", "]")
  
  j <- 1
  res <- ""
  matches <- gregexpr(paste0("~\\", sep), s)[[1]]
  if (length(matches) == 1 && matches == -1) {
    # no regexp to interpret, return cell content as is
    return(s)
  }
  
  start_pos <- matches[1]
  for (start_pos in matches) {
    close_pos <- regexpr(closing_sep, substr(s, start_pos, nchar(s)))
    if (close_pos != -1) {
      close_pos <- close_pos + start_pos - 1
      content_within_curly_brackets <- substr(s, start_pos + 2, close_pos - 1) # e.g. "super a"
      method_to_apply <- gsub(" .*", "", content_within_curly_brackets) #e.g. "super"
      flextable_command <- switch(method_to_apply,
                                  "super" = "flextable::as_sup",
                                  "sub" = "flextable::as_sub",
                                  method_to_apply
      )
      
      r <- c(start_pos + 2 + nchar(method_to_apply), close_pos - 1)
      text_to_interpret <- substr(s, r[1], r[2]) # e.g. " a"
      text_to_interpret <- trimws(x =  text_to_interpret, which = "left")
      if (res == "") {
        res <- paste0("'", substr(s, j, start_pos - 1), "', ", flextable_command,"('", text_to_interpret, "')")
      } else {
        res <- paste0(res, ", '", substr(s, j, start_pos - 1), "', ", flextable_command, "('", text_to_interpret, "')")
      }
      j <- r[2] + 2
    }
  }
  if (j < nchar(s)) {
    # just append the rest of the sentence
    res <- paste0(res, ", '", substr(s, j, nchar(s)), "'")
    j <- nchar(s) + 1
  }
  res <- paste0("flextable::as_paragraph(", res, ")")
  res
}


interpret_all_cell_content <- function(flx) {
  
  # Title and Headers
  tmp <- flx$header$dataset
  for (i in seq(nrow(tmp))) {
    for (j in seq(ncol(tmp))) {
      s <- tmp[i, j]
      res <- interpret_cell_content(s)
      if (s != res) {
        flx <- flextable::compose(flx, part = "header", i = i, j = j, value = eval(parse(text = res)))
      }
    }
  }
  
  # Body
  tmp <- flx$body$dataset
  idx <- which(grepl("~\\[", tmp[, 1]), arr.ind = TRUE)
  for (i in idx) {
    s <- tmp[i, 1]
    res <- interpret_cell_content(s, sep = "[")
    flx <- flextable::compose(flx, part = "body", i = i, j = 1, value = eval(parse(text = res)))
  }
  
  # Footers
  tmp <- flx$footer$dataset
  idx <- which(grepl("~\\{", tmp[, 1]), arr.ind = TRUE)
  for (i in idx) {
    s <- tmp[i, 1]
    res <- interpret_cell_content(s)
    flx <- flextable::compose(flx, part = "footer", i = i, j = 1, value = eval(parse(text = res)))
  }
  
  flx
}



my_pg_width_by_orient <- function(orientation = "portrait") {
  if (orientation == "landscape") {
    # return(8.82)
    return(8.88)
  }
  return(6.38)
}

# based on rtables.officer::theme_docx_default
my_theme_docx_default <- function(font = "Arial",
                                  font_size = 9,
                                  cell_margins = c(0, 0, 0, 0),
                                  bold = c("header", "content_rows", "label_rows", "top_left"),
                                  bold_manual = NULL,
                                  border = flextable::fp_border_default(width = 0.75, color = "black")) {
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
    
    # NOTE: the following line sets tab stop = 0.5 inches in the Title,
    # necessary for the gap just after ":" to match the RTF
    # flx <- flextable::tab_settings(flx, i = 1, j = 1, part = "header",
    #                                value = officer::fp_tabs(officer::fp_tab(pos = 0.8, style = "left")))
    
    flx <- flx %>%
      flextable::valign(j = seq(2, tbl_ncol_body), valign = "bottom", part = "body") %>%
      flextable::valign(j = 1, valign = "bottom", part = "all") %>%
      flextable::valign(j = 1, valign = "bottom", part = "header") %>%
      flextable::valign(j = seq(2, tbl_ncol_body), valign = "bottom", part = "header")
    flx <- flextable::padding(flx, part = "header", padding = 0, j = -1)
    flx <- rtables.officer:::.apply_indentation_and_margin(flx, cell_margins = cell_margins, 
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
    flx <- rtables.officer:::.apply_bold_manual(flx, bold_manual)
    
    # NOTE: the following block styles the footer and footnotes
    n_footnotes <- nrow(flx$footer$dataset)
    flx <- flx %>%
      flextable::fontsize(part = "footer", i = n_footnotes, size = 8) %>%
      flextable::align(part = "footer", i = n_footnotes, align = "right") %>%
      rtables.officer:::.remove_hborder(part = "footer", w = "bottom") %>% 
      rtables.officer:::.add_hborder(part = "footer", ii = n_footnotes - 1, border = border) %>% 
      flextable::padding(padding = 0, part = "footer")
    # END
    
    flx
  }
}


# based on rtables.officer::tt_to_flextable
my_tt_to_flextable <- function(tt,
                               theme = my_theme_docx_default(
                                 font = "Times New Roman",
                                 font_size = 9L,
                                 bold = NULL
                               ),
                               border = flextable::fp_border_default(width = 0.75, color = "black"),
                               indent_size = NULL,
                               titles_as_header = TRUE,
                               bold_titles = TRUE, 
                               integrate_footers = TRUE,
                               counts_in_newline = FALSE,
                               paginate = FALSE, 
                               fontspec = formatters::font_spec("Times", 9L, 1.2),
                               lpp = NULL,
                               cpp = NULL,
                               ..., 
                               colwidths = NULL, 
                               tf_wrap = !is.null(cpp),
                               max_width = cpp,
                               total_page_height = 10, 
                               # total_page_width = junco:::pg_width_by_orient(orientation == "landscape"),
                               total_page_width = my_pg_width_by_orient(orientation),
                               autofit_to_page = TRUE,
                               orientation = "portrait",
                               tblid,
                               nosplitin = character(),
                               string_map = junco::default_str_map,
                               reduce_first_col_indentation = FALSE
                               ) {
  
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
  
  if (is.null(colwidths)) {
    # NOTE: calculate page width and column widths
    # total_page_width <- junco:::pg_width_by_orient(orientation == "landscape")
    
    label_width_ins <- 2
    tlgtype <- junco:::tlg_type(tt)
    col_gap <- ifelse(tlgtype == "Listing", .5, 3)
    colwidths <- junco::def_colwidths(
      tt,
      fontspec,
      col_gap = col_gap,
      label_width_ins = label_width_ins,
      type = tlgtype
    )
    colwidths_2 <- colwidths
    if (tlgtype == "Table") {
      colwidths <- junco:::cwidths_final_adj(
        labwidth_ins = label_width_ins,
        total_width = total_page_width,
        colwidths = colwidths[-1]
      )
    }
    colwidths <- (colwidths / sum(colwidths)) * total_page_width
    # END
  }
  
  
  autofit_to_page <- FALSE
  # NOTE
  # left_right_fixed_margins <- rtables.officer::word_mm_to_pt(0)
  left_right_fixed_margins <- rtables.officer::word_mm_to_pt(1.9)
  
  if (paginate) {
    ## implies type Table
    if (tlgtype != "Table") {
      stop(
        "pagination is not currently supported for tlg types other than Table."
      )
    }
    if (methods::is(tt, "VTableTree")) {
      hdrmpf <- rtables::matrix_form(tt[1, ])
    } else if (methods::is(tt, "list") && methods::is(tt[[1]], "MatrixPrintForm")) {
      hdrmpf <- tt[[1]]
    } else {
      hdrmpf <- tt
    }
    # pags <- paginate_to_mpfs(
    #   tt,
    #   fontspec = fontspec,
    #   landscape = orientation == "landscape",
    #   colwidths = colwidths,
    #   col_gap = col_gap,
    #   pg_width = pg_width,
    #   pg_height = NULL,
    #   margins = margins,
    #   lpp = NULL,
    #   nosplitin = nosplitin,
    #   verbose = verbose
    # ) ##
    pags <- formatters::paginate_to_mpfs(
      tt,
      fontspec = fontspec,
      landscape = orientation == "landscape",
      colwidths = colwidths_2,
      col_gap = col_gap,
      pg_width = my_pg_width_by_orient(orientation),
      pg_height = NULL,
      margins = rep(0, 4),
      lpp = NULL,
      nosplitin = nosplitin,
      verbose = F
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
    border_mat <- junco:::make_header_bordmat(obj = tt)
    pag_bord_mats <- lapply(
      seq_along(pags),
      function(i) {
        if (methods::is(pags[[i]], "MatrixPrintForm")) {
          partmpf <- pags[[i]]
        } else {
          partmpf <- pags[[i]][[1]]
        }
        junco:::subset_border_mat(border_mat, hdrmpf, partmpf)
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
        if (
          is.list(full_pag_i) &&
          !methods::is(full_pag_i, "MatrixPrintForm")
        ) {
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
        cum_ii <- 0
        # for each page (vertical pagination), splice tt into subt,
        # and convert each subt into a flextable
        # return a list of flextables
        l_ft <- list()
        for (vi in seq(1, length(full_pag_i))) {
          ii <- as.integer(rownames(full_pag_i[[vi]]$row_info)) + cum_ii
          subt <- tt[ii, jj, drop = FALSE, keep_titles = TRUE, keep_topleft = TRUE, reindex_refs = FALSE]
          sub_ft <- my_tt_to_flextable(
            # tt = full_pag_i,
            tt = subt,
            theme = theme,
            border = border,
            indent_size = indent_size,
            titles_as_header = titles_as_header,
            bold_titles = bold_titles,
            integrate_footers = integrate_footers,
            counts_in_newline = counts_in_newline,
            paginate = FALSE,
            fontspec = fontspec,
            lpp = lpp,
            cpp = cpp,
            ... = ...,
            # colwidths = junco:::j_mf_col_widths(pgi_for_cw),
            colwidths = NULL,
            tf_wrap = tf_wrap,
            max_width = max_width,
            total_page_height = total_page_height,
            total_page_width = total_page_width,
            autofit_to_page = autofit_to_page,
            orientation = orientation,
            tblid = fname,
            nosplitin = nosplitin,
            string_map = string_map,
            reduce_first_col_indentation = (length(full_pag_i) > 1)
          )
          
          # NOTE: if we are not in the last page, remove the footers
          # in the future, if we want the footers in all pages,
          # it can be adjusted here
          if (vi < length(full_pag_i)) {
            sub_ft <- flextable::delete_part(x = sub_ft, part = "footer")
            sub_ft <- sub_ft %>% rtables.officer:::.remove_hborder(part = "body", w = "bottom")
          }
          
          l_ft[[vi]] <- sub_ft
          
          cum_ii <- tail(ii, 1)
        }
        
        return(l_ft)
        
      }
    )
    
    if (is.null(file) && length(pags) > 1) {
      ret <- unlist(ret, recursive = FALSE)
    }
    return(ret)
  }
  
  
  matform <- rtables::matrix_form(tt, fontspec = fontspec, 
                                  indent_rownames = FALSE)
  body <- formatters::mf_strings(matform)
  spans <- formatters::mf_spans(matform)
  mpf_aligns <- formatters::mf_aligns(matform)
  hnum <- formatters::mf_nlheader(matform)
  rdf <- rtables::make_row_df(tt)
  
  # NOTE: convert the '>=', '<=', etc symbols
  body <- junco:::strmodify(body, string_map)
  
  
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
  flx <- flextable::qflextable(content) %>%
    rtables.officer:::.remove_hborder(part = "body", w = "bottom")
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
  
  # NOTE: this block of code calculates where to put horizontal borders
  # within the Header
  l_pos <- list()
  
  if (hnum > 1) {
    for (i in seq(hnum - 1, 1)) {
      sel <- formatters::spans_to_viscell(spans[i, ])
      flx <- flextable::add_header_row(flx, top = TRUE, 
                                       values = as.vector(hdr[i, sel]),
                                       colwidths = as.integer(spans[i, sel]))
      
      col_widths_in_header <- as.integer(spans[i, sel])
      cnt <- 1
      for (j in seq(1, length(col_widths_in_header))) {
        cur_width <- col_widths_in_header[j]
        if (cur_width > 1 &&
            !grepl("^N=", as.vector(hdr[i + 1, sel])[j]) &&
            trimws(as.vector(hdr[i, sel])[j]) != "") {
          l_pos <- append(l_pos, list(c(i, cnt)))
        }
        cnt <- cnt + cur_width
      }
    }
  }
  nr_body <- flextable::nrow_part(flx, part = "body")
  nr_header <- flextable::nrow_part(flx, part = "header")
  flx <- flx %>% rtables.officer:::.remove_hborder(part = "header", w = "all") %>% 
    rtables.officer:::.add_hborder("header", ii = c(0, hnum), border = border)
  
  for (ij in l_pos) {
    i <- ij[1]
    j <- ij[2]
    flx <- flx %>% flextable::hline(part = "header", i = i, j = j, border = border)
  }
  # END
  
  flx <- flx %>%
    rtables.officer:::.apply_alignments(mpf_aligns[seq_len(hnum), , drop = FALSE], "header") %>%
    rtables.officer:::.apply_alignments(mpf_aligns[-seq_len(hnum), , drop = FALSE], "body")
  checkmate::check_number(indent_size, null.ok = TRUE)
  if (is.null(indent_size)) {
    indent_size <- matform$indent_size * rtables.officer::word_mm_to_pt(1)
  }
  else {
    indent_size <- indent_size * rtables.officer::word_mm_to_pt(1)
  }
  # NOTE:
  flx <- flextable::line_spacing(flx, space = 0, part = "body")
  indent_size <- rtables.officer::word_mm_to_pt(3.175)
  left_right_fixed_margins <- 0
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
    # NOTE: this line adds the left padding to each row column 1 (Body only)
    flx <- flextable::padding(flx, i = i, j = 1,
                              # padding.left = indent_size * rdf$indent[[i]] + left_right_fixed_margins,
                              padding.left = indent_size * updated_indents[[i]] + left_right_fixed_margins, # NOTE:
                              padding.right = left_right_fixed_margins,
                              padding.top = 0, padding.bottom = 0,
                              part = "body")
  }
  for (i in seq_len(nr_header)) {
    # NOTE: conversion ratio
    # 1 inches = 72 points = 25.4 mm
    # I'd like 0.1 inches every 2 spaces
    # 0.05 inches = 3.6 points = 1.27 mm
    leading_spaces_count <- nchar(hdr[i, 1]) - 
      nchar(stringi::stri_replace(hdr[i, 1], regex = "^ +", ""))
    header_indent_size <- leading_spaces_count * rtables.officer::word_mm_to_pt(1.27)
    hdr[i, 1] <- stringi::stri_replace(hdr[i, 1], regex = "^ +", "")
    
    flx <- flextable::compose(flx, i = i, j = 1, value = flextable::as_paragraph(hdr[i, 1]), part = "header")
    
    # NOTE: this line adds the left padding to each row column 1 (Header only)
    flx <- flextable::padding(flx, i = i, j = 1,
                              padding.left = header_indent_size + left_right_fixed_margins,
                              padding.right = left_right_fixed_margins, 
                              part = "header")
  }
  
  footers_with_blank_line <- c()
  if (length(matform$ref_footnotes) > 0 && isTRUE(integrate_footers)) {
    # NOTE: add 2 line above and below footnotes
    footers_with_blank_line <- c("", matform$ref_footnotes)
    flx <- flextable::add_footer_lines(flx, values = footers_with_blank_line) %>% 
      rtables.officer:::.add_hborder(part = "body", ii = nrow(content), border = border) %>% 
      rtables.officer:::.add_hborder(part = "footer", ii = length(footers_with_blank_line), border = border)
    

  }
  if (length(formatters::all_footers(tt)) > 0 && isTRUE(integrate_footers)) {
    # NOTE: add 2 line above and below footnotes
    footers_with_blank_line <- c("", formatters::all_footers(tt))
    flx <- flextable::add_footer_lines(flx, values = footers_with_blank_line) %>% 
      rtables.officer:::.add_hborder(part = "body", ii = nrow(content), border = border) %>% 
      rtables.officer:::.add_hborder(part = "footer", ii = length(footers_with_blank_line), border = border)
    
    
  }
  
  # NOTE: the following block adds the footer, this is, the last line below footnotes
  footer_text <- paste0(
    "[", tolower(tblid), ".docx]",
    "[", tidytlg:::getFileName(), "]",
    toupper(format(Sys.time(), format = "%d%b%Y, %H:%M"))
  )
  flx <- flextable::add_footer_lines(flx, values = footer_text)
  # here you can use ii = nrow(content), nr_body, ...
  flx <- flx %>%
    rtables.officer:::.add_hborder(part = "body", ii = nr_body, border = border)
  
  flx <- rtables.officer:::.apply_themes(flx, theme = theme, tbl_row_class = rtables::make_row_df(tt)$node_class)
  
  if (is.null(fontspec)) {
    fontspec <- rtables.officer:::.extract_fontspec(flx)
  }
  if (is.null(colwidths)) {
    colwidths <- formatters::propose_column_widths(matform,
                                                   fontspec = fontspec,
                                                   indent_size = indent_size)
  }
  if (titles_as_header &&
      length(formatters::all_titles(tt)) > 0 &&
      any(nzchar(formatters::all_titles(tt)))) {
    # NOTE: the following block appends the Table ID to the Title,
    # inserts the Title with the hanging indent as part of the Header,
    # and adds the borders above and below the Title
    ts_tbl <- formatters::all_titles(tt)
    ts_tbl <- paste0(tblid, ":", ts_tbl)
    
    
    flx <- insert_title_hanging_indent_v2(flx = flx,
                                       title = ts_tbl,
                                       orientation = orientation,
                                       border = border,
                                       bold_titles = bold_titles)
    # END
  }
  # NOTE: here, even though page width is 8.88 inches, table width has 
  # to be 8.82 inches, so leave a gap of 0.03 inches on both sides
  if (orientation == "landscape") {
    final_cwidths <- (total_page_width - 0.03*2 ) * colwidths/sum(colwidths)
  } else {
    final_cwidths <- total_page_width * colwidths/sum(colwidths)
  }
  flx <- flextable::width(flx, width = final_cwidths)
  flx <- add_hanging_indent_first_column(flx = flx, column_widths = final_cwidths)
  flx <- flextable::set_table_properties(
    flx,
    layout = ifelse(autofit_to_page, 
                    "autofit", "fixed"),
    align = "left", opts_word = list(split = FALSE, keep_with_next = TRUE))
  
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
                                    padding = 0) # NOTE: here, we used to have padding = 10
    # END
  }
  flx <- flextable::fix_border_issues(flx)
  
  
  # NOTE:
  # the following block adds the Title with its own style
  # rtables.officer::add_flextable_separators(ft = flx, trailing_sep = c("-", rep(NA, 29)), border = border)
  # flextable::hline(flx, i = NULL, j = NULL, border = border, part = "header")
  if (isFALSE(titles_as_header)) {
    
    title <- formatters::all_titles(tt)
    title <- paste0(tblid, ":\t", title)
        
    w <- ifelse(orientation == "portrait", 155, 215)
    new_title <- formatters::wrap_string_ttype(str = title,
                width =  w, 
                fontspec = formatters::font_spec("Times", 10L, 1.2),
                collapse = "\n\t",
                wordbreak_ok = FALSE)
    new_title <- sub(": ", ":\t", new_title)

    # 3.175 mm = 9 ms word points = 0.125 inches
    # multiply everything by 10
    # 31.75 mm = 90 ms word points = 1.25 inches
    # rtables.officer::word_mm_to_pt(31.75)
    # argument "padding" is in MS Word points
    # e.g. padding = 90 means 1.25 inches
    # we want padding left and right = 0.125 inches, then
    # padding left and right has to be 9
    # page_gutter_width <- junco:::gutter_width # 0.12 inches
    # inches_to_points <- function(x) {
    #   (90/1.25) * x
    # }
    p <- ifelse(orientation == "portrait", 4.32, 7.8)
    # p <- inches_to_points(page_gutter_width) # 8.64
    fpp <- officer::fp_par(
      text.align = "left", 
      padding.left = p,
      padding.right = p,
      border.top = border
    )
    
    flx_fpt <- rtables.officer:::.extract_font_and_size_from_flx(flx)
    title_style <- flx_fpt$fpt
    title_style$font.size <- title_style$font.size + 1 # 10
    title_style$bold <- bold_titles
    flx <- flx %>% flextable::set_caption(
      # caption = title,
      # caption = "a caption",
      caption = flextable::as_paragraph(flextable::as_chunk(new_title, title_style)),
      word_stylename = "Caption",
      # style = "Caption",
      fp_p = fpp,
      align_with_table = F
    )
  }
  # END
  
  # NOTE: convert the super- and sub-scripts
  flx <- interpret_all_cell_content(flx)
  # NOTE: convert the '>=', '<=', etc symbols
  # flx$header$dataset <- apply(flx$header$dataset, 1:2, junco:::strmodify) %>% as.data.frame()
  # flx$body$dataset <- apply(flx$body$dataset, 1:2, junco:::strmodify) %>% as.data.frame()
  # flx$footer$dataset <- apply(flx$footer$dataset, 1:2, junco:::strmodify) %>% as.data.frame()

  #  NOTE: add the little gap between the bottom borders of 2 spanning headers
  flx <- add_little_gap_bottom_borders_spanning_headers(flx, border)
  
  flx
}

# based on rtables.officer::export_as_docx
my_export_as_docx <- function(tt,
                              output_dir,
                              add_page_break = FALSE,
                              titles_as_header = TRUE, 
                              integrate_footers = TRUE,
                              section_properties = officer::prop_section(
                                page_size = officer::page_size(width = 11, height = 8.5, orient = orientation),
                                page_margins = officer::page_mar(bottom = 1, top = 1, right = 1, left = 1, gutter = 0)
                              ),
                              doc_metadata = NULL,
                              template_file = "doc/template_file.docx",
                              # template_file = NULL,
                              orientation = "portrait",
                              tblid,
                              paginate = FALSE,
                              nosplitin = character(),
                              string_map = junco::default_str_map,
                              combined_docx = FALSE,
                              ...) 
{
  
  # tt2 <- tt
  
  
  checkmate::assert_flag(add_page_break)
  do_tt_error <- FALSE
  
  
  if (combined_docx) {
    my_export_as_docx(
      tt = tt,
      output_dir = output_dir,
      add_page_break = add_page_break,
      titles_as_header = titles_as_header,
      integrate_footers = integrate_footers,
      section_properties = section_properties,
      doc_metadata = doc_metadata,
      template_file = template_file,
      orientation = orientation,
      tblid = paste0(tblid, "allparts"),
      paginate = FALSE,
      string_map = string_map,
      combined_docx = FALSE,
      ... = ...
    )
  }
  
  if (inherits(tt, "VTableTree") || inherits(tt, "listing_df")) {
    tt <- my_tt_to_flextable(tt, titles_as_header = titles_as_header, 
                          integrate_footers = integrate_footers, 
                          tblid = tblid,
                          orientation = orientation,
                          paginate = paginate,
                          nosplitin = nosplitin,
                          string_map = string_map,
                          ...)
    
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
      flex_tbl_list <- mapply(my_tt_to_flextable,
                              tt = tt, 
                              MoreArgs = list(
                                titles_as_header = titles_as_header, 
                                integrate_footers = integrate_footers,
                                tblid = tblid,
                                orientation = orientation,
                                paginate = paginate,
                                nosplitin = nosplitin,
                                string_map = string_map,
                                ...), 
                              SIMPLIFY = FALSE)
    }
    else if (inherits(tt[[1]], "flextable") ||
             (inherits(tt[[1]], "list") && inherits(tt[[1]][[1]], "flextable"))) {
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
    if (inherits(flex_tbl_list[[1]], "list")) {
      flx_fpt <- rtables.officer:::.extract_font_and_size_from_flx(flex_tbl_list[[1]][[1]])
    } else {
      flx_fpt <- rtables.officer:::.extract_font_and_size_from_flx(flex_tbl_list[[1]])
    }
  }
  if (!is.null(template_file) && !file.exists(template_file)) {
    template_file <- NULL
  }
  
  if (paginate && length(flex_tbl_list) > 1) {
    # export individual subtables as separate docx files
    for (i in seq(1, length(flex_tbl_list))) {
      flex_tbl_i <- flex_tbl_list[[i]]
      fmti <- paste0("%0", ceiling(log(length(flex_tbl_list), base = 10)), "d")
      fname <- paste0(tolower(tblid), "part", sprintf(fmti, i), "of", length(flex_tbl_list))
      # fname <- paste0(dirname(file), "/", fname)
      my_export_as_docx(
        tt = flex_tbl_i,
        output_dir = output_dir,
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
        combined_docx = FALSE,
        ... = ...
      )
    }
    
  } else {
    if (!is.null(template_file)) {
      doc <- officer::read_docx(template_file)
      doc <- officer::body_remove(doc)
    }
    else {
      doc <- officer::read_docx()
    }
    doc <- officer::body_set_default_section(doc, section_properties)
    flex_tbl_list <- lapply(flex_tbl_list, function(flx) {
      if (flx$properties$layout != "autofit") {
        # page_width <- section_properties$page_size$width
        # page_width <- junco:::pg_width_by_orient(section_properties$page_size$orient == "landscape")
        page_width <- my_pg_width_by_orient(section_properties$page_size$orient)
        # NOTE: here, even though page width is 8.88 inches, table width has 
        # to be 8.82 inches, so leave a gap of 0.03 inches on both sides
        if (orientation == "landscape") {
          page_width <- (page_width - 0.03 * 2)
        }
        dflx <- dim(flx)
        if (abs(sum(unname(dflx$widths)) - page_width) > 0.01) {
          warning("The total table width does not match the page width. The column widths", 
                  " will be resized to fit the page. Please consider modifying the parameter", 
                  " total_page_width in my_tt_to_flextable().")
          final_cwidths <- page_width * unname(dflx$widths)/sum(unname(dflx$widths))
          flx <- flextable::width(flx, width = final_cwidths)
        }
      }
      flx
    })
    # NOTE: the following block adds the Title
    # if (isFALSE(titles_as_header) && inherits(tt2, "VTableTree")) {
    #   ts_tbl <- formatters::all_titles(tt2)
    #   if (length(ts_tbl) > 0) {
    #     title_style <- flx_fpt$fpt
    #     title_style$font.size <- title_style$font.size + 1 # 10
    #     title_style$bold <- TRUE
    #     doc <- rtables.officer:::add_text_par(doc, ts_tbl, title_style)
    #   }
    # }
    # END
    for (ii in seq(1, length(flex_tbl_list))) {
      flex_tbl_i <- flex_tbl_list[[ii]]
      # NOTE: this align = "left" is the alignment of the entire table relative to the page,
      # not the content within the table
      # calculate vertical pagination, where to place the new page breaks
      doc <- flextable::body_add_flextable(doc, flex_tbl_i, align = "center") # NOTE: here it was align = "left" before
      if (isTRUE(add_page_break) && ii < length(flex_tbl_list)) {
        doc <- officer::body_add_break(doc)
      }
    }
    if (isFALSE(integrate_footers) && inherits(tt, "VTableTree")) {
      matform <- rtables::matrix_form(tt, indent_rownames = TRUE)
      if (length(matform$ref_footnotes) > 0) {
        doc <- rtables.officer:::add_text_par(doc, matform$ref_footnotes, flx_fpt$fpt_footer)
      }
      if (length(formatters::all_footers(tt)) > 0) {
        doc <- rtables.officer:::add_text_par(doc, formatters::all_footers(tt), 
                                              flx_fpt$fpt_footer)
      }
    }
    if (!is.null(doc_metadata)) {
      doc <- do.call(officer::set_doc_properties, c(list(x = doc), 
                                                    doc_metadata))
    }

    
    add_title_style_caption(doc)
    
    
    print(doc, target = paste0(output_dir, "/", tolower(tblid), ".docx"))
    invisible(TRUE)
  }
  
  
}


