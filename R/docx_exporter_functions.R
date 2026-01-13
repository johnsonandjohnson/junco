dps_markup_df_docx <- tibble::tibble(
  keyword = c("super", "sub"),
  replace_by = c("flextable::as_sup", "flextable::as_sub")
)

remove_table_shading <- function(doc) {
  # by default, Word adds a table shading white, which covers the watermark
  # the XML nodes responsible for this are:
  # <w:shd w:val="clear" w:color="auto" w:fill="FF00FF"/>
  # if we delete all these nodes, then the table becomes transparent

  xml2::xml_remove(xml2::xml_find_all(doc$doc_obj$get(), ".//w:shd"))
}

remove_security_popup_page_numbers <- function(doc, tlgtype = "Table",
                                               pagenum = tlgtype == "Listing") {
  # if we are working with listings, we previously had added
  # the page numbers using officer::run_word_field()
  # the problem with this function is that it inserts in the XML
  # the field w:dirty="true", see:
  # https://github.com/davidgohel/officer/blob/master/R/ooxml_run_objects.R#L225
  # w:dirty="true" tells Word that the current "Page" and "NumPages" fields
  # are outdated and need to updated.
  # when trying to open such docx, a security pop up emerges saying:
  # "This document contains fields that may refer to other files. Do you want
  # to update the fields in the document?"
  # To disable that pop up, we just need to remove field w:dirty="true".
  # This functions does that.

  # this function must only be run for Word docs that
  # contain page numbers in the footers, i.e. listings
  # look for all nodes with attribute w:dirty="true"
  # for example:
  # [1] <w:fldChar w:fldCharType="begin" w:dirty="true"/>
  # [2] <w:instrText xml:space="preserve" w:dirty="true">Page</w:instrText>
  # [3] <w:fldChar w:fldCharType="end" w:dirty="true"/>
  # [4] <w:fldChar w:fldCharType="begin" w:dirty="true"/>
  # [5] <w:instrText xml:space="preserve" w:dirty="true">NumPages</w:instrText>
  # [6] <w:fldChar w:fldCharType="end" w:dirty="true"/>
  # and remove that attribute

  if (tlgtype != "Listing" || isFALSE(pagenum)) {
    return(invisible(NULL))
  }

  l_x <- xml2::xml_find_all(doc$doc_obj$get(), ".//w:instrText[@w:dirty='true'] | .//w:fldChar[@w:dirty='true']")
  for (x in l_x) {
    xml2::xml_set_attr(x = x, attr = "w:dirty", value = "NULL")
  }
}

insert_keepNext_vertical_pagination <- function(tt, flx) {
  # this function updates flx by calling flextable::keep_with_next()

  # calculate where to add the page breaks
  df <- utils::getFromNamespace("tt_to_tbldf", "junco")(tt = tt)
  newrows <- df$newrows
  idx_page_breaks <- as.integer(df$row_type == "VALUE" & df$newrows == 1)
  idx <- which(newrows == 1)
  new_idx_page_breaks <- idx_page_breaks
  for (i in rev(idx)) {
    new_idx_page_breaks <- append(new_idx_page_breaks, 0, after = i - 2)
  }
  new_idx_page_breaks <- which(new_idx_page_breaks == 1)

  # update the flextable
  flx <- flx |>
    flextable::keep_with_next(i = new_idx_page_breaks, value = TRUE, part = "body")

  # remove the lines just above the beginning of the chunk
  # these lines are expected to be all blank
  # they are not needed because there will be a page break in their place
  # i.e. flx$body$dataset[new_idx_page_breaks - 1, ] should be all blank

  mask <- flx$body$dataset[new_idx_page_breaks - 1, ] |> apply(1, function(x) {
    all(x == "")
  })
  lines_to_remove <- new_idx_page_breaks[mask] - 1
  flx <- flx |> flextable::delete_rows(part = "body", i = lines_to_remove)

  return(flx)
}

add_vertical_pagination_XML <- function(doc) {
  # look for all nodes '<w:keepNext/>'
  l_x <- xml2::xml_find_all(doc$doc_obj$get(), ".//w:keepNext")

  # look for a parent 'w:pPr'
  # this parent should have a first child 'w:pageBreakBefore'
  for (x in l_x) {
    siblings <- x |> xml2::xml_siblings()

    has_jc_left <- any(
      sapply(siblings, function(node) {
        xml2::xml_name(node) == "jc" && xml2::xml_attr(node, "val") == "left"
      })
    )

    if (has_jc_left) {
      # insert pageBreakBefore only if one of the siblings is:
      # <w:jc w:val="left"/>
      # i.e. the first column in that row
      x |> xml2::xml_add_sibling(.value = "w:pageBreakBefore")
    }
  }
}


add_title_style_caption <- function(doc, string_to_look_for) {
  # this function modifies the XML of the docx to add the "Caption" style to the Title

  s_xpath <- paste0("//*[contains(text(),'", string_to_look_for, "')]")
  l_x <- doc$doc_obj$get() |>
    xml2::xml_find_all(s_xpath)

  for (x2 in l_x) {
    # look for a parent 'w:pPr'
    # this parent should have a first child 'w:pStyle w:val="Caption"'
    x <- x2 |> xml2::xml_parent()
    children <- x |> xml2::xml_children()
    while (!any(xml2::xml_name(children) == "pPr")) {
      x <- x |> xml2::xml_parent()
      children <- x |> xml2::xml_children()
    }

    # set style "Caption"
    child_i <- which(xml2::xml_name(children) == "pPr") |> head(1)
    x <- x |> xml2::xml_child(child_i)
    x |> xml2::xml_add_child(.value = 'w:pStyle w:val="Caption"', .where = 0)

    # set hanging indent of 0.8 inches
    children <- x |> xml2::xml_children()
    child_i <- which(xml2::xml_name(children) == "ind") |> head(1)
    x <- x |> xml2::xml_child(child_i)
    xml2::xml_set_attr(x, "w:hanging", 1152)
    xml2::xml_set_attr(x, "w:left", 1152)
  }
}

add_little_gap_bottom_borders_spanning_headers <- function(
  flx,
  border = flextable::fp_border_default(width = 0.75, color = "black")
) {
  n <- nrow(flx$header$styles$cells$border.width.bottom$data)
  spanning_headers <- flx$header$spans$rows
  for (i in seq_len(n - 1)) {
    j <- which(flx$header$styles$cells$border.width.bottom$data[i, ] > 0)
    if (length(which(spanning_headers[i, j] > 0)) > 1) {
      # remove the border before inserting a paragraph border
      flx <- flextable::border(
        x = flx,
        part = "header",
        i = i,
        j = j,
        border.bottom = officer::fp_border(color = "white", width = 0.1)
      )
      flx <- flextable::style(
        x = flx,
        part = "header",
        i = i,
        j = j,
        pr_c = officer::fp_cell(margin.right = 3, margin.left = 3, vertical.align = "bottom"),
        pr_p = officer::fp_par(
          border.bottom = border,
          text.align = "center"
        )
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
      size = font_size
    )

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
    left_padding_in_inches <- (table_of_paddings[[i, 1]] / 9) * 0.125
    # this is the available space for the string to be displayed at a particular level,
    # which is equal to the width of the first column, minus the left padding at that level
    available_space_in_inches <- cw_in_inches - left_padding_in_inches

    s <- flx$body$dataset[[i, 1]]
    new_s <- wrap_string_with_indent(s,
      max_width_inch = available_space_in_inches,
      dpi = 78
    )

    if (grepl("\n\t", new_s)) {
      # insert new_s in row = i, j = 1
      flx <- flextable::compose(flx,
        part = "body", i = i, j = 1,
        value = flextable::as_paragraph(new_s)
      )
      # insert a tab stop at current indentation level + hanging indent
      # nolint start
      flx <- flextable::tab_settings(flx,
        part = "body", i = i, j = 1,
        value = officer::fp_tabs(officer::fp_tab(pos = left_padding_in_inches + hanging_indent, style = "left"))
      )
      # nolint end
    }
  }

  return(flx)
}

insert_footer_text <- function(flx, tblid) {
  if (is.null(getOption("docx.add_datetime")) || isTRUE(getOption("docx.add_datetime"))) {
    footer_text <- paste0(
      "[", tolower(tblid), ".docx]",
      "[", utils::getFromNamespace("get_file_name", "tidytlg")(), "] ",
      toupper(format(Sys.time(), format = "%d%b%Y, %H:%M"))
    )
    flx <- flextable::add_footer_lines(flx, values = footer_text)
  }
  return(flx)
}

insert_title_hanging_indent_v3 <- function(flx,
                                           title,
                                           border = flextable::fp_border_default(width = 0.75, color = "black")) {
  # this version of the function inserts the Title as a header but does not attempt
  # to simulate the hanging indent. Instead, it adds the string as is, and the hanging indent
  # will be handled further downstream when exporting to docx by manipulating the XML.
  # see function "add_title_style_caption()"

  if (flextable::nrow_part(flx, "header") == 0) {
    title_font_size <- 10
    title_font_family <- "Times New Roman"
  } else {
    flx_fpt <- utils::getFromNamespace(".extract_font_and_size_from_flx", "rtables.officer")(flx)
    title_style <- flx_fpt$fpt
    title_font_size <- title_style$font.size + 1 # 10
    title_font_family <- title_style$font.family
  }


  new_title <- sub(":", ":\t", title)

  flx <-
    utils::getFromNamespace(".add_titles_as_header", "rtables.officer")(flx, all_titles = new_title) |>
    flextable::padding(part = "header", i = 1, padding.left = 0) |>
    flextable::fontsize(part = "header", i = 1, size = title_font_size) |>
    flextable::border(part = "header", i = 1, border.top = border, border.bottom = border)

  # nolint start
  flx <- flx |> flextable::tab_settings(
    i = 1, j = 1, part = "header",
    value = officer::fp_tabs(officer::fp_tab(pos = 0.8, style = "left"))
  )
  # nolint end

  flx <- flextable::style(x = flx, part = "header", i = 1, pr_p = officer::fp_par(word_style = "Caption"))

  return(flx)
}

interpret_cell_content <- function(str_before, markup_df_docx = dps_markup_df_docx) {
  if (inherits(str_before, "list") && length(str_before) == 1 &&
        inherits(str_before[[1]], "character") && length(str_before[[1]]) > 1) {
    str_before <- paste(str_before[[1]], collapse = "")
  }

  pos_start <- gregexpr("~\\{|~\\[", str_before)[[1]]
  pos_end <- gregexpr("\\}|\\]", str_before)[[1]]
  pos_end <- pos_start |>
    lapply(function(x) {
      y <- which(pos_end > x)
      if (length(y) <= 0) {
        return(NA)
      }
      return(pos_end[min(y)])
    }) |>
    unlist()
  pos <- data.frame(pos_start = pos_start, pos_end = pos_end)
  pos <- pos |> dplyr::filter(!is.na(pos_end))
  pos$replacement <- ""

  for (i in seq_len(nrow(pos))) {
    text_inside_brackets <-
      substr(str_before, pos[i, "pos_start"] + 2, pos[i, "pos_end"] - 1)
    if (grepl(" ", text_inside_brackets)) {
      operation <- sub(pattern = " .*", "", text_inside_brackets)
      operation <- trimws(x = operation)
      text_after_operation <-
        sub("^[^[:space:]]*\\s", "", text_inside_brackets)
      text_after_operation <- trimws(x = text_after_operation)
      if (operation %in% markup_df_docx[["keyword"]]) {
        idx <- which(operation == markup_df_docx[["keyword"]]) |> head(1)
        new_op <- as.character(markup_df_docx[idx, "replace_by"])
        pos[i, "replacement"] <-
          paste0(", ", new_op, "('", text_after_operation, "'), ")
      } else {
        # the operation is not found in the 'markup_df_docx' dictionary,
        # e.g. operation 'optional'
        # in this case, just add the text after the operation as is, without
        # interpreting it.
        pos[i, "replacement"] <- paste0(", '", text_after_operation, "', ")
      }
    }
  }

  # from now on we only need 'str_before' and 'pos'
  res <- ""
  j <- 1
  for (i in seq_len(nrow(pos))) {
    res <- paste0(res, "'", substr(str_before, j, pos[i, "pos_start"] - 1), "'")
    res <- paste0(res, pos[i, "replacement"])
    j <- pos[i, "pos_end"] + 1
  }
  if (j <= nchar(str_before)) {
    res <- paste0(res, "'", substr(str_before, j, nchar(str_before)), "'")
  } else {
    res <- sub(", $", "", res)
  }

  res <- paste0("flextable::as_paragraph(", res, ")")
  res
}


interpret_all_cell_content <- function(flx, markup_df_docx = dps_markup_df_docx) {
  pattern <- "~\\[|~\\{"

  # Title and Headers
  tmp <- flx$header$content$data |> apply(1:2, function(x) {
    return(x[[1]]$txt)
  })
  locations <- which(apply(tmp, 1:2, function(x) grepl(pattern, x)), arr.ind = TRUE)
  for (idx in seq_len(nrow(locations))) {
    i <- locations[idx, "row"]
    j <- locations[idx, "col"]
    str_before <- tmp[i, j]
    str_after <- interpret_cell_content(str_before, markup_df_docx)
    flx <- flextable::compose(flx,
      part = "header", i = i, j = j,
      value = eval(parse(text = str_after))
    )
  }

  # Body
  # only look in the first column of the body
  tmp <- flx$body$content$data[, 1] |> lapply(function(x) x$txt)
  tmp <- tmp |>
    lapply(function(x) paste(x, collapse = "")) |>
    unlist()
  locations <- which(grepl(pattern, tmp))
  for (idx in seq_along(locations)) {
    i <- locations[idx]
    j <- 1
    str_before <- tmp[i]
    str_after <- interpret_cell_content(str_before, markup_df_docx)
    flx <- flextable::compose(flx,
      part = "body", i = i, j = j,
      value = eval(parse(text = str_after))
    )
  }

  # Footers
  tmp <- flx$footer$content$data |> apply(1:2, function(x) {
    return(x[[1]]$txt)
  })
  # only look in the first column of the footer
  matches <- as.data.frame(lapply(tmp[, 1] |> as.data.frame(), function(col) grepl(pattern, col)))
  locations <- which(as.matrix(matches), arr.ind = TRUE)
  for (idx in seq_len(nrow(locations))) {
    i <- locations[idx, "row"]
    j <- locations[idx, "col"]
    str_before <- tmp[i, j]
    str_after <- interpret_cell_content(str_before, markup_df_docx)
    flx <- flextable::compose(flx,
      part = "footer", i = i, j = j,
      value = eval(parse(text = str_after))
    )
  }

  flx
}

my_insert_empty_rows <- function(huxme, newrows) {
  # this is a simplified version of tidytlg:::insert_empty_rows()
  # https://github.com/pharmaverse/tidytlg/blob/main/R/insert_empty_rows.R
  # This is to avoid calling a non-exported function
  
  assertthat::assert_that(nrow(huxme) == length(newrows))
  
  data_columns <- names(huxme)
  emptyrow <- huxme[NA, ][1, ]
  emptyrow[, data_columns] <- ""
  cum_sum <- cumsum(newrows)
  df_chunks <- split(huxme, cum_sum)
  df_chunks <- lapply(df_chunks, function(chunk) {
    rbind(chunk, emptyrow)
  })
  names(df_chunks) <- NULL
  merged_df <- do.call(rbind, df_chunks)
  merged_df <- merged_df[-nrow(merged_df), ]
  attr(merged_df, "row.names") <- as.integer(seq_len(nrow(merged_df)))
  return(merged_df)
}

my_apply_alignments <- function(flx, aligns_df, part) {
  # this is a copy of rtables.officer:::.apply_alignments()
  # https://github.com/insightsengineering/rtables.officer/blob/main/R/as_flextable.R#L504
  # This is to avoid calling a non-exported function
  
  # List of characters you want to search for
  search_chars <- unique(c(aligns_df))
  
  # Loop through each character and find its indexes
  for (char in search_chars) {
    indexes <- which(aligns_df == char, arr.ind = TRUE)
    tmp_inds <- as.data.frame(indexes)
    flx <- flx |>
      flextable::align(
        i = tmp_inds[["row"]],
        j = tmp_inds[["col"]],
        align = char,
        part = part
      )
  }
  
  flx
}

#' Obtain the default theme for the docx
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' This function is based on [rtables.officer::theme_docx_default()].
#' See notes to understand why this is experimental.
#'
#'
#' @param font (`string`)\cr font. Defaults to "Times New Roman".
#' @param font_size (`integer(1)`)\cr font size. Defaults to 9.
#' @param cell_margins (`numeric(1) or numeric(4)`)\cr
#' a numeric or a vector of four numbers indicating
#' c("left", "right", "top", "bottom"). It defaults to 0mm in Word pt to all 4 margins.
#' @param bold (`character`)\cr parts of the table text that should be in bold.
#' Can be any combination of c("header", "content_rows", "label_rows", "top_left").
#' The first one renders all column names bold (not topleft content).
#' The second and third option use formatters::make_row_df() to render
#' content or/and label rows as bold.
#' @param bold_manual (`named list or NULL`)\cr list of index lists.
#' See example for needed structure. Accepted groupings/names are c("header", "body").
#' @param border (`fp_border`)\cr border to use. Defaults to width = 0.75
#' and color = "black"
#'
#' @note
#' This function has been tested for common use cases but may not work or have
#' unexpected or undesired behavior in corner cases. As such it is not considered
#' fully production ready and is being made available for further testing
#' and early adoption. Please report any issues you encounter to the developers.
#' This function may be removed from junco in the future if the functionality
#' is merged into `rtables.officer`.
#'
#' @returns a function that applies the given theme to a flextable.
#' @export
theme_docx_default_j <- function(
  font = "Times New Roman",
  font_size = 9L,
  cell_margins = c(0, 0, 0, 0),
  bold = c("header", "content_rows", "label_rows", "top_left"),
  bold_manual = NULL,
  border = flextable::fp_border_default(width = 0.75, color = "black")
) {
  function(flx, ...) {
    if (!inherits(flx, "flextable")) {
      stop(sprintf(
        "Function `%s` supports only flextable objects.",
        "theme_box()"
      ))
    }
    checkmate::assert_int(font_size, lower = 6, upper = 12)
    checkmate::assert_string(font)
    checkmate::assert_subset(bold, eval(formals(theme_docx_default_j)$bold),
      empty.ok = TRUE
    )
    if (length(cell_margins) == 1) {
      cell_margins <- rep(cell_margins, 4)
    }
    checkmate::assert_numeric(cell_margins, lower = 0, len = 4)
    args <- list(...)
    tbl_row_class <- args$tbl_row_class
    tbl_ncol_body <- flextable::ncol_keys(flx)
    # NOTE: this is new
    flx <- flextable::fontsize(flx, size = font_size, part = "all") |>
      flextable::fontsize(size = font_size, part = "header") |>
      flextable::font(fontname = font, part = "all")


    flx <- flx |>
      flextable::valign(j = seq(2, tbl_ncol_body), valign = "bottom", part = "body") |>
      flextable::valign(j = 1, valign = "bottom", part = "all") |>
      flextable::valign(j = 1, valign = "bottom", part = "header") |>
      flextable::valign(j = seq(2, tbl_ncol_body), valign = "bottom", part = "header")
    flx <- flextable::padding(flx, part = "header", padding = 0, j = -1)
    flx <-
      utils::getFromNamespace(
        ".apply_indentation_and_margin",
        "rtables.officer"
      )(flx, cell_margins = cell_margins,
        tbl_row_class = tbl_row_class, tbl_ncol_body = tbl_ncol_body)
    if (any(tbl_row_class == "LabelRow")) {
      flx <- flextable::padding(flx,
        j = 1, i = which(tbl_row_class == "LabelRow"),
        padding.top = 0,
        padding.bottom = 0,
        part = "body"
      )
    }
    if (any(tbl_row_class == "ContentRow")) {
      flx <- flextable::padding(flx,
        i = which(tbl_row_class == "ContentRow"),
        padding.top = 0,
        padding.bottom = 0,
        part = "body"
      )
    }
    flx <- flextable::line_spacing(flx, space = 1, part = "all")

    if (any(bold == "header")) {
      flx <- flextable::bold(flx,
        j = seq(2, tbl_ncol_body),
        part = "header"
      )
    }
    if (any(bold == "content_rows")) {
      if (is.null(tbl_row_class)) {
        stop("bold = \"content_rows\" needs tbl_row_class = rtables::make_row_df(tt).")
      }
      flx <- flextable::bold(flx, j = 1, i = which(tbl_row_class == "ContentRow"), part = "body")
    }
    if (any(bold == "label_rows")) {
      if (is.null(tbl_row_class)) {
        stop("bold = \"content_rows\" needs tbl_row_class = rtables::make_row_df(tt).")
      }
      flx <- flextable::bold(flx, j = 1, i = which(tbl_row_class == "LabelRow"), part = "body")
    }
    if (any(bold == "top_left")) {
      flx <- flextable::bold(flx, j = 1, part = "header")
    }
    flx <- utils::getFromNamespace(".apply_bold_manual", "rtables.officer")(flx, bold_manual)

    # NOTE: the following block styles the footer and footnotes
    n_footnotes <- flextable::nrow_part(flx, "footer")
    if (n_footnotes > 0) {
      if (is.null(getOption("docx.add_datetime")) || isTRUE(getOption("docx.add_datetime"))) {
        flx <- flx |>
          flextable::fontsize(part = "footer", i = n_footnotes, size = 8) |>
          flextable::align(part = "footer", i = n_footnotes, align = "right") |>
          utils::getFromNamespace(".remove_hborder", "rtables.officer")(part = "footer", w = "bottom") |>
          utils::getFromNamespace(
            ".add_hborder",
            "rtables.officer"
          )(part = "footer", ii = n_footnotes - 1, border = border)
      }
      flx <- flx |>
        flextable::padding(padding = 0, part = "footer")
    }
    # END

    flx
  }
}


#' Convert a VTableTree or a listing_df object to a flextable
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' This function is based on [rtables.officer::tt_to_flextable()].
#' See notes to understand why this is experimental.
#'
#' @param tt a VTableTree or a listing_df object
#' @param tblid Character. Output ID to be displayed in the Title and last line of footer.
#' @param theme (optional) a function factory. See theme_docx_default_j()
#' or rtables.officer::theme_docx_default() for more details.
#' @param border (optional) an `fp_border` object.
#' @param titles_as_header (optional) Default = TRUE.
#' @param bold_titles (optional) Default = TRUE.
#' @param integrate_footers (optional) Default = TRUE.
#' @param counts_in_newline (optional) Default = FALSE.
#' @param paginate (optional) Default = FALSE.
#' @param fontspec (optional) a font_spec object.
#' @param lpp (optional) Default = NULL. Not used.
#' @param cpp (optional) Default = NULL. Not used.
#' @param colwidths (`numeric` vector)\cr Column widths for the table
#' @param label_width_ins (`numeric`)\cr Label width in inches
#' @param tf_wrap (optional) Default = FALSE. Not used.
#' @param max_width (optional) Default = NULL. Not used.
#' @param total_page_height (optional) Default = 10. Not used.
#' @param total_page_width (optional). No need to be specified by end user.
#' Set to 6.38 ("portrait") or 8.88 ("landscape").
#' @param orientation (optional) Default = "portrait".
#' One of: "portrait", "landscape".
#' @param nosplitin (optional) Default = character(). Named list.
#' @param string_map (optional) Default = default_str_map.
#' @param markup_df_docx (optional) Default = dps_markup_df_docx.
#' @param reduce_first_col_indentation (optional) Default = FALSE. Flag to reduce
#' by 1 the indentation if we have vertical pagination. No need to be specified by
#' the end user.
#' @param tlgtype (optional). No need to be specified by end user.
#' @param col_gap (optional). Default = 3 (Tables) or 0.5 (Listings).
#' @param pagenum (optional). Default = FALSE (Tables) or TRUE (Listings).
#' @param round_type (`"iec"` or `"sas"`)\cr the type of rounding to perform. iec,
#' the default, performs rounding compliant with IEC 60559, while
#' sas performs nearest-value rounding consistent with rounding within SAS.
#' See `[formatters::format_value()]` for more details.
#' @param alignments (`list`)\cr List of named lists. Vectorized.
#' (Default = `list()`) Used to specify individual column or cell alignments.
#' Each named list contains `row`, `col`, and `value`.
#' @param border_mat (`matrix`)\cr A `m x k` matrix where m is the number of columns of `tt`
#' and k is the number of lines the header takes up. See [tidytlg::add_bottom_borders]
#' for what the matrix should contain. Users should only specify this when the
#' default behavior does not meet their needs.
#' @param ... other arguments.
#'
#'
#' @note
#' This function has been tested for common use cases but may not work or have
#' unexpected or undesired behavior in corner cases. As such it is not considered
#' fully production ready and is being made available for further testing
#' and early adoption. Please report any issues you encounter to the developers.
#' This function may be removed from junco in the future if the functionality
#' is merged into `rtables.officer`.
#'
#' @returns a flextable object.
#' @export
tt_to_flextable_j <- function(
  tt,
  tblid,
  theme = theme_docx_default_j(font = "Times New Roman", font_size = 9L, bold = NULL),
  border = flextable::fp_border_default(width = 0.75, color = "black"),
  titles_as_header = TRUE,
  bold_titles = TRUE,
  integrate_footers = TRUE,
  counts_in_newline = FALSE,
  paginate = FALSE,
  fontspec = formatters::font_spec("Times", 9L, 1.2),
  lpp = NULL,
  cpp = NULL,
  colwidths = def_colwidths(
    tt,
    fontspec,
    col_gap = col_gap,
    label_width_ins = label_width_ins,
    type = tlgtype
  ),
  label_width_ins = 2,
  tf_wrap = !is.null(cpp),
  max_width = cpp,
  total_page_height = 10,
  total_page_width = pg_width_by_orient(orientation == "landscape"),
  orientation = "portrait",
  nosplitin = character(),
  string_map = junco::default_str_map,
  markup_df_docx = dps_markup_df_docx,
  reduce_first_col_indentation = FALSE,
  tlgtype = utils::getFromNamespace("tlg_type", "junco")(tt),
  col_gap = ifelse(tlgtype == "Listing", .5, 3),
  pagenum = ifelse(tlgtype == "Listing", TRUE, FALSE),
  round_type = formatters::obj_round_type(tt),
  alignments = list(),
  border_mat = make_header_bordmat(obj = tt),
  ...) {
  if (inherits(tt, "list")) {
    stop("Please use paginate = TRUE or mapply() to create multiple outputs. export_as_docx accepts lists.")
  }
  if (!inherits(tt, "VTableTree") && !inherits(tt, "listing_df")) {
    stop("Input object is not an rtables' or rlistings' object.")
  }
  checkmate::assert_flag(titles_as_header)
  checkmate::assert_flag(integrate_footers)
  checkmate::assert_flag(counts_in_newline)
  checkmate::assert_number(total_page_width, lower = 1)
  checkmate::assert_number(total_page_height, lower = 1)
  checkmate::assert_numeric(colwidths, lower = 0, len = ncol(tt) + 1, null.ok = TRUE)

  # Validate `alignments` here because of its complicated data structure
  stopifnot("`alignments` must be a list" = is.list(alignments))
  for (alignment in alignments) {
    stopifnot("Each item of `alignments` must be a list" = is.list(alignment))
  }

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


  colwidths_2 <- colwidths
  if (tlgtype == "Table") {
    colwidths <- cwidths_final_adj(
      labwidth_ins = label_width_ins,
      total_width = total_page_width,
      colwidths = colwidths[-1]
    )
  }
  colwidths <- (colwidths / sum(colwidths)) * total_page_width


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
          lpp = lpp,
          cpp = cpp,
          ... = ...,
          colwidths = NULL,
          tf_wrap = tf_wrap,
          max_width = max_width,
          total_page_height = total_page_height,
          total_page_width = total_page_width,
          orientation = orientation,
          tblid = fname,
          nosplitin = nosplitin,
          string_map = string_map,
          markup_df_docx = markup_df_docx,
          reduce_first_col_indentation = (length(full_pag_i) > 1),
          tlgtype = tlgtype,
          col_gap = col_gap,
          pagenum = pagenum,
          round_type = round_type,
          alignments = alignments,
          border_mat = pag_bord_mats[[i]],
        )

        return(sub_ft)
      }
    )

    if (is.null(file) && length(pags) > 1) {
      ret <- unlist(ret, recursive = FALSE)
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
  content <- my_insert_empty_rows(content, newrows)
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
    flx <- flx |>
      my_apply_alignments(mpf_aligns[seq_len(hnum), , drop = FALSE], "header") |>
      my_apply_alignments(mpf_aligns[-seq_len(hnum), , drop = FALSE], "body")
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

  # NOTE: 3.175mm = 9 ms word points = 0.125 inches
  flx <- flextable::line_spacing(flx, space = 0, part = "body")
  indent_size <- rtables.officer::word_mm_to_pt(3.175)
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

    # NOTE: this line adds the left padding to each row in column 1 (Header only)
    flx <- flextable::padding(flx,
      i = i, j = 1,
      padding.left = header_indent_size,
      padding.right = 0,
      part = "header"
    )
  }

  browser()
  footers_with_blank_line <- c()
  # if (length(matform$ref_footnotes) > 0 && isTRUE(integrate_footers)) {
  #   footers_with_blank_line <- c("", matform$ref_footnotes)
  #   footers_with_blank_line <- strmodify(footers_with_blank_line, string_map)
  #   flx <- flextable::add_footer_lines(flx, values = footers_with_blank_line) |>
  #     utils::getFromNamespace(".add_hborder", "rtables.officer")(part = "body", ii = nrow(content), border = border) |>
  #     utils::getFromNamespace(
  #       ".add_hborder",
  #       "rtables.officer"
  #     )(part = "footer", ii = length(footers_with_blank_line), border = border)
  # }
  if (length(formatters::all_footers(tt)) > 0 && isTRUE(integrate_footers)) {
    footers_with_blank_line <- c("", formatters::all_footers(tt))
    footers_with_blank_line <- strmodify(footers_with_blank_line, string_map)
    flx <- flextable::add_footer_lines(flx, values = footers_with_blank_line) |>
      utils::getFromNamespace(".add_hborder", "rtables.officer")(part = "body", ii = nrow(content), border = border) |>
      utils::getFromNamespace(
        ".add_hborder",
        "rtables.officer"
      )(part = "footer", ii = length(footers_with_blank_line), border = border)
  }

  # NOTE: the following block adds the footer, this is, the last line below footnotes
  flx <- insert_footer_text(flx, tblid)

  # here you can use ii = nrow(content), nr_body, ...
  flx <- flx |>
    utils::getFromNamespace(".add_hborder", "rtables.officer")(part = "body", ii = nr_body, border = border)

  flx <- utils::getFromNamespace(".apply_themes", "rtables.officer")(flx, theme = theme, tbl_row_class = rdf$node_class)

  # NOTE: for Listings, vertical alignment is "top" for the whole body
  if (tlgtype == "Listing") {
    flx <- flx |> flextable::valign(part = "body", valign = "top")
    flx <- flx |> flextable::align(part = "body", j = 1, align = "left")
    flx <- flx |> flextable::align(part = "body", j = -1, align = "center")
  }
  # END

  if (is.null(fontspec)) {
    fontspec <- utils::getFromNamespace(".extract_fontspec", "rtables.officer")(flx)
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

    flx <- insert_title_hanging_indent_v3(
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
  if (tlgtype == "Table") {
    flx <- add_hanging_indent_first_column(flx = flx, column_widths = final_cwidths)
  }
  autofit_to_page <- FALSE
  flx <- flextable::set_table_properties(flx,
    layout = ifelse(autofit_to_page, "autofit", "fixed"),
    align = "left", opts_word = list(split = FALSE, keep_with_next = TRUE)
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
      flx <- insert_keepNext_vertical_pagination(tt = tt, flx = flx)
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

    flx_fpt <- utils::getFromNamespace(".extract_font_and_size_from_flx", "rtables.officer")(flx)
    title_style <- flx_fpt$fpt
    title_style$font.size <- title_style$font.size + 1 # 10
    title_style$bold <- bold_titles
    flx <- flx |> flextable::set_caption(
      caption = flextable::as_paragraph(flextable::as_chunk(new_title, title_style)),
      word_stylename = "Caption",
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

#' Export a VTableTree or a listing_df object into docx
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' This function is based on [rtables.officer::export_as_docx()].
#' See notes to understand why this is experimental.
#'
#' @param tt a VTableTree or a listing_df object to export.
#' @param tblid Character. Output ID to be displayed in the Title and last line of footer.
#' @param output_dir a directory path to save the docx.
#' @param theme (optional) a function factory. See theme_docx_default_j()
#' or rtables.officer::theme_docx_default() for more details.
#' @param add_page_break (optional) Default = FALSE.
#' @param titles_as_header (optional) Default = TRUE.
#' @param integrate_footers (optional) Default = TRUE.
#' @param section_properties (optional). A "prop_section" object containing
#' information about page size, orientation, margins, etc.
#' See officer::prop_section() for more details.
#' No need to be specified by end user.
#' @param doc_metadata (optional). Default = NULL.
#' @param template_file (optional). Default = "doc/template_file.docx".
#' Paragraph styles are inherited from this file.
#' @param orientation (optional) Default = "portrait".
#' One of: "portrait", "landscape".
#' @param paginate (optional) Default = FALSE.
#' @param nosplitin (optional) Default = character(). Named list.
#' @param string_map (optional) Default = default_str_map.
#' @param markup_df_docx (optional) Default = dps_markup_df_docx.
#' @param combined_docx (optional). Default = FALSE. Whether to also export an "allparts"
#' docx version.
#' @param tlgtype (optional). No need to be specified by end user.
#' @param col_gap (optional). Default = 3 (Tables) or 0.5 (Listings).
#' @param pagenum (optional). Whether to display page numbers. Only applicable
#' to listings (i.e. for tables and figures this argument is ignored).
#' @param round_type (`"iec"` or `"sas"`)\cr the type of rounding to perform. iec,
#' the default, performs rounding compliant with IEC 60559, while
#' sas performs nearest-value rounding consistent with rounding within SAS.
#' See `[formatters::format_value()]` for more details.
#' @param alignments (`list`)\cr List of named lists. Vectorized.
#' (Default = `list()`) Used to specify individual column or cell alignments.
#' Each named list contains `row`, `col`, and `value`.
#' @param border (optional) an `fp_border` object.
#' @param border_mat (`matrix`)\cr A `m x k` matrix where m is the number of columns of `tt`
#' and k is the number of lines the header takes up. See [tidytlg::add_bottom_borders]
#' for what the matrix should contain. Users should only specify this when the
#' default behavior does not meet their needs.
#' @param watermark (`logical`)\cr whether to display the watermark "Confidential".
#' By default, this is set to FALSE. In the future, this argument will be the
#' actual watermark (i.e. a string) to display.
#' @param ... other parameters.
#'
#' @note
#' This function has been tested for common use cases but may not work or have
#' unexpected or undesired behavior in corner cases. As such it is not considered
#' fully production ready and is being made available for further testing
#' and early adoption. Please report any issues you encounter to the developers.
#' This function may be removed from junco in the future if the functionality
#' is merged into `rtables.officer`.
#'
#' @export
export_as_docx_j <- function(
  tt,
  tblid,
  output_dir,
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
  template_file = system.file("template_file.docx", package = "junco"),
  orientation = "portrait",
  paginate = FALSE,
  nosplitin = character(),
  string_map = junco::default_str_map,
  markup_df_docx = dps_markup_df_docx,
  combined_docx = FALSE,
  tlgtype = utils::getFromNamespace("tlg_type", "junco")(tt),
  col_gap = ifelse(tlgtype == "Listing", .5, 3),
  pagenum = ifelse(tlgtype == "Listing", TRUE, FALSE),
  round_type = formatters::obj_round_type(tt),
  alignments = list(),
  border = flextable::fp_border_default(width = 0.75, color = "black"),
  border_mat = make_header_bordmat(obj = tt),
  watermark = FALSE,
  ...
) {
  # Validate `alignments` here because of its complicated data structure
  stopifnot("`alignments` must be a list" = is.list(alignments))
  for (alignment in alignments) {
    stopifnot("Each item of `alignments` must be a list" = is.list(alignment))
  }

  checkmate::assert_flag(add_page_break)
  checkmate::assert_flag(watermark)

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
      pagenum = pagenum,
      theme = theme,
      round_type = round_type,
      alignments = alignments,
      border = border,
      border_mat = border_mat,
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
            pagenum = pagenum,
            theme = theme,
            round_type = round_type,
            alignments = alignments,
            border = border,
            border_mat = border_mat,
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
    if (inherits(flex_tbl_list[[1]], "list")) {
      flx_fpt <- utils::getFromNamespace(".extract_font_and_size_from_flx", "rtables.officer")(flex_tbl_list[[1]][[1]])
    } else {
      flx_fpt <- utils::getFromNamespace(".extract_font_and_size_from_flx", "rtables.officer")(flex_tbl_list[[1]])
    }
  }
  if (!is.null(template_file) && !file.exists(template_file)) {
    template_file <- NULL
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
        ... = ...
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
        ... = ...
      )
    }
  } else {
    if (isTRUE(watermark)) {
      template_file <- "template_file_watermark"
      template_file <- paste0(template_file, "_", orientation)
      if (pagenum) {
        template_file <- paste0(template_file, "_pagenum")
      }
      template_file <- paste0(template_file, ".docx")
      template_file <- system.file(template_file, package = "junco")
    }

    if (!is.null(template_file)) {
      doc <- officer::read_docx(template_file)
      doc <- officer::body_remove(doc)
    } else {
      doc <- officer::read_docx()
    }

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
    if (isFALSE(watermark)) {
      doc <- officer::body_set_default_section(doc, section_properties)
    }

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
        doc <- utils::getFromNamespace(
          "add_text_par",
          "rtables.officer"
        )(doc, matform$ref_footnotes, flx_fpt$fpt_footer)
      }
      if (length(formatters::all_footers(tt)) > 0) {
        doc <- utils::getFromNamespace(
          "add_text_par",
          "rtables.officer"
        )(doc, formatters::all_footers(tt),
          flx_fpt$fpt_footer)
      }
    }
    if (!is.null(doc_metadata)) {
      doc <- do.call(officer::set_doc_properties, c(
        list(x = doc),
        doc_metadata
      ))
    }
    if (endsWith(tblid, "allparts")) {
      string_to_look_for <- paste0(tblid, ":")
    } else {
      string_to_look_for <- sub(pattern = ":\t.*", replacement = ":", flex_tbl_list[[1]]$header$dataset[1, 1])
    }
    add_title_style_caption(doc, string_to_look_for)
    add_vertical_pagination_XML(doc)
    remove_security_popup_page_numbers(doc, tlgtype, pagenum)
    if (isTRUE(watermark)) {
      remove_table_shading(doc)
    }

    print(doc, target = paste0(output_dir, "/", tolower(tblid), ".docx"))
    invisible(TRUE)
  }
}


#' export_graph_as_docx
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Export graph in DOCX format. See notes to understand why this is experimental.
#'
#' @param g (optional) Default = NULL. A `ggplot2` object, or a list
#' of them, to export. At least one of `g` or `plotnames` must be provided.
#' If both are provided, 'g' precedes and 'plotnames' will be ignored.
#' @param plotnames (optional) Default = NULL. A file path, or a list of them,
#' to previously saved .png files. These will be opened and
#' exported in the output file. At least one of `g` or `plotnames` must be provided.
#' If both are provided, 'g' precedes and 'plotnames' will be ignored.
#' @param tblid Character. Output ID that will appear in the Title and footer.
#' @param output_dir Character. File path where to save the output.
#' @param title (optional) Default = NULL. Character, or list of them,
#' with the titles to be displayed.
#' @param footers (optional) Default = NULL. Character, or list of them,
#' with the footers to be displayed.
#' @param orientation (optional) Default = "portrait".
#' One of: "portrait", "landscape".
#' @param plotwidth (optional) Default = 8. Plot size in units expressed by
#' the units argument. If not supplied, uses the size of the current graphics device.
#' @param plotheight (optional) Default = 5.51. Plot size in units expressed by
#' the units argument. If not supplied, uses the size of the current graphics device.
#' @param units (optional) Default = "in". One of the following units in which the
#' plotwidth and plotheight arguments are expressed: "in", "cm", "mm" or "px".
#' @param border (optional). An `fp_border` object to use as borders for the Title
#' and Footers.
#'
#' @note
#' This function has been tested for common use cases but may not work or have
#' unexpected or undesired behavior in corner cases. As such it is not considered
#' fully production ready and is being made available for further testing
#' and early adoption. Please report any issues you encounter to the developers.
#' This function may be removed from junco in the future if the functionality
#' is merged into `rtables.officer`.
#'
#' @export
export_graph_as_docx <- function(g = NULL,
                                 plotnames = NULL,
                                 tblid,
                                 output_dir,
                                 title = NULL,
                                 footers = NULL,
                                 orientation = "portrait",
                                 plotwidth = 8,
                                 plotheight = 5.51,
                                 units = c("in", "cm", "mm", "px")[1],
                                 border = flextable::fp_border_default(width = 0.75, color = "black")) {
  # TREATMENT OF ARGUMENTS ----

  if (is.null(g) && is.null(plotnames)) {
    stop("Both arguments 'g' and 'plotnames' are NULL. Please provide at least one.")
  }

  if (!is.null(g) && !is.null(plotnames)) {
    message("Both arguments 'g' and 'plotnames' are not NULL. Argument 'g' precedes and 'plotnames' will be ignored.")
    plotnames <- NULL
  }

  if (!is.null(g)) {
    if (length(g) == 1) {
      g <- list(g)
    }

    for (e in g) {
      checkmate::assert_true(ggplot2::is.ggplot(e))
    }
  }

  if (!is.null(plotnames)) {
    if (length(plotnames) == 1) {
      plotnames <- list(plotnames)
    }

    for (e in plotnames) {
      checkmate::assert_character(e)
      checkmate::assert_file_exists(e)
    }
  }

  checkmate::assert_character(tblid)
  checkmate::assert_character(output_dir)
  checkmate::assert_directory_exists(output_dir)
  checkmate::assert_numeric(plotwidth, null.ok = TRUE)
  checkmate::assert_numeric(plotheight, null.ok = TRUE)
  checkmate::assert_choice(units, choices = c("in", "cm", "mm", "px"))
  checkmate::assert_choice(orientation, choices = c("portrait", "landscape"))
  checkmate::assert_character(title)
  checkmate::assert_character(footers, null.ok = TRUE)

  if (is.null(plotnames)) {
    # we have to save the ggplot2 objects (argument 'g') as temporary files
    # and save those paths
    plotnames <- list()
    for (e in g) {
      temp_file <- paste0(base::tempfile(), ".png")
      grDevices::png(temp_file,
        width  = 22,
        height = 14,
        units  = "cm",
        res    = 300,
        type   = "cairo"
      )
      print(e)
      grDevices::dev.off()

      plotnames <- append(plotnames, temp_file)
    }
  }


  # CREATION OF THE FLEXTABLE ----

  # create initial flextable with just 1 column
  flx <- matrix(nrow = length(plotnames), ncol = 1) |>
    as.data.frame() |>
    flextable::flextable()
  flx <- flx |> flextable::delete_part("header")
  nrow_body <- flextable::nrow_part(x = flx, part = "body")

  # set the Title
  ts_tbl <- paste0(tblid, ":", title)
  flx <- insert_title_hanging_indent_v3(flx = flx, title = ts_tbl)

  # set the Body (the plots)
  for (i in seq_along(plotnames)) {
    f <- plotnames[[i]]
    flx <- flextable::compose(
      flx,
      part = "body",
      i = i,
      j = 1,
      value =
        flextable::as_paragraph(flextable::as_image(
          src = f, width = plotwidth,
          height = plotheight, unit = "in"
        ))
    )
  }

  # set the Footers
  if (!is.null(footers)) {
    footers <- c("", footers)
  }
  for (line in footers) {
    flx <- flextable::add_footer_lines(flx, values = line)
  }

  flx <- insert_footer_text(flx, tblid)

  # style the flextable
  flx <- flx |> flextable::border_remove()
  flx <- flx |>
    flextable::fontsize(part = "header", size = 10)
  flx <- flx |>
    flextable::align(part = "body", align = "center")
  flx <- flx |> flextable::valign(part = "body", valign = "top")
  nrow_footers <- flextable::nrow_part(flx, "footer")
  flx <- flx |>
    flextable::fontsize(part = "footer", size = 8) |>
    flextable::align(part = "footer", i = nrow_footers, align = "right") |>
    flextable::padding(padding = 0, part = "footer")
  flx <- flx |>
    flextable::font(fontname = "Times New Roman", part = "all")
  flx <- flx |>
    flextable::border(
      part = "header", i = 1,
      border.top = border, border.bottom = border
    ) |>
    flextable::border(
      part = "body", i = nrow_body,
      border.bottom = border
    )
  if (nrow_footers > 1) {
    flx <- flx |>
      flextable::border(
        part = "footer", i = nrow_footers - 1,
        border.bottom = border
      )
  }

  w <- ifelse(orientation == "portrait", 6.38, 8.82)
  flx <- flx |> flextable::width(width = w)
  flx <- flextable::padding(flx, part = "all", padding = 0)
  # add a little paragraph padding on top so that the figures don't overlap
  # with the bottom border of the Titles
  if (nrow_body > 1) {
    flx <- flextable::padding(flx,
      part = "body", i = seq(2, nrow_body),
      padding.top = 1
    )
  }


  # EXPORT AS DOCX ----
  template_file <- system.file("template_file.docx", package = "junco")
  section_properties <- officer::prop_section(
    page_size = officer::page_size(width = 11, height = 8.5, orient = orientation),
    page_margins = officer::page_mar(bottom = 1, top = 1, right = 1, left = 1, gutter = 0, footer = 1, header = 1)
  )
  doc <- officer::read_docx(template_file)
  doc <- officer::body_remove(doc)
  doc <- officer::body_set_default_section(doc, section_properties)
  doc <- flextable::body_add_flextable(doc, flx, align = "center")
  string_to_look_for <- paste0(tblid, ":")
  add_title_style_caption(doc, string_to_look_for)
  print(doc, target = paste0(output_dir, "/", tolower(tblid), ".docx"))
}
