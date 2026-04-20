# ==========================================
# Junco Hotfix Patch File
# ==========================================
# Summary of injected hotfixes:
# - Hotfix #178: Ensures tt_to_tlgrtf() does not lose the title when exporting an empty listing.
# - Hotfix #165: Implements optional CSV export functionality within tt_to_tlgrtf() (#142).
# - Hotfix #221: Fixes a bug where a_freq_j fails with countsource = altdf in nested row splits
#         (applies to h_a_freq_dataprep, s_freq_j, and a_freq_j).
# ==========================================

pkg <- "junco"

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
  export_csv = FALSE,   #### TODO: hotfix :142 optional csv #165
  output_csv_directory = NULL, #### TODO: hotfix :142 optional csv #165
  ...) {

  #### TODO: hotfix :142 optional csv #165
  get_output_csv_filename <- function(output_csv_directory, fpath, fname) {
    if (is.null(output_csv_directory)) {
      output_csv_filename <- file.path(fpath, paste0(tolower(fname), ".csv"))
    } else if (!dir.exists(output_csv_directory)) {
      output_csv_filename <- file.path(fpath, paste0(tolower(fname), ".csv"))
      message("Output dir for csv ", output_csv_directory,
              " does not exist; csv will be saved as ",
              output_csv_filename)
    } else {
      output_csv_filename <- file.path(output_csv_directory,
                                      paste0(tolower(fname), ".csv"))
      message("Saving csv as ", output_csv_filename)
    }
    return(output_csv_filename)
  } #### TODO: hotfix :142 optional csv #165
  
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
  
  checkmate::assert_flag(export_csv) #### TODO: hotfix :142 optional csv #165
  checkmate::assert_character(output_csv_directory, null.ok = TRUE, len = 1) #### TODO: hotfix :142 optional csv #165
  
  if (tlgtype == "Listing" && nrow(tt) == 0) {
    dat <- as.list(c("No data to report", rep("", ncol(tt) - 1)))
    names(dat) <- names(tt)
    df <- as.data.frame(dat)
    var_labels(df) <- var_labels(tt)
    
    tt <- as_listing(
      df,
      key_cols = get_keycols(tt),
      disp_cols = listing_dispcols(tt),
      main_title = attr(tt, "main_title"),  #### TODO: hotfix in tt_to_tlgrtf() when exporting an empty listing do not lose title #178
      main_footer = attr(tt, "main_footer") #### TODO: hotfix in tt_to_tlgrtf() when exporting an empty listing do not lose title #178
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
          export_csv = export_csv, #### TODO: hotfix :142 optional csv #165
          output_csv_directory = output_csv_directory, #### TODO: hotfix :142 optional csv #165
          label_width_ins = label_width_ins, #### TODO: hotfix :142 optional csv #165
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
          export_csv = export_csv,  #### TODO: hotfix :142 optional csv #165
          output_csv_directory = output_csv_directory, #### TODO: hotfix :142 optional csv #165
          label_width_ins = label_width_ins, #### TODO: hotfix :142 optional csv #165
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
      utils::head(tt, 1),
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
    ### gentlg is not vectorized on wcol.  x.x x.x x.x
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
  
  if (!is.null(fname) && tlgtype == "Table" && is.data.frame(df) && export_csv) { #### TODO: hotfix :142 optional csv #165
    output_csv_filename <- get_output_csv_filename(output_csv_directory, fpath, fname) #### TODO: hotfix :142 optional csv #165
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
    ...
  )
}


h_a_freq_dataprep <- function(
    df,
    labelstr = NULL,
    .var = NA,
    val = NULL,
    drop_levels = FALSE,
    excl_levels = NULL,
    new_levels = NULL,
    new_levels_after = FALSE,
    addstr2levs = NULL,
    .df_row,
    .spl_context,
    .N_col,
    id = "USUBJID",
    denom = c("N_col", "n_df", "n_altdf", "N_colgroup", "n_rowdf", "n_parentdf"),
    variables,
    label = NULL,
    label_fstr = NULL,
    label_map = NULL,
    .alt_df_full = NULL,
    denom_by = NULL,
    .stats,
    countsource = c("df", "altdf", "altdf_subset") #### TODO: hotfix: a freq j fails with countsource = altdf in nested row splits (#221)
) {
  denom <- match.arg(denom)
  
  df <- df[!is.na(df[[.var]]), ]
  .df_row <- .df_row[!is.na(.df_row[[.var]]), ]
  
  # if no stats requested, get all stats
  .stats <- junco_get_stats("a_freq_j", stats_in = .stats, custom_stats_in = NULL)
  
  ### combine all preprocessing of incoming df/.df_row in one function do this outside stats derivation functions
  ### (s_freq_j/) use all of val/excl_levels/drop_levels//new_levels/ label/label_map/labelstr/label_fstr
  upd_dfrow <- h_upd_dfrow(
    df_row = .df_row,
    .var = .var,
    val = val,
    excl_levels = excl_levels,
    drop_levels = drop_levels,
    new_levels = new_levels,
    new_levels_after = new_levels_after,
    addstr2levs = addstr2levs,
    label = label,
    label_map = label_map,
    labelstr = labelstr,
    label_fstr = label_fstr,
    .spl_context = .spl_context
  )
  
  .df_row <- upd_dfrow$df_row
  df <- upd_dfrow$df
  
  val <- upd_dfrow$val
  
  # from here onwards proceed with drop_levels = FALSE action has already been done in h_upd_dfrow, and proper
  # observed values will be passed to val for s_freq_j
  drop_levels <- FALSE
  excl_levels <- NULL
  
  ### derive appropriate alt_df based upon .spl_context and .alt_df_full note that only row-based splits are done for
  ### now only for variables from the first split_rows_by
  alt_df <- h_create_altdf(
    .spl_context,
    .df_row,
    .alt_df_full,
    denom_by = denom_by,
    id = id,
    variables = variables,
    denom = denom
  )
  
  
  if (identical(countsource, "altdf_subset")) { #### TODO: hotfix: a freq j fails with countsource = altdf in nested row splits (#221)
    # prefer explicit val, otherwise use the levels present in .df_row.
    if (!is.null(alt_df) && !is.na(.var) && (.var %in% names(alt_df))) {
      keep_vals <- NULL
      if (!is.null(val)) {
        keep_vals <- val
      } else if (!is.null(.df_row) && !is.null(.df_row[[.var]])) {
        keep_vals <- if (is.factor(.df_row[[.var]])) levels(.df_row[[.var]]) else unique(.df_row[[.var]])
      }
      
      if (!is.null(keep_vals)) {
        keep_vals <- intersect(keep_vals, unique(alt_df[[.var]]))
        if (length(keep_vals) > 0) {
          alt_df <- alt_df[alt_df[[.var]] %in% keep_vals, , drop = FALSE]
          alt_df <- h_update_factor(alt_df, .var, keep_vals)
        }
      }
    }
  } #### TODO: hotfix: a freq j fails with countsource = altdf in nested row splits (#221)
  
  new_denomdf <- alt_df
  
  parentdf <- h_denom_parentdf(.spl_context, denom, denom_by)
  if (denom == "n_parentdf") {
    new_denomdf <- parentdf
  }
  
  return(list(
    df = df,
    .df_row = .df_row,
    val = val,
    drop_levels = drop_levels,
    excl_levels = excl_levels,
    alt_df = alt_df,
    parentdf = parentdf,
    new_denomdf = new_denomdf,
    .stats = .stats
  ))
}



s_freq_j <- function(
    df,
    .var,
    .df_row,
    val = NULL,
    drop_levels = FALSE,
    excl_levels = NULL,
    alt_df,
    parent_df,
    id = "USUBJID",
    denom = c("n_df", "n_altdf", "N_col", "n_rowdf", "n_parentdf"),
    .N_col,
    countsource = c("df", "altdf", "altdf_subset") #### TODO: hotfix: a freq j fails with countsource = altdf in nested row splits (#221)
) {
  if (is.na(.var) || is.null(.var)) {
    stop("Argument .var cannot be NA or NULL.")
  }
  
  countsource <- match.arg(countsource)
  
  if (countsource %in% c("altdf", "altdf_subset")) { #### TODO: hotfix: a freq j fails with countsource = altdf in nested row splits (#221)
    df <- alt_df
  }
  
  .alt_df <- alt_df
  
  n1 <- length(unique(.alt_df[[id]]))
  n2 <- length(unique(df[[id]]))
  
  n3 <- length(unique(.df_row[[id]]))
  
  if (is.null(parent_df)) {
    parent_df <- df
  }
  n4 <- length(unique(parent_df[[id]]))
  
  
  denom <- match.arg(denom) |> switch(
    "n_altdf" = n1,
    "n_df" = n2,
    "n_rowdf" = n3,
    "N_col" = .N_col,
    "n_parentdf" = n4
  )
  
  y <- list()
  
  y$n_altdf <- c("n_altdf" = n1)
  y$n_df <- c("n_df" = n2)
  y$n_rowdf <- c("n_rowdf" = n3)
  y$n_parentdf <- c("n_parentdf" = n4)
  y$denom <- c("denom" = denom)
  
  if (drop_levels) {
    obs_levs <- unique(.df_row[[.var]])
    obs_levs <- intersect(levels(.df_row[[.var]]), obs_levs)
    
    if (!is.null(excl_levels)) obs_levs <- setdiff(obs_levs, excl_levels)
    
    if (!is.null(val)) {
      stop("argument val cannot be used together with drop_levels = TRUE.")
    }
    val <- obs_levs
  }
  
  if (!is.null(val)) {
    df <- df[df[[.var]] %in% val, ]
    .df_row <- .df_row[.df_row[[.var]] %in% val, ]
    
    df <- h_update_factor(df, .var, val)
    .df_row <- h_update_factor(.df_row, .var, val)
  }
  
  if (!is.null(excl_levels) && drop_levels == FALSE) {
    # restrict the levels to the ones specified in val argument
    df <- df[!(df[[.var]] %in% excl_levels), ]
    .df_row <- .df_row[!(.df_row[[.var]] %in% excl_levels), ]
    
    df <- h_update_factor(df, .var, excl_levels = excl_levels)
    .df_row <- h_update_factor(.df_row, .var, excl_levels = excl_levels)
  }
  
  x <- df[[.var]]
  x_unique <- unique(df[, c(.var, id)])[[.var]]
  
  if (identical(levels(df[[.var]]), no_data_to_report_str)) {
    xy <- list()
    nms <- c(
      "count",
      "count_unique",
      "count_unique_fraction",
      "count_unique_denom_fraction"
    )
    xy <- replicate(length(nms), list(setNames(list(NULL), no_data_to_report_str)))
    names(xy) <- nms
    y <- append(y, xy)
  } else {
    y$count <- lapply(
      as.list(table(x, useNA = "ifany")),
      stats::setNames,
      nm = "count"
    )
    
    y$count_unique <- lapply(
      as.list(table(x_unique, useNA = "ifany")),
      stats::setNames,
      nm = "count_unique"
    )
    
    y$count_unique_fraction <- lapply(
      y$count_unique,
      function(x) {
        ## we want to return - when denom = 0
        ## this is built into formatting function, when fraction is NA
        c(x, "p" = ifelse(denom > 0, x / denom, NA))
      }
    )
    
    y$count_unique_denom_fraction <- lapply(
      y$count_unique,
      function(x) {
        ## we want to return - when denom = 0
        ## this is built into formatting function, when fraction is NA
        c(x, "d" = denom, "p" = ifelse(denom > 0, x / denom, NA))
      }
    )
  }
  
  return(y)
}


a_freq_j <- function(
    df,
    labelstr = NULL,
    .var = NA,
    val = NULL,
    drop_levels = FALSE,
    excl_levels = NULL,
    new_levels = NULL,
    new_levels_after = FALSE,
    addstr2levs = NULL,
    .df_row,
    .spl_context,
    .N_col,
    id = "USUBJID",
    denom = c("N_col", "n_df", "n_altdf", "N_colgroup", "n_rowdf", "n_parentdf"),
    riskdiff = TRUE,
    ref_path = NULL,
    variables = list(strata = NULL),
    conf_level = 0.95,
    method = c(
      "wald",
      "waldcc",
      "cmh",
      "ha",
      "newcombe",
      "newcombecc",
      "strat_newcombe",
      "strat_newcombecc"
    ),
    weights_method = "cmh",
    label = NULL,
    label_fstr = NULL,
    label_map = NULL,
    .alt_df_full = NULL,
    denom_by = NULL,
    .stats = c("count_unique_denom_fraction"),
    .formats = NULL,
    .indent_mods = NULL,
    na_str = rep("NA", 3),
    .labels_n = NULL,
    extrablankline = FALSE,
    extrablanklineafter = NULL,
    restr_columns = NULL,
    colgroup = NULL,
    countsource = c("df", "altdf", "altdf_subset") #### TODO: hotfix: a freq j fails with countsource = altdf in nested row splits (#221)
) {
  denom <- match.arg(denom)
  method <- match.arg(method)
  
  if (!is.null(labelstr) && is.na(.var)) {
    stop(
      "Please specify var call to summarize_row_groups when using cfun = a_freq_j, i.e.,\n",
      "summarize_row_groups('varname', cfun = a_freq_j)"
    )
  }
  
  if (denom == "N_colgroup") {
    if (is.null(colgroup)) {
      stop("Colgroup must be specified when denom = N_colgroup.")
    }
    
    checkmate::assert_character(colgroup, null.ok = FALSE, max.len = 1)
    
    if (colgroup == tail(.spl_context$cur_col_split[[1]], 1)) {
      stop(
        "N_colgroup cannot be used when colgroup is lowest column split."
      )
    }
  }
  
  check_alt_df_full(denom, c("n_altdf", "N_colgroup"), .alt_df_full)
  
  res_dataprep <- h_a_freq_dataprep(
    df = df,
    labelstr = labelstr,
    .var = .var,
    val = val,
    drop_levels = drop_levels,
    excl_levels = excl_levels,
    new_levels = new_levels,
    new_levels_after = new_levels_after,
    addstr2levs = addstr2levs,
    .df_row = .df_row,
    .spl_context = .spl_context,
    .N_col = .N_col,
    id = id,
    denom = denom,
    variables = variables,
    label = label,
    label_fstr = label_fstr,
    label_map = label_map,
    .alt_df_full = .alt_df_full,
    denom_by = denom_by,
    .stats = .stats,
    countsource = countsource
  )
  # res_dataprep is list with elements
  # df .df_row val
  # drop_levels excl_levels
  # alt_df parentdf new_denomdf
  # .stats
  # make these elements available in current environment
  df <- res_dataprep$df
  .df_row <- res_dataprep$.df_row
  val <- res_dataprep$val
  drop_levels <- res_dataprep$drop_levels
  excl_levels <- res_dataprep$excl_levels
  alt_df <- res_dataprep$alt_df
  parentdf <- res_dataprep$parentdf
  new_denomdf <- res_dataprep$new_denomdf
  .stats <- res_dataprep$.stats
  
  ## prepare for column based split
  col_expr <- .spl_context$cur_col_expr[[1]]
  ## colid can be used to figure out if we're in the relative risk columns or not
  colid <- .spl_context$cur_col_id[[1]]
  inriskdiffcol <- grepl("difference", tolower(colid), fixed = TRUE)
  
  if (!is.null(colgroup)) {
    colexpr_substr <- h_colexpr_substr(colgroup, .spl_context$cur_col_expr[[1]])
    
    if (is.null(colexpr_substr)) {
      stop("\n Problem a_freq_j: incorrect colgroup specification.")
    }
    
    new_denomdf <- subset(.alt_df_full, eval(parse(text = colexpr_substr)))
    .df_row <- subset(.df_row, eval(parse(text = colexpr_substr)))
  }
  
  if (!inriskdiffcol) {
    if (denom != "N_colgroup" && !is.null(new_denomdf)) {
      ### for this part : perform column split on denominator dataset
      new_denomdf <- subset(new_denomdf, eval(col_expr))
    }
    if (denom == "N_colgroup") {
      denom <- "n_altdf"
    }
    
    x_stats <- s_freq_j(
      df,
      .var = .var,
      .df_row = .df_row,
      val = val,
      drop_levels = drop_levels,
      excl_levels = excl_levels,
      alt_df = new_denomdf,
      parent_df = new_denomdf,
      id = id,
      denom = denom,
      .N_col = .N_col,
      countsource = countsource
    )
    ## remove relrisk stat from .stats
    .stats_adj <- .stats[!(.stats %in% "rr_ci_3d")]
  } else {
    if (riskdiff && is.null(ref_path)) {
      stop("argument ref_path cannot be NULL.")
    }
    ### denom N_colgroup should not be used in layout with risk diff columns
    if (denom == "N_colgroup") {
      stop(
        "denom N_colgroup cannot be used in a layout with risk diff columns."
      )
    }
    if (!riskdiff) {
      trt_var <- NULL
      ctrl_grp <- NULL
      cur_trt_grp <- NULL
    }
    
    if (riskdiff) {
      trt_var_refpath <- h_get_trtvar_refpath(
        ref_path,
        .spl_context,
        df
      )
      # trt_var_refpath is list with elements
      # trt_var trt_var_refspec cur_trt_grp ctrl_grp
      # make these elements available in current environment
      trt_var <- trt_var_refpath$trt_var
      trt_var_refspec <- trt_var_refpath$trt_var_refspec
      cur_trt_grp <- trt_var_refpath$cur_trt_grp
      ctrl_grp <- trt_var_refpath$ctrl_grp
      
      if (!is.null(colgroup) && trt_var == colgroup) {
        stop(
          "\n Problem: a_freq_j: colgroup and treatment variable from ref_path are the same.
             This is not intented for usage with relative risk columns.
             Either remove risk difference columns from layout, set riskdiff = FALSE, or update colgroup."
        )
      }
    }
    
    x_stats <- s_rel_risk_val_j(
      df,
      .var = .var,
      .df_row = .df_row,
      val = val,
      drop_levels = drop_levels,
      excl_levels = excl_levels,
      denom_df = new_denomdf,
      id = id,
      riskdiff = riskdiff,
      # treatment/ref group related arguments
      trt_var = trt_var,
      ctrl_grp = ctrl_grp,
      cur_trt_grp = cur_trt_grp,
      # relrisk specific arguments
      variables = variables,
      conf_level = conf_level,
      method = method,
      weights_method = weights_method
    )
    
    ## this will ensure the following stats will be shown as empty column in relative risk column
    xy <- sapply(
      c(
        "count",
        "count_unique",
        "n_df",
        "n_altdf",
        "n_rowdf",
        "n_parentdf",
        "denom"
      ),
      function(x) {
        stats::setNames(list(x = NULL), x)
      },
      USE.NAMES = TRUE,
      simplify = FALSE
    )
    x_stats <- append(x_stats, xy)
    
    ## restrict to relrisk stat from .stats
    # when both count_unique_fraction and count_unique_denom_fraction are requested, the rr_ci_3d stat is in here twice
    # this does not seem to introduce a problem, although might not be ideal
    # see further
    .stats_adj <- replace(
      .stats,
      .stats %in%
        c(
          "count_unique_fraction",
          "count_unique_denom_fraction",
          "fraction_count_unique_denom"
        ),
      "rr_ci_3d"
    )
  }
  
  res_prepinrows <- h_a_freq_prepinrows(
    x_stats,
    .stats_adj,
    .formats,
    labelstr,
    label_fstr,
    label,
    .indent_mods,
    .labels_n,
    na_str
  )
  # res_prepinrows is list with elements
  # x_stats .formats .labels .indent_mods .format_na_strs
  # make these elements available in current environment
  x_stats <- res_prepinrows$x_stats
  .formats <- res_prepinrows$.formats
  .labels <- res_prepinrows$.labels
  .indent_mods <- res_prepinrows$.indent_mods
  .format_na_strs <- res_prepinrows$.format_na_strs
  
  ### blank out columns not in restr_columns
  # get column label
  colid_lbl <- utils::tail(
    .spl_context$cur_col_split_val[[NROW(.spl_context)]],
    1
  )
  if (!is.null(restr_columns) && !(tolower(colid_lbl) %in% tolower(restr_columns))) {
    x_stats <- lapply(x_stats, FUN = function(x) {
      NULL
    })
  }
  
  ### final step: turn requested stats into rtables rows
  inrows <- in_rows(
    .list = x_stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods,
    .format_na_strs = .format_na_strs
  )
  
  ### add extra blankline to the end of inrows --- as long as section_div is not working as expected
  # nolint start
  if (!is.null(inrows) && extrablankline ||
      (!is.null(extrablanklineafter) && length(.labels) == 1 && .labels == extrablanklineafter)) {
    inrows <- add_blank_line_rcells(inrows)
  } # nolint end
  
  return(inrows)
}

# ==========================================
# Export to list
# ==========================================

patch_list <- list(
  tt_to_tlgrtf = tt_to_tlgrtf,
  h_a_freq_dataprep = h_a_freq_dataprep,
  a_freq_combos_j = a_freq_combos_j,
  s_freq_j = s_freq_j,
  a_freq_j = a_freq_j
)