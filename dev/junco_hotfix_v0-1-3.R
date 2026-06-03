# ==========================================
# Junco Hotfix Patch File
# ==========================================
# Summary of injected hotfixes:
# - Hotfix #178: Ensures tt_to_tlgrtf() does not lose the title when exporting an empty listing (introduced in junco v0.1.4)
# - Hotfix #165: Implements optional CSV export functionality within tt_to_tlgrtf() (#142) (introduced in junco v0.1.6)
# - Hotfix #167: label_width_ins was not being passed recursively #166 (introduced in junco v0.1.4)
# - Hotfix #221: Fixes a bug where a_freq_j fails with countsource = altdf in nested row splits #200 (introduced in junco v0.1.5)
#         (applies to h_a_freq_dataprep, s_freq_j, and a_freq_j).
# - Hotfix #257 `s_summarize_desc_j()` fixed when applied to almost constant data due to behavior from `t.test.default()` (introduced in junco v0.1.5)
# - Hotfix #375 `tt_to_tlgrtf()` sometimes missed the titles (issue #373) (introduced in junco v0.1.6)
# ==========================================


# ==========================================
# Surgical Patches
# ==========================================

# - Hotfix #257 `s_summarize_desc_j()` fixed when applied to almost constant data due to behavior from `t.test.default()`

s_summarize_desc_j <- function(df, .var, .ref_group, .in_ref_col, control = tern::control_analyze_vars()) {
  #### TODO: hotfix : 257 `s_summarize_desc_j()` fixed when applied to almost constant data due to behavior from `t.test.default()`
  safe_t_test <- function(x, y = NULL, ...) {
    tryCatch(
      t.test(x, y, ...),
      error = function(e) {
        if (!is.null(y)) {
          estimate <- c(mean_x = mean(x), mean_y = mean(y))
          dname <- paste(deparse1(substitute(x)), "and", deparse1(substitute(y)))
        } else {
          estimate <- c(mean_x = mean(x))
          dname <- deparse1(substitute(x))
        }
        estimate[is.nan(estimate)] <- NA_real_
        list(
          statistic = NA_real_,
          parameter = NA_real_,
          p.value   = NA_real_,
          estimate  = estimate,
          conf.int  = c(NA_real_, NA_real_),
          method    = "t-test (failed)",
          data.name = dname,
          error_text = e$message
        )
      }
    )
  } #### TODO: hotfix : 257 `s_summarize_desc_j()` fixed when applied to almost constant data due to behavior from `t.test.default()`

  x <- df[[.var]]
  y1 <- s_summary(x)
  y2 <- NULL

  # diff in means versus control group, based upon 2 sample t.test
  y2$mean_diffci <- numeric()
  if (!is.null(.ref_group) && !.in_ref_col) {
    x1 <- df[[.var]]
    x2 <- .ref_group[[.var]]

    x1 <- x1[!is.na(x1)]
    x2 <- x2[!is.na(x2)]

    ttest_stat <- safe_t_test(x1, x2, conf.level = control$conf_level) #### TODO: hotfix : 257 `s_summarize_desc_j()` fixed when applied to almost constant data due to behavior from `t.test.default()`

    stat <- ttest_stat[c("estimate", "conf.int")]
    stat$diff <- stat$estimate[1] - stat$estimate[2]
    stat <- c(stat$diff, stat$conf.int)

    y2$mean_diffci <- with_label(
      c(mean_diffci = stat),
      paste("Difference in Mean + ", tern::f_conf_level(control$conf_level))
    )

  }
  y <- c(y1, y2)

  return(y)
}


# Percolations:

s_aval_chg_col23_diff <- function(
    df,
    .var,
    .df_row,
    .ref_group,
    .in_ref_col,
    ancova,
    interaction_y,
    interaction_item,
    conf_level,
    variables,
    trt_var,
    ctrl_grp,
    cur_param,
    cur_lvl) {
  .df_row <- subset(.df_row, !is.na(.df_row[[.var]]))
  df <- subset(df, !is.na(df[[.var]]))
  .ref_group <- subset(.ref_group, !is.na(.ref_group[[.var]]))

  if (nrow(.df_row) == 0) {
    #### this is only when input row no non-missing records in any of the columns expected to occur for baseline
    #### timepoint for analysis variable change only here we want a blank cell, not a cell with all NA's NULL is
    #### generating a blank cell
    x_stats <- NULL
    mystat1 <- c("mean_ci_3d", "mean_diffci")
  } else if (!ancova) {
    mystat1 <- c("mean_ci_3d", "mean_diffci")

    control <- tern::control_analyze_vars()
    control$conf_level <- conf_level
    x_stats <- s_summarize_desc_j(
      df = df,
      .var = .var,
      .ref_group = .ref_group,
      .in_ref_col = .in_ref_col,
      control = control
    )
  } else {
    mystat1 <- c("lsmean_ci", "lsmean_diffci")

    ### sparse data problems with underlying ancova function 1/ if nrow(.df_row) = 0 NULL (blank columns)

    ### 2/ if nrow(df) = 0 no ancova - lsmean and lsmean diff should be na

    ### 3/ if nrow(df) > 0, & nrow(.ref_group) = 0 ancova for ls mean only, NULL .ref_group -- lsmean diff should
    ### be na

    ### 4/ if nrow(df) > 0, & nrow(.ref_group) > 0 and at least one group with 0 data update levels in
    ### .df_row/df/.ref_group to avoid problems for contrast

    #### by making updates to .df_row/df/.ref_group, situation 3 and 4 could be used together with general case

    .df_row_trtlevels <- unique(.df_row[[trt_var]])

    if (NROW(.df_row_trtlevels) == 0) {
      ### No data at all
      x_stats <- NULL
    } else if (NROW(df) == 0 || NROW(.df_row_trtlevels) == 1) {
      ### current column no data/less than 2 treatment levels

      x_stats <- list()
      x_stats[[mystat1[1]]] <- rep(NA, 3)
      x_stats[[mystat1[2]]] <- rep(NA, 3)
    } else {
      ### ancova situation, and some updates to prevent problems with sparse data
      if ((!ctrl_grp %in% .df_row_trtlevels)) {
        # LS means for current group can be estimated from model, but not difference in LS means set .ref_group
        # to NULL to proceed with s_summarize_ancova_j function update to underlying s_ancova_j to cover NULL
        # ref_group in call
        .ref_group <- NULL
      }
      if (NROW(.df_row_trtlevels) < length(levels(.df_row[[trt_var]]))) {
        # missing levels need to be removed from the factor, in order to have the correct estimates, and avoid
        # errors with underlying tern:::s_ancova function
        .df_row[[trt_var]] <- droplevels(.df_row[[trt_var]])
        df[[trt_var]] <- factor(as.character(df[[trt_var]]), levels = levels(.df_row[[trt_var]]))
        .ref_group[[trt_var]] <- factor(as.character(.ref_group[[trt_var]]), levels = levels(.df_row[[trt_var]]))
      }

      x_stats <- junco:::s_summarize_ancova_j(
        df = df,
        .var = .var,
        .ref_group = .ref_group,
        .in_ref_col = .in_ref_col,
        .df_row = .df_row,
        conf_level = conf_level,
        interaction_y = interaction_y,
        interaction_item = interaction_item,
        variables = variables
      )
    }
  }

  y <- list(mean_ci_3d = x_stats[[mystat1[1]]], meandiff_ci_3d = x_stats[[mystat1[2]]])
  return(y)
}

# - Hotfix #178: Ensures tt_to_tlgrtf() does not lose the title when exporting an empty listing.
# - Hotfix #165: Implements optional CSV export functionality within tt_to_tlgrtf() (#142).
# - Hotfix #167: label_width_ins was not being passed recursively #166

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
    pg_width = junco:::pg_width_by_orient(orientation == "landscape"),
    margins = c(0, 0, 0, 0),
    paginate = junco:::tlg_type(tt) == "Table",
    col_gap = ifelse(tlgtype == "Listing", .5, 3),
    nosplitin = list(
      row = character(),
      col = character()
    ),
    verbose = FALSE,
    tlgtype = junco:::tlg_type(tt),
    string_map = default_str_map,
    markup_df = junco:::dps_markup_df,
    combined_rtf = FALSE,
    one_table = TRUE,
    border_mat = junco:::make_header_bordmat(obj = tt),
    round_type = obj_round_type(tt),
    alignments = list(),
    validate = TRUE,
    export_csv = FALSE, #### TODO: hotfix :142 optional csv #165
    output_csv_directory = NULL, #### TODO: hotfix :142 optional csv #165
    ...) {
  checkmate::assert_flag(export_csv) #### TODO: hotfix :142 optional csv #165
  checkmate::assert_character(output_csv_directory, null.ok = TRUE, len = 1) #### TODO: hotfix :142 optional csv #165
  
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
      main_title = attr(tt, "main_title"), #### TODO: hotfix in tt_to_tlgrtf() when exporting an empty listing do not lose title #178
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
    nc <- junco:::get_ncol(tt)
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
        junco:::subset_border_mat(border_mat, hdrmpf, partmpf)
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
          colwidths = junco:::j_mf_col_widths(pgi_for_cw),
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
          label_width_ins = label_width_ins, #### TODO: hotfix:  label_width_ins was not being passed recursively #166
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
          export_csv = export_csv, #### TODO: hotfix :142 optional csv #165
          output_csv_directory = output_csv_directory, #### TODO: hotfix :142 optional csv #165
          label_width_ins = label_width_ins, #### TODO: hotfix:  label_width_ins was not being passed recursively #166
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
        junco:::tt_to_tbldf,
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
      df <- junco:::tt_to_tbldf(
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
    df <- junco:::listingdf_dataframe_formats(df, round_type = round_type)
  }

  ## we only care about the col labels here...
  if (tlgtype == "Table" && is.list(tt) && !methods::is(tt, "MatrixPrintForm")) {
    mpf <- tt[[1]]
    if (!one_table) {
      colinfo <- lapply(
        tt,
        junco:::mpf_to_colspan,
        markup_df = markup_df,
        string_map = string_map
      )
      csph <- lapply(colinfo, function(x) x$colspan)
      colheader <- lapply(colinfo, function(x) x$colheader)
    } else {
      colinfo <- junco:::mpf_to_colspan(
        mpf,
        markup_df = markup_df,
        string_map = string_map
      )
      csph <- colinfo$colspan
      colheader <- colinfo$colheader
    }
  } else if (methods::is(tt, "MatrixPrintForm")) {
    mpf <- tt
    colinfo <- junco:::mpf_to_colspan(
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
    colinfo <- junco:::mpf_to_colspan(
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
    colwidths <- junco:::cwidths_final_adj(
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

  footer_val <- junco:::prep_strs_for_rtf(
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
    title = junco:::prep_strs_for_rtf(
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


# - Hotfix #221: Fixes a bug where a_freq_j fails with countsource = altdf in nested row splits #200
#         (applies to h_a_freq_dataprep, s_freq_j, and a_freq_j).



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
  upd_dfrow <- junco:::h_upd_dfrow(
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
  alt_df <- junco:::h_create_altdf(
    .spl_context,
    .df_row,
    .alt_df_full,
    denom_by = denom_by,
    id = id,
    variables = variables,
    denom = denom
  )

  if (identical(countsource, "altdf_subset")) {
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
          alt_df <- junco:::h_update_factor(alt_df, .var, keep_vals)
        }
      }
    }
  }

  new_denomdf <- alt_df

  parentdf <- junco:::h_denom_parentdf(.spl_context, denom, denom_by)
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

    df <- junco:::h_update_factor(df, .var, val)
    .df_row <- junco:::h_update_factor(.df_row, .var, val)
  }

  if (!is.null(excl_levels) && drop_levels == FALSE) {
    # restrict the levels to the ones specified in val argument
    df <- df[!(df[[.var]] %in% excl_levels), ]
    .df_row <- .df_row[!(.df_row[[.var]] %in% excl_levels), ]

    df <- junco:::h_update_factor(df, .var, excl_levels = excl_levels)
    .df_row <- junco:::h_update_factor(.df_row, .var, excl_levels = excl_levels)
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

  junco:::check_alt_df_full(denom, c("n_altdf", "N_colgroup"), .alt_df_full)

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
    countsource = countsource #### TODO: hotfix: a freq j fails with countsource = altdf in nested row splits (#221)
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
    colexpr_substr <- junco:::h_colexpr_substr(colgroup, .spl_context$cur_col_expr[[1]])

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
      trt_var_refpath <- junco:::h_get_trtvar_refpath(
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

    x_stats <- junco:::s_rel_risk_val_j(
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

  res_prepinrows <- junco:::h_a_freq_prepinrows(
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
    inrows <- junco:::add_blank_line_rcells(inrows)
  } # nolint end

  return(inrows)
}

# Percolations:

a_freq_combos_j <- function(
    df,
    labelstr = NULL,
    .var = NA,
    val = NULL,
    # arguments specific to a_freq_combos_j
    combosdf = NULL,
    do_not_filter = NULL,
    filter_var = NULL,
    flag_var = NULL,
    # arguments specific to a_freq_combos_j till here
    .df_row,
    .spl_context,
    .N_col,
    id = "USUBJID",
    denom = c("N_col", "n_df", "n_altdf", "n_rowdf", "n_parentdf"),
    label = NULL,
    label_fstr = NULL,
    label_map = NULL,
    .alt_df_full = NULL,
    denom_by = NULL,
    .stats = "count_unique_denom_fraction",
    .formats = NULL,
    .labels_n = NULL,
    .indent_mods = NULL,
    na_str = rep("NA", 3)) {
  denom <- match.arg(denom)

  junco:::check_alt_df_full(denom, "n_altdf", .alt_df_full)

  if (!is.null(combosdf) && !all(c("valname", "label") %in% names(combosdf))) {
    stop("a_freq_combos_j: combosdf must have variables valname and label.")
  }

  res_dataprep <- h_a_freq_dataprep(
    df = df,
    labelstr = labelstr,
    .var = .var,
    val = val,
    drop_levels = FALSE,
    excl_levels = NULL,
    new_levels = NULL,
    new_levels_after = FALSE,
    .df_row = .df_row,
    .spl_context = .spl_context,
    .N_col = .N_col,
    id = id,
    denom = denom,
    variables = NULL,
    label = label,
    label_fstr = label_fstr,
    label_map = label_map,
    .alt_df_full = .alt_df_full,
    denom_by = denom_by,
    .stats = .stats
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

  ## colid can be used to figure out if we're in the combo column or not
  colid <- .spl_context$cur_col_id[[1]]

  ### this is the core code for subsetting to appropriate combo level
  df <- junco:::h_subset_combo(
    df = df,
    combosdf = combosdf,
    do_not_filter = do_not_filter,
    filter_var = filter_var,
    flag_var = flag_var,
    colid = colid
  )

  ## the same s-function can be used as in a_freq_j
  x_stats <- s_freq_j(
    df,
    .var = .var,
    .df_row = .df_row,
    val = val,
    alt_df = new_denomdf,
    parent_df = new_denomdf,
    id = id,
    denom = denom,
    .N_col = .N_col,
    countsource = "df"
  )

  .stats_adj <- .stats

  res_prepinrows <- junco:::h_a_freq_prepinrows(
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

  ### final step: turn requested stats into rtables rows
  in_rows(
    .list = x_stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods,
    .format_na_strs = .format_na_strs
  )
}


a_freq_j_with_exclude <- function(
    df,
    labelstr = NULL,
    exclude_levels,
    .var = NA,
    .spl_context,
    .df_row,
    .N_col,
    .alt_df_full = NULL,
    .stats = "count_unique_denom_fraction",
    .formats = NULL,
    .indent_mods = NULL,
    .labels_n = NULL,
    ...
) {
  if (do_exclude_split(exclude_levels, .spl_context)) {
    NULL
  } else {
    a_freq_j(
      df = df,
      labelstr = labelstr,
      .var = .var,
      .spl_context = .spl_context,
      .df_row = .df_row,
      .N_col = .N_col,
      .alt_df_full = .alt_df_full,
      .stats = .stats,
      .formats = .formats,
      .indent_mods = .indent_mods,
      .labels_n = .labels_n,
      ...
    )
  }
}

a_freq_subcol_j <- function(
    df,
    labelstr = NULL,
    .var = NA,
    val = NULL,
    # arguments specific to a_freq_subcol_j
    subcol_split = NULL,
    subcol_var = NULL,
    subcol_val = NULL,
    # arguments specific to a_freq_subcol_j till here
    .df_row,
    .spl_context,
    .N_col,
    id = "USUBJID",
    denom = c("N_col", "n_df", "n_altdf", "n_rowdf", "n_parentdf"),
    label = NULL,
    label_fstr = NULL,
    label_map = NULL,
    .alt_df_full = NULL,
    denom_by = NULL,
    .stats = c("count_unique_denom_fraction"),
    .formats = NULL,
    .labels_n = NULL,
    .indent_mods = NULL,
    na_str = rep("NA", 3)) {
  denom <- match.arg(denom)

  if (!is.null(labelstr) && is.na(.var)) {
    stop(
      "Argument var must be specified in call to summarize_row_groups when using cfun = a_freq_subcol_j."
    )
  }

  junco:::check_alt_df_full(denom, "n_altdf", .alt_df_full)

  res_dataprep <- h_a_freq_dataprep(
    df = df,
    labelstr = labelstr,
    .var = .var,
    val = val,
    drop_levels = FALSE,
    excl_levels = NULL,
    new_levels = NULL,
    new_levels_after = FALSE,
    .df_row = .df_row,
    .spl_context = .spl_context,
    .N_col = .N_col,
    id = id,
    denom = denom,
    variables = NULL,
    label = label,
    label_fstr = label_fstr,
    label_map = label_map,
    .alt_df_full = .alt_df_full,
    denom_by = denom_by,
    .stats = .stats
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
  alt_df <- res_dataprep$alt_df
  parentdf <- res_dataprep$parentdf
  new_denomdf <- res_dataprep$new_denomdf
  .stats <- res_dataprep$.stats

  ## colid can be used to figure out if we're in subcolum
  colid <- .spl_context$cur_col_id[[1]]

  ### this is the core code for subsetting to appropriate subcol_val
  insubcol <- grepl(subcol_split, colid, fixed = TRUE)
  if (insubcol) {
    df <- subset(df, df[[subcol_var]] == subcol_val)
  }

  ## the same s-function can be used as in a_freq_j
  x_stats <- s_freq_j(
    df,
    .var = .var,
    .df_row = .df_row,
    val = val,
    alt_df = new_denomdf,
    parent_df = new_denomdf,
    id = id,
    denom = denom,
    .N_col = .N_col,
    countsource = "df"
  )

  .stats_adj <- .stats

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

  ### final step: turn requested stats into rtables rows
  inrows <- in_rows(
    .list = x_stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods,
    .format_na_strs = .format_na_strs
  )

  return(inrows)
}

a_freq_resp_var_j <- function(
    df,
    .var,
    .df_row,
    .N_col,
    .spl_context,
    resp_var = NULL,
    id = "USUBJID",
    drop_levels = FALSE,
    riskdiff = TRUE,
    ref_path = NULL,
    variables = formals(s_proportion_diff)$variables,
    conf_level = formals(s_proportion_diff)$conf_level,
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
    weights_method = formals(s_proportion_diff)$weights_method,
    .formats = NULL,
    na_str = rep("NA", 3),
    ...) {
  # ---- Derive statistics: xx / xx (xx.x%)

  if (is.null(resp_var)) {
    stop(
      "afun a_freq_resp_var_j: resp_var cannot be NULL."
    )
  }

  resp_var_values <- unique(df[[resp_var]][!is.na(df[[resp_var]])])
  if (
    is.character(df[[resp_var]]) &&
    any(is.na(df[[resp_var]])) &&
    all(resp_var_values == "Y")
  ) {
    stop(
      paste0(
        "afun a_freq_resp_var_j: it is not clear if missing resp_var should be considered non-response. ",
        "Please make it a factor with appropriate Y(/N) levels."
      )
    )
  }

  if (length(setdiff(resp_var_values, c("Y", "N"))) > 0) {
    stop("afun a_freq_resp_var_j: resp_var must contain only Y/N values.")
  }

  if (is.character(df[[resp_var]])) {
    df[[resp_var]] <- factor(df[[resp_var]], levels = c("Y", "N"))
  }

  df <- df[!is.na(df[[.var]]), ]

  # nolint start
  if ((is.factor(df[[resp_var]]) &&
       (identical(levels(df[[resp_var]]), c("Y", "N")) || identical(levels(df[[resp_var]]), c("N", "Y")))) ||
      is.character(df[[resp_var]])
  ) { # nolint end
    # missing values in resp_var should be excluded, not considered as not met response
    # subject will then not contribute to denominator
    df <- df[!is.na(df[[resp_var]]), ]
    .df_row <- .df_row[!is.na(.df_row[[resp_var]]), ]
  }

  if (drop_levels) {
    obs_levs <- unique(.df_row[[.var]])
    obs_levs <- intersect(levels(.df_row[[.var]]), obs_levs)

    val_var <- obs_levs
    # restrict the levels to the ones specified in val_var
    df <- df[df[[.var]] %in% val_var, ]
    .df_row <- .df_row[.df_row[[.var]] %in% val_var, ]

    df <- junco:::h_update_factor(df, .var, val_var)
  }

  varvec <- df[[.var]]

  levs <- if (is.factor(varvec)) levels(varvec) else sort(unique(varvec))

  colid <- .spl_context$cur_col_id[[1]]
  inriskdiffcol <- grepl("difference", tolower(colid), fixed = TRUE)

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
  }

  fn <- function(levii) {
    dfii <- df[df[[.var]] == levii, ]
    dfrowii <- .df_row[.df_row[[.var]] == levii, ]

    if (!inriskdiffcol) {
      # use the core s_freq_j function on the current level of the incoming variable (.var)
      # note that the response variable will become .var in the below call
      # val is restricted to Y to show number of response on the current level of .var
      rslt <- s_freq_j(
        dfii,
        .df_row = dfrowii,
        .var = resp_var,
        alt_df = NULL,
        parent_df = NULL,
        val = "Y",
        denom = "n_df"
      )

      .stat <- "count_unique_denom_fraction"
      x_stat <- rslt[[.stat]]$Y
      # use .formats if provided, otherwise default to jjcsformat_count_denom_fraction
      fmt <- if (is.null(.formats)) jjcsformat_count_denom_fraction else .formats
      rslt <- rcell(x_stat, format = fmt)
    } else {
      # use the risk differenc function junco:::s_rel_risk_val_j on the current level of the incoming variable (.var)
      # note that the response variable will become .var in the below call
      # val is restricted to Y to show number of response on the current level of .var
      denom_df <- dfrowii

      # for combined facet, denom_df value for the treatment group needs update
      denom_df <- upd_denom_df_combo(
        denom_df,
        trt_var,
        cur_trt_grp,
        .spl_context)

      rslt <- junco:::s_rel_risk_val_j(
        df = dfii,
        .var = resp_var,
        .df_row = dfrowii,
        val = "Y",
        denom_df = denom_df,
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
      x_stat <- rslt[["rr_ci_3d"]]$Y
      # use .formats if provided, otherwise default to rr format
      fmt <- if (is.null(.formats)) jjcsformat_xx("xx.x (xx.x, xx.x)") else .formats
      rslt <- rcell(
        x_stat,
        format = fmt,
        format_na_str = na_str
      )
    }

    # rslt is a single rcell row
    return(rslt)
  }

  ### apply function fn to all levels of incoming .var
  ### cls is a list of single rcell rows - ie one line per level is presented
  cls <- lapply(levs, fn)

  names(cls) <- levs
  return(cls)
}
