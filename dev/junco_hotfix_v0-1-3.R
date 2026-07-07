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
# - Hotfix #395 `def_colwidths()` crashes when long column labels (issue #281) (introduced in junco v0.1.6)
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

# - Hotfix #395 `def_colwidths()` crashes when long column labels (issue #281) (introduced in junco v0.1.6)
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

  res_prepinrows <- junco::h_a_freq_prepinrows(
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


#helpers


remove_security_popup_page_numbers_XML <- function(doc, tlgtype = "Table",
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


insert_title_as_header <- function(flx,
                                   title,
                                   border = flextable::fp_border_default(width = 0.875, color = "black")) {
  # this function inserts the Title as a header but does not attempt
  # to simulate the hanging indent. Instead, it adds the string as is, and the hanging indent
  # will be handled further downstream when exporting to docx by manipulating the XML.
  # see function "add_hanging_indent_in_title_XML()"

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

  flx <- flx |>
    flextable::add_header_lines(values = new_title, top = TRUE) |>
    flextable::align(part = "header", i = 1, align = "left") |>
    flextable::bg(part = "header", i = 1, bg = "white") |>
    flextable::padding(part = "header", i = 1, padding.left = 0) |>
    flextable::fontsize(part = "header", i = 1, size = 10) |>
    flextable::bold(part = "header", i = 1) |>
    flextable::border(part = "header", i = 1, border.top = border, border.bottom = border)

  # nolint start
  flx <- flx |> flextable::tab_settings(
    i = 1, j = 1, part = "header",
    value = officer::fp_tabs(officer::fp_tab(pos = 0.8, style = "left"))
  )
  # nolint end

  flx <- flextable::style(x = flx, part = "header", i = 1, pr_p = officer::fp_par(word_style = "caption"))

  return(flx)
}


insert_hanging_indent_first_col_XML <- function(doc, n_rows_footer = 0, hanging_indent = 87) {
  # 87 MS Word points = 0.06 inches
  # this function looks for all table nodes 'w:tbl'
  #   for each of them, looks for all table rows 'w:tr'
  #     for each of them, takes the first cell 'w:tc'
  #       takes its paragraph 'w:p'
  #         takes its paragraph properties 'w:pPr'
  #           inserts a child <w:ind w:left="87" w:hanging="87"/>

  # look for all table nodes
  tbl_nodes <- doc$doc_obj$get() |>
    xml2::xml_find_all(".//w:tbl")

  for (tbl_node in tbl_nodes) {
    # look for all table rows
    tr_nodes <- tbl_node |> xml2::xml_find_all(".//w:tr")

    # ignore the last 'n_rows_footer' nodes, as we don't want hanging indent in the footer
    tr_nodes <- head(tr_nodes, length(tr_nodes) - n_rows_footer)

    for (tr_node in tr_nodes) {
      # if it contains a child
      # <w:trPr>
      #   <w:tblHeader/>
      # </w:trPr>
      # it means that the current row belongs to the table header, so skip it
      is_row_header <-
        tr_node |>
        xml2::xml_find_first(".//w:trPr") |>
        xml2::xml_find_first(".//w:tblHeader")
      if (length(is_row_header) > 0) {
        next()
      }

      # otherwise, ...
      # look for first cell
      tc_node <- tr_node |> xml2::xml_find_first(".//w:tc")
      # take its paragraph node
      p_node <- tc_node |> xml2::xml_find_first(".//w:p")
      # take its paragraph properties
      pPr_node <- p_node |> xml2::xml_find_first(".//w:pPr")

      # set hanging indent of 0.06 inches
      children <- pPr_node |> xml2::xml_children()
      child_i <- which(xml2::xml_name(children) == "ind") |> head(1)
      x <- pPr_node |> xml2::xml_child(child_i)
      # it is possible that the current cell is already indented
      # we don't want to lose that left-indentation value
      # therefore extract it and add up 87 (0.06 inches)
      left_indentation <- xml2::xml_attr(x, "left")
      left_indentation <- ifelse(is.na(left_indentation), 0, as.integer(left_indentation))
      xml2::xml_set_attr(x, "w:hanging", hanging_indent)
      xml2::xml_set_attr(x, "w:left", hanging_indent + left_indentation)
    }
  }
}

dps_markup_df_docx <- tibble::tibble(
  keyword = c("super", "sub"),
  replace_by = c("flextable::as_sup", "flextable::as_sub")
)

remove_trHeight_nodes_XML <- function(doc) {
  # this function is only called when exporting a table or listing.
  # in the parsing process, there is a large spacing separating the rows.
  # in 'trHeight' nodes, we should not have both 'w:val' and 'w:hRule'
  # attributes together:
  # <w:trHeight w:val="617" w:hRule="auto"/>
  # Ideally, we'd like to remove only "w:hRule" attribute, but a simpler
  # solution is to completely remove all 'trHeight' nodes
  nodes <- xml2::xml_find_all(doc$doc_obj$get(), ".//w:trHeight")
  # remove all row height nodes entirely
  xml2::xml_remove(nodes)
}

fix_namespaces_Graphs_XML <- function(doc) {
  # nolint start
  # This function is only called when exporting a Graph, and
  # fixes the "parsing failed" error.
  # The root cause of this error is that namespace definitions are moved to the parent node:
  #   <w:tbl
  #     xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
  #     xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture">
  # but they should appear at the children level:
  #   <a:graphicFrameLocks
  #     xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" noChangeAspect="1"/>
  #   <a:graphic
  #     xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main">
  #   <pic:pic
  #     xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture">
  #
  # in order to prevent this moving, we just remove those 2 namespace definitions
  # from the parent node 'w:tbl'
  # nolint end

  # remove namespaces from parent node
  nodes <- xml2::xml_find_all(doc$doc_obj$get(), ".//w:tbl")
  for (node in nodes) {
    xml2::xml_attr(node, "xmlns:a") <- NULL
    xml2::xml_attr(node, "xmlns:pic") <- NULL
  }
}

align_rows_with_rtf <- function(doc) {
  # when we have vertical pagination in the case of tables and listings,
  # from page 2 onwards, the rows visually look a little bit shifted downwards
  # in RTF compared to DOCX.
  # It's a tiny difference caused by a little horizontal spacing located
  # between the repeated table header and the first body row in each page,
  # starting from page 2.
  # This visual difference has to do with file "settings.xml" in the docx,
  # and more precisely with this node:
  # <w:compat>
  #   <w:compatSetting w:name="compatibilityMode" w:uri="http://schemas.microsoft.com/office/word" w:val="15"/>
  # </w:compat>
  # we need to change the value from 15 to 11
  doc$settings$compatibility_mode <- "11"
  return(doc)
}

validate_tabletree <- function(tt, validate, tlgtype) {
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
}

get_template_file <- function() {
  template_file <- "template_file.docx"
  template_file <- system.file(template_file, package = "junco")
  return(template_file)
}

export_as_csv <- function(tlgtype, export_csv, pags, fontspec,
                          string_map, markup_df, round_type,
                          output_csv_directory, output_dir, fname) {

  if (tlgtype != "Table" || !isTRUE(export_csv)) {
    return(invisible(NULL))
  }

  # 'pags' should be a list of "MatrixPrintForm"
  # even if there is no horizontal pagination, it should be a list of 1 element
  checkmate::assert_true(is.list(pags) && methods::is(pags[[1]], "MatrixPrintForm"))

  df <- lapply(
    pags,
    tt_to_tbldf,
    fontspec = fontspec,
    string_map = string_map,
    markup_df = markup_df,
    round_type = round_type
  )

  # 'one_table' is TRUE if there is vertical pagination
  # when TRUE, it merges all pages in the same "part" into 1 csv
  one_table <- length(pags) > 1
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
  } else {
    df <- df[[1]]
  }

  output_csv_filename <- get_output_csv_filename(output_csv_directory,
                                                 output_dir,
                                                 tolower(fname))

  utils::write.csv(
    df,
    file = output_csv_filename,
    row.names = FALSE
  )
}

insert_fake_watermark_XML <- function(doc, watermark, orientation, tlgtype) {
  # This function inserts a fake watermark WordArt in the Title of
  # the flextable (that is, in the document body) by inserting an XML node.
  # This function should only be called when exporting Figures.
  # This ensures the watermark is duplicated on every page.
  # Argument 'orientation' is needed to calculate the location of the watermark.

  checkmate::assert_character(watermark, len = 1)
  checkmate::assert_choice(orientation, choices = c("portrait", "landscape"))

  if (orientation == "portrait") {
    margin_left <- -114
    margin_top <- 266
  } else {
    # to shift to the right, increase margin_left
    # to shift upwards, decrease margin_top
    margin_left <- -24
    margin_top <- ifelse(tlgtype == "Listing", 165, 176)
    if (tlgtype == "Figure") {
      margin_left <- -30
    }
  }


  # find out where to insert the node
  nodes <- xml2::xml_find_all(doc$doc_obj$get(), ".//w:tbl")
  for (node in nodes) {
    node <- xml2::xml_find_first(node, ".//w:tr")
    node <- xml2::xml_find_first(node, ".//w:tc")
    node <- xml2::xml_find_first(node, ".//w:p")
    node <- xml2::xml_find_first(node, ".//w:r")

    # nolint start
    node_to_insert <- paste0('
      <w:r>
				<w:rPr>
					<w:noProof/>
					<w:szCs w:val="24"/>
					<w:lang w:val="en-GB" w:eastAsia="en-GB"/>
				</w:rPr>
				<w:pict w14:anchorId="7C7FF34C">
					<v:shapetype id="_x0000_t136" coordsize="21600,21600" o:spt="136" adj="10800" path="m@7,l@8,m@5,21600l@6,21600e">
						<v:formulas>
							<v:f eqn="sum #0 0 10800"/>
							<v:f eqn="prod #0 2 1"/>
							<v:f eqn="sum 21600 0 @1"/>
							<v:f eqn="sum 0 0 @2"/>
							<v:f eqn="sum 21600 0 @3"/>
							<v:f eqn="if @0 @3 0"/>
							<v:f eqn="if @0 21600 @1"/>
							<v:f eqn="if @0 0 @2"/>
							<v:f eqn="if @0 @4 21600"/>
							<v:f eqn="mid @5 @6"/>
							<v:f eqn="mid @8 @5"/>
							<v:f eqn="mid @7 @8"/>
							<v:f eqn="mid @6 @7"/>
							<v:f eqn="sum @6 0 @5"/>
						</v:formulas>
						<v:path textpathok="t" o:connecttype="custom" o:connectlocs="@9,0;@10,10800;@11,21600;@12,10800" o:connectangles="270,180,90,0"/>
						<v:textpath on="t" fitshape="t"/>
						<v:handles>
							<v:h position="#0,bottomRight" xrange="6629,14971"/>
						</v:handles>
						<o:lock v:ext="edit" text="t" shapetype="t"/>
					</v:shapetype>
					<v:shape id="         PowerPlusWaterMarkObject357476642" o:spid="_x0000_s1026" type="#_x0000_t136" style="position:absolute;margin-left:', margin_left, 'pt;margin-top:', margin_top, 'pt;width:696.05pt;height:116pt;rotation:315;z-index:251657728;mso-position-horizontal-relative:margin;mso-position-vertical-relative:margin" o:allowincell="f" fillcolor="silver" stroked="f">
						<v:fill opacity="15000f"/>
						<v:textpath style="font-family:&quot;      Calibri&quot;;font-size:1pt" string="   ', watermark, '"/>
					</v:shape>
				</w:pict>
			</w:r>
    ')

    node_to_insert <- paste0(
      '<w:tmp ',
      '  xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"',
      '  xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"',
      '  xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"',
      '  xmlns:o="urn:schemas-microsoft-com:office:office"',
      '  xmlns:v="urn:schemas-microsoft-com:vml"',
      '  xmlns:w10="urn:schemas-microsoft-com:office:word"',
      '  xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"',
      '  xmlns:wp14="http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing"',
      '  xmlns:wps="http://schemas.microsoft.com/office/word/2010/wordprocessingShape" mc:Ignorable="w14 w15 w16se w16cid w16 w16cex w16sdtdh w16sdtfl w16du wp14"',
      '  xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"',
      '  xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"',
      '>',
      node_to_insert,
      '</w:tmp>'
    )
    # nolint end
    node_to_insert <- xml2::read_xml(node_to_insert)
    node_to_insert <- xml2::xml_root(node_to_insert)
    xml2::xml_add_sibling(node, node_to_insert, .where = "before")
  }
}

upd_denom_df_combo <- function(
  denom_df,
  trt_var,
  cur_trt_grp,
  .spl_context
) {
  # check if we are in a combo facet, trt_var in that facet is not the same as in denom_df
  in_combo <- !(cur_trt_grp %in% unique(denom_df[[trt_var]]))
  if (in_combo) {
    colexpr_substr_combo <- h_colexpr_substr(trt_var, .spl_context$cur_col_expr[[1]])
    # adjust denom_df to update values of trt_var to reflect the combo value as in df
    y1 <- subset(denom_df, eval(parse(text = colexpr_substr_combo)))
    y1[[trt_var]] <- cur_trt_grp
    y2 <- subset(denom_df, eval(parse(text = paste0("!", colexpr_substr_combo))))
    denom_df <- rbind(y1, y2)
  }
  denom_df
}

remove_trHeight_nodes_XML <- function(doc) {
  # this function is only called when exporting a table or listing.
  # in the parsing process, there is a large spacing separating the rows.
  # in 'trHeight' nodes, we should not have both 'w:val' and 'w:hRule'
  # attributes together:
  # <w:trHeight w:val="617" w:hRule="auto"/>
  # Ideally, we'd like to remove only "w:hRule" attribute, but a simpler
  # solution is to completely remove all 'trHeight' nodes
  nodes <- xml2::xml_find_all(doc$doc_obj$get(), ".//w:trHeight")
  # remove all row height nodes entirely
  xml2::xml_remove(nodes)
}

align_rows_with_rtf <- function(doc) {
  # when we have vertical pagination in the case of tables and listings,
  # from page 2 onwards, the rows visually look a little bit shifted downwards
  # in RTF compared to DOCX.
  # It's a tiny difference caused by a little horizontal spacing located
  # between the repeated table header and the first body row in each page,
  # starting from page 2.
  # This visual difference has to do with file "settings.xml" in the docx,
  # and more precisely with this node:
  # <w:compat>
  #   <w:compatSetting w:name="compatibilityMode" w:uri="http://schemas.microsoft.com/office/word" w:val="15"/>
  # </w:compat>
  # we need to change the value from 15 to 11
  doc$settings$compatibility_mode <- "11"
  return(doc)
}

validate_tabletree <- function(tt, validate, tlgtype) {
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
}

add_little_gap_bottom_borders_spanning_headers <- function(
  flx,
  border = flextable::fp_border_default(width = 0.875, color = "black")
) {
  nrow_header <- flextable::nrow_part(x = flx, part = "header")
  if (nrow_header > 1) {
    bottom_borders <- flx$header$styles$cells$border.width.bottom$data
    spanning_headers <- flx$header$spans$rows
    rows_to_inspect <- seq(1, nrow_header - 1)

    for (i in rows_to_inspect) {
      # if there are at least 2 spanning headers in the current header row
      pos_spanning_header <- which(spanning_headers[i, ] > 1)
      num_spanning_header <- length(pos_spanning_header)
      if (num_spanning_header >= 2) {
        for (j in pos_spanning_header[seq(1, num_spanning_header - 1)]) {
          width_spanning_header <- spanning_headers[i, j]
          # if the spanning headers are consecutive and both have bottom borders,
          # then replace the cell borders by paragraph borders
          if (spanning_headers[i, j + width_spanning_header] > 1 &&
                bottom_borders[i, j + width_spanning_header - 1] > 0 &&
                bottom_borders[i, j + width_spanning_header] > 0) {
            start_pos <- j
            end_pos <- start_pos + width_spanning_header +
              spanning_headers[i, j + width_spanning_header] - 1
            # remove the border before inserting a paragraph border
            flx <- flextable::hline(
              x = flx,
              part = "header",
              i = i,
              j = seq(start_pos, end_pos),
              border = officer::fp_border(width = 0)
            )
            # add the paragraph border
            top_cell_borders <- flx$header$styles$cells$border.width.top$data[i, seq(start_pos, end_pos)]
            flx <- flextable::style(
              x = flx,
              part = "header",
              i = i,
              j = seq(start_pos, end_pos),
              pr_p = officer::fp_par(
                border.bottom = border,
                text.align = "center"
              )
            )
            flx$header$styles$cells$border.width.top$data[i, seq(start_pos, end_pos)] <- top_cell_borders

            flx$header$styles$cells$margin.left$data[i, start_pos + width_spanning_header] <- 3
            flx$header$styles$cells$margin.right$data[i, start_pos] <- 3
            flx$header$styles$pars$border.width.bottom$data[i, c(start_pos, start_pos + width_spanning_header)] <-
              border$width
          }
        }
      }
    }
  }
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

add_hanging_indent_in_title_XML <- function(doc) {
  # this function modifies the XML of the docx to add the
  # hanging indent to the Title

  # obtain all table nodes
  tbl_nodes <- doc$doc_obj$get() |>
    xml2::xml_find_all(".//w:tbl")
  # for each of them, obtain the first row in the header
  for (tbl_node in tbl_nodes) {
    first_header_row <- xml2::xml_find_first(tbl_node, ".//w:tblHeader")
    x <- first_header_row |> xml2::xml_parent() |> xml2::xml_parent()
    x <- xml2::xml_find_first(x, ".//w:tc")
    x <- xml2::xml_find_first(x, ".//w:p")
    x <- xml2::xml_find_first(x, ".//w:pPr")
    x <- xml2::xml_find_first(x, ".//w:ind")
    # set hanging indent of 0.8 inches
    xml2::xml_set_attr(x, "w:hanging", 1152)
    xml2::xml_set_attr(x, "w:left", 1152)
  }
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
        junco:::export_as_csv(tlgtype = tlgtype,
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
  flx <- junco:::insert_footer_text(flx, tblid)

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
      flx <- junco:::insert_keepNext_vertical_pagination(flx = flx,
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
  flx <- junco:::interpret_all_cell_content(flx, markup_df_docx)

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
      ret <- junco:::no_cellwrap_colwidths(tt, fontspec, col_gap = col_gap, label_width_ins = label_width_ins)
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
  possdf <- junco:::make_poss_wdf(
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
