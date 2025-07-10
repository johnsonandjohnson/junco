#' Non-blank Sentinel
#'
#' @keywords internal
non_blank_sentinel <- structure("", class = "non_blank_sentinel")

#' Get Control Subset
#'
#' Retrieves a subset of the DataFrame based on treatment variable and control group.
#'
#' @param df (`data.frame`)\cr Data frame to subset.
#' @param trt_var (`character`)\cr Treatment variable name.
#' @param ctrl_grp (`character`)\cr Control group value.
#' @return Subset of the data frame.
#' @keywords internal
get_ctrl_subset <- function(df, trt_var, ctrl_grp) {
  df[df[[trt_var]] == ctrl_grp, ]
}


# sfunction to perform counting of records or subjects on an incoming df and .alt_df
#' Null Function
#'
#' A function that returns NULL.
#'
#' @return NULL
#' @keywords internal
null_fn <- function(...) {
  NULL
}


#' Create Alternative Data Frame
#'
#' Creates an alternative data frame based on the current split context.
#'
#' @param .spl_context Current split context.
#' @param .df_row Current data frame row.
#' @param denomdf Denominator data frame.
#' @param denom_by Denominator grouping variable.
#' @param id Identifier variable.
#' @param variables Variables to include in the analysis.
#' @param denom Denominator type.
#' @return Grand parent dataset.
#' @noRd
#' @keywords Internal
h_create_altdf <- function(.spl_context, .df_row, denomdf, denom_by = NULL, id, variables, denom) {
  ### parent df in the current row-split (all col splits are still in)
  pardf <- .spl_context$full_parent_df[[NROW(.spl_context)]]

  ## if no denomdf defined, use input if you want to use alt_source df, you better pass on the same dataframe as
  ## denomdf in the function call

  colsplit <- .spl_context$cur_col_split[[1]]
  if (length(colsplit) == 1 && tolower(colsplit) == "total") {
    colsplit <- NULL
  }

  if (is.null(denomdf) || denom %in% c("n_df", "n_rowdf")) {
    #### once we have the rtables version >= 0.6.12 -- is.null(denomdf) only will happen when build_table has been
    #### used without alt_counts_df argument denom options c('N_col', 'n_df', 'n_altdf', 'N_colgroup', 'n_rowdf',
    #### 'n_parentdf') note that n_altdf always has a non-null denomdf N_colgroup and n_parentdf will be handled
    #### separately n_parentdf in h_denom_parentdf N_colgroup with h_colexpr_substr

    denomdf <- unique(.df_row[, c(id, variables$strata, colsplit, denom_by), drop = FALSE])
  }

  # grand parent dataset starts of from denomdf -- not yet rowsplits this is for the risk diff columns to include all
  # subjects to start from
  gpardf <- denomdf

  nm_gpardf <- names(gpardf)

  sbgrpvar <- intersect(.spl_context$split, nm_gpardf)

  cursbgrp_value <- NULL

  if (!is.null(denom_by) && length(sbgrpvar) > 0) {
    ### assumption: the subgroup is the first variable in the row-split if this assumption is not valid ---
    ### re-design and let user pass in the subgroup variable
    sbgrpvar <- denom_by[1]
    cur_index <- which(.spl_context$split == sbgrpvar)
    if (length(cur_index) > 0) {
      cursbgrp_value <- .spl_context$value[[cur_index]]

      gpardf <- subset(denomdf, eval(denomdf[[sbgrpvar]] == cursbgrp_value))
    }
    if (length(cur_index) == 0) {
      # cat(paste('sbgrpvar', sbgrpvar)) cat(paste( unique(.spl_context$value), sep = ', '))
    }
  }

  ## note this is a rowsplit only, not yet column split
  return(gpardf)
}

#' No Data to Report String
#'
#' A constant string used when there is no data to display in a table.
#' This is used as a placeholder in tables when no data is available for a particular category.
#'
#' @return A character string with the value "No data to report".
#'
#' @export
#' @keywords internal
no_data_to_report_str <- "No data to report"


#' Update Factor
#'
#' Updates a factor variable in a data frame based on specified values.
#'
#' @param df Data frame containing the variable to update.
#' @param .var Variable name to update.
#' @param val Values to keep.
#' @param excl_levels Levels to exclude from the factor.
#' @return Updated data frame.
#' @noRd
h_update_factor <- function(df, .var, val = NULL, excl_levels = NULL) {
  if (!is.factor(df[[.var]]) || (is.null(val) && is.null(excl_levels))) {
    return(df)
  }

  if ((!is.null(val) && !is.null(excl_levels))) {
    stop("update_factor cannot be used with both val and excl_levels specified.")
  }

  all_levels <- levels(df[[.var]])

  if (!is.null(val)) {
    exclude_levels <- all_levels[!(all_levels %in% val)]
    remaining_levels <- setdiff(all_levels, exclude_levels)
  }
  if (!is.null(excl_levels)) {
    exclude_levels <- all_levels[all_levels %in% excl_levels]
    remaining_levels <- setdiff(all_levels, exclude_levels)
  }

  ## introduce level No data to report
  if (length(remaining_levels) == 0) {
    remaining_levels <- no_data_to_report_str
  }
  df[[.var]] <- factor(as.character(df[[.var]]), levels = remaining_levels)

  return(df)
}


#' Extract Substring from Column Expression
#'
#' Retrieves the substring from a column expression related to a variable component.
#'
#' get substring from col_expr related to var component
#' intended usage is on strings coming from .spl_context$cur_col_expr
#' these strings are of type '!(is.na(var) & var %in% 'xxx') & !(is.na(var2) & var2 %in% 'xxx')'
#'
#' @param var Variable to extract from the expression.
#' @param col_expr Column expression string.
#' @return Substring corresponding to the variable.
#' @noRd
#' @keywords internal
h_colexpr_substr <- function(var, col_expr) {
  # reconstructing the strings is not an option as doesn't work for combined columns facets
  cur_col_expr <- as.character(col_expr)

  if (!grepl(var, cur_col_expr, fixed = TRUE)) {
    return(NULL)
  }

  z2 <- paste0("(!is.na(", var, ") & ", var, " %in%")
  start <- regexpr(z2, cur_col_expr, fixed = TRUE)
  end <- start + attr(start, "match.length") - 1
  z1 <- cbind(start, end)

  ### start of the string
  z1_start <- z1[1, 1]

  ### figure out what is the appropriate end expression has several & in string

  positions <- gregexpr("&", cur_col_expr, fixed = TRUE)
  start <- unlist(positions)
  end <- start + attr(positions[[1]], "match.length") - 1
  z3 <- cbind(start, end)[, 1]

  ### get the first & after the end of z3 ((!is.na(TRT01A) & TRT01A %in%)
  h <- z3 > z1[, 2]
  if (any(h)) {
    z4 <- which.max(h)
    z1_end <- z3[z4] - 3
  } else {
    z1_end <- nchar(cur_col_expr)
  }

  col_expr_substr <- substr(cur_col_expr, z1_start, z1_end)

  return(col_expr_substr)
}

#' Get Denominator Parent Data Frame
#'
#' Retrieves the parent data frame based on denominator.
#'
#' @param .spl_context Current split context.
#' @param denom Denominator type.
#' @param denom_by Denominator grouping variable.
#' @return Parent data frame.
#' @noRd
#' @keywords internal
h_denom_parentdf <- function(.spl_context, denom, denom_by) {
  if (denom != "n_parentdf") {
    return(NULL)
  }
  if (is.null(denom_by)) {
    stop("denom_by must be specified when using denom = 'n_parentdf'.")
  }
  split <- .spl_context$split
  if (split[1] == "root" && !is.null(denom_by)) {
    denom_by <- c("root", denom_by)
  }
  split <- intersect(denom_by, split)
  parentdf <- .spl_context$full_parent_df[[length(split)]]
  return(parentdf)
}

#' Add New Levels to Data Frame
#'
#' Adds new factor levels to a specified variable in the data frame.
#'
#' @param df Data frame to update.
#' @param .var Variable to which new levels will be added.
#' @param new_levels List of new levels to add.
#' @param addstr2levs String to add to new levels.
#' @param new_levels_after Boolean, indicating if new levels should be added after existing levels.
#' @return Updated data frame.
#' @noRd
#' @keywords internal
h_df_add_newlevels <- function(df, .var, new_levels, addstr2levs = NULL, new_levels_after) {
  varvec <- df[[.var]]

  levs <- if (is.factor(varvec)) levels(varvec) else sort(unique(varvec))

  if (!is.null(new_levels)) {
    ## assumption: new_levels[[1]] : names of the new levels new_levels[[2]] : values of the new levels
    if (length(new_levels[[1]]) != length(new_levels[[2]])) {
      stop(
        "new_levels must be a list of length 2, second element must be a ",
        "list of same length as first element."
      )
    }
    if (any(duplicated(unlist(new_levels[[2]])))) {
      stop(
        "unlist(new_levels[[2]]) contains duplicates: duplicate assignment ",
        "of a level to a new level is not allowed."
      )
    }

    sortnewlevs <- unlist(lapply(X = new_levels[[2]], FUN = function(x) {
      min(which(levs %in% x)) - 0.1
    }))

    if (new_levels_after) {
      sortnewlevs <- unlist(lapply(X = new_levels[[2]], FUN = function(x) {
        max(which(levs %in% x)) + 0.1
      }))
    }

    newlevs <- c(levs, new_levels[[1]])
    sortx <- c(seq_along(levs), sortnewlevs)

    newlevsx <- newlevs[order(sortx)]

    levs <- newlevsx

    ### add newlevels to df
    for (i in seq_along(new_levels[[1]])) {
      levii <- unlist(new_levels[[2]][i])

      addi <- df[df[[.var]] %in% levii, ]
      addi[[.var]] <- new_levels[[1]][i]

      df <- dplyr::bind_rows(df, addi)
    }
  }

  levlabels <- levs
  if (!is.null(addstr2levs)) {
    levlabels <- paste0(levs, addstr2levs)
  }

  df[[.var]] <- factor(as.character(df[[.var]]), levels = levs, labels = levlabels)

  return(df)
}


#' Get Treatment Variable Reference Path
#'
#' Retrieves the treatment variable reference path from the provided context.
#'
#' @param ref_path (`character`)\cr Reference path for treatment variable.
#' @param .spl_context (`data.frame`)\cr Current split context.
#' @param df (`data.frame`)\cr Data frame.
#' @return List containing treatment variable details.
#' @export
h_get_trtvar_refpath <- function(ref_path, .spl_context, df) {
  checkmate::check_character(ref_path, min.len = 2L, names = "unnamed")
  checkmate::assert_true(length(ref_path) %% 2 == 0) # Even number of elements in ref_path.

  trt_var <- utils::tail(.spl_context$cur_col_split[[length(.spl_context$cur_col_split)]], n = 1)
  trt_var_refspec <- utils::tail(ref_path, n = 2)[1]

  checkmate::assert_true(identical(trt_var, trt_var_refspec))

  # current group and ctrl_grp
  cur_trt_grp <- utils::tail(.spl_context$cur_col_split_val[[length(.spl_context$cur_col_split_val)]], n = 1)
  ctrl_grp <- utils::tail(ref_path, n = 1)

  ### check that ctrl_grp is a level of the treatment variable, in case riskdiff is requested
  if (!ctrl_grp %in% levels(df[[trt_var]])) {
    stop(paste0(
      "control group specification in ref_path argument (",
      ctrl_grp,
      ") is not a level of your treatment group variable (",
      trt_var,
      ")."
    ))
  }
  return(list(trt_var = trt_var, trt_var_refspec = trt_var_refspec, cur_trt_grp = cur_trt_grp, ctrl_grp = ctrl_grp))
}


#' Update Data Frame Row
#'
#' Updates a row in the data frame based on various parameters.
#'
#' @param df_row Data frame row to update.
#' @param .var Variable name to update.
#' @param val Values to keep.
#' @param excl_levels Levels to exclude from the factor.
#' @param drop_levels Boolean, indicating if levels should be dropped.
#' @param new_levels New levels to add.
#' @param new_levels_after Boolean, indicating if new levels should be added after existing levels.
#' @param addstr2levs String to add to new levels.
#' @param label Label string.
#' @param label_map Mapping for labels.
#' @param labelstr Label string to replace.
#' @param label_fstr Format string for labels.
#' @param .spl_context Current split context.
#' @return List containing updated data frames and values.
#' @noRd
#' @keywords internal
h_upd_dfrow <- function(
    df_row,
    .var,
    val,
    excl_levels,
    drop_levels,
    new_levels,
    new_levels_after,
    addstr2levs,
    label,
    label_map,
    labelstr,
    label_fstr,
    .spl_context) {
  if (!is.null(label) && !is.null(label_map)) {
    stop("a_freq_j: label and label_map cannot be used together.")
  }

  if (!is.null(labelstr) && (!is.null(val) || !is.null(new_levels) || !is.null(excl_levels))) {
    stop("a_freq_j: val/excl_levels/new_levels cannot be used in a summarize_row_group call.")
  }

  if (!is.null(new_levels) && (is.null(.var) || is.na(.var))) {
    stop("When using new_levels, var must be provided in analyze call with a_freq_j.")
  }

  if (!is.null(val) && drop_levels == TRUE) {
    stop("argument val cannot be used together with drop_levels = TRUE.")
  }

  if (!is.null(val) && !is.null(excl_levels)) {
    stop("argument val and excl_levels cannot be used together.")
  }

  ## start making updates to factor information on incoming data

  if (!is.null(new_levels)) {
    df_row <- h_df_add_newlevels(
      df = df_row,
      .var = .var,
      new_levels = new_levels,
      new_levels_after = new_levels_after,
      addstr2levs = addstr2levs
    )
  }

  # if character var turn incoming data into factor, with levels from row-based df
  if (!is.na(.var) && is.character(df_row[[.var]]) && is.null(labelstr)) {
    levels <- sort(unique(df_row[[.var]]))
    df_row[[.var]] <- factor(df_row[[.var]], levels = levels)
    drop_levels <- FALSE
  }

  if (!is.null(labelstr)) {
    single_level <- labelstr

    if (!is.null(label_fstr) && grepl("%s", label_fstr, fixed = TRUE)) {
      single_level <- sprintf(label_fstr, single_level)
    }

    df_row[[.var]] <- as.character(df_row[[.var]])
    df_row[[.var]][!is.na(df_row[[.var]])] <- single_level

    df_row[[.var]] <- factor(as.character(df_row[[.var]]), levels = single_level)
    drop_levels <- FALSE
  }

  if (drop_levels) {
    obs_levs <- unique(df_row[[.var]])
    obs_levs <- intersect(levels(df_row[[.var]]), obs_levs)

    if (!is.null(excl_levels)) obs_levs <- setdiff(obs_levs, excl_levels)

    val <- obs_levs
    excl_levels <- NULL
  }

  if (!is.null(val)) {
    # do not yet restrict to val levels, only update factors to the requested levels df_row <-
    df_row <- h_update_factor(df_row, .var, val)
  }
  if (!is.null(excl_levels)) {
    # do not yet exclude the level specified in excl_levels, only update factors to remove requested levels df_row

    df_row <- h_update_factor(df_row, .var, excl_levels = excl_levels)
    val <- levels(df_row[[.var]])
  }

  ## update data with level coming from label -- important that this is done after restriction to val levels!!!
  if (!is.null(label)) {
    # similar to processing when labelstr
    single_level <- label
    df_row[[.var]] <- as.character(df_row[[.var]])
    df_row[[.var]][!is.na(df_row[[.var]])] <- single_level
    val <- label
    df_row[[.var]] <- factor(df_row[[.var]], levels = single_level)
  }

  # now update labels coming from label_map
  if (!is.null(label_map)) {
    split_info <- .spl_context[c("split", "value")]
    new_labels <- h_get_label_map(levels(df_row[[.var]]), label_map, .var, split_info)

    df_row[[.var]] <- factor(as.character(df_row[[.var]]), levels = levels(df_row[[.var]]), labels = new_labels)

    val <- new_labels
  }

  # now apply the updated factors from df_row to df as well due to drop_levels = TRUE can have different results when
  # applying all of the above to df
  col_expr <- .spl_context$cur_col_expr[[1]]
  df <- subset(df_row, eval(col_expr))

  return(list(df_row = df_row, df = df, val = val))
}


#' Get Label Map
#'
#' Maps labels based on the provided label map and split context.
#'
#' @param .labels Current labels.
#' @param label_map Mapping for labels.
#' @param .var Variable name.
#' @param split_info Current split information.
#' @return Mapped labels.
#' @noRd
#' @keywords internal
h_get_label_map <- function(.labels, label_map, .var, split_info) {
  if (!is.null(label_map)) {
    if (!all(c("split", "value") %in% names(split_info))) {
      stop("split_info does not contain required elements.")
    }

    ### if label_map has a variable from row split, apply current splits on label_map tibble as well
    rowsplits <- split_info$split

    label_map_split <- intersect(names(label_map), rowsplits)

    if (!(length(label_map_split) == 0)) {
      for (i in seq_along(label_map_split)) {
        cursplvar <- label_map_split[i]
        cid <- match(cursplvar, rowsplits)
        cursplval <- split_info$value[cid]

        label_map <- label_map[label_map[[cursplvar]] == cursplval, ]
      }
    }

    if ("var" %in% names(label_map)) {
      label_map <- label_map[label_map[["var"]] == .var, ]
    }

    .labels <- label_map$label[match(.labels, label_map$value)]

    if (anyNA(.labels)) {
      stop("got a label map that doesn't provide labels for all values.")
    }
  }

  return(.labels)
}


#' A Frequency Data Preparation Function
#'
#' Prepares frequency data for analysis.
#' @noRd
#' @param df Data frame to prepare.
#' @param labelstr Label string.
#' @param .var Variable name.
#' @param val Values for analysis.
#' @param drop_levels Boolean, indicating if levels should be dropped.
#' @param excl_levels Levels to exclude.
#' @param new_levels New levels to add.
#' @param new_levels_after Boolean for adding new levels after existing ones.
#' @param addstr2levs String to add to new levels.
#' @param .df_row Current data frame row.
#' @param .spl_context Current split context.
#' @param .N_col Number of columns.
#' @param id Identifier variable.
#' @param denom Denominator types.
#' @param variables Variables to include in the analysis.
#' @param label Label string.
#' @param label_fstr Formatted label string.
#' @param label_map Mapping for labels.
#' @param .alt_df_full Alternative full data frame.
#' @param denom_by Denominator grouping variable.
#' @param .stats Statistics to compute.
#' @return List containing prepared data frames and values.
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
    .stats) {
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


#' Frequency Preparation in Rows
#'
#' Prepares frequency data in rows based on provided parameters.
#' @noRd
#' @param x_stats Statistics data.
#' @param .stats_adj Adjusted statistics.
#' @param .formats Format settings.
#' @param labelstr Label string.
#' @param label_fstr Formatted label string.
#' @param label Label string.
#' @param .indent_mods Indentation settings.
#' @param .labels_n Labels for statistics.
#' @param na_str String for NA values.
#' @return List containing prepared statistics, formats, labels, and indentation.
#' @noRd
#' @keywords internal
h_a_freq_prepinrows <- function(
    x_stats,
    .stats_adj,
    .formats,
    labelstr,
    label_fstr,
    label,
    .indent_mods,
    .labels_n,
    na_str) {
  # Fill in formatting defaults

  x_stats <- x_stats[.stats_adj]

  levels_per_stats <- lapply(x_stats, names)

  # Fill in formats/indents/labels with custom input and defaults
  .formats <- junco_get_formats_from_stats(.stats_adj, .formats, levels_per_stats)

  # lbls
  all_stats <- junco_get_stats("a_freq_j", stats_in = NULL, custom_stats_in = NULL)
  count_rr_stats <- grep("(count|rr_ci)", all_stats, value = TRUE)
  n_stats <- setdiff(all_stats, count_rr_stats)
  n_stats <- .stats_adj[.stats_adj %in% n_stats]

  if (length(.stats_adj) == 1 && length(n_stats) == 1) {
    if (!is.null(labelstr) && (!is.null(label))) {
      lbls <- label
    } else if (!is.null(labelstr) && !is.null(.labels_n)) {
      lbls <- .labels_n
    } else if (!is.null(labelstr) && is.null(label_fstr)) {
      lbls <- labelstr
    } else if (!is.null(labelstr) && !is.null(label_fstr) && grepl("%s", label_fstr, fixed = TRUE)) {
      lbls <- sprintf(label_fstr, labelstr)
    } else if (!is.null(label)) {
      lbls <- label
    } else {
      .labels <- .labels_n
      lbls <- junco_get_labels_from_stats(.stats_adj, .labels, levels_per_stats)
    }
  } else {
    if (length(n_stats) > 1 && is.null(.labels_n)) {
      msg <- paste0(
        "recommend to specify non-null .labels_n argument when multiple n_stats selected (",
        paste0(n_stats, collapse = ", "),
        ")"
      )
      message(msg)
    }
    ### labels from the non n-stat statistics are present on levels_per_stats labels for the n-stat statistics come
    ### from .labels_n (user) or from defaults
    .labels <- .labels_n
    lbls <- junco_get_labels_from_stats(.stats_adj, .labels, levels_per_stats)
  }
  .labels <- lbls
  .labels <- .unlist_keep_nulls(.labels)

  # indents
  .indent_mods_orig <- .indent_mods
  .indent_mods <- junco_get_indents_from_stats(.stats_adj, .indent_mods, levels_per_stats)

  # adjust indents when n_stat is included and no indents where passed by user
  if (length(n_stats) >= 1 && is.null(.indent_mods_orig)) {
    .indent_mods_new <- sapply(
      names(.indent_mods),
      FUN = function(x) {
        ret <- 0
        if (!(x %in% n_stats)) ret <- 1
        return(ret)
      },
      simplify = FALSE,
      USE.NAMES = TRUE
    )
    .indent_mods <- .indent_mods_new
  }

  .indent_mods <- .unlist_keep_nulls(.indent_mods)

  # .format_na_strs processing if na_str = c(NA, NA, NA) this will ensure the ci (NA, NA, NA) will be represented as
  # NE (NE, NE) the value NE is defined as the default to replace NA in our jjcs format

  if (!is.null(na_str)) {
    # Create a list of na_str values for each format in .formats
    .format_na_strs <- lapply(names(.formats), FUN = function(x) {
      na_str
    })
    names(.format_na_strs) <- names(.formats)
  } else {
    .format_na_strs <- NULL
  }

  # Unlist stats + names
  x_stats <- .unlist_keep_nulls(x_stats)
  names(x_stats) <- names(.formats)

  return(list(
    x_stats = x_stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods,
    .format_na_strs = .format_na_strs
  ))
}

#' Subset Combination
#'
#' Subsets a data frame based on specified combination criteria.
#' @noRd
#' @param df Data frame to subset.
#' @param combosdf Data frame containing combinations.
#' @param do_not_filter Variables to not filter.
#' @param filter_var Variable used for filtering.
#' @param flag_var Flag variable for filtering.
#' @param colid Column ID for identification.
#' @return Subsetted data frame.
h_subset_combo <- function(df, combosdf, do_not_filter, filter_var, flag_var, colid) {
  ### this is the core code for subsetting to appropriate combo level
  if (!is.null(flag_var)) {
    df <- df[df[[flag_var]] == "Y", ]
  }

  # get the string related to combosdf text from colid it is the last part of the column id after the .  eg 'Active
  # Study Agent.Apalutamide.Thru 3 months' colid_str is 'Thru 3 months' colid_str <- stringr::str_split_i(colid,
  # '\\.', i = -1)
  colid_str <- tail(unlist(strsplit(colid, "\\.")), 1)

  filter_val <- combosdf[combosdf$valname == colid_str, ]$label

  if (!(colid_str %in% do_not_filter)) {
    df <- df |>
      dplyr::filter(get(filter_var) == filter_val)
  }

  return(df)
}
