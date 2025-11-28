# Functions ----

do_exclude_split <- function(exclude_levels, .spl_context) {
  checkmate::assert_list(exclude_levels, names = "unique")
  split_vals <- .spl_context[, c("split", "value")]
  split_vals_list <- as.list(split_vals$value) |> stats::setNames(split_vals$split)
  found_splits <- intersect(names(split_vals_list), names(exclude_levels))
  for (split_var in found_splits) {
    if (any(split_vals_list[[split_var]] %in% exclude_levels[[split_var]])) {
      return(TRUE)
    }
  }
  FALSE
}

a_freq_j_with_exclude <- function(df, labelstr = NULL, exclude_levels, 
                                  .var = NA, .spl_context, 
                                  .df_row, .N_col, .alt_df_full, 
                                  .stats = "count_unique_denom_fraction", 
                                  .formats = NULL, 
                                  .indent_mods = NULL,
                                  .labels_n = NULL, 
                                  ...) {
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

a_cmhrms_j_with_exclude <- function(df, exclude_levels, .var, .spl_context, 
                                    .ref_group, .in_ref_col, .df_row, ...,
                                    .stats = NULL, .formats = NULL, .indent_mods = NULL, .labels = NULL) {
  if (do_exclude_split(exclude_levels, .spl_context)) {
    NULL
  } else {
    a_cmhrms_j(
      df = df, 
      .var = .var, 
      .spl_context = .spl_context, 
      .ref_group = .ref_group, 
      .in_ref_col = .in_ref_col, 
      .df_row = .df_row, 
      ...,
      .stats = .stats, 
      .formats = .formats,
      .indent_mods = .indent_mods,
      .labels = .labels
    )
  }
}
