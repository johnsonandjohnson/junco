h_get_eair_df <- function(levii, df, denom_df, .var, id, occ_var, occ_dy, fup_var, count_events = FALSE) {
  dfii <- df[df[[.var]] == levii & !is.na(df[[.var]]), ]

  df_denom <- unique(denom_df[, c(id, fup_var), drop = FALSE])
  if (!is.null(occ_var)) {
    df_num <- unique(subset(dfii, dfii[[occ_var]] == "Y")[, c(id, .var, occ_var, occ_dy), drop = FALSE])
    df_num[["n_events"]] <- 1L
    if (any(duplicated(df_num[[id]]))) {
      stop("Input dataset must uniquely identify one record per subject/.var/occ_var.")
    }
  } else {
    if (count_events) {
      df_num <- dfii[, c(id, .var), drop = FALSE]
      df_num[["n_events"]] <- ave(as.character(df_num[[id]]), as.character(df_num[[id]]), FUN = length)
      df_num <- unique(df_num)
    } else {
      df_num <- unique(dfii[, c(id, .var), drop = FALSE])
      df_num[["n_events"]] <- 1L
    }
  }
  df_num[["n_events"]] <- as.numeric(df_num[["n_events"]])
  ### construct modified fup var subjects not in numerator - use fup_var from df_denom
  df_denom[["mod_fup_var"]] <- df_denom[[fup_var]]

  if (!is.null(occ_var)) {
    ### add vars from df_num onto df_denom
    df_denom <- merge(df_denom, df_num, by = id, all.x = TRUE)

    # subjects in numerator dataset, use occ_dy variable/365.25
    id_to_update <- df_denom[[id]] %in% df_num[[id]]
    df_denom[id_to_update, "mod_fup_var"] <- df_denom[id_to_update, occ_dy] / 365.25
  }

  return(list(df_denom = df_denom, df_num = df_num))
}

extract_x_stats <- function(list_with_stats, stat_nms) {
  sapply(
    stat_nms,
    function(stat) {
      sapply(
        names(list_with_stats),
        function(x) {
          list_with_stats[[x]][[stat]]
        },
        simplify = FALSE
      )
    },
    simplify = FALSE
  )
}
