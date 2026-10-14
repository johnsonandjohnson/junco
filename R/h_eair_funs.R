#' @importFrom stats ave
h_get_eair_df <- function(
  levii,
  df,
  denom_df,
  .var,
  id,
  occ_var,
  occ_dy,
  fup_var,
  count_multiple_events = FALSE,
  strata = NULL
) {
  dfii <- df[df[[.var]] == levii & !is.na(df[[.var]]), ]

  df_denom <- unique(denom_df[, c(id, fup_var, strata), drop = FALSE])
  if (!is.null(occ_var)) {
    df_num <- unique(subset(dfii, dfii[[occ_var]] == "Y")[, c(id, .var, occ_var, occ_dy, strata), drop = FALSE])
    df_num[["n_events"]] <- 1L
    if (any(duplicated(df_num[[id]]))) {
      stop("Input dataset must uniquely identify one record per subject/.var/occ_var.")
    }
  } else {
    if (count_multiple_events) {
      df_num <- dfii[, c(id, .var, strata), drop = FALSE]
      df_num[["n_events"]] <- ave(as.character(df_num[[id]]), as.character(df_num[[id]]), FUN = length)
      df_num <- unique(df_num)
    } else {
      df_num <- unique(dfii[, c(id, .var, strata), drop = FALSE])
      df_num[["n_events"]] <- 1L
    }
  }
  df_num[["n_events"]] <- as.numeric(df_num[["n_events"]])
  ### construct modified fup var subjects not in numerator - use fup_var from df_denom
  df_denom[["mod_fup_var"]] <- df_denom[[fup_var]]

  if (!is.null(occ_var)) {
    ### add vars from df_num onto df_denom
    df_denom <- merge(df_denom, df_num, by = c(id, strata), all.x = TRUE)

    # subjects in numerator dataset, use occ_dy variable/365.25
    id_to_update <- df_denom[[id]] %in% df_num[[id]]
    df_denom[id_to_update, "mod_fup_var"] <- df_denom[id_to_update, occ_dy] / 365.25
  }

  return(list(df_denom = df_denom, df_num = df_num))
}

# helper: get n and T per stratum for one arm's df/denom
.get_stratum_stats <- function(strata, strata_levels, df_arm, denom_arm,
                               levii, .var, id, fup_var, occ_var, occ_dy, count_multiple_events) {
  lapply(strata_levels, function(stratum) {
    df_s <- df_arm[df_arm[[strata]] == stratum, , drop = FALSE]
    denom_s <- denom_arm[denom_arm[[strata]] == stratum, , drop = FALSE]
    eair_dfs <- h_get_eair_df(
      levii = levii,
      df = df_s,
      denom_df = denom_s,
      .var = .var,
      id = id,
      fup_var = fup_var,
      occ_var = occ_var,
      occ_dy = occ_dy,
      count_multiple_events = count_multiple_events,
      strata = strata
    )
    list(
      n  = sum(eair_dfs$df_num[["n_events"]]),
      py = sum(eair_dfs$df_denom[["mod_fup_var"]])
    )
  })
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
