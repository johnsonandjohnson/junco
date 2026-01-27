h_get_eair_df <- function(levii, df, denom_df, .var, id, occ_var, occ_dy, fup_var) {
  dfii <- df[df[[.var]] == levii & !is.na(df[[.var]]), ]

  df_denom <- unique(denom_df[, c(id, fup_var)])
  df_num <- subset(dfii, dfii[[occ_var]] == "Y")[, c(id, .var, occ_var, occ_dy)]

  ### construct modified fup var subjects not in numerator - use fup_var from df_denom
  df_denom$mod_fup_var <- df_denom[[fup_var]]

  ### add vars from df_num onto df_denom
  df_denom <- dplyr::left_join(df_denom, df_num, by = id)

  # subjects in numerator dataset, use occ_dy variable/365.25
  id_to_update <- df_denom[[id]] %in% df_num[[id]]
  df_denom[id_to_update, "mod_fup_var"] <- df_denom[id_to_update, occ_dy] / 365.25

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
