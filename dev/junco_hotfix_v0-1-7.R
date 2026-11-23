# ==========================================
# Junco Hotfix Patch File
# ==========================================
# Summary of injected hotfixes:
# - Hotfix #539 `a_maxlev()` a_maxlev() displays "-" instead of 0 when denominator exists but no records are presen (issue #538) (introduced in junco v0.1.7)
# ==========================================

# ==========================================
# Surgical Patches
# ==========================================

a_maxlev <- function(df,
                     labelstr = NULL,
                     .var,
                     .spl_context,
                     id = "USUBJID",
                     .alt_df_full = NULL,
                     any_level = FALSE,
                     any_level_exclude = "Missing",
                     ...) {
  checkmate::assert_factor(df[[.var]], ordered = TRUE, any.missing = FALSE)
  checkmate::assert_string(id)
  checkmate::assert_subset(id, choices = colnames(df))
  checkmate::assert_subset(id, choices = colnames(.alt_df_full))
  checkmate::assert_flag(any_level)
  checkmate::assert_character(any_level_exclude, any.missing = FALSE, unique = TRUE)

  varvec <- df[[.var]]
  levs <- levels(varvec)
  levs_lab <- if (any_level) {
    labelstr
  } else {
    levs
  }

  N_row <- nrow(df) #### TODO: hotfix #539 

  cur_col <- subset(.alt_df_full, eval(.spl_context$cur_col_expr[[1]])) #### TODO: hotfix #539 
  N <- length(unique(cur_col[[id]])) #### TODO: hotfix #539 

  count_pct <- if (N_row != 0 && N > 0) { #### TODO: hotfix #539 
    max_per_id <- tapply(varvec, df[[id]], max)
    max_per_id <- ordered(levs[max_per_id], levels = levs) # tapply loses levels of varvec.

    count_max <- if (any_level) {
      sum(max_per_id != any_level_exclude)
    } else {
      table(max_per_id)
    }

    as.list(data.frame(rbind(count_max, count_max / N)))
  } else if (N > 0) { #### TODO: hotfix #539 
    rep(list(c(0, 0)), length(levs_lab)) #### TODO: hotfix #539 
  } else {
    rep(list(NA), length(levs_lab))
  }

  count_pct <- setNames(count_pct, levs_lab)

  in_rows(
    .list = count_pct,
    .formats = jjcsformat_count_fraction,
    .format_na_strs = "-"
  )
}