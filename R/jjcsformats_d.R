xxd_to_xx <- function(str, d = 0) {
  checkmate::assert_integerish(d, null.ok = TRUE)
  if (checkmate::test_list(str, null.ok = FALSE)) {
    checkmate::assert_list(str, null.ok = FALSE)
    # Or it may be a vector of characters
  } else {
    checkmate::assert_character(str, null.ok = FALSE)
  }
  
  nmstr <- names(str)
  
  if (any(grepl("xx.d", str, fixed = TRUE))) {
    checkmate::assert_integerish(d)
    str <- gsub("xx.d", paste0("xx.", strrep("x", times = d)), str, fixed = TRUE)
  }
  str <- stats::setNames(str, nmstr)
  return(str)
}

format_xxd <- function(str, d = 0, .df_row, formatting_fun = NULL) {
  if (is.function(str)){
    return(str)
  }
  
  # Handling of data precision
  if (!is.numeric(d)) {
    if (is.character(d) && length(d) == 1) {
      # check if d is a variable name available in .df_row
      if (d %in% names(.df_row)) {
        d <- max(.df_row[[d]], na.rm = TRUE)
      } else {
        message(paste("precision has been reset to d = 0, as variable", d, "not present on input"))
        d <- 0
      }
    }
  }
  # convert xxd type of string to xx
  fmt <- xxd_to_xx(str = str, d = d)
  
  if (!is.null(formatting_fun)) {
    fmt <- formatting_fun(fmt)
  }
  
  return(fmt)
}


junco_def_d <- c("mean" = jjcsformat_xx("xx.xxxxx"),
                 "mean_sd" = "xx.dx (xx.dxx)",
                 "mean_se" = "xx.dx (xx.dxx)",
                 "range" = "xx. - xx.")

fmt_spec_single_d <- function(d = 1,
                              stats_in = NULL,
                              fmt_d_def = junco_def_d,
                              fmt_d_in = NULL){
  
  if (is.null(stats_in)) stats_in <- names(fmt_d_def)
  
  fmt_d <- fmt_d_def[stats_in]
  
  formats <- lapply(fmt_d, FUN = format_xxd, d = d, formatting_fun = jjcsformat_xx)
  
  formats
}


fmt_spec_df_d <- function(df_d,
                          d_column = "d",
                          fmt_column = "fmt_d",
                          stats_in = NULL,
                          fmt_d_def = junco_def_d,
                          fmt_d_in = NULL){
  
  df_d[[fmt_column]] <- lapply(df_d[[d_column]], 
                               fmt_spec_single_d, 
                               stats_in = stats_in,
                               fmt_d_def = fmt_d_def,
                               fmt_d_in = fmt_d_in)

  df_d
}



df_d <- tribble(~PARAMCD, ~d,
                "DIABP", 2L,
                "PULSE", 3L,
                "RESP", 1L)

yy <- fmt_spec_df_d(df_d,
                    d_column = "d",
                    fmt_column = "fmt_d",
                    stats_in = NULL,
                    fmt_d_def = junco_def_d,
                    fmt_d_in = NULL)
str(yy$fmt_d[1])


df <- ex_advs |> 
  dplyr::filter(PARAMCD %in% c("DIABP", "PULSE",  "RESP")) |> 
  dplyr::filter(AVISIT %in% c("BASELINE", "WEEK 1 DAY 8", "WEEK 2 DAY 15"))

df2 <- df |> 
  dplyr::left_join(yy)


lyt <- basic_table() |> 
  split_cols_by("ARMCD") |> 
  split_rows_by("PARAMCD", split_fun = drop_split_levels) |> 
  split_rows_by("AVISIT", split_fun = drop_split_levels) |> 
  analyze(vars = "AVAL",
          afun = a_summary,
          extra_args = list(.stats = c("n", "mean_se", "range", "mean"),
                            .formats = "default"),
          formats_var = "fmt_d")

rslt <- build_table(lyt, df2, alt_counts_df = ex_adsl)

rslt
