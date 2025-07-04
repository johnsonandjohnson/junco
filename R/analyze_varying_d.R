#' @describeIn junco_varying_decimal_precision Junco specific analysis function as alternative to `tern::a_summary` that can handle varying decimal precision
#' @inheritParams proposal_argument_convention
#' @inheritParams junco_varying_decimal_precision
#' @param .format_defaults Named vector of default formats (defaulted to `junco_default_formats_d`, but `tern::tern_default_formats` can be used as well.)
#' @return
#' * `a_summary_d_j()` returns the corresponding list with formatted [rtables::CellValue()].
#' @export
#' @order 1

a_summary_d_j <- function(df,
                          ...,
                          .var,
                          .df_row,
                          .stats = NULL,
                          .formats = NULL,
                          .labels = NULL,
                          .indent_mods = NULL,
                          .format_defaults = junco_default_formats_d,
                          formatting_fun = jjcsformat_xx,
                          d = 1,
                          d_unspecified = 0) {
  dots_extra_args <- list(...)
  
  ## merge incoming .formats with junco default formats using xx.d format specifications
  ## incoming .formats specs overrule junco default formats
  .formats_notin_common <- setdiff(names(.format_defaults), names(.formats))
  .formats <- c(.formats, .format_defaults[.formats_notin_common])
  
  # basic check on specifications regarding decimal precision
  # will stop if a check fails
  h_check_d(d, .var, d_unspecified, msg_pre = "a_summary_d_j - issue with extra args")
  
  # get the numeric value of d
  d <-  h_get_d(d, .df_row, .var, d_unspecified)
  
  ## translation of xx.d into xx.x
  .formats <- sapply(.formats, 
                     FUN = function(fmt_str) {
                       if (!is.function(fmt_str) && grepl("xx.d", fmt_str, fixed = TRUE)) {
                         fmt_new <- format_xxd(fmt_str, d = d, formatting_fun = formatting_fun)
                       } else {fmt_new <- fmt_str}
                     })  
  
  fnc_args <- list(x = df[[.var]],
                   .stats = .stats,
                   .formats = .formats,
                   .labels = .labels,
                   .indent_mods = .indent_mods
  )
  
  fnc_args <- append(fnc_args, dots_extra_args)
  
  afun <- a_summary
  do.call(afun, fnc_args)
}

#' @describeIn junco_varying_decimal_precision Junco specific analysis function as alternative to `tern:::a_ancova` that can handle varying decimal precision
#' @inheritParams a_summary_d_j
#' @return
#' * `a_summarize_ancova_d_j()` returns the corresponding list with formatted [rtables::CellValue()].
#' @export
#' @order 2
a_summarize_ancova_d_j <- function(df,
                                   .var,
                                   .df_row,
                                   ref_path, 
                                   .spl_context,
                                   ...,
                                   .stats = NULL,
                                   .formats = NULL,
                                   .labels = NULL,
                                   .indent_mods = NULL,
                                   .format_defaults = junco_default_formats_d,
                                   formatting_fun = jjcsformat_xx,
                                   d = 1,
                                   d_unspecified = 0) {
  
  dots_extra_args <- list(...)
  
  ## merge incoming .formats with junco default formats using xx.d format specifications
  ## incoming .formats specs overrule junco default formats
  .formats_notin_common <- setdiff(names(.format_defaults), names(.formats))
  .formats <- c(.formats, .format_defaults[.formats_notin_common])
  
  # basic check on specifications regarding decimal precision
  # will stop if a check fails
  h_check_d(d, .var, d_unspecified, msg_pre = "a_summarize_ancova_d_j - issue with extra args")
  
  # get the numeric value of d
  d <-  h_get_d(d, .df_row, .var, d_unspecified)
  
  ## translation of xx.d into xx.x
  .formats <- sapply(.formats, 
                     FUN = function(fmt_str) {
                       if (!is.function(fmt_str) && grepl("xx.d", fmt_str, fixed = TRUE)) {
                         fmt_new <- format_xxd(fmt_str, d = d, formatting_fun = formatting_fun)
                       } else {fmt_new <- fmt_str}
                     })  
  
  fnc_args <- list(df,
                   .var = .var,
                   ref_path = ref_path,
                   .spl_context = .spl_context,
                   .df_row = .df_row,
                   .stats = .stats,
                   .formats = .formats,
                   .labels = .labels,
                   .indent_mods = .indent_mods
  )
  fnc_args <- append(fnc_args, dots_extra_args)
  
  a_fun <- a_summarize_ancova_j
  do.call(a_fun, fnc_args)  
}
