#' @name jj_complex_scorefun
#' @title Complex Scoring Function
#' @description
#' A function used for sorting AE tables (and others) as required.
#' @details
#' This sort function sorts as follows:
#' * Takes all the columns from a specified spanning column header (default= colspan_trt)
#' and sorts by the last treatment
#' column within this.
#' * If no spanning column header variable exists (e.g you have only one active treatment arm and have decided to
#' remove the spanning header from your layout), it will sort by the first treatment column in your table.
#'
#' This function is not really designed for tables that have sub-columns. However, if users wish to override any
#' default sorting behavior, they can simply specify their own colpath to use for sorting on (default = NULL)
#'
#' @param spanningheadercolvar (`character`)\cr
#' Name of spanning header variable that defines the active treatment columns.
#' If you do not have an active treatment spanning header column then user can define this as NA.
#' @param usefirstcol (`logical`)\cr
#' This allows you to just use the first column of the table to sort on.
#' @param colpath (`character`)\cr
#' Name of column path that is needed to sort by (default=NULL).
#' This overrides other arguments if specified
#' (except firstcat and lastcat which will be applied if requested on this colpath).
#' @param firstcat (`logical`)\cr
#' If you wish to put any category at the top of the list despite any n's, user can specify it here.
#' @param lastcat (`logical`)\cr
#' If you wish to put any category at the bottom of the list despite any n's, user can specify it here.
#' @export
#' @returns A function which can be used as a score function (scorefun in `sort_at_path`).
# @examples #result <- sort_at_path(result, c('root', 'AEBODSYS'), scorefun = jj_complex_scorefun())
#' @examples
#' if (require("pharmaverseadamjnj")) {
#'   library(dplyr)
#'
#'   ADAE <- pharmaverseadamjnj::adae |>
#'     select(USUBJID, AEBODSYS, AEDECOD, TRT01A, TRTEMFL) |>
#'     filter(AEBODSYS %in% c("Cardiac disorders", "Nervous system disorders")) |>
#'     filter(AEDECOD %in% c(
#'       "DIZZINESS", "SYNCOPE", "SINUS BRADYCARDIA", "PALPITATIONS"
#'     )) |>
#'     mutate(colspan_trt = factor(
#'       if_else(TRT01A == "Placebo", " ", "Active Study Agent"),
#'       levels = c("Active Study Agent", " ")
#'     )) |>
#'     mutate(rrisk_header = "Risk Difference (%) (95% CI)") |>
#'     mutate(rrisk_label = paste(TRT01A, paste("vs", "Placebo")))
#'
#'   colspan_trt_map <- create_colspan_map(ADAE,
#'     non_active_grp = "Placebo",
#'     non_active_grp_span_lbl = " ",
#'     active_grp_span_lbl = "Active Study Agent",
#'     colspan_var = "colspan_trt",
#'     trt_var = "TRT01A"
#'   )
#'
#'   ref_path <- c("colspan_trt", " ", "TRT01A", "Placebo")
#'
#'   lyt <- basic_table() |>
#'     split_cols_by(
#'       "colspan_trt",
#'       split_fun = trim_levels_to_map(map = colspan_trt_map)
#'     ) |>
#'     split_cols_by("TRT01A") |>
#'     split_cols_by("rrisk_header", nested = FALSE) |>
#'     split_cols_by(
#'       "TRT01A",
#'       labels_var = "rrisk_label",
#'       split_fun = remove_split_levels("Placebo")
#'     ) |>
#'     analyze(
#'       "TRTEMFL",
#'       a_freq_j,
#'       show_labels = "hidden",
#'       extra_args = list(
#'         method = "wald",
#'         label = "Subjects with >=1 AE",
#'         ref_path = ref_path,
#'         .stats = "count_unique_fraction"
#'       )
#'     ) |>
#'     split_rows_by("AEBODSYS",
#'       split_label = "System Organ Class",
#'       split_fun = trim_levels_in_group("AEDECOD"),
#'       label_pos = "topleft",
#'       section_div = c(" "),
#'       nested = FALSE
#'     ) |>
#'     summarize_row_groups(
#'       "AEBODSYS",
#'       cfun = a_freq_j,
#'       extra_args = list(
#'         method = "wald",
#'         ref_path = ref_path,
#'         .stats = "count_unique_fraction"
#'       )
#'     ) |>
#'     analyze(
#'       "AEDECOD",
#'       afun = a_freq_j,
#'       extra_args = list(
#'         method = "wald",
#'         ref_path = ref_path,
#'         .stats = "count_unique_fraction"
#'       )
#'     )
#'
#'   result <- build_table(lyt, ADAE)
#'
#'   result
#'
#'   result <- sort_at_path(
#'     result,
#'     c("root", "AEBODSYS"),
#'     scorefun = jj_complex_scorefun()
#'   )
#'
#'   result <- sort_at_path(
#'     result,
#'     c("root", "AEBODSYS", "*", "AEDECOD"),
#'     scorefun = jj_complex_scorefun()
#'   )
#'
#'   result
#' }
#' @rdname complex_scoring_function
#' @aliases jj_complex_scorefun
jj_complex_scorefun <- function(
    spanningheadercolvar = "colspan_trt",
    usefirstcol = FALSE,
    colpath = NULL,
    firstcat = NULL,
    lastcat = NULL) {
  paths <- NULL

  function(tt) {
    if (is.null(paths)) {
      paths <<- col_paths(tt)
      if (is.null(colpath)) {
        if (is.na(spanningheadercolvar)) {
          atrt_paths <- vapply(paths, function(pth) pth[[1]] != " ", TRUE)
          use_paths <- atrt_paths
          act_trt_lst <- paths[use_paths]
          first_list <- sapply(act_trt_lst, utils::head, 1)
          first_at_value <- utils::head(first_list, n = 1)
          colpath <<- first_at_value
        } else {
          atrt_paths <- vapply(paths, function(pth) pth[[1]] == spanningheadercolvar && pth[[2]] != " ", TRUE)
          use_paths <- atrt_paths
          act_trt_lst <- paths[use_paths]
          last_list <- sapply(act_trt_lst, utils::tail, 1)
          last_at_value <- utils::tail(last_list, n = 1)
          colpath <<- last_at_value
        }

        if (usefirstcol) {
          atrt_paths <- vapply(paths, function(pth) pth[[1]] != " ", TRUE)
          use_paths <- atrt_paths
          act_trt_lst <- paths[use_paths]
          first_list <- sapply(act_trt_lst, utils::head, 1)
          first_at_value <- utils::head(first_list, n = 1)
          colpath <<- first_at_value
        }
      }
    }
    score <- unlist(cell_values(tt, colpath = colpath), use.names = FALSE)[1]

    if (length(score) == 0) {
      score <- 1
    }

    if (!is.null(firstcat)) {
      if (obj_name(tt) == firstcat) {
        score <- Inf
      }
    }
    if (!is.null(lastcat)) {
      if (obj_name(tt) == lastcat) {
        score <- -99999
      }
    }
    if (obj_name(tt) == " ") {
      score <- -Inf
    }

    return(score)
  }
}
