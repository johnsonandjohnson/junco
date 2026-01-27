make_dps_regex <- function(keyword, type = c("[", "{")) {
  mustart <- match.arg(type)
  muend <- ifelse(mustart == "[", "]", "}")

  paste0("[~][", mustart, "]", keyword, " ([^", muend, "]+)[", muend, "]")
}

handle_one_markup <- function(strs, keyword, rtfstart, rtfend) {
  start <- ifelse(nzchar(rtfstart), paste0("\\", rtfstart, " "), "")
  end <- ifelse(nzchar(rtfstart), paste0("\\", rtfend, " "), "")
  out <- gsub(make_dps_regex(keyword), paste0(start, "\\1", end), strs)
  out <- gsub(make_dps_regex(keyword, "{"), paste0(start, "\\1", end), out)
  out
}


dps_markup_df <- tibble::tribble(
  ~keyword,
  ~rtfstart,
  ~rtfend,
  "super",
  "\\super",
  "\\nosupersub",
  "sub",
  "\\sub",
  "\\nosupersub",
  "optional",
  "",
  ""
)


#' Default String Mapping for Special Characters
#'
#' A tibble that maps special characters to their UTF-8 equivalents for use in RTF output.
#' Currently it maps ">=" and "<=" to the Unicode characters.
#'
#' @return A tibble with columns 'pattern' and 'value', where 'pattern' contains
#'   the string to be replaced and 'value' contains the replacement.
#'
#' @export
#' @keywords internal
default_str_map <- tibble::tribble(
  ~pattern,
  ~value,
  ">=",
  intToUtf8(strtoi(2265, base = 16)),
  "<=",
  intToUtf8(strtoi(2264, base = 16))
)

convert_dps_markup <- function(strs, markup_df = dps_markup_df) {
  for (i in seq_len(nrow(markup_df))) {
    strs <- handle_one_markup(strs, markup_df$keyword[i], markup_df$rtfstart[i], markup_df$rtfend[i])
  }
  strs
}

strmodify <- function(strs, replacement_str = default_str_map) {
  map_tbl <- replacement_str
  if (!is.null(map_tbl) && nrow(map_tbl) > 0) {
    for (i in seq_len(nrow(map_tbl))) {
      pattern <- map_tbl$pattern[[i]]
      value <- map_tbl$value[i]
      strs <- gsub(pattern, value, strs, fixed = TRUE)
    }
  }
  return(strs)
}

prep_strs_for_rtf <- function(strs, string_map = default_str_map, markup_df = dps_markup_df) {
  strs <- convert_dps_markup(strs, markup_df)
  strs <- strmodify(strs, string_map)
  return(strs)
}


#' Relabel Variables in a Dataset
#'
#' This function relabels variables in a dataset based on a provided list of labels.
#' It can either replace existing labels or only add labels to variables without them.
#'
#' @param x (`data.frame`)\cr dataset containing variables to be relabeled.
#' @param lbl_list (`list`)\cr named list of labels to apply to variables.
#' @param replace_existing (`logical`)\cr if TRUE, existing labels will be replaced;
#'   if FALSE, only variables without labels will be updated.
#'
#' @return The dataset with updated variable labels.
#'
#' @export
#' @keywords internal
var_relabel_list <- function(x, lbl_list, replace_existing = TRUE) {
  if (replace_existing) {
    vars_to_relabel <- intersect(names(x), names(lbl_list))
  } else {
    get_variables_with_empty_labels <- function(x) {
      labels <- var_labels(x)
      mask <- lapply(labels, is.na) |> unlist()
      return(names(labels[mask]))
    }

    vars_without_labels <- get_variables_with_empty_labels(x)
    vars_to_relabel <- intersect(vars_without_labels, names(lbl_list))
  }
  do.call(var_relabel, c(list(x = x), lbl_list[vars_to_relabel]))
}
