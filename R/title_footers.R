.find_titles_file <- function(path = ".") {
  if (file.exists(file.path(path, "titles.csv"))) {
    file.path(path, "titles.csv")
  } else if (file.exists(file.path(path, "titles.xlsx"))) {
    file.path(path, "titles.xlsx")
  } else {
    stop(
      "No titles.csv/xlsx file detected at path ", path,
      "please specify parent directory, full file path, or ",
      "pass the full titles data.frame directly."
    )
  }
}

#' @importFrom utils read.csv
.read_titles_file <- function(file) {
  if (grepl("csv$", file, ignore.case = TRUE)) {
    df <- read.csv(file, check.names = FALSE)
  } else if (grepl("xlsx$", file, ignore.case = TRUE)) {
    if (!requireNamespace("readxl")) {
      stop("readxl package is required for xslx file support, please install it.")
    }
    df <- readxl::read_excel(path = file, sheet = 1, range = readxl::cell_cols("A:C"))
  } else {
    stop("Unrecognized titles file type. file: ", file)
  }
  df
}

# ----
#' @name  get_titles_from_file
#' @title Get Titles/Footers For Table From Sources
#'
#' @description Retrieves the titles and footnotes for a given table from a CSV/XLSX file or a data.frame.
#'
#' @details Retrieves the titles for a given output id (see below) and outputs
#'          a list containing the title and footnote objects supported by
#'          rtables. Both titles.csv and titles.xlsx (*if `readxl` is
#'          installed*) files are supported, with titles.csv being checked
#'          first.
#'
#'          Data is expected to have `TABLE ID`, `IDENTIFIER`, and `TEXT` columns,
#'          where `IDENTIFIER` has the value `TITLE` for a title and `FOOT*` for
#'          footer materials where `*` is a positive integer. `TEXT` contains
#'          the value of the title/footer to be applied.
#'
#' @param file (`character(1)`)\cr A path to CSV or xlsx file containing title
#'   and footer information for one or more outputs. See Details. Ignored if
#'   `title_df` is specified.
#' @param input_path (`character(1)`)\cr A path to look for
#'   titles.csv/titles.xlsx. Ignored if `file` or `title_df` is specified.
#' @param title_df (`data.frame`)\cr A data.frame containing titles and footers for
#'   one or more  outputs. See Details.
#' @param id (`character(1)`)\cr The identifier for the table of interest.
#'
#' @export
#' @returns  List object containing: title, subtitles, main_footer, prov_footer
#'           for the table of interest.  Note: the subtitles and prov_footer are
#'           currently set to NULL. Suitable for use with [`set_titles()`].
# ----
get_titles_from_file <- function(id,
                                 file = .find_titles_file(input_path),
                                 input_path = ".",
                                 title_df = .read_titles_file(file)) {
  ## "TABLE ID" gets munged to "TABLE.ID"
  title_df <- title_df[title_df[["TABLE ID"]] == id, , drop = FALSE]

  message(paste0("Static titles file/data.frame used: "))

  msg <- NULL

  if (nrow(title_df) == 0) {
    msg <- paste0(
      paste("Warning: Table ID", id, "not found in Title file."),
      "\n",
      "A dummy title will be generated to be able to get rtf file produced.",
      "\n",
      "Ensure the titles file gets updated to include the table identifier"
    )

    title <- "Table ID not found in titles file, dummy title for rtf generation purpose"
    main_footer <- "Ensure the titles file gets updated to include the table identifier"
  } else {
    title <- title_df[title_df$IDENTIFIER == "TITLE", ]$TEXT

    if (length(title) != 1) {
      msg <- "Warning: Title file should contain exactly one title record per Table ID"
    } else {
      message(file)
    }

    main_footer <- title_df[grep("^FOOT", title_df$IDENTIFIER), ]$TEXT

    if (length(main_footer) == 0) {
      main_footer <- character()
    }
  }

  if (!is.null(msg)) {
    warning(msg)
  }

  # ---- Return titles and footnotes.

  title_foot <- list(
    title = title,
    subtitles = NULL,
    main_footer = main_footer,
    prov_footer = NULL
  )

  return(title_foot)
}

# ----
#' @name  set_titles
#' @title Set Output Titles
#'
#' @description Retrieves titles and footnotes from the list specified in the titles
#'              argument and appends them to the TableTree specified in the obj argument.
#'
#'
#' @param obj (`TableTree`)\cr The TableTree to which the titles and footnotes will be appended.
#' @param titles (`list`)\cr The list object containing the titles and footnotes to be appended.
#' @export
#'
#' @returns  The TableTree object specified in the obj argument, with titles
#'           and footnotes appended.
# ----
set_titles <- function(obj, titles) {
  ## add title and footers
  main_title(obj) <- titles$title
  if (!is.null(titles$subtitles)) {
    subtitles(obj) <- titles$subtitles
  }
  main_footer(obj) <- titles$main_footer
  if (!is.null(titles$prov_footer)) {
    prov_footer(obj) <- titles$prov_footer
  }

  return(obj)
}
