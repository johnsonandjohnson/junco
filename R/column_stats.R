calc_one_visit <- function(datvec, decimal, statnm, visit, varnm, roundmethod = c("sas", "iec"), exclude_visits,
                           var_names = c("AVAL", "CHG", "BASE")) {
  roundmethod <- match.arg(roundmethod)
  if (is.na(decimal)) {
    decimal <- 0
  }
  if ((varnm == var_names[2] || varnm == var_names[3]) && (visit %in% exclude_visits)) {
    return(NULL)
  }
  if (roundmethod == "sas") {
    switch(statnm,
      N = length(stats::na.omit(datvec)),
      SE = format(
        tidytlg::roundSAS(stats::sd(datvec) / sqrt(length(stats::na.omit(datvec))), decimal + 2),
        nsmall = decimal + 2
      ),
      SD = format(
        tidytlg::roundSAS(stats::sd(datvec), decimal + 2),
        nsmall = decimal +
          2
      ),
      Mean = format(tidytlg::roundSAS(mean(datvec), decimal + 1), nsmall = decimal + 1),
      mean_sd = paste0(
        format(tidytlg::roundSAS(mean(datvec), decimal + 1), nsmall = decimal + 1),
        " (",
        format(
          tidytlg::roundSAS(stats::sd(datvec), decimal + 2),
          nsmall = decimal +
            2
        ),
        ")"
      ),
      Med = format(tidytlg::roundSAS(stats::median(datvec), decimal + 1), nsmall = decimal + 1),
      Min = format(tidytlg::roundSAS(min(datvec), decimal), nsmall = decimal),
      Max = format(tidytlg::roundSAS(max(datvec), decimal), nsmall = decimal)
    )
  } else {
    switch(statnm,
      N = length(stats::na.omit(datvec)),
      SE = format(round(stats::sd(datvec) / sqrt(length(stats::na.omit(datvec))), decimal + 2), nsmall = decimal + 2),
      SD = format(round(stats::sd(datvec), decimal + 2), nsmall = decimal + 2),
      Mean = format(round(mean(datvec), decimal + 1), nsmall = decimal + 1),
      mean_sd = paste0(
        format(round(mean(datvec), decimal + 1), nsmall = decimal + 1),
        " (",
        format(
          round(stats::sd(datvec), decimal + 2),
          nsmall = decimal +
            2
        ),
        ")"
      ),
      Med = format(round(stats::median(datvec), decimal + 1), nsmall = decimal + 1),
      Min = format(round(min(datvec), decimal), nsmall = decimal),
      Max = format(round(max(datvec), decimal), nsmall = decimal)
    )
  }
}

#' @name column_stats
#' @title Statistics within the column space
#' @description
#' A function factory used for obtaining statistics within the columns of your table.
#' Used in change from baseline tables. This takes the visit names as its row labels.
#' @param exclude_visits (`character vector`)\cr Vector of visit(s) for which you do not want the statistics displayed
#' in the baseline mean or change from baseline sections of the table.
#' @param var_names (`character vector`)\cr Vector of variable names to use instead of the default AVAL, CHG, BASE.
#' The first two elements are treated as main variables with full statistics, and the third element
#' is treated as the base variable. By default, the function expects these specific variable names in your data,
#' but you can customize them to match your dataset's column names.
#' @param stats (`list`)\cr A list with two components, `main` and `base`, that define the statistics to be calculated
#' for the main variables (default: AVAL, CHG) and the base variable (default: BASE).\cr
#' Default for main variables: c(N = "N", mean = "Mean", SD = "SD", SE = "SE",
#' Med = "Med", Min = "Min", Max = "Max").\cr
#' Default for base variable: c(mean = "Mean").\cr
#' You can customize these statistics by providing your own named vectors in the list. The names are used
#' internally for calculations, and the values are used as display labels in the table.
#'
#' @return An analysis function (for use with [rtables::analyze]) implementing
#'   the specified statistics.
#' @export
column_stats <- function(exclude_visits = c("Baseline (DB)"),
                         var_names = c("AVAL", "CHG", "BASE"),
                         stats = list(
                           main = c(
                             N = "N", mean = "Mean", SD = "SD", SE = "SE",
                             Med = "Med", Min = "Min", Max = "Max"
                           ),
                           base = c(mean = "Mean")
                         )) {
  function(df, .var, .spl_context) {
    allcolsplvals <- .spl_context[nrow(.spl_context), "cur_col_split_val"][[1]]
    statnm <- utils::tail(allcolsplvals, 1)
    varnm <- allcolsplvals[length(allcolsplvals) - 1]
    datvec <- df[[varnm]]
    decimalp <- utils::tail(df$dp, 1)
    datpervis <- split(datvec, df[[.var]]) ## ,var is AVISIT
    in_rows(
      .list = mapply(
        calc_one_visit,
        datvec = datpervis,
        decimal = decimalp,
        visit = names(datpervis),
        MoreArgs = list(
          statnm = statnm,
          varnm = varnm,
          exclude_visits = exclude_visits,
          var_names = var_names
        )
      ),
      .names = names(datpervis)
    )
  }
}

calc_N <- function(datvec, statnm, trt, varnm) {
  if (varnm != "AVAL") {
    return(NULL)
  }
  length(stats::na.omit(datvec))
}
