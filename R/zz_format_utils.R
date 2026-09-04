#' @keywords internal
formatter_references <- list(
  "jjcsformat_xx" =
    jjcsformat_xx("xx.xxxxxxxxx"),
  "jjcsformat_fraction" =
    jjcsformat_cnt_den_fract_fct(type = "count_fraction"),
  "jjcsformat_pval" =
    jjcsformat_pval_fct(alpha = 0),
  "jjcsformat_range" =
    jjcsformat_range_fct("xx.xx"),
  "format_xx" =
    format_xx("xx"),
  "format_extreme_values" =
    format_extreme_values(2L),
  "format_extreme_values_ci" =
    format_extreme_values_ci(2L),
  "format_count_fraction_fixed_dp" =
    format_count_fraction_fixed_dp,
  "format_count_fraction" =
    format_count_fraction,
  "format_fraction_fixed_dp" =
    format_fraction_fixed_dp
)

#' @keywords internal
body_matches <- function(fmtfun, reference_fun) {
  identical(deparse(body(fmtfun)), deparse(body(reference_fun)))
}

#' @keywords internal
get_input_jjcs_fmtfun <- function(fmtfun) {
  if (length(fmtfun) != 1) {
    stop("fmtfun should be of length 1")
  }

  if (is.list(fmtfun)) {
    fmtfun <- fmtfun[[1]]
  }

  result <- list(
    fun_fact = "",
    str = "",
    str_formatters = NA,
    type = "",
    alpha = "",
    message = "",
    fmt_package = "",
    is_fun = FALSE,
    fun = fmtfun,
    round_type = FALSE
  )

  valid_fmt_str <- unlist(list_valid_format_labels(), use.names = FALSE)

  # Handle character inputs separately.
  if (is.character(fmtfun)) {
    if (fmtfun %in% valid_fmt_str) {
      result$fun_fact <- ""
      result$str <- fmtfun
      result$str_formatters <- TRUE
      result$fmt_package <- "formatters"
      result$round_type <- TRUE
    } else {
      result$fun_fact <- "string"
      result$str <- fmtfun
      result$str_formatters <- FALSE
      result$message <- paste(
        "fmtfun is a string but not a supported formatting value"
      )
    }

    return(result)
  }

  if (!is.function(fmtfun)) {
    result$message <- paste(
      "fmtfun is not a function/character"
    )

    message(result$message)

    return(result)
  }

  # from here on fmtfun is a function
  x_body_matches <- sapply(formatter_references, body_matches, fmtfun = fmtfun)
  formatter_key <- if (any(x_body_matches)) {
    names(formatter_references)[which(x_body_matches)[1L]]
  } else {
    "unknown"
  }

  result$is_fun <- TRUE
  if (formatter_key != "unknown") {
    result$round_type <- "round_type" %in% names(formals(fmtfun))
  }
  switch(formatter_key,
    "jjcsformat_xx" = {
      result$fun_fact <- "jjcsformat_xx"
      result$str <- get0(
        "str",
        envir = environment(fmtfun),
        ifnotfound = ""
      )
      result$str_formatters <- FALSE
      result$fmt_package <- "junco"
    },
    "jjcsformat_fraction" = {
      result$fun_fact <- "jjcsformat_fraction"
      result$type <- get0(
        "type",
        envir = environment(fmtfun),
        ifnotfound = ""
      )
      result$fmt_package <- "junco"
    },
    "jjcsformat_pval" = {
      result$fun_fact <- "jjcsformat_pval"
      result$alpha <- as.character(
        get0(
          "alpha",
          envir = environment(fmtfun),
          ifnotfound = ""
        )
      )
      result$fmt_package <- "junco"
    },
    "jjcsformat_range" = {
      result$fun_fact <- "jjcsformat_range"
      result$str <- as.character(
        get0(
          "str",
          envir = environment(fmtfun),
          ifnotfound = ""
        )
      )
      result$fmt_package <- "junco"
    },
    "format_xx" = {
      result$fun_fact <- "format_xx"
      result$str <- as.character(
        get0(
          "str",
          envir = environment(fmtfun),
          ifnotfound = ""
        )
      )
      result$fmt_package <- "tern"
    },
    "format_extreme_values" = {
      result$fun_fact <- "format_extreme_values"
      result$str <- "???"
      result$fmt_package <- "tern"
    },
    "format_extreme_values_ci" = {
      result$fun_fact <- "format_extreme_values_ci"
      result$str <- "???"
      result$fmt_package <- "tern"
    },
    "format_count_fraction_fixed_dp" = {
      result$fun_fact <- "format_count_fraction_fixed_dp"
      result$fmt_package <- "tern"
    },
    "format_count_fraction" = {
      result$fun_fact <- "format_count_fraction"
      result$fmt_package <- "tern"
    },
    "format_fraction_fixed_dp" = {
      result$fun_fact <- "format_fraction_fixed_dp"
      result$fmt_package <- "tern"
    },
    "unknown" = {
      result$fun_fact <- "???"
      result$fmt_package <- "???"
      result$message <- paste(
        "fmtfun is an unknown formatting function not defined",
        "in formatters/tern/junco"
      )
    }
  )

  result
}


#' @name fmt_utils
#'
#' @title Utilities for reviewing format specification objects
#'
#' @details fmt_utils general details
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param myfmts A named list of either formatting specifications (ie stats with its associated formats)
#'  or a list of variables with formatting specifications.
#'
#' @param recursive `logical(1)`
#' Set to `TRUE` when requesting details from named list of variables with specifications.
#' Set to `FALSE` when requesting details from named list of statistics with format specifications.
#'
#' @param as_tibble `logical(1)` When  `TRUE`, convert the information into a tibble for ease of reviewing.
#'
#'
#' @param fmt1 A named list of formatting specifications (ie stats with its associated format)
#' @param fmt2 Another named list of formatting specifications (ie stats with its associated format),
#'  to compare against `fmt1`
NULL

#' @describeIn fmt_utils A utility function to collect information on formatting specification in list or tibble format.
#'
#' The following information is collected:
#' - var (variable name): Only applicable if the format specification is a named list with formattings specs
#' (eg list("AGE" = c("mean" = "xx", "se" = "xx.x"), "BMI" = c("mean" = "xx.x", "se" = "xx.xx"))), eg processed via
#' `fmt_spec_var_d` function.
#' - stat (name of statistic)
#' - fun_fact : name of function factory that has produced the format specification
#'  (if spec is from formatting function, blank if spec is string).
#'
#'  Only the following known formatting functions are supported:
#'    - jjcsformat_xx
#'    - jjcsformat_cnt_den_fract_fct,
#'    - jjcsformat_pval_fct,
#'    - jjcsformat_range_fct,
#'    - tern::format_xx,
#'    - tern::format_extreme_values,
#'    - tern::format_extreme_values_ci,
#'    - tern::format_count_fraction_fixed_dp,
#'    - tern::format_count_fraction,
#'    - tern::format_fraction_fixed_dp
#'  - str: string from specification, or str from formatting function factory (`jjcsformat_xx`, ....)
#'  - str_formatters: (`logical`) Is the str value a valid format string as in ([list_valid_format_labels()])?
#'  - type: when formatting function factory `jjcsformat_cnt_den_fract_fct` is used
#'  - alpha: when formatting function factory `jjcsformat_pval_fct` is used
#'  - message: Message when formatting function is unkown, or input string was
#'    not valid format string as in ([list_valid_format_labels()])
#'  - fmt_package: In which package was the formatting specification defined.
#'  - is_fun: (`logical`) Is the formatting specification a function?
#'  - round_type: (`logical`) Does the formatting support `round_type` argument/behavior?
#'  - fun: Formatting string or body of the formatting function.
#' @examples
#' # Example for get_fmt_details --
#' junco_def_d <- c(
#'   "mean" = "xx.dx",
#'   "mean_sd" = "xx.dx (xx.dxx)",
#'   "range" = "(xx.d, xx.d)"
#' )
#'
#' myfmts <- fmt_spec_single_d(
#'   d = 1,
#'   stats_in = c("mean", "mean_sd"),
#'   fmt_d_def = junco_def_d,
#'   fmt_d_in = NULL
#' )
#'
#' fmt_details <- get_fmt_details(myfmts, as_tibble = TRUE)
#'
#' myfmts2 <- fmt_spec_single_d(
#'   d = 3,
#'   stats_in = c("mean", "mean_sd"),
#'   fmt_d_def = junco_def_d,
#'   fmt_d_in = NULL
#' )
#' fmt_details2 <- get_fmt_details(myfmts2, as_tibble = TRUE)
#' fmt_details2
#'
#' # Example for get_fmt_details using format specs for variables --
#' # processed with fmt_spec_var_d
#' var <- c("AGE" = 0, "BMI" = 1, "BMRKR1" = 2)
#' vars_fmt <- fmt_spec_var_d(var,
#'   stats_in = NULL,
#'   fmt_d_def = junco_def_d,
#'   fmt_d_in = NULL
#' )
#' fmt_details2 <- get_fmt_details(vars_fmt, as_tibble = TRUE, recursive = TRUE)
#'
#' @export
get_fmt_details <- function(myfmts, recursive = FALSE, as_tibble = TRUE) {
  lst <- if (recursive) {
    lapply(myfmts, get_fmt_details, as_tibble = as_tibble)
  } else {
    lapply(myfmts, get_input_jjcs_fmtfun)
  }

  names(lst) <- names(myfmts)

  if (!recursive && as_tibble) {
    df <- do.call(
      rbind,
      lapply(names(lst), function(nm) {
        xx <- lst[[nm]]
        xx$fun <- NULL
        data.frame(
          stat = nm,
          as.data.frame(xx, stringsAsFactors = FALSE)
        )
      })
    )
    df <- dplyr::as_tibble(df)

    df$fun <- lapply(lst, `[[`, "fun")
    lst <- df
  }

  if (recursive && as_tibble) {
    lst <- dplyr::bind_rows(lst, .id = "var")
  }

  lst
}


#' @noRd
#' @param fmt_t1 Tibble information from specification 1
#' @param fmt_t2 Tibble information from specification 2
#' @keywords internal
compare_fmt_tibble <- function(fmt_t1, fmt_t2) {
  nms <- names(fmt_t1)
  nms <- nms[nms != "fun"]
  fmt_t1_x <- fmt_t1[, nms]
  fmt_t2_x <- fmt_t2[, nms]
  c1 <- identical(fmt_t1_x, fmt_t2_x)

  fmt_t1_y <- fmt_t1[["fun"]][[1]]
  fmt_t2_y <- fmt_t2[["fun"]][[1]]
  if (is.function(fmt_t1_y) && is.function(fmt_t2_y)) {
    c2 <- body_matches(fmt_t1_y, fmt_t2_y)
  } else if (is.character(fmt_t1_y) && is.character(fmt_t2_y)) {
    c2 <- identical(fmt_t1_y, fmt_t2_y)
  } else {
    c2 <- FALSE
  }

  rslt <- c1 & c2
}

#' @describeIn fmt_utils A utility to report findings in differences between 2 formatting specification vectors.
#' It returns a list object with the following information:
#' - spec1 : For statistics with a different specification, tibble with specification details from `fmt1`.
#' - spec2 : For statistics with a different specification, tibble with specification details from `fmt2`.
#' The tibbles in `spec1` and `spec2` contain information as described in `get_fmt_details()`.
#' - msg: Message on how many stats have a difference in specification.
#' - comm_stats: Vector of stats in common between `fmt1` and `fmt2`.
#' - diff_stats: Vector of stats with any difference identified.
#' @examples
#'
#' # Example for compare_fmt_specs --
#' test1_target <- junco_default_formats
#' # junco_def_d_all has been constructed with d = 1 as reference
#' # compare against junco_default_formats
#' test1_d <- fmt_spec_single_d(
#'   d = 1,
#'   stats_in = NULL,
#'   fmt_d_def = junco_def_d_all,
#'   fmt_d_in = NULL
#' )
#'
#' check1 <- compare_fmt_specs(test1_d, test1_target)
#' check1
#'
#' # more differences when using other d
#' test2_d <- fmt_spec_single_d(
#'   d = 2,
#'   stats_in = NULL,
#'   fmt_d_def = junco_def_d_all,
#'   fmt_d_in = NULL
#' )
#'
#' check2 <- compare_fmt_specs(test2_d, test1_target)
#'
#' @export
#'
compare_fmt_specs <- function(fmt1, fmt2) {
  result <- list(
    spec1 = NULL,
    spec2 = NULL,
    msg = NULL,
    comm_stats = NULL,
    diff_stats = NULL
  )
  stats_diff <- NULL

  stats1 <- names(fmt1)
  stats2 <- names(fmt2)
  stats <- intersect(stats1, stats2)
  if (length(stats) == 0) {
    result$msg <- "No stats in common"
    result$comm_stats <- NULL
  } else {
    result$comm_stats <- stats
    fmt1 <- fmt1[stats]
    fmt2 <- fmt2[stats]

    check <- sapply(names(fmt1), \(stat) {
      fmt_t1 <- get_fmt_details(list(stat = fmt1[[stat]]))
      fmt_t2 <- get_fmt_details(list(stat = fmt2[[stat]]))
      fmt_t1[["stat"]] <- stat
      fmt_t2[["stat"]] <- stat

      compare_fmt_tibble(fmt_t1, fmt_t2)
    })

    if (sum(!check) == 0) {
      result$msg <- "All common stats specs identical"
    } else {
      stats_diff <- check[!check]
      stats_diff <- names(stats_diff)
      result$spec1 <- get_fmt_details(fmt1[stats_diff])
      result$spec2 <- get_fmt_details(fmt2[stats_diff])
      result$msg <- paste0(length(stats_diff), " common stats with differences in specs")
      result$diff_stats <- stats_diff
    }
  }

  if (!is.null(result$msg)) {
    message(result$msg)
    message("More details are available in created object.")
  }
  return(result)
}
