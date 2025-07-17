#' @name create_colspan_var
#' @title Creation of Column Spanning Variables
#' @description
#' A function used for creating a spanning variable for treatment groups
#' @details
#' This function creates a spanning variable for treatment groups that is intended to
#' be used within the column space.
#' @param df The name of the data frame in which the spanning variable is to be appended to
#' @param non_active_grp The value(s) of the treatments that represent the non-active or comparator
#' treatment groups
#' default value = c('Placebo')
#' @param non_active_grp_span_lbl The assigned value of the spanning variable for the non-active or comparator
#' treatment groups
#' default value = ''
#' @param active_grp_span_lbl The assigned value of the spanning variable for the active treatment group(s)
#' default value = 'Active Study Agent'
#' @param colspan_var The desired name of the newly created spanning variable
#' default value = 'colspan_trt'
#' @param trt_var The name of the treatment variable that is used to determine which
#' spanning treatment group value to apply.
#' default value = 'TRT01A'
#' @returns a data frame that contains the new variable as specified in colspan_var
#' @rdname colspan_var
#' @export
#' @aliases create_colspan_var
#' @examples
#'
#' library(tibble)
#'
#' df <- tribble(
#'   ~TRT01A,
#'   "Placebo",
#'   "Active 1",
#'   "Active 2"
#' )
#'
#' df$TRT01A <- factor(df$TRT01A, levels = c("Placebo", "Active 1", "Active 2"))
#'
#' colspan_var <- create_colspan_var(
#'   df = df,
#'   non_active_grp = c("Placebo"),
#'   non_active_grp_span_lbl = " ",
#'   active_grp_span_lbl = "Active Treatment",
#'   colspan_var = "colspan_trt",
#'   trt_var = "TRT01A"
#' )
#'
#' colspan_var
create_colspan_var <- function(
    df,
    non_active_grp = c("Placebo"),
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = "TRT01A") {
  # Create a new column with the specified name using base R
  df[[colspan_var]] <- factor(
    ifelse(df[[trt_var]] %in% non_active_grp, non_active_grp_span_lbl, active_grp_span_lbl),
    levels = c(active_grp_span_lbl, non_active_grp_span_lbl)
  )

  return(df)
}

#' @name create_colspan_map
#' @title Creation of Column Spanning Mapping Dataframe
#' @description
#' A function used for creating a data frame containing the map that is compatible with rtables split function
#' `trim_levels_to_map`
#' @details
#' This function creates a data frame containing the map that is compatible with rtables split function
#' `trim_levels_to_map`.
#' The levels of the specified trt_var variable will be stored within the trt_var variable
#' and the colspan_var variable will contain the corresponding spanning header value for each treatment group.
#' @inheritParams create_colspan_var
#' @param active_first whether the active columns come first.
#' @returns a data frame that contains the map to be used with rtables split function `trim_levels_to_map`
#' @rdname colspan_map
#' @export
#' @examples
#' library(tibble)
#'
#' df <- tribble(
#'   ~TRT01A,
#'   "Placebo",
#'   "Active 1",
#'   "Active 2"
#' )
#'
#' df$TRT01A <- factor(df$TRT01A, levels = c("Placebo", "Active 1", "Active 2"))
#'
#' colspan_map <- create_colspan_map(
#'   df = df,
#'   non_active_grp = c("Placebo"),
#'   non_active_grp_span_lbl = " ",
#'   active_grp_span_lbl = "Active Study Agent",
#'   colspan_var = "colspan_trt",
#'   trt_var = "TRT01A"
#' )
#'
#' colspan_map
create_colspan_map <- function(
    df,
    non_active_grp = c("Placebo"),
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = "TRT01A",
    active_first = TRUE) {
  act_trtlv <- setdiff(levels(df[[trt_var]]), non_active_grp)
  act_map_df <- data.frame(a = active_grp_span_lbl, b = act_trtlv)
  nact_map_df <- data.frame(a = non_active_grp_span_lbl, b = non_active_grp)
  df <- if (active_first) {
    rbind(act_map_df, nact_map_df)
  } else {
    rbind(nact_map_df, act_map_df)
  }
  names(df) <- c(colspan_var, trt_var)

  return(df)
}
