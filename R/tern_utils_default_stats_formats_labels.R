#' Get default statistical methods and their associated formats, labels, and indent modifiers
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @note These functions have been copied from the `tern` package file
#'   `utils_default_stats_formats_labels.R` from GitHub development version 0.9.7.9017.
#'   Slight modifications have been applied to enhance functionality:
#'
#'   * `tern_get_stats` added the `tern_stats` argument to avoid hardcoding within the function's body.
#'   * `tern_get_labels_from_stats` is more careful when receiving partial `labels_in`
#'      and partial `label_attr_from_stats`.
#'
#'   Once these features are included in the `tern` package, this file could be removed from
#'   the `junco` package, and the functions could be used from the `tern` namespace directly.
#'
#' @name tern_default_stats_formats_labels
NULL


# Utility function used to separate custom stats (user-defined functions) from defaults
.split_std_from_custom_stats <- function(stats_in) {
  out <- list(default_stats = NULL, custom_stats = NULL, all_stats = NULL)
  if (is.list(stats_in)) {
    is_custom_fnc <- sapply(stats_in, is.function)
    checkmate::assert_list(
      stats_in[is_custom_fnc],
      types = "function",
      names = "named"
    )
    out[["custom_stats"]] <- stats_in[is_custom_fnc]
    out[["default_stats"]] <- unlist(stats_in[!is_custom_fnc])
    all_stats <- names(stats_in) # to keep the order
    all_stats[!is_custom_fnc] <- out[["default_stats"]]
    out[["all_stats"]] <- all_stats
  } else {
    out[["default_stats"]] <- out[["all_stats"]] <- stats_in
  }
  out
}

# Utility function to apply statistical functions
.apply_stat_functions <- function(
  default_stat_fnc,
  custom_stat_fnc_list,
  args_list
) {
  # Default checks
  checkmate::assert_function(default_stat_fnc)
  checkmate::assert_list(
    custom_stat_fnc_list,
    types = "function",
    null.ok = TRUE,
    names = "named"
  )
  checkmate::assert_list(args_list)

  # Checking custom stats have same formals
  if (!is.null(custom_stat_fnc_list)) {
    fundamental_call_to_data <- names(formals(default_stat_fnc))[[1]]
    for (fnc in custom_stat_fnc_list) {
      if (!identical(names(formals(fnc))[[1]], fundamental_call_to_data)) {
        stop(
          "The first parameter of a custom statistical function needs to be the same (it can be `df` or `x`) ",
          "as the default statistical function. In this case your custom function has ",
          names(formals(fnc))[[1]],
          " as first parameter, while the default function has ",
          fundamental_call_to_data,
          "."
        )
      }
      if (!any(names(formals(fnc)) == "...")) {
        stop(
          "The custom statistical function needs to have `...` as a parameter to accept additional arguments. ",
          "In this case your custom function does not have `...`."
        )
      }
    }
  }

  # Applying
  out_default <- do.call(default_stat_fnc, args = args_list)
  out_custom <- lapply(
    custom_stat_fnc_list,
    function(fnc) do.call(fnc, args = args_list)
  )

  # Merging
  c(out_default, out_custom)
}

#' @describeIn tern_default_stats_formats_labels Get statistics available for a given method
#'   group (analyze function).
#' @keywords internal
tern_get_stats <- function(
  method_groups = "analyze_vars_numeric",
  stats_in = NULL,
  custom_stats_in = NULL,
  add_pval = FALSE,
  tern_defaults = tern_default_stats
) {
  checkmate::assert_character(method_groups)
  checkmate::assert_character(stats_in, null.ok = TRUE)
  checkmate::assert_character(custom_stats_in, null.ok = TRUE)
  checkmate::assert_flag(add_pval)

  # Default is still numeric
  if (any(method_groups == "analyze_vars")) {
    method_groups[method_groups == "analyze_vars"] <- "analyze_vars_numeric"
  }

  type_tmp <- ifelse(any(grepl("counts$", method_groups)), "counts", "numeric") # for pval checks

  # Defaults for loop
  out <- NULL

  # Loop for multiple method groups
  for (mgi in method_groups) {
    if (mgi %in% names(tern_defaults)) {
      out_tmp <- tern_defaults[[mgi]]
    } else {
      stop(
        "The selected method group (",
        mgi,
        ") has no default statistical method."
      )
    }
    out <- unique(c(out, out_tmp))
  }

  # Add custom stats
  out <- c(out, custom_stats_in)

  # If you added pval to the stats_in you certainly want it
  if (!is.null(stats_in) && any(grepl("^pval", stats_in))) {
    stats_in_pval_value <- stats_in[grepl("^pval", stats_in)]

    # Must be only one value between choices
    checkmate::assert_choice(
      stats_in_pval_value,
      c("pval", "pval_counts", "pvalue")
    )

    # Mismatch with counts and numeric
    if (
      any(grepl("counts", method_groups)) &&
        stats_in_pval_value != "pval_counts" ||
        any(grepl("numeric", method_groups)) && stats_in_pval_value != "pval"
    ) {
      # nolint
      stop(
        "Inserted p-value (",
        stats_in_pval_value,
        ") is not valid for type ",
        type_tmp,
        ". Use ",
        paste(ifelse(stats_in_pval_value == "pval", "pval_counts", "pval")),
        " instead."
      )
    }

    # Lets add it even if present (thanks to unique)
    add_pval <- TRUE
  }

  # Mainly used in "analyze_vars" but it could be necessary elsewhere
  if (isTRUE(add_pval)) {
    if (any(grepl("counts", method_groups))) {
      out <- unique(c(out, "pval_counts"))
    } else {
      out <- unique(c(out, "pval"))
    }
  }

  # Filtering for stats_in (character vector)
  if (!is.null(stats_in)) {
    out <- intersect(stats_in, out) # It orders them too
  }

  # If intersect did not find matches (and no pval?) -> error
  if (length(out) == 0) {
    stop(
      "The selected method group(s) (",
      paste0(method_groups, collapse = ", "),
      ")",
      " do not have the required default statistical methods:\n",
      paste0(stats_in, collapse = " ")
    )
  }

  out
}

#' @describeIn tern_default_stats_formats_labels Get formats corresponding to a list of statistics.
#' @keywords internal
tern_get_formats_from_stats <- function(
  stats,
  formats_in = NULL,
  levels_per_stats = NULL,
  tern_defaults = tern_default_formats
) {
  checkmate::assert_character(stats, min.len = 1)
  # It may be a list if there is a function in the formats
  if (checkmate::test_list(formats_in, null.ok = TRUE)) {
    checkmate::assert_list(formats_in, null.ok = TRUE)
    # Or it may be a vector of characters
  } else {
    checkmate::assert_character(formats_in, null.ok = TRUE)
  }
  checkmate::assert_list(levels_per_stats, null.ok = TRUE)

  # If unnamed formats given as formats_in and same number of stats, use one format per stat
  if (
    !is.null(formats_in) &&
      length(formats_in) == length(stats) &&
      is.null(names(formats_in)) &&
      is.null(levels_per_stats)
  ) {
    out <- as.list(formats_in) |> stats::setNames(stats)
    return(out)
  }

  # If levels_per_stats not given, assume one row per statistic
  if (is.null(levels_per_stats)) levels_per_stats <- as.list(stats) |> stats::setNames(stats)

  # Apply custom formats
  out <- .fill_in_vals_by_stats(levels_per_stats, formats_in, tern_defaults)

  # Default to NULL if no format
  case_input_is_not_stat <- unlist(out, use.names = FALSE) == unlist(levels_per_stats, use.names = FALSE)
  out[names(out) == out | case_input_is_not_stat] <- list(NULL)

  out
}

#' @describeIn tern_default_stats_formats_labels Get labels corresponding to a list of statistics.
#' @keywords internal
tern_get_labels_from_stats <- function(
  stats,
  labels_in = NULL,
  levels_per_stats = NULL,
  label_attr_from_stats = NULL,
  tern_defaults = tern_default_labels
) {
  checkmate::assert_character(stats, min.len = 1)

  # Modification:
  # If any label_attr_from_stats is available and valid, save in labels_in
  # if not specified there already (so don't overwrite labels_in).
  if (!is.null(label_attr_from_stats)) {
    valid_label_attr_from_stats <- label_attr_from_stats[
      nzchar(label_attr_from_stats) &
        !sapply(label_attr_from_stats, is.null) &
        !is.na(label_attr_from_stats)
    ]
    if (length(valid_label_attr_from_stats)) {
      do_save <- setdiff(
        names(valid_label_attr_from_stats),
        names(labels_in)
      )
      labels_in <- c(labels_in, valid_label_attr_from_stats[do_save])
    }
  }

  # It may be a list
  if (checkmate::test_list(labels_in, null.ok = TRUE)) {
    checkmate::assert_list(labels_in, null.ok = TRUE)
    # Or it may be a vector of characters
  } else {
    checkmate::assert_character(labels_in, null.ok = TRUE)
  }
  checkmate::assert_list(levels_per_stats, null.ok = TRUE)

  # If unnamed labels given as labels_in and same number of stats, use one label per stat
  if (
    !is.null(labels_in) &&
      length(labels_in) == length(stats) &&
      is.null(names(labels_in)) &&
      is.null(levels_per_stats)
  ) {
    out <- as.list(labels_in) |> stats::setNames(stats)
    return(out)
  }

  # If levels_per_stats not given, assume one row per statistic
  if (is.null(levels_per_stats)) levels_per_stats <- as.list(stats) |> stats::setNames(stats)

  # Apply custom labels
  out <- .fill_in_vals_by_stats(levels_per_stats, labels_in, tern_defaults)
  out
}

# Function to loop over each stat and levels to set correct values
.fill_in_vals_by_stats <- function(levels_per_stats, user_in, tern_defaults) {
  out <- list()

  for (stat_i in names(levels_per_stats)) {
    # Get all levels of the statistic
    all_lvls <- levels_per_stats[[stat_i]]

    if ((length(all_lvls) == 1 && all_lvls == stat_i) || is.null(all_lvls)) {
      # One row per statistic
      out[[stat_i]] <- if (stat_i %in% names(user_in)) {
        # 1. Check for stat_i in user input
        user_in[[stat_i]]
      } else if (stat_i %in% names(tern_defaults)) {
        # 2. Check for stat_i in tern defaults
        tern_defaults[[stat_i]]
      } else {
        # 3. Otherwise stat_i
        stat_i
      }
    } else {
      # One row per combination of variable level and statistic
      # Loop over levels for each statistic
      for (lev_i in all_lvls) {
        # Construct row name (stat_i.lev_i)
        row_nm <- paste(stat_i, lev_i, sep = ".")

        out[[row_nm]] <- if (row_nm %in% names(user_in)) {
          # 1. Check for stat_i.lev_i in user input
          user_in[[row_nm]]
        } else if (lev_i %in% names(user_in)) {
          # 2. Check for lev_i in user input
          user_in[[lev_i]]
        } else if (stat_i %in% names(user_in)) {
          # 3. Check for stat_i in user input
          user_in[[stat_i]]
        } else if (lev_i %in% names(tern_defaults)) {
          # 4. Check for lev_i in tern defaults (only used for labels)
          tern_defaults[[lev_i]]
        } else if (stat_i %in% names(tern_defaults)) {
          # 5. Check for stat_i in tern defaults
          tern_defaults[[stat_i]]
        } else {
          # 6. Otherwise lev_i
          lev_i
        }
      }
    }
  }

  out
}

#' @describeIn tern_default_stats_formats_labels Get row indent modifiers corresponding to a list of statistics/rows.
#' @keywords internal
tern_get_indents_from_stats <- function(stats,
                                        indents_in = NULL,
                                        levels_per_stats = NULL,
                                        tern_defaults = as.list(rep(0L, length(stats))) |> stats::setNames(stats)) {
  checkmate::assert_character(stats, min.len = 1)
  # It may be a list
  if (checkmate::test_list(indents_in, null.ok = TRUE)) {
    checkmate::assert_list(indents_in, null.ok = TRUE)
    # Or it may be a vector of integers
  } else {
    checkmate::assert_integerish(indents_in, null.ok = TRUE)
  }
  checkmate::assert_list(levels_per_stats, null.ok = TRUE)

  # If levels_per_stats not given, assume one row per statistic
  if (is.null(levels_per_stats)) levels_per_stats <- as.list(stats) |> stats::setNames(stats)

  # Single indentation level for all rows
  if (is.null(names(indents_in)) && length(indents_in) == 1) {
    out <- rep(indents_in, length(levels_per_stats |> unlist()))
    return(out)
  }

  # Apply custom indentation
  out <- .fill_in_vals_by_stats(levels_per_stats, indents_in, tern_defaults)
  out
}

# tern_default_labels ----

#' @describeIn tern_default_stats_formats_labels Named `character` vector of default labels for `tern`.
#'   This is only copied here from the latest GitHub version, because otherwise a tern test fails.
#'
#' @keywords internal
tern_default_labels <- c(
  cv = "CV (%)",
  iqr = "IQR",
  geom_cv = "CV % Geometric Mean",
  geom_mean = "Geometric Mean",
  geom_mean_sd = "Geometric Mean (SD)",
  geom_mean_ci = "Geometric Mean 95% CI",
  geom_mean_ci_3d = "Geometric Mean (95% CI)",
  geom_sd = "Geometric SD",
  mad = "Median Absolute Deviation",
  max = "Maximum",
  mean = "Mean",
  mean_ci = "Mean 95% CI",
  mean_ci_3d = "Mean (95% CI)",
  mean_pval = "Mean p-value (H0: mean = 0)",
  mean_sd = "Mean (SD)",
  mean_sdi = "Mean -/+ 1xSD",
  mean_se = "Mean (SE)",
  mean_sei = "Mean -/+ 1xSE",
  median = "Median",
  median_ci = "Median 95% CI",
  median_ci_3d = "Median (95% CI)",
  median_range = "Median (Min - Max)",
  min = "Minimum",
  n = "n",
  n_blq = "n_blq",
  nonunique = "Number of events",
  pval = "p-value (t-test)", # Default for numeric
  pval_counts = "p-value (chi-squared test)", # Default for counts
  quantiles = "25% and 75%-ile",
  quantiles_lower = "25%-ile (95% CI)",
  quantiles_upper = "75%-ile (95% CI)",
  range = "Min - Max",
  range_censor = "Range (censored)",
  range_event = "Range (event)",
  rate = "Adjusted Rate",
  rate_ratio = "Adjusted Rate Ratio",
  sd = "SD",
  se = "SE",
  sum = "Sum",
  unique = "Number of patients with at least one event"
)
