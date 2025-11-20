#' Helper for Finding AVISIT after which CHG are all Missing
#'
#' @param df (`data.frame`)\cr with `CHG` and `AVISIT` variables.
#'
#' @return A string with either the factor level after which `AVISIT` is all missing,
#'   or `NA`.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   AVISIT = factor(c(1, 2, 3, 4, 5)),
#'   CHG = c(5, NA, NA, NA, 3)
#' )
#' find_missing_chg_after_avisit(df)
#'
#' df2 <- data.frame(
#'   AVISIT = factor(c(1, 2, 3, 4, 5)),
#'   CHG = c(5, NA, 3, NA, NA)
#' )
#' find_missing_chg_after_avisit(df2)
#'
#' df3 <- data.frame(
#'   AVISIT = factor(c(1, 2, 3, 4, 5)),
#'   CHG = c(NA, NA, NA, NA, NA)
#' )
#' find_missing_chg_after_avisit(df3)
find_missing_chg_after_avisit <- function(df) {
  checkmate::assert_data_frame(df)
  checkmate::assert_factor(df$AVISIT, unique = TRUE, any.missing = FALSE)
  checkmate::assert_numeric(df$CHG)

  # Ensure the dataframe is sorted by AVISIT
  df <- df[order(df$AVISIT), ]

  # Last visit with available data.
  visit_levels_available <- as.integer(df[!is.na(df$CHG), ]$AVISIT)

  if (!length(visit_levels_available)) {
    return(levels(df$AVISIT)[1])
  }
  visit_levels_max_available <- max(visit_levels_available)

  # Visits with missing data.
  visit_levels_missing <- as.integer(df[is.na(df$CHG), ]$AVISIT)

  # Missing visits at the end.
  visit_levels_missing_end <- visit_levels_missing[visit_levels_missing > visit_levels_max_available]

  # Return first one if there is any.
  if (length(visit_levels_missing_end)) {
    levels(df$AVISIT)[min(visit_levels_missing_end)]
  } else {
    NA_character_
  }
}

# Copy from rbmi package, because this was only added recently, so not yet available in our package version.

#' Create a `rbmi` ready cluster
#'
#' @param cluster_or_cores Number of parallel processes to use or an existing cluster to make use of
#' @param objects a named list of objects to export into the sub-processes
#' @param packages a character vector of libraries to load in the sub-processes
#'
#' This function is a wrapper around `parallel::makePSOCKcluster()` but takes
#' care of configuring `rbmi` to be used in the sub-processes as well as loading
#' user defined objects and libraries and setting the seed for reproducibility.
#'
#' @return If `cluster_or_cores` is `1` this function will return `NULL`. If `cluster_or_cores`
#'   is a number greater than `1`, a cluster with `cluster_or_cores`  cores is returned.
#'
#' If `cluster_or_cores` is a cluster created via `parallel::makeCluster()` then this function
#' returns it after inserting the relevant `rbmi` objects into the existing cluster.
#'
#' @examples
#' \dontrun{
#' make_rbmi_cluster(5)
#' closeAllConnections()
#'
#' VALUE <- 5
#' myfun <- function(x) {
#'   x + day(VALUE)
#' }
#' make_rbmi_cluster(5, list(VALUE = VALUE, myfun = myfun), c("lubridate"))
#' closeAllConnections()
#'
#' cl <- parallel::makeCluster(5)
#' make_rbmi_cluster(cl)
#' closeAllConnections()
#' }
#' @export
make_rbmi_cluster <- function(cluster_or_cores = 1, objects = NULL, packages = NULL) {
  if (!requireNamespace("rbmi", quietly = TRUE)) {
    stop("The 'rbmi' package is needed for this function to work. Please install it.", call. = FALSE)
  }

  if (is.numeric(cluster_or_cores) && cluster_or_cores == 1) {
    return(NULL)
  } else if (is.numeric(cluster_or_cores)) {
    cl <- parallel::makePSOCKcluster(cluster_or_cores)
  } else if (methods::is(cluster_or_cores, "cluster")) {
    cl <- cluster_or_cores
  } else {
    stop(sprintf("`cluster_or_cores` has unsupported class of: %s", paste(class(cluster_or_cores), collapse = ", ")))
  }

  # Load user defined objects into the globalname space
  if (!is.null(objects) && length(objects)) {
    export_env <- list2env(objects)
    parallel::clusterExport(cl, names(objects), export_env)
  }

  # Load user defined packages
  packages <- c(packages, "assertthat")
  # Remove attempts to load `rbmi` as this will be covered later
  packages <- setdiff(packages, "rbmi")
  devnull <- parallel::clusterCall(
    cl,
    function(pkgs) lapply(pkgs, function(x) library(x, character.only = TRUE)),
    as.list(packages)
  )

  # Ensure reproducibility
  parallel::clusterSetRNGStream(cl, sample.int(1))

  # If user has previously configured `rbmi` sub-processes then early exit
  exported_rbmi <- unlist(parallel::clusterEvalQ(cl, exists("..exported..parallel..rbmi")))
  if (all(exported_rbmi)) {
    return(cl)
  }

  # Ensure that exported and unexported objects are all directly accessible from the globalenv in the sub-processes
  is_in_rbmi_development <- FALSE
  if (is_in_rbmi_development) {
    devnull <- parallel::clusterEvalQ(cl, pkgload::load_all())
  } else {
    devnull <- parallel::clusterEvalQ(cl, {
      .namespace <- getNamespace("rbmi")
      for (.nsfun in ls(.namespace)) {
        assign(.nsfun, get(.nsfun, envir = .namespace))
      }
    })
  }

  # Set variable to signify `rbmi` has been configured
  devnull <- parallel::clusterEvalQ(cl, {
    ..exported..parallel..rbmi <- TRUE
  })

  return(cl)
}

#' Parallelise Lapply
#'
#' Simple wrapper around `lapply` and [`parallel::clusterApplyLB`] to abstract away
#' the logic of deciding which one to use
#' @param cl Cluster created by [`parallel::makeCluster()`] or `NULL`
#' @param fun Function to be run
#' @param x object to be looped over
#' @param ... extra arguments passed to `fun`
#' @return `list` of results of calling `fun` on elements of `x`.
par_lapply <- function(cl, fun, x, ...) {
  result <- if (is.null(cl)) {
    lapply(x, fun, ...)
  } else {
    parallel::clusterApplyLB(cl, x, fun, ...)
  }
  return(result)
}

#' Analyse Multiple Imputed Datasets
#'
#' @description
#' This function takes multiple imputed datasets (as generated by
#' the [rbmi::impute()] function) and runs an analysis function on
#' each of them.
#'
#' @importFrom assertthat assert_that
#'
#' @details
#' This function works by performing the following steps:
#'
#' 1. Extract a dataset from the `imputations` object.
#' 2. Apply any delta adjustments as specified by the `delta` argument.
#' 3. Run the analysis function `fun` on the dataset.
#' 4. Repeat steps 1-3 across all of the datasets inside the `imputations`
#' object.
#' 5. Collect and return all of the analysis results.
#'
#' The analysis function `fun` must take a `data.frame` as its first
#' argument. All other options to [rbmi_analyse()] are passed onto `fun`
#' via `...`.
#' `fun` must return a named list with each element itself being a
#' list containing a single
#' numeric element called `est` (or additionally `se` and `df` if
#' you had originally specified [rbmi::method_bayes()] or [rbmi::method_approxbayes()])
#' i.e.:
#' \preformatted{
#' myfun <- function(dat, ...) {
#'     mod_1 <- lm(data = dat, outcome ~ group)
#'     mod_2 <- lm(data = dat, outcome ~ group + covar)
#'     x <- list(
#'         trt_1 = list(
#'             est = coef(mod_1)[['group']],  # Use [[ ]] for safety
#'             se = sqrt(vcov(mod_1)['group', 'group']), # Use ['','']
#'             df = df.residual(mod_1)
#'         ),
#'         trt_2 = list(
#'             est = coef(mod_2)[['group']],  # Use [[ ]] for safety
#'             se = sqrt(vcov(mod_2)['group', 'group']), # Use ['','']
#'             df = df.residual(mod_2)
#'         )
#'      )
#'      return(x)
#'  }
#' }
#'
#' Please note that the `vars$subjid` column (as defined in the original call to
#' [rbmi::draws()]) will be scrambled in the data.frames that are provided to `fun`.
#' This is to say they will not contain the original subject values and as such
#' any hard coding of subject ids is strictly to be avoided.
#'
#' By default `fun` is the [rbmi_ancova()] function.
#' Please note that this function
#' requires that a `vars` object, as created by [rbmi::set_vars()], is provided via
#' the `vars` argument e.g. `rbmi_analyse(imputeObj, vars = rbmi::set_vars(...))`. Please
#' see the documentation for [rbmi_ancova()] for full details.
#' Please also note that the theoretical justification for the conditional mean imputation
#' method (`method = method_condmean()` in [rbmi::draws()]) relies on the fact that ANCOVA is
#' a linear transformation of the outcomes.
#' Thus care is required when applying alternative analysis functions in this setting.
#'
#' The `delta` argument can be used to specify offsets to be applied
#' to the outcome variable in the imputed datasets prior to the analysis.
#' This is typically used for sensitivity or tipping point analyses. The
#' delta dataset must contain columns `vars$subjid`, `vars$visit` (as specified
#' in the original call to [rbmi::draws()]) and `delta`. Essentially this `data.frame`
#' is merged onto the imputed dataset by `vars$subjid` and `vars$visit` and then
#' the outcome variable is modified by:
#'
#' ```
#' imputed_data[[vars$outcome]] <- imputed_data[[vars$outcome]] + imputed_data[['delta']]
#' ```
#'
#' Please note that in order to provide maximum flexibility, the `delta` argument
#' can be used to modify any/all outcome values including those that were not
#' imputed. Care must be taken when defining offsets. It is recommend that you
#' use the helper function [rbmi::delta_template()] to define the delta datasets as
#' this provides utility variables such as `is_missing` which can be used to identify
#' exactly which visits have been imputed.
#'
#' @seealso [rbmi::extract_imputed_dfs()] for manually extracting imputed
#' datasets.
#' @seealso [rbmi::delta_template()] for creating delta data.frames.
#' @seealso [rbmi_ancova()] for the default analysis function.
#'
#' @param imputations An `imputations` object as created by [rbmi::impute()].
#' @param fun An analysis function to be applied to each imputed dataset. See details.
#' @param delta A `data.frame` containing the delta transformation to be applied to the imputed
#' datasets prior to running `fun`. See details.
#' @param ... Additional arguments passed onto `fun`.
#' @param cluster_or_cores The number of parallel processes to use when running this function. Can also be a
#' cluster object created by [`make_rbmi_cluster()`]. See the parallelisation section below.
#' @param .validate Should `imputations` be checked to ensure it conforms to the required format
#' (default = `TRUE`) ? Can gain a small performance increase if this is set to `FALSE` when
#' analysing a large number of samples.
#'
#' @section Parallelisation:
#' To speed up the evaluation of `rbmi_analyse()` you can use the `cluster_or_cores` argument to enable parallelisation.
#' Simply providing an integer will get `rbmi` to automatically spawn that many background processes
#' to parallelise across. If you are using a custom analysis function then you need to ensure
#' that any libraries or global objects required by your function are available in the
#' sub-processes. To do this you need to use the [`make_rbmi_cluster()`] function for example:
#' ```
#' my_custom_fun <- function(...) <some analysis code>
#' cl <- make_rbmi_cluster(
#'     4,
#'     objects = list('my_custom_fun' = my_custom_fun),
#'     packages = c('dplyr', 'nlme')
#' )
#' rbmi_analyse(
#'     imputations = imputeObj,
#'     fun = my_custom_fun,
#'     cluster_or_cores = cl
#' )
#' parallel::stopCluster(cl)
#' ```
#'
#' Note that there is significant overhead both with setting up the sub-processes and with
#' transferring data back-and-forth between the main process and the sub-processes. As such
#' parallelisation of the `rbmi_analyse()` function tends to only be worth it when you have
#' `> 2000` samples generated by [rbmi::draws()]. Conversely using parallelisation if your samples
#' are smaller than this may lead to longer run times than just running it sequentially.
#'
#' It is important to note that the implementation of parallel processing within [rbmi::analyse()`] has
#' been optimised around the assumption that the parallel processes will be spawned on the same
#' machine and not a remote cluster. One such optimisation is that the required data is saved to
#' a temporary file on the local disk from which it is then read into each sub-process. This is
#' done to avoid the overhead of transferring the data over the network. Our assumption is that
#' if you are at the stage where you need to be parallelising your analysis over a remote cluster
#' then you would likely be better off parallelising across multiple `rbmi` runs rather than within
#' a single `rbmi` run.
#'
#' Finally, if you are doing a tipping point analysis you can get a reasonable performance
#' improvement by re-using the cluster between each call to `rbmi_analyse()` e.g.
#' ```
#' cl <- make_rbmi_cluster(4)
#' ana_1 <- rbmi_analyse(
#'     imputations = imputeObj,
#'     delta = delta_plan_1,
#'     cluster_or_cores = cl
#' )
#' ana_2 <- rbmi_analyse(
#'     imputations = imputeObj,
#'     delta = delta_plan_2,
#'     cluster_or_cores = cl
#' )
#' ana_3 <- rbmi_analyse(
#'     imputations = imputeObj,
#'     delta = delta_plan_3,
#'     cluster_or_cores = cl
#' )
#' parallel::clusterStop(cl)
#' ```
#'
#' @return An `analysis` object, as defined by `rbmi`, representing the desired
#' analysis applied to each of the imputed datasets in `imputations`.
#' @examples
#' library(rbmi)
#' library(dplyr)
#'
#' dat <- antidepressant_data
#' dat$GENDER <- as.factor(dat$GENDER)
#' dat$POOLINV <- as.factor(dat$POOLINV)
#' set.seed(123)
#' pat_ids <- sample(levels(dat$PATIENT), nlevels(dat$PATIENT) / 4)
#' dat <- dat |>
#'   filter(PATIENT %in% pat_ids) |>
#'   droplevels()
#' dat <- expand_locf(
#'   dat,
#'   PATIENT = levels(dat$PATIENT),
#'   VISIT = levels(dat$VISIT),
#'   vars = c("BASVAL", "THERAPY"),
#'   group = c("PATIENT"),
#'   order = c("PATIENT", "VISIT")
#' )
#' dat_ice <- dat %>%
#'   arrange(PATIENT, VISIT) %>%
#'   filter(is.na(CHANGE)) %>%
#'   group_by(PATIENT) %>%
#'   slice(1) %>%
#'   ungroup() %>%
#'   select(PATIENT, VISIT) %>%
#'   mutate(strategy = "JR")
#' dat_ice <- dat_ice[-which(dat_ice$PATIENT == 3618), ]
#' vars <- set_vars(
#'   outcome = "CHANGE",
#'   visit = "VISIT",
#'   subjid = "PATIENT",
#'   group = "THERAPY",
#'   covariates = c("THERAPY")
#' )
#' drawObj <- draws(
#'   data = dat,
#'   data_ice = dat_ice,
#'   vars = vars,
#'   method = method_condmean(type = "jackknife", covariance = "csh"),
#'   quiet = TRUE
#' )
#' references <- c("DRUG" = "PLACEBO", "PLACEBO" = "PLACEBO")
#' imputeObj <- impute(drawObj, references)
#'
#' rbmi_analyse(imputations = imputeObj, vars = vars)
#' @export
rbmi_analyse <- function(imputations, fun = rbmi_ancova, delta = NULL, ..., cluster_or_cores = 1, .validate = TRUE) {
  # nocov

  if (!requireNamespace("rbmi", quietly = TRUE)) {
    stop("The 'rbmi' package is needed for this function to work. Please install it.", call. = FALSE)
  }

  if (.validate) rbmi::validate(imputations)

  assertthat::assert_that(is.function(fun), msg = "`fun` must be a function")

  assertthat::assert_that(is.null(delta) | is.data.frame(delta), msg = "`delta` must be NULL or a data.frame")

  vars <- imputations$data$vars

  if (.validate) devnull <- lapply(imputations$imputations, function(x) rbmi::validate(x))

  if (!is.null(delta)) {
    expected_vars <- c(vars$subjid, vars$visit, "delta")
    assertthat::assert_that(
      all(expected_vars %in% names(delta)),
      msg = sprintf("The following variables must exist witin `delta`: `%s`", paste0(expected_vars, collapse = "`, `"))
    )
  }

  # Mangle name to avoid any conflicts with user defined objects if running in a cluster
  ..rbmi..analysis..imputations <- imputations
  ..rbmi..analysis..delta <- delta
  ..rbmi..analysis..fun <- fun
  objects <- list(
    ..rbmi..analysis..imputations = ..rbmi..analysis..imputations,
    ..rbmi..analysis..delta = ..rbmi..analysis..delta,
    ..rbmi..analysis..fun = ..rbmi..analysis..fun
  )

  cl <- make_rbmi_cluster(cluster_or_cores)

  if (methods::is(cl, "cluster")) {
    ..rbmi..analysis..data..path <- tempfile()
    saveRDS(objects, file = ..rbmi..analysis..data..path, compress = FALSE)
    devnull <- parallel::clusterExport(cl, "..rbmi..analysis..data..path", environment())
    devnull <- parallel::clusterEvalQ(cl, {
      ..rbmi..analysis..objects <- readRDS(..rbmi..analysis..data..path)
      list2env(..rbmi..analysis..objects, envir = environment())
    })
  }

  # If the user provided the clusters object directly then do not close it on completion
  if (!methods::is(cluster_or_cores, "cluster")) {
    on.exit(
      {
        if (!is.null(cl)) parallel::stopCluster(cl)
      },
      add = TRUE,
      after = FALSE
    )
  }

  # Chunk up requests for significant speed improvement when running in parallel
  number_of_cores <- ifelse(is.null(cl), 1, length(cl))
  indexes <- seq_along(imputations$imputations)
  indexes_split <- split(indexes, (indexes %% number_of_cores) + 1)

  results <- par_lapply(
    cl,
    function(indicies, ...) {
      inner_fun <- function(idx, ...) {
        dat2 <- (utils::getFromNamespace("extract_imputed_df", "rbmi"))(
          ..rbmi..analysis..imputations$imputations[[idx]],
          ..rbmi..analysis..imputations$data,
          ..rbmi..analysis..delta
        )
        ..rbmi..analysis..fun(dat2, ...)
      }
      lapply(indicies, inner_fun, ...)
    },
    indexes_split,
    ...
  ) |>
    unlist(recursive = FALSE, use.names = FALSE)

  # Re-order to ensure results are returned in same order as imputations
  results <- results[order(unlist(indexes_split, use.names = FALSE))]
  names(results) <- NULL

  fun_name <- deparse(substitute(fun))
  if (length(fun_name) > 1) {
    fun_name <- "<Anonymous Function>"
  } else if (is.null(fun_name)) {
    fun_name <- "<NULL>"
  }

  ret <- (utils::getFromNamespace("as_analysis", "rbmi"))(
    results = results,
    fun_name = fun_name,
    delta = delta,
    fun = fun,
    method = imputations$method
  )
  rbmi::validate(ret)
  return(ret)
}
