get_all_comp_lvls <- function(colspan_trt_map) {
    nonactlvl  <- unique(as.character(colspan_trt_map[[1]]))[2]
    unique(as.character(colspan_trt_map[colspan_trt_map[[1]] == nonactlvl, 2]))
}

get_first_comp_lvl <- function(colspan_trt_map) {
    get_all_comp_lvls(colspan_trt_map)[1]
}

get_comp_path <- function(map, lvl) {
    rw <- map[map[[2]] == lvl,]
    c(names(map)[1], rw[[1]], names(map)[2], rw[[2]])
}


## ugh. thisisfine.jpg XXX  :( :( :(
##
## this is where we munge treatment names/labels into comparison versions
do_sib_val_surgery <- function(splval, comp_lvl, newexargs) {
    splval@value <- make_comp_name(splval@value, comp_lvl)
    splval@label <- make_comp_name(splval@label, comp_lvl)
    args <- c(newexargs, splval@extra)
    splval@extra <- args
    splval
}


## do "surgery" on all of the original values to enforce uniqueness and
## correct comparison labels
surgical_suite <- function(orig_ret, comp_lvl, newexargs) {
    out <- orig_ret
    out$values <- lapply(out$values,
                         do_sib_val_surgery,
                         comp_lvl = comp_lvl,
                         newexargs = newexargs)
    out$labels <- vapply(out$values, function(x) x@label, "")
    newnms <- vapply(out$values, value_names, "")
    out <- lapply(out,
                  function(part) {
          names(part) <- newnms
          part
      })
    names(out) <- names(orig_ret)
    out
}


add_sib_facets <- function(comp_level, colspan_trt_map, combo_map_all) {
    function(ret, spl, .spl_context, fulldf) {
    combo_map <- NULL
    if (!is.null(combo_map_all)) {
      combo_map <- combo_map_all[combo_map_all$comparator_level == comp_level, ]
    }
    comp_path <- get_comp_path(colspan_trt_map, comp_level)
    ret <- insert_subset_exprs(ret, spl, comp_path = comp_path)
    out <- ret
    exargs <- list(ref_path = get_comp_path(colspan_trt_map, comp_level))
    if (NROW(combo_map) > 0) {
      for (i in seq_len(NROW(combo_map))) {
        out <- add_combo_facet(
          combo_map$valname[i],
          combo_map$label[i],
          combo_map$levelcombo[[i]],
          combo_map$exargs[[i]]
        )(
          ret = out,
          spl = spl,
          .spl_context = .spl_context,
          fulldf = fulldf
        )
      }                                        #     }
    }
    out <- surgical_suite(out, comp_level, exargs)
    names(out) <- names(ret)
    out
  }
}

#' Make Multi-comparator Split Function
#'
#' Create a custom splitting function suitable for creating risk
#' difference columns against one or more comparators. This is used
#' within `col_struct_w_risk_diffs`.
#'
#' @param colspan_trt_map (`data.frame`)\cr A data.frame defining the
#'     active and non-active groups of treatment arms, including
#'     combination arms defined in `combo_levels_map`, as returned by
#'     [create_colspan_map()].
#' 
#' @param combo_levels_map (`data.frame` or `NULL`)\cr NULL (the
#'     default) or a data.frame indicating combination levels to added
#'     to some or all blocks of comparisons. See Details.
#'
#' @param comp_level_map (`data.frame` or `NULL`)\cr A data.frame with
#'     columns `active` and `comparator` indivating which risk
#'     difference comparisons to include in the column structure, or
#'     `NULL` (the default), indicating all active vs non-active
#'     pairwise comparisons as defined `colspan_trt_map` treatment
#'     groupings.
#'
#' @param .pre `(list)`\cr A list of additional preprocessing
#'     functions to be provided to `make_split_fun`. Defaults to
#'     `list()`.
#'
#' @param .post `(list)`\cr A list of additional post-processing
#'     functions to be provided to `make_split_fun` *after* those
#'     which provide this function's primary multi-comparator
#'     functionality. Defaults to `list()`
#'
#' @return A split function suitable for use in both `split_rows_by`
#'     and `split_cols_by`.
#'
#' @details
#' This split function is intended to create a set of risk difference
#' or similar columns. As such it will automatically exclude the facet
#' for each comparator level (e.g., Placebo vs Placebo) as determined
#' by the last element of each element of `comp_level_paths`.
#'
#' Further control of facets is provided by `comp_level_map`. If
#' `NULL` (the default), all non-control/reference groups will be
#' compared pairwise with all control/reference groups as defined by
#' the grouping in `colspan_trt_map`.
#'
#'  If specified, `comp_level_map` must be a `data.frame` (including
#'  `tbl_df`) with three columns:
#'
#' - `active` - (character) the value to be compared to a reference level,
#' - `comparator` - (character) the level that should be compared against, and
#' - `active_is_combo` - (logical) is the level specified in `active` a virtual combination level.
#' - `comparator_is_combo` - (logical) is the level specified in `comparator` a virtual combination level.
#'
#' If a `data.frame` with only the `active` and `comparator` columns
#' is given for `comp_level_map`, `active_is_combo` and
#' `comparator_is_combo` are inferred from `colspan_trt_map`.
#'
#' If any rows of `comp_level_map` have `active_is_combo == TRUE` or
#' `comparator_is_combo`, the relevant values in those rows *must*
#' also appear in `combo_levels_map` with the correct level for
#' comp_level (or the `select_all_levels` sentinel value which
#' indicates inclusion for all comparators).
#'
#' If specified, `combo_levels_map` must be a `data.frame` (including `tbl_df`)
#' with the following columns:
#'
#' - `valname` - (`character`) The name(s) for the combination level(s),
#' - `label` - (`character`) the label(s) for the combination level(s),
#' - `levelcombo` - (`list` of `character`) the levels of the split
#'   variable to be combined, or `select_all_levels` for all levels,
#' - `exargs` - (`list`) the extra_args values for each combo level. If not
#'   present this will be assumed to be `list()` for all combo levels.
#' - `compare_against` - (`list` of `character`) Optional. The reference level(s) the
#'   combo level should be compared against, or `select_all_levels` for
#'   inclusion against all comparators.
#' - `is_control` - (`logical`) Optional. Is this combination level going
#'   to be used as a reference level (must appear as the last element in
#'   one of `comp_level_paths` if so).
#'
#' When specifying `combo_levels_map` if the `compare_against` column
#' is omitted, comparison against all reference levels will be
#' performed for all combination levels. If `is_control` is omitted,
#' it will be assumed as `FALSE` for all combination levels.
#'
#' Order of combination levels when multiple are present for a single
#' comparator, as well as their position relative to non-combination
#' comparisons, is determined by row order in `combo_levels_map`.
#'
#' Labels and names of comparison columns involving combination levels
#' will be automatically computed in the form of
#' `"<combo level name/label> vs <ref group name>"`. Note currently
#' ref group *name* is always used as it needs to be inferable from
#' `colspan_trt_map`.
#'
#'
#' The comparator reference path is calculated based on
#' `colspan_trt_map` and then added as `ref_path` to the extra_args
#' associated with generated facet. As such, analysis (or content)
#' functions used underneath a split using the generated split
#' function must accept either `ref_path` or `...`.
#'
#' @note It is not currently possible to use a virtual combination
#'     level as a comparator/reference group. If you need this
#'     functionality please contact the maintainers by filing an issue
#'     at https://github.com/johnsonandjohnson/junco/issues
#'
#' @family riskdiff_col_struct
#'
#' @export
make_multicomp_splfun <- function(colspan_trt_map,
                                   combo_levels_map = NULL,
                                   comp_level_map = NULL,
                                   .pre = list(),
                                   .post = list()) {
  post <- c(.post)
  if (is.null(comp_level_map)) {
    comp_levels <- get_all_comp_lvls(colspan_trt_map)

  } else {
    comp_levels <- unique(as.character(comp_level_map$comparator))
  }
  if (!is.null(combo_levels_map)) {
    ## comp_level_map <- fix_combo_comp_levels(comp_level_map,
    ##   combo_levels_map,
    ##   ref_lvls = comp_levels
    ## )
    combo_levels_map <- expand_combo_map(combo_levels_map, ref_lvls = comp_levels)
  }

  funlst <- list(
      function(ret, spl, .spl_context, fulldf) {
      sib_sets <- lapply(
          comp_levels,
          function(lvl) {

          sib_fac_fun <- add_sib_facets(
              lvl,
              colspan_trt_map = colspan_trt_map,
              combo_map_all = combo_levels_map
          )
          sib_fac_fun(ret, spl, .spl_context, fulldf)
      })
      out <- lapply(names(ret),
                    function(nm) {
          unlist(lapply(seq_along(sib_sets), function(ii) sib_sets[[ii]][[nm]]),
                 recursive = FALSE)
      })
      names(out) <- names(ret)
      out
  })

  make_split_fun(
    pre = .pre,
    post = c(
      funlst,
      apply_comp_map(splvar = names(colspan_trt_map)[2], comp_levels, comp_map = comp_level_map, combo_map = combo_levels_map),
      post
    )
  )
}






make_comp_name <- function(act_nm, comp_nm) paste0(act_nm, " vs ", comp_nm)

## must be called ***before*** expand_combo_map so the old
## valnames are still there
fix_combo_comp_levels <- function(comp_map, combo_map, ref_lvls, ref_labs = ref_lvls) {
  if (NROW(combo_map) == 0 || NROW(comp_map) == 0) {
    return(comp_map)
  }
  comp_inds <- which(comp_map$active_is_combo)
  comp_map$active[comp_inds] <- vapply(comp_inds, function(ii) {
    comp_rw <- comp_map[ii, ]
    combo_ind <- match(comp_rw$active, combo_map$valname)
    make_comp_name(combo_map$label[combo_ind], ref_labs[match(comp_rw$comparator, ref_lvls)[1]])
  }, "")
  comp_map
}

## conversion of names/labels to comparison versions is now
## handled (much) later, in surgical_suite and apply_comp_map
expand_combo_map <- function(combo_map, ref_lvls) {
  if (NROW(combo_map) == 0) {
    return(combo_map)
  }

  if (!("compare_against" %in% names(combo_map))) {
    combo_map$compare_against <- lapply(seq_len(NROW(combo_map)), function(i) select_all_levels)
  }

  rws <- lapply(
    seq_len(NROW(combo_map)),
    function(ii) {
      first_ref <- ref_lvls[1]
      remaining_refs <- ref_lvls[-1]
      mp_rw <- combo_map[ii, ]
      comp_against <- mp_rw$compare_against[[1]]
      if (is.null(comp_against) || is(comp_against, "AllLevelsSentinel")) {
        comp_against <- ref_lvls
      }


      rws_out <- lapply(
        comp_against,
        function(cur_ref_lvl) {
          ref_lvl_ind <- match(cur_ref_lvl, ref_lvls)
          cur_rw <- combo_map[ii, ] ## AllLvlsSentinel class getting dropped from map_rw somehow...
          if (!is(cur_rw$levelcombo[[1]], "AllLevelsSentinel")) {
            cur_rw$levelcombo[[1]] <- vapply(
              as.character(cur_rw$levelcombo[[1]]),
              function(lvl) {
                if (!is(lvl, "AllLevelsSentinel")) {
                  lvl <- make_comp_name(lvl, cur_ref_lvl)
                }
                lvl
              }, ""
            )
          }
          cur_rw$comparator_level <- cur_ref_lvl
          cur_rw
        }
      )

      do.call(rbind.data.frame, rws_out)
    }
  )
  do.call(rbind.data.frame, rws)
}


apply_comp_map <- function(splvar, ref_lvls, comp_map, combo_map) {
    function(ret, spl, fulldf, ...) {
    all_lvls <- levels(fulldf[[splvar]])

    if (is.null(comp_map)) {
      comp_map <- make_dflt_comp_map(fulldf, splvar, ref_lvls, combo_map)
    }
    lvls_to_keep <- make_comp_name(comp_map$active, comp_map$comparator) #levels_from_comp_map2(comp_map, combodf)
    restrict_facets(lvls_to_keep, op = "keep")(ret, spl, fulldf)
  }
}

#' @export
#' @rdname make_multicomp_splfun
make_dflt_comp_map <- function(df, spl_var, ref_lvls, combo_map) {
  all_lvls <- as.character(levels(df[[spl_var]]))

  non_ref <- setdiff(all_lvls, ref_lvls)
  base_rws <- lapply(
    ref_lvls,
    function(ref_lvl_i) {
      data.frame(active = non_ref, comparator = ref_lvl_i, active_is_combo = FALSE, comparator_is_combo = FALSE)
    }
  )

  combo_rws <- combodf_to_comp_map(combodf = combo_map, ref_lvls = ref_lvls, all_base_lvls = all_lvls)
  ret <- do.call(rbind.data.frame, c(base_rws, list(combo_rws)))

  ret$tmp_fact <- factor(ret$comparator, levels = ref_lvls)
  o <- order(ret$tmp_fact)
  ret <- ret[o, ]
  ret$tmp_fact <- NULL
  ret
}


combodf_to_comp_map <- function(combodf, ref_lvls, all_base_lvls) {
  nrcombo <- NROW(combodf)
  if (nrcombo == 0) {
    return(NULL)
  }

  if (!("is_control" %in% names(combodf))) {
    combodf$is_control <- combodf$valname %in% ref_lvls
  }

  non_ref_lvls <- c(
    setdiff(all_base_lvls, ref_lvls),
    combodf$valname[!combodf$is_control]
  )
  if (!("compare_against" %in% names(combodf))) {
    combodf$compare_against <- ifelse(combodf$is_control,
      replicate(nrcombo, list(non_ref_lvls)),
      replicate(nrcombo, list(ref_lvls))
    )
  }


  if (any(combodf$is_control) && !all(combodf$valname[combodf$is_control] %in% ref_lvls)) {
    stop(
      "Combination levels [",
      paste(combodf$valname[combodf$is_control], collapse = ", "),
      "] were listed as control groups in the combination df but are ",
      "missing from specified reference levels"
    )
  }


  rws <- lapply(
    seq_len(nrcombo),
    function(i) {
      cur_rw <- combodf[i, ]
      is_ctrl <- cur_rw$is_control
      comp_against <- cur_rw$compare_against[[1]]
      if (is(comp_against, "AllLevelsSentinel")) {
        if (is_ctrl) {
          comp_against <- non_ref_lvls
        } else {
          comp_against <- ref_lvls
        }
      }
      if (is_ctrl) {
        data.frame(
          active = comp_against,
          comparator = cur_rw$valname,
          active_is_combo = !(comp_against %in% all_base_lvls),
          comparator_is_combo = TRUE
        )
      } else {
        data.frame(
          active = cur_rw$valname,
          comparator = comp_against,
          active_is_combo = TRUE,
          comparator_is_combo = comp_against %in% combodf$valname
        )
      }
    }
  )

  ## if there are combinations acting as both active and reference
  ## levels the comparisons between them will be duplicated
  ret <- do.call(rbind.data.frame, rws)
  dups <- duplicated(ret[, c("active", "comparator")])
  ret[!dups, ]
}


#' Construct column structure with main and risk difference sections
#'
#' @param lyt (`PreDataTableLayouts`). The layout to modify. This
#'     should virtually always be the object returned by `basic_table`.
#' @param colspan_trt_map (`data.frame`). The spanning label map for
#'     the main columns, as given by `create_colspan_map`.
#' @param combo_map_df (`data.frame` or `NULL`). A combination data
#'     frame as defined by [add_combo_levels()] with an additional
#'     `is_control` column indicating whether the virtual level will
#'     act as a reference (`TRUE`) or active (`FALSE`) group.
#' @param comp_map (`data.frame` or `NULL`). A data.frame with columns
#'     `"active"`, `"comparator"`, `"active_is_combo"` and
#'     `"comparator_is_combo"`, or `NULL` indicating the default
#'     comparison behavior (See Details).
#' @param rrisk_header (`character(1)`). The spanning label for the
#'     risk difference section of columns
#'
#' @param .main_pre (`list` of `function`s). Passed to
#'     [rtables::make_split_fun()] as `pre` for treatment split in main
#'     structure.
#' @param .main_post (`list` of `function`s). Passed to
#'     [rtables::make_split_fun()] as `post` for treatment split in
#'     main structure.
#' @param .rr_pre (`list` of `function`s). Passed to
#'     [make_multicomp_splfun()] as `.pre` for risk difference
#'     faceting.
#' @param .rr_post (`list` of `function`s). Passed to
#'     [make_multicomp_splfun()] as `.post` for risk difference
#'     faceting.
#'
#' @details
#'
#' This function combines multiple `rtables` column splitting
#' instructions with customized split functions to create a column
#' structure with treatment columns for each treatment arm (optionally
#' including combination arms), grouped by active and non-active, with
#' risk difference columns comparing active arm(s) against one or more
#' non-active controls. It is intended for use in layouts that will
#' use [a_freq_j()] or similar `junco`-style analysis functions which
#' support risk difference columns and accept a `ref_path` argument.
#'
#' It is equivalent to the following sequence of layout instructions:
#'
#'  1. splitting on a colspan labeling variable with
#'  [rtables::trim_levels_to_map()] as the split function;
#'  2. splitting on treatment;
#'  3. adding a (non-nested) overall column acting as the risk difference spanning label; and finally
#'  4. splitting on treatment using [make_multicomp_splfun()] as the
#'  split function
#'
#' In addition, it supports:
#'
#'  - comparison against multiple control groups (as specified by `colspan_trt_map` and/or `comp_map`), 
#'  - virtual combination-levels as active an/or control "treatments" (via `combo_map_df`),
#'  - full control of which comparisons are performed, and their order (via `comp_map`).
#'
#' If combination levels are declared via `combo_map_df` but none
#' appear in `colspan_trt_map`, all combinations will be added to the
#' appropriate group within the map based on `combo_map_df$is_control`
#' (assumed to be `FALSE` if the column is missing), with a warning.
#'
#' If some combination levels *do* appear in `combo_map_df` but
#' others do not, a warning will be thrown but the missing combination
#' levels will *not* be added to the treatment map.
#'
#' By default (when `comp_map` is `NULL`), all active treatments,
#' including active combinations, will be compared against all control
#' groups.
#'
#' The risk difference section of the structure is declared using
#' [make_multicomp_splfun()]. Reference paths are inferred
#' automatically from `colspan_trt_map` (after combination levels have
#' been added if necessary).
#'
#' For the purposes of pathin in the resulting structure,
#' `rrisk_header` will be both the split name and split value of the
#' parent containing the individual risk difference columns.
#'
#' @returns `lyt` updated with the specified main and risk difference
#'     column structures added
#'
#'
#' @family riskdiff_col_struct
#' @export

col_struct_w_risk_diffs <- function(lyt,
                                    colspan_trt_map,
                                    combo_map_df = NULL,
                                    ## default behavior for comp_map is taken care of in make_multicomp_splfun
                                    comp_map = NULL,
                                    rrisk_header = "Risk Differences",
                                    .main_pre = list(),
                                    .main_post = list(),
                                    .rr_pre = list(),
                                    .rr_post = list()) {
  trtvar <- names(colspan_trt_map)[2]
  spanvar <- names(colspan_trt_map)[1]

  if (!is.null(combo_map_df)) {
    if (!("is_control" %in% names(combo_map_df))) {
      combo_map_df$is_control <- FALSE
    }
    main_post <- lapply(
      seq_len(NROW(combo_map_df)),
      function(i) {
        force(i)
        add_combo_facet(
          name = combo_map_df$valname[i],
          label = combo_map_df$label[i],
          levels = combo_map_df$levelcombo[[i]],
          extra = combo_map_df$exargs[[i]]
        )
      }
    )

    combo_nms <- combo_map_df$valname

    ## we're guaranteed to have some combo levels at this point
    combo_found <- combo_nms %in% colspan_trt_map[[trtvar]]
    if (!any(combo_found)) {
      warning(
        "none of the combination levels appeared in the colspan treatment map",
        " adding them automatically."
      )
      colspan_trt_map <- add_combo_levs_to_trtmap(colspan_trt_map, combo_map_df)
    } else if (any(!combo_found)) {
      warning("some combination levels defined in combo_map_df do not appear in colspan_trt_map")
    }
  } else {
    main_post <- list()
  }

  main_post <- c(main_post, .trtmap_to_post_funs(colspan_trt_map), .main_post)

  main_splfun <- make_split_fun(pre = .main_pre, post = main_post)

  rr_splfun <- make_multicomp_splfun(
    colspan_trt_map, 
    comp_level_map = comp_map,
    combo_levels_map = combo_map_df,
    .pre = .rr_pre,
    .post = .rr_post
  )


  lyt <- lyt |>
    split_cols_by(names(colspan_trt_map)[1]) |>
    split_cols_by(trtvar, split_fun = main_splfun) |>
    add_overall_col(label = rrisk_header) |>
    split_cols_by(trtvar, split_fun = rr_splfun)
  lyt
}


combos_to_trtmap_rows <- function(combo_map, trtmap_sect) {
  ret <- data.frame(
    trtmap_sect[[1]][1],
    combo_map$valname
  )
  names(ret) <- names(trtmap_sect)
  ret
}


add_combo_levs_to_trtmap <- function(trtmap, combo_map) {
  span_vec <- trtmap[[1]]
  spltrtmap <- split(trtmap, span_vec)
  spanorder <- unique(span_vec) ## need this because split sorts things... like a clown.
  activedf <- spltrtmap[[spanorder[1]]]
  ctrldf <- spltrtmap[[spanorder[2]]]

  if (any(combo_map$is_control)) {
    ctrldf <- rbind(
      ctrldf,
      combos_to_trtmap_rows(combo_map[combo_map$is_control, ], ctrldf)
    )
  }

  if (any(!combo_map$is_control)) {
    activedf <- rbind(
      activedf,
      combos_to_trtmap_rows(combo_map[!combo_map$is_control, ], activedf)
    )
  }
  rbind(activedf, ctrldf)
}


.trtmap_to_post_funs <- function(trtmap) {
  splmap <- split(trtmap, trtmap[[1]])
  lapply(
    splmap,
    .map_sect_to_post_fun
  )
}

.map_sect_to_post_fun <- function(map) {
  cond_rm_facets(map[[2]], value = unique(map[[1]]), keep_matches = TRUE)
}
