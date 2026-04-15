
library(dplyr)
library(junco)
library(tibble)


adsl_jnj <- pharmaverseadamjnj::adsl

adae_jnj <- pharmaverseadamjnj::adae

        

fix_usubjid <- function(adsl) {
    rws <- which(adsl$TRT01P == "Std Of Care")

    usubj_char <- as.character(adsl$USUBJID)
    subjid <- as.integer(as.character(adsl$SUBJID))
    subjid[rws] <- subjid[rws] + 1000
    substr(usubj_char, 8, 11) <- as.character(subjid)
    adsl$USUBJID <- factor(usubj_char)
    adsl$SUBJID <- factor(as.character(subjid))
    adsl

}
        
        

make_fake_adsl <- function(adsl) {
    fakeyfake <- filter(adsl, TRT01P == "Placebo")
    fakeyfake$TRT01P <- "Std Of Care"
    fakeyfake$AGE <- floor(runif(NROW(fakeyfake), 30,  90))
    adsl$TRT01P <- as.character(adsl$TRT01P)
     adsl <- rbind(adsl, fakeyfake)
    adsl$TRT01P <- factor(adsl$TRT01P)

    fix_usubjid(adsl)
 }



borrow_aes <- function(adae, adsl, mult = 1) { #runif(1, .9, 1.1)) {
    plac_count <- sum(adae$TRT01P == "Placebo", na.rm = TRUE)
    new_count <- floor(plac_count * mult)
    soc_usubjids <- as.character(adsl$USUBJID)[!is.na(adsl$TRT01P) & adsl$TRT01P == "Std Of Care"]

    duprows <- sample(seq_len(NROW(adae)), new_count, replace = TRUE)

    newrws <- adae[duprows,]
    print(c(new_count, length(duprows), length(unique(duprows)), NROW(newrws)))
    newrws$USUBJID <- sample(soc_usubjids, NROW(newrws), replace = TRUE)
    rbind(adae, newrws)
}





trtvar <- "TRT01P"

adsl <- adsl_jnj |>
 # filter(!!rlang::sym(popfl) == "Y") |>
  make_fake_adsl() |>
  create_colspan_var(
    non_active_grp = c("Placebo", "Std Of Care"),
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = trtvar
  ) |>
  mutate(
    rrisk_header = "Risk Difference (%) (95% CI)",
    rrisk_label = paste(!!rlang::sym(trtvar), "vs Placebo"),
    rrisk_label2 = paste(!!rlang::sym(trtvar), "vs Std of Care")
  ) |>
  select(
    USUBJID,
 #   !!rlang::sym(popfl),
    !!rlang::sym(trtvar),
    colspan_trt,
    rrisk_header,
    rrisk_label,
    rrisk_label2
  )




adae <- adae_jnj |>
  filter(TRTEMFL == "Y") |>
  borrow_aes(adsl) |>  
  select(
    USUBJID,
    AESER,
    AESDTH,
    AESLIFE,
    AESHOSP,
    AESDISAB,
    AESCONG,
    AESMIE,
    AEACN_DECODE,
    AESEV
  ) |>
  group_by(USUBJID) |>
  mutate(maxsev = max(as.character(AESEV), na.rm = TRUE)) |>
  ungroup() |>
  mutate(maxsev = ifelse(is.na(maxsev), "Missing", maxsev)) |>
  mutate(
    maxsev = factor(maxsev, levels = c("Mild", "Moderate", "Severe", "Missing"))
  )


adae <- inner_join(adae, adsl, by = c("USUBJID"), multiple = "all")

ctrl_grp <- c("Placebo", "Std Of Care")

colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

ref_paths <- lapply(ctrl_grp, function(ctrl) c("colspan_trt", " ", trtvar, ctrl))

rr_splitfun <- make_multicomp_splfun(c(trtvar, "rrisk_label2"), ref_paths)

#debugonce(rr_splitfun)

lvls <- levels(adsl[[trtvar]])
combodf <- tribble(~valname, ~label, ~levelcombo, ~exargs,
                   "all_active", "All Active", lvls[3:4],list(), 
                   "all_patients", "All Patients", select_all_levels, list())


comp_map <- junco:::make_dflt_comp_map(adsl, trtvar, ctrl_grp, combodf, c(trtvar, "rrisk_label2"))
                  ## tribble(~active, ~comparator, ~is_combo_active,
                  ##         "all_active", "Placebo", TRUE,
                  ##         "all_active", "Std Of Care", TRUE,
                  ##         "all_patients", "Placebo", TRUE,
                  ##         "all_patients", "Std Of Care", TRUE))


lyt_basic <- basic_table() |>
    split_cols_by("rrisk_header") |>
    split_cols_by("TRT01P", split_fun = add_combo_levels(combodf)) |>
    analyze("TRT01P")

build_table(lyt_basic, adsl)




lyt <- basic_table() |>
    split_cols_by("colspan_trt", split_fun = trim_levels_to_map(colspan_trt_map)) |>
    split_cols_by(trtvar, show_colcounts = TRUE) |>
    split_cols_by("rrisk_header", nested = FALSE) |>
    split_cols_by(trtvar, labels_var = "rrisk_label", split_fun = rr_splitfun) |>
    analyze("AESDTH", afun = function(x, ...) rcell("-"))

build_table(lyt, adae, adsl)

rr_splitfun2 <- make_multicomp_splfun(c(trtvar, "rrisk_label2"), ref_paths, comp_level_map = comp_map, combo_levels_map = combodf)


lyt <- basic_table() |>
    split_cols_by("colspan_trt", split_fun = trim_levels_to_map(colspan_trt_map)) |>
    split_cols_by(trtvar, show_colcounts = TRUE, split_fun = add_combo_levels(combodf)) |>
    split_cols_by("rrisk_header", nested = FALSE) |>
    split_cols_by(trtvar, labels_var = "rrisk_label", split_fun = rr_splitfun2) |>
    analyze("AESDTH", afun = function(x, ref_path = NULL, ...) rcell(paste(ref_path, collapse = ".")))

build_table(lyt, adae, adsl)

lyt <- basic_table() |>
    col_struct_w_risk_diffs(colspan_trt_map,
                            combodf,
                            comp_map,
                            comp_vars = c(trtvar, "rrisk_label2")) |>
    analyze("AESDTH", afun = function(x, ref_path = NULL, ...) rcell(paste(ref_path, collapse = ".")))

tbl <- build_table(lyt, adae, adsl)


lyt <- basic_table() |>
    col_struct_w_risk_diffs(colspan_trt_map,
                            combo_map_df = NULL,
                            comp_map = NULL,
                            comp_vars = c(trtvar, "rrisk_label2")) |>
    analyze("AESDTH", afun = function(x, ref_path = NULL, ...) rcell(paste(ref_path, collapse = ".")))

tbl <- build_table(lyt, adae, adsl)
