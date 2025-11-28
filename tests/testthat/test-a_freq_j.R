test_that("a_freq_j with label_map works in a table layout as expected", {
    set.seed(12)
    dta <- data.frame(
        id = 1:100,
        rsp = factor(sample(c(TRUE, FALSE), 100, TRUE)),
        grp = factor(rep(c("A", "B"), each = 50), levels = c("A", "B"))
    )
    label_map <- data.frame(
        value = c("TRUE", "FALSE"), 
        label = c("Response", "No Response")
    )
    lyt <- basic_table() |>
        split_cols_by("grp") |>
        analyze(
            "rsp",
            afun = a_freq_j,
            extra_args = list(
                label_map = label_map,
                id = "id"
            )
        )
    result <- build_table(lyt, dta)
})

test_that("a_freq_j with label_map restricts the values according to row split and label_map", {
    set.seed(12)
    dta <- data.frame(
        id = 1:100,
        visit = factor(rep(c("Baseline", "Week 1"), length.out = 100)),
        rsp = factor(sample(c("a", "b", "c", "d"), 100, TRUE)),
        grp = factor(rep(c("A", "B"), each = 50), levels = c("A", "B"))
    )
    label_map <- data.frame(
        visit = rep(c("Baseline", "Week 1"), each = 2),
        value = c("a", "b", "c", "d"), 
        label = c("Response A", "Response B", "Response C", "Response D")
    )
    lyt <- basic_table() |>
        split_cols_by("grp") |>
        split_rows_by("visit") |>
        analyze(
            "rsp",
            afun = a_freq_j,
            extra_args = list(
                label_map = label_map,
                id = "id"
            )
        )
    result <- build_table(lyt, dta)
})

test_that("a_freq_j_with_exclude allows to exclude row split levels from the analysis", {
    set.seed(12)
    dta <- data.frame(
        id = 1:100,
        visit = factor(rep(c("Baseline", "Week 1"), length.out = 100)),
        rsp = factor(sample(c("a", "b", "c", "d"), 100, TRUE)),
        grp = factor(rep(c("A", "B"), each = 50), levels = c("A", "B"))
    )
    label_map <- data.frame(
        visit = rep(c("Baseline", "Week 1"), each = 2),
        value = c("a", "b", "c", "d"), 
        label = c("Response A", "Response B", "Response C", "Response D")
    )
    lyt <- basic_table() |>
        split_cols_by("grp") |>
        split_rows_by("visit") |>
        analyze(
            "rsp",
            afun = a_freq_j_with_exclude,
            extra_args = list(
                label_map = label_map,
                exclude_levels = list(visit = "Week 1"),
                id = "id"
            )
        )
    result <- build_table(lyt, dta) |>
        safe_prune_table(prune_func = tern::keep_rows(keep_non_null_rows))
    expect_snapshot(result)
})