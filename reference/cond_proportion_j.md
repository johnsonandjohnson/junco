# Conditional proportion with adaptive CI (exact vs. Wald)

**\[experimental\]**

## Usage

``` r
a_cond_proportion_j(
  df,
  .var,
  ...,
  .stats = NULL,
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL,
  .df_row = NULL
)

s_cond_proportion_j(
  df,
  .var,
  conf_level = 0.95,
  long = FALSE,
  na.rm = FALSE,
  num_limit = 0,
  denom_limit = 10,
  denom = c("n", "N_row"),
  method_scope = c("cell", "row"),
  .df_row = NULL
)

d_cond_proportion_j(
  conf_level,
  long = FALSE,
  num_limit = NULL,
  denom_limit = NULL,
  method = NULL,
  method_denom = NULL,
  method_rsp = NULL
)
```

## Arguments

- df:

  (`logical` or `data.frame`)  
  if only a logical vector is used, it indicates whether each subject is
  a responder or not. `TRUE` represents a successful outcome. If a
  `data.frame` is provided, the logical vector of responses must be
  indicated as a variable name in `.var`.

- .var:

  (`string`)

- ...:

  additional arguments for the lower level functions.

- .stats:

  (`character`)  
  statistics to select for the table.

- .formats:

  (named `character` or `list`)  
  formats for the statistics. See Details in `analyze_vars` for more
  information on the `'auto'` setting.

- .labels:

  (named `character`)  
  labels for the statistics (without indent).

- .indent_mods:

  (named `integer`)  
  indent modifiers for the labels. Defaults to 0, which corresponds to
  the unmodified default behavior. Can be negative.

- .df_row:

  (`data.frame`)  
  data for the current row across all columns, automatically supplied by
  `rtables`.

- conf_level:

  (`numeric`)

- long:

  (`flag`)  
  whether a long description is required.

- na.rm:

  (`flag`)  
  whether `NA` responses should be removed before analysis. If `FALSE`
  and `NA` values are present, an error is raised. Note that missing
  values are also removed from the row-wise counts which is relevant
  when `method_scope = "row"` or `denom = "N_row"`.

- num_limit:

  (`int`)  
  numerator limit to trigger the exact method.

- denom_limit:

  (`int`)  
  denominator limit to trigger the exact method.

- denom:

  (`character`)  
  denominator to use for percentage and CI computation: "n" (default,
  number of observed records), or "N_row" (number of observations in the
  row).

- method_scope:

  (`string`)  
  select the CI method using counts from the current cell (`"cell"`) or
  all columns in the current row (`"row"`). See details.

- method:

  (`string` or `NULL`)  
  selected CI method to show in the label. `NULL` retains the combined
  method description.

- method_denom:

  (`int`)  
  denominator used for the selected CI method.

- method_rsp:

  (`int`)  
  numerator used for the selected CI method.

## Value

- `a_cond_proportion_j()` returns the corresponding list with formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

&nbsp;

- `s_cond_proportion_j()` returns statistics `n_prop` (`n` responders
  and proportion) and `prop_ci` (proportion CI), formatted consistently
  with
  [`tern::s_proportion()`](https://pharmaverse.github.io/tern/latest-tag/reference/estimate_proportion.html).

&nbsp;

- `d_cond_proportion_j()` returns a string describing the analysis
  label.

## Details

The analysis function `a_cond_proportion_j()` is used to create a layout
element that estimates a response proportion with confidence interval,
automatically selecting between exact and Wald methods based on observed
counts and user-defined limits.

The statistics function mirrors
[`tern::s_proportion()`](https://pharmaverse.github.io/tern/latest-tag/reference/estimate_proportion.html)
usage and output but removes the `method` argument and decides
internally between "clopper-pearson" and "wald".

The exact method is used when any of these are true: (a) the number of
responders is `num_limit` (or less), (b) all subjects except `num_limit`
(or less) have observed response, or (c) the observed group size is less
than `denom_limit`.

Depending on the `method_scope` choice, this decision is either taken
based on the response data for an individual cell
(`method_scope = "cell"`), or based on the response data for the entire
row (`method_scope = "row"`):

- With `method_scope = "cell"`, the counts used for the decision are:

  - numerator: the number of responders in the current cell;

  - denominator: the total number of observations in the cell (using
    `df`) if `denom = "n"`, or across the row (using `.df_row`) if
    `denom = "N_row"`.

- With `method_scope = "row"`, the counts used for the decision are:

  - numerator: the total number of responders across the row (using
    `.df_row`);

  - denominator: the total number of observations across the row (from
    `.df_row`).

CI computation follows
[`tern::s_proportion()`](https://pharmaverse.github.io/tern/latest-tag/reference/estimate_proportion.html)
conventions: helper functions are called with `n = denom`, so when
`denom = "N_row"`, this row-level denominator is used for the interval.

## Functions

- `a_cond_proportion_j()`: Formatted analysis function which is used as
  `afun` for conditional proportion with adaptive CI selection (exact
  vs. Wald).

- `s_cond_proportion_j()`: Statistics function estimating a proportion
  along with its confidence interval, with adaptive method selection.

- `d_cond_proportion_j()`: Description helper function for the
  conditional proportion summary label.

## Examples

``` r
nex <- 100
dta <- data.frame(
  "rsp" = sample(c(TRUE, FALSE), nex, TRUE),
  "grp" = sample(c("A", "B"), nex, TRUE),
  "f1"  = sample(c("a1", "a2"), nex, TRUE),
  stringsAsFactors = TRUE
)

l <- basic_table() |>
  split_cols_by(var = "grp") |>
  analyze(
    vars = "rsp",
    afun = a_cond_proportion_j,
    extra_args = list(
      conf_level = 0.90,
      num_limit = 0,
      denom_limit = 10
    )
  )

build_table(l, df = dta)
#>                                        A              B      
#> —————————————————————————————————————————————————————————————
#> Responders                         29 (55.8%)     27 (56.2%) 
#> 90% CI (Wald / Clopper-Pearson)   (44.4, 67.1)   (44.5, 68.0)

# Logical vector input
rsp_v <- c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE)
s_cond_proportion_j(rsp_v)
#> $n_prop
#> [1] 4.0 0.5
#> attr(,"label")
#> [1] "Responders"
#> 
#> $prop_ci
#> [1] 15.70128 84.29872
#> attr(,"label")
#> [1] "95% CI (Wald / Clopper-Pearson)"
#> 

# Data frame input
dta <- data.frame(rsp = c(TRUE, TRUE, FALSE, TRUE, FALSE, NA))
s_cond_proportion_j(dta, .var = "rsp", na.rm = TRUE)
#> $n_prop
#> [1] 3.0 0.6
#> attr(,"label")
#> [1] "Responders"
#> 
#> $prop_ci
#> [1] 14.66328 94.72550
#> attr(,"label")
#> [1] "95% CI (Wald / Clopper-Pearson)"
#> 

# Using method_scope = "row" (requires .df_row in ...)
df_row <- data.frame(rsp = rep(rsp_v, 2))
s_cond_proportion_j(
  dta, .var = "rsp", method_scope = "row",
  .df_row = df_row, na.rm = TRUE
)
#> $n_prop
#> [1] 3.0 0.6
#> attr(,"label")
#> [1] "Responders"
#> 
#> $prop_ci
#> [1]  17.05934 100.00000
#> attr(,"label")
#> [1] "95% CI (Wald)"
#> 

d_cond_proportion_j(conf_level = 0.90, long = FALSE, num_limit = 0, denom_limit = 10)
#> [1] "90% CI (Wald / Clopper-Pearson)"

# With dedicated row-wise method:
d_cond_proportion_j(
  conf_level = 0.90, long = TRUE, num_limit = 0, denom_limit = 10,
  method = "wald", method_denom = 10, method_rsp = 8)
#> [1] "90% CI for Response Rates (Wald because n >= 10, x = 8)"
```
