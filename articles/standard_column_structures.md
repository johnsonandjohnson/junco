# Standard Column Structures

## Structures With Difference Columns

Many standard tables call for a set of difference columns to be added
alongside the main tabulation; this, combined with the spanning column
labels favored by Johnson&Johnson shells, results in a complex final
column structure.

To ease the creation of these structures, particularly in the presence
of virtual *combination* arms, we provide the `grouped_cols_w_diffs`
function. It provides the flexibility to be used in standard template
scripts, as we will see below.

### Basic Usage

At it’s most basic, `grouped_cols_w_diffs` takes a column-span treatment
map, as created by `create_colspan_map`. This creates a column structure
with spanning labels over active and non-active treatment groups, as
well as difference columns comparing each active treatments individually
against each non-active treatment:

``` r

library(junco)
#> Loading required package: formatters
#> 
#> Attaching package: 'formatters'
#> The following object is masked from 'package:base':
#> 
#>     %||%
#> Loading required package: rtables
#> Loading required package: magrittr
#> 
#> Attaching package: 'rtables'
#> The following object is masked from 'package:utils':
#> 
#>     str
#> Registered S3 method overwritten by 'tern':
#>   method   from 
#>   tidy.glm broom
adsl <- create_colspan_var(pharmaverseadamjnj::adsl)
adae <- create_colspan_var(pharmaverseadamjnj::adae)
trt_map <- create_colspan_map(adsl)
print(trt_map)
#>          colspan_trt               TRT01A
#> 1 Active Study Agent Xanomeline High Dose
#> 2 Active Study Agent  Xanomeline Low Dose
#> 3                                 Placebo
```

``` r

lyt <- basic_table() |>
  grouped_cols_w_diffs(trt_map)


build_table(lyt, adsl)
#>                Active Study Agent                                                 Risk Differences                        
#>    Xanomeline High Dose   Xanomeline Low Dose   Placebo   Xanomeline High Dose vs Placebo   Xanomeline Low Dose vs Placebo
#> ——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
```

#### A Note About Types Of Differences

`grouped_cols_w_diffs` creates only the *column* structure; the contents
of the difference columns are calculated by the analysis function used
in the layout. The *structure* for the various types of difference
columns (risk, mean, eair, etc) differs only in the spanning label for
the difference columns. `grouped_cols_w_diffs` accepts `diffs_label` for
this purpose:

``` r

lyt2 <- basic_table() |>
  grouped_cols_w_diffs(trt_map, diffs_label = "Mean Differences")


build_table(lyt2, adsl)
#>                Active Study Agent                                                 Mean Differences                        
#>    Xanomeline High Dose   Xanomeline Low Dose   Placebo   Xanomeline High Dose vs Placebo   Xanomeline Low Dose vs Placebo
#> ——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
```

Care should be taken to choose the correct analysis function to create
the types of differences required by your shell.

#### Turning Off The Difference Columns

Despite the function’s name, we can turn off difference columns when
using `grouped_cols_w_diffs` via the `diff_cols` argument. This allows
us to use the function unconditionally in our layouts while retaining
the ability to turn off difference columns , e.g., in shells where they
are optional.

``` r

lyt_nodiff <- basic_table() |>
  grouped_cols_w_diffs(trt_map, diff_cols = FALSE)


build_table(lyt_nodiff, adsl)
#>                Active Study Agent                      
#>    Xanomeline High Dose   Xanomeline Low Dose   Placebo
#> ———————————————————————————————————————————————————————
```

### Advanced Usage

Beyond the basic usage, `grouped_cols_w_diffs` offers fine-grained
control over both the arms (notably the addition of combination arms)
and comparisons between arms displayed in your table.

#### Including Virtual Combination Arms

Combination levels are declared via a “combo data.frame” similar to
those accepted by
[`rtables::add_combo_levels`](https://insightsengineering.github.io/rtables/latest-tag/reference/add_overall_level.html).
In particular, we use a data.frame with the following columns:

- **`valname`** - (`character`) Name of the combination level (as it
  will appear in paths)
- **`label`** - (`character`) Label for the combination level (as it
  will appear in the table)
- **`levelcombo`** - (`list` of `character`s) The levels which should be
  combined into the combination level
- **`exargs`** - (`list`) Extra arguments to be passed to a/cfuns
  associated with the combination level
- **`is_control`** *Optional* (`logical`) When `TRUE`, the combination
  level will be added to the second (non-active) group of treatments.
  Assumed `FALSE` if not specified.
- **`compare_against`** *Optional* (`list` of `character`s) The levels
  the combination level should be compared against (as the first/active
  element). Assumed to be `select_all_levels` if not specified.

Thus at its most basic, we use this argument like so:

``` r

library(tibble)
combodf1 <- tribble(
  ~valname, ~label, ~levelcombo, ~exargs,
  "all_active", "All Xanomeline", c("Xanomeline High Dose", "Xanomeline Low Dose"), list()
)

lyt3 <- basic_table() |>
  grouped_cols_w_diffs(trt_map, combo_map_df = combodf1)
#> none of the combination levels appeared in the colspan treatment map; adding them automatically.

build_table(lyt3, adsl)
#>                        Active Study Agent                                                                        Risk Differences                                      
#>    Xanomeline High Dose   Xanomeline Low Dose   All Xanomeline   Placebo   Xanomeline High Dose vs Placebo   Xanomeline Low Dose vs Placebo   All Xanomeline vs Placebo
#> ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
```

We can instead add a second virtual control or referene group by
including the `is_control` column and setting it to `TRUE` for some or
all of our combination levels:

``` r

combodf2 <- tribble(
  ~valname, ~label, ~levelcombo, ~exargs, ~is_control,
  "all_active", "All Xanomeline", c("Xanomeline High Dose", "Xanomeline Low Dose"), list(), FALSE,
  "placebo_redux", "Double Placebo!!", c("Placebo"), list(), TRUE
)

lyt4 <- basic_table() |>
  grouped_cols_w_diffs(trt_map, combo_map_df = combodf2)
#> none of the combination levels appeared in the colspan treatment map; adding them automatically.

build_table(lyt4, adsl)
#>                        Active Study Agent                                                                                                                                                        Risk Differences                                                                                                   
#>    Xanomeline High Dose   Xanomeline Low Dose   All Xanomeline   Placebo   Double Placebo!!   Xanomeline High Dose vs Placebo   Xanomeline Low Dose vs Placebo   All Xanomeline vs Placebo   Xanomeline High Dose vs Double Placebo!!   Xanomeline Low Dose vs Double Placebo!!   All Xanomeline vs Double Placebo!!
#> ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
```

As we can see, this allows us full flexibility to include combination
levels as both active and non-active virtual arms, including using them
in both sides of comparisons (even simultaneously).

#### Direct Control Of Comparisons

Finally, we have complete control over which comparisons appear in our
table via the `comp_map` argument. When non-null, the exact set of
comparisons specified in `comp_map` will appear in our table.

`comp_map` accepts a data.frame with the columns `active`, and
`comparator`, as well optional `active_is_combo` and
`comparator_is_combo` columns. The values in `active` and `comparator`
should be the *names* (as opposed to labels) of the respective arms:

``` r

comp_map1 <- tribble(
  ~active, ~comparator,
  "Xanomeline High Dose", "Placebo",
  "Xanomeline Low Dose", "placebo_redux",
  "all_active", "placebo_redux"
)


lyt5 <- basic_table() |>
  grouped_cols_w_diffs(trt_map, combo_map_df = combodf2, comp_map = comp_map1)
#> none of the combination levels appeared in the colspan treatment map; adding them automatically.

build_table(lyt5, adsl)
#>                        Active Study Agent                                                                                                    Risk Differences                                               
#>    Xanomeline High Dose   Xanomeline Low Dose   All Xanomeline   Placebo   Double Placebo!!   Xanomeline High Dose vs Placebo   Xanomeline Low Dose vs Double Placebo!!   All Xanomeline vs Double Placebo!!
#> ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
```

We can also use `comp_map` to compare one active treatment to another:

``` r

comp_map2 <- tribble(
  ~active, ~comparator,
  "Xanomeline High Dose", "Xanomeline Low Dose",
  "Xanomeline High Dose", "Placebo",
  "Xanomeline Low Dose", "Placebo"
)


lyt6 <- basic_table() |>
  grouped_cols_w_diffs(trt_map, comp_map = comp_map2)

build_table(lyt6, adsl)
#>                Active Study Agent                                                                        Risk Differences                                               
#>    Xanomeline High Dose   Xanomeline Low Dose   Placebo   Xanomeline High Dose vs Xanomeline Low Dose   Xanomeline High Dose vs Placebo   Xanomeline Low Dose vs Placebo
#> ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
```
