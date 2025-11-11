# Tabulation of RBMI Results

**\[stable\]**

These functions can be used to produce tables from RBMI.

## Usage

``` r
h_tidy_pool(x, visit_name, group_names)

s_rbmi_lsmeans(df, .in_ref_col, show_relative = c("reduction", "increase"))

a_rbmi_lsmeans(
  df,
  ref_path,
  .spl_context,
  ...,
  .stats = NULL,
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL
)
```

## Arguments

- x:

  (`list`)  
  is a list of pooled object from `rbmi` analysis results. This list
  includes analysis results, confidence level, hypothesis testing type.

- visit_name:

  (`string`)  
  single visit level.

- group_names:

  (`character`)  
  group levels.

- df:

  (`data.frame`)  
  input with LS means results.

- .in_ref_col:

  (`flag`)  
  whether reference column is specified.

- show_relative:

  (`string`)  
  'reduction' if (`control - treatment`, default) or 'increase'
  (`treatment - control`) of relative change from baseline?

- ref_path:

  (`character`)  
  global reference group specification, see
  [`get_ref_info()`](https://johnsonandjohnson.github.io/junco/reference/get_ref_info.md).

- .spl_context:

  (`data.frame`)  
  gives information about ancestor split states that is passed by
  `rtables`.

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

## Value

The `data.frame` with results of pooled analysis for a single visit.

A list of statistics extracted from a tidied LS means data frame.

## Functions

- `h_tidy_pool()`: Helper function to produce data frame with results of
  pool for a single visit.

- `s_rbmi_lsmeans()`: Statistics function which is extracting estimates
  from a tidied RBMI results data frame.

- `a_rbmi_lsmeans()`: Formatted Analysis function which is used as
  `afun`.

## Note

These functions have been forked from `tern.rbmi`. Additional features
are:

- Additional `ref_path` argument.

- Extraction of variance statistics in the
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) method.

- Adapted to `rbmi` forked functions update with more than two treatment
  groups.
