# Standard Arguments

The documentation to this function lists all the arguments in `tern`
that are used repeatedly to express an analysis.

## Arguments

- ...:

  additional arguments for the lower level functions.

- .aligns:

  (`character`)  
  alignment for table contents (not including labels). When `NULL`,
  `'center'` is applied. See
  [`formatters::list_valid_aligns()`](https://insightsengineering.github.io/formatters/latest-tag/reference/list_formats.html)
  for a list of all currently supported alignments.

- .all_col_counts:

  (`vector` of `integer`)  
  each value represents a global count for a column. Values are taken
  from `alt_counts_df` if specified (see
  [`rtables::build_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/build_table.html)).

- .df_row:

  (`data.frame`)  
  data frame across all of the columns for the given row split.

- .formats:

  (named `character` or `list`)  
  formats for the statistics. See Details in `analyze_vars` for more
  information on the `'auto'` setting.

- .in_ref_col:

  (`logical`)  
  `TRUE` when working with the reference level, `FALSE` otherwise.

- .indent_mods:

  (named `integer`)  
  indent modifiers for the labels. Defaults to 0, which corresponds to
  the unmodified default behavior. Can be negative.

- .labels:

  (named `character`)  
  labels for the statistics (without indent).

- .N_col:

  (`integer`)  
  column-wise N (column count) for the full column being analyzed that
  is typically passed by `rtables`.

- .N_row:

  (`integer`)  
  row-wise N (row group count) for the group of observations being
  analyzed (i.e. with no column-based subsetting) that is typically
  passed by `rtables`.

- .ref_group:

  (`data.frame` or `vector`)  
  the data corresponding to the reference group.

- ref_path:

  (`character`)  
  global reference group specification, see
  [`get_ref_info()`](https://johnsonandjohnson.github.io/junco/reference/get_ref_info.md).

- .spl_context:

  (`data.frame`)  
  gives information about ancestor split states that is passed by
  `rtables`.

- .stats:

  (`character`)  
  statistics to select for the table.

- .var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

- add_total_level:

  (`flag`)  
  adds a 'total' level after the others which includes all the levels
  that constitute the split. A custom label can be set for this level
  via the `custom_label` argument.

- alternative:

  (`string`)  
  whether `two.sided`, or one-sided `less` or `greater` p-value should
  be displayed.

- col_by:

  (`factor`)  
  defining column groups.

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- control:

  (`list`)  
  relevant list of control options.

- data:

  (`data.frame`)  
  the dataset containing the variables to summarize.

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- draw:

  (`flag`)  
  whether the plot should be drawn.

- exclude_levels:

  (`list`)  
  A named list where names correspond to split variables and values are
  vectors of levels to exclude.

- grp:

  (`factor`)  
  defining the groups.

- groups_lists:

  (named `list` of `list`)  
  optionally contains for each `subgroups` variable a list, which
  specifies the new group levels via the names and the levels that
  belong to it in the character vectors that are elements of the list.

- id:

  (`string`)  
  subject variable name.

- is_event:

  (`character`)  
  variable name storing Logical values: `TRUE` if event, `FALSE` if time
  to event is censored.

- indent_mod:

  **\[deprecated\]** Please use the `.indent_mods` argument instead.

- label_all:

  (`string`)  
  label for the total population analysis.

- labelstr:

  (`character`)  
  label of the level of the parent split currently being summarized
  (must be present as second argument in Content Row Functions). See
  [`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html)
  for more information.

- lyt:

  (`layout`)  
  input layout where analyses will be added to.

- method:

  (`string`)  
  specifies the test used to calculate the p-value for the difference
  between two proportions. For options, see
  [`tern::s_test_proportion_diff()`](https://insightsengineering.github.io/tern/latest-tag/reference/prop_diff_test.html).
  Default is `NULL` so no test is performed.

- na.rm:

  (`flag`)  
  whether `NA` values should be removed from `x` prior to analysis.

- na_level:

  **\[deprecated\]** Please use the `na_str` argument instead.

- na_str:

  (`string`)  
  string used to replace all `NA` or empty values in the output.

- nested:

  (`flag`)  
  whether this layout instruction should be applied within the existing
  layout structure *if possible* (`TRUE`, the default) or as a new
  top-level element (`FALSE`). Ignored if it would nest a split.
  underneath analyses, which is not allowed.

- newpage:

  (`flag`)  
  whether the plot should be drawn on a new page. Only considered if
  `draw = TRUE` is used.

- prune_zero_rows:

  (`flag`)  
  whether to prune all zero rows.

- riskdiff:

  (`flag`)  
  whether a risk difference column is present. When set to `TRUE`,
  [`tern::add_riskdiff()`](https://insightsengineering.github.io/tern/latest-tag/reference/add_riskdiff.html)
  must be used as `split_fun` in the prior column split of the table
  layout, specifying which columns should be compared. See
  [`tern::stat_propdiff_ci()`](https://insightsengineering.github.io/tern/latest-tag/reference/stat_propdiff_ci.html)
  for details on risk difference calculation.

- rsp:

  (`logical`)  
  whether each subject is a responder or not.

- section_div:

  (`string`)  
  string which should be repeated as a section divider after each group
  defined by this split instruction, or `NA_character_` (the default)
  for no section divider.

- show_labels:

  (`string`)  
  label visibility: one of 'default', 'visible' and 'hidden'.

- show_relative:

  (`string`)  
  should the 'reduction' (`control - treatment`, default) or the
  'increase' (`treatment - control`) be shown for the relative change
  from baseline?

- strata:

  (`character` or `NULL`)  
  variable names indicating stratification factors.

- table_names:

  (`character`)  
  this can be customized in case that the same `vars` are analyzed
  multiple times, to avoid warnings from `rtables`.

- tte:

  (`numeric`)  
  contains time-to-event duration values.

- var_labels:

  (`character`)  
  character for label.

- variables:

  (named `list` of `string`)  
  list of additional analysis variables.

- vars:

  (`character`)  
  variable names for the primary analysis variable to be iterated over.

- var:

  (`string`)  
  single variable name for the primary analysis variable.

- x:

  (`numeric`)  
  vector of numbers we want to analyze.

- ctrl_grp:

  (`string`)  
  Level of the control group for the relative risk derivation.

## Details

Although this function just returns `NULL` it has two uses, for the
`tern` users it provides a documentation of arguments that are commonly
and consistently used in the framework. For the developer it adds a
single reference point to import the `roxygen` argument description
with: `@inheritParams proposal_argument_convention`
