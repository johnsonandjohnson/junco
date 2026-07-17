The get_ref_info() refactor removes its local implementation of current-column path parsing and matching.

Before the refactor, get_ref_info() read the leaf .spl_context row, compared its cur_col_split values with the variable tokens in ref_path, and then compared cur_col_split_val values with the level tokens in ref_path. This duplicated the concepts introduced by #404.

The refactored implementation uses cur_col_split_path(.spl_context) to obtain one canonical, interleaved representation of the current path. It uses in_column() twice.

First, it replaces every level token in ref_path with "*". For example, the reference path c("colspan_trt", " ", "ARM", "B: Placebo") becomes c("colspan_trt", "*", "ARM", "*"). in_column() then checks whether the current column has the same split-variable structure, without requiring it to be the reference level. This decides whether a reference group is available for the current column.

Second, it calls in_column() with the original, exact ref_path. This decides whether the current column is itself the reference column.

This distinction is important. Treatment columns other than the control column need the reference group but are not reference columns. Columns outside the reference split hierarchy, such as Total columns or later layout-specific splits, should receive ref_group = NULL and in_ref_col = NULL. The wildcard structural check preserves that existing behavior.

The refactor also retains trt_var, ctrl_grp, and cur_col_val in the return value. cur_col_val is extracted by matching trt_var only against the variable positions in the interleaved current path, then selecting the corresponding value position. This avoids a subtle collision in which an earlier split value has the same text as the treatment variable name. A regression test covers this case.

get_ref_info() should remain the public global reference resolver. It is appropriate for custom analysis functions because it handles a NULL ref_path, determines whether a reference applies to the current column, retrieves the globally indexed reference data, and returns the metadata needed by analysis functions.

h_get_trtvar_refpath() overlaps with get_ref_info(), but it is not identical. It assumes that the last current column split is the treatment variable, and asserts that it agrees with the last variable specified by ref_path. That assumption is fragile in layouts with additional column splits, for example split_cols_by_multivar(). It also checks that ctrl_grp is a factor level of df[[trt_var]], while get_ref_info() does not require a data frame and deliberately does not perform this data-level validation.

The recommended direction for h_get_trtvar_refpath() is to keep it exported and superseded for now, so existing users do not experience an API break. Internal callers can gradually move to common reference-path metadata logic based on get_ref_info(), while preserving the factor-level validation in a dedicated validator or at the caller. It should only be unexported in a planned breaking release after external usage has been assessed.

a_summarize_aval_chg_diff_j() has a different reference-group construction strategy for a valid reason. It already uses get_ref_info() to obtain reference metadata such as trt_var and ctrl_grp. However, it constructs its reference group as .df_row[.df_row[[trt_var]] == ctrl_grp, ]. This is row-scoped: for example, it obtains the control records for the same parameter and visit as the current analysis. Its ANCOVA and difference calculations need that row-scoped dataset.

get_ref_info()$ref_group is global to the reference column context and is appropriate for standard global-reference analysis functions. Replacing the row-scoped construction in a_summarize_aval_chg_diff_j() with get_ref_info()$ref_group could pull a broader reference dataset and change the statistical scope. Therefore, the data-group construction should remain separate there.

The intended boundary is two layers. The context and path layer resolves ref_path, trt_var, ctrl_grp, the current treatment value, and whether the current column matches the reference structure. This belongs with get_ref_info(), cur_col_split_path(), and in_column(). The data-scope layer chooses whether the reference data is global, using get_ref_info()$ref_group, or restricted to the current row context, using .df_row filtered to ctrl_grp. This distinction avoids accidental statistical changes while removing duplicated path-matching logic.
