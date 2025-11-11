# Relabel Variables in a Dataset

This function relabels variables in a dataset based on a provided list
of labels. It can either replace existing labels or only add labels to
variables without them.

## Usage

``` r
var_relabel_list(x, lbl_list, replace_existing = TRUE)
```

## Arguments

- x:

  (`data.frame`)  
  dataset containing variables to be relabeled.

- lbl_list:

  (`list`)  
  named list of labels to apply to variables.

- replace_existing:

  (`logical`)  
  if TRUE, existing labels will be replaced; if FALSE, only variables
  without labels will be updated.

## Value

The dataset with updated variable labels.
