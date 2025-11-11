# Adding Labels To Variables For Model

Adding Labels To Variables For Model

## Usage

``` r
h_is_specified(x, vars)

h_is_specified_and_in_data(x, vars, data)

h_check_and_get_label(x, vars, data)

h_labels(vars, data)
```

## Arguments

- x:

  (`character`)  
  an element in vars.

- vars:

  (`list`)  
  variables to use.

- data:

  (`data.frame`)  
  data to use.

## Functions

- `h_is_specified()`: checks if element in `vars` is not `NULL` and not
  empty.

- `h_is_specified_and_in_data()`: checks if element in vars is not NULL
  and exists in dataset.

- `h_check_and_get_label()`: gets label for each element in vars.

- `h_labels()`: returns the list of variables with labels.
