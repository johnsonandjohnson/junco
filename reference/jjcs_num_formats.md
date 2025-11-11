# Numeric Formatting Function

Formatting setter for selected numerical statistics

## Usage

``` r
jjcs_num_formats(d, cap = 4)
```

## Arguments

- d:

  precision of individual values

- cap:

  cap to numerical precision (d \> cap – will use precision as if cap
  was specified as precision)

## Value

list:

- fmt : named vector with formatting function (jjcsformat_xx) for
  numerical stats: range, median, mean_sd, sd

- spec : named vector with formatting specifications for numerical
  stats: range, median, mean_sd, sd

## Examples

``` r
P1_precision <- jjcs_num_formats(d=0)$fmt
jjcs_num_formats(2)$fmt
#> $range
#> function (x, output, na_str = na_str_dflt) 
#> {
#>     if (anyNA(na_str) || (replace_na_dflt && any(na_str == "NA"))) {
#>         na_inds <- which(is.na(na_str) | (replace_na_dflt & na_str == 
#>             "NA"))
#>         na_str[na_inds] <- rep(na_str_dflt, length.out = length(na_str))[na_inds]
#>     }
#>     if (length(x) == 0 || isTRUE(all(x == ""))) {
#>         return(NULL)
#>     }
#>     else if (!length(positions[[1]]) == length(x)) {
#>         stop("Error: input str in call to jjcs_format_xx must contain same number of xx as the number of stats.")
#>     }
#>     values <- Map(y = x, fun = roundings, na_str = na_str, function(y, 
#>         fun, na_str) fun(y, na_str = na_str))
#>     regmatches(x = str, m = positions)[[1]] <- values
#>     return(str)
#> }
#> <bytecode: 0x561c8588f578>
#> <environment: 0x561c856925a8>
#> 
#> $mean_sd
#> function (x, output, na_str = na_str_dflt) 
#> {
#>     if (anyNA(na_str) || (replace_na_dflt && any(na_str == "NA"))) {
#>         na_inds <- which(is.na(na_str) | (replace_na_dflt & na_str == 
#>             "NA"))
#>         na_str[na_inds] <- rep(na_str_dflt, length.out = length(na_str))[na_inds]
#>     }
#>     if (length(x) == 0 || isTRUE(all(x == ""))) {
#>         return(NULL)
#>     }
#>     else if (!length(positions[[1]]) == length(x)) {
#>         stop("Error: input str in call to jjcs_format_xx must contain same number of xx as the number of stats.")
#>     }
#>     values <- Map(y = x, fun = roundings, na_str = na_str, function(y, 
#>         fun, na_str) fun(y, na_str = na_str))
#>     regmatches(x = str, m = positions)[[1]] <- values
#>     return(str)
#> }
#> <bytecode: 0x561c8588f578>
#> <environment: 0x561c85347678>
#> 
#> $sd
#> function (x, output, na_str = na_str_dflt) 
#> {
#>     if (anyNA(na_str) || (replace_na_dflt && any(na_str == "NA"))) {
#>         na_inds <- which(is.na(na_str) | (replace_na_dflt & na_str == 
#>             "NA"))
#>         na_str[na_inds] <- rep(na_str_dflt, length.out = length(na_str))[na_inds]
#>     }
#>     if (length(x) == 0 || isTRUE(all(x == ""))) {
#>         return(NULL)
#>     }
#>     else if (!length(positions[[1]]) == length(x)) {
#>         stop("Error: input str in call to jjcs_format_xx must contain same number of xx as the number of stats.")
#>     }
#>     values <- Map(y = x, fun = roundings, na_str = na_str, function(y, 
#>         fun, na_str) fun(y, na_str = na_str))
#>     regmatches(x = str, m = positions)[[1]] <- values
#>     return(str)
#> }
#> <bytecode: 0x561c8588f578>
#> <environment: 0x561c8530c7e8>
#> 
#> $median
#> function (x, output, na_str = na_str_dflt) 
#> {
#>     if (anyNA(na_str) || (replace_na_dflt && any(na_str == "NA"))) {
#>         na_inds <- which(is.na(na_str) | (replace_na_dflt & na_str == 
#>             "NA"))
#>         na_str[na_inds] <- rep(na_str_dflt, length.out = length(na_str))[na_inds]
#>     }
#>     if (length(x) == 0 || isTRUE(all(x == ""))) {
#>         return(NULL)
#>     }
#>     else if (!length(positions[[1]]) == length(x)) {
#>         stop("Error: input str in call to jjcs_format_xx must contain same number of xx as the number of stats.")
#>     }
#>     values <- Map(y = x, fun = roundings, na_str = na_str, function(y, 
#>         fun, na_str) fun(y, na_str = na_str))
#>     regmatches(x = str, m = positions)[[1]] <- values
#>     return(str)
#> }
#> <bytecode: 0x561c8588f578>
#> <environment: 0x561c85280448>
#> 
jjcs_num_formats(2)$spec
#>              range            mean_sd                 sd             median 
#>     "xx.xx, xx.xx" "xx.xxx (xx.xxxx)"          "xx.xxxx"           "xx.xxx" 
```
