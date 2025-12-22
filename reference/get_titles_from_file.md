# Get Titles/Footers For Table From Sources

Retrieves the titles and footnotes for a given table from a CSV/XLSX
file or a data.frame.

## Usage

``` r
get_titles_from_file(
  id,
  file = .find_titles_file(input_path),
  input_path = ".",
  title_df = .read_titles_file(file)
)
```

## Arguments

- id:

  (`character(1)`)  
  The identifier for the table of interest.

- file:

  (`character(1)`)  
  A path to CSV or xlsx file containing title and footer information for
  one or more outputs. See Details. Ignored if `title_df` is specified.

- input_path:

  (`character(1)`)  
  A path to look for titles.csv/titles.xlsx. Ignored if `file` or
  `title_df` is specified.

- title_df:

  (`data.frame`)  
  A data.frame containing titles and footers for one or more outputs.
  See Details.

## Value

List object containing: title, subtitles, main_footer, prov_footer for
the table of interest. Note: the subtitles and prov_footer are currently
set to NULL. Suitable for use with
[`set_titles()`](https://johnsonandjohnson.github.io/junco/reference/set_titles.md).

## Details

Retrieves the titles for a given output id (see below) and outputs a
list containing the title and footnote objects supported by rtables.
Both titles.csv and titles.xlsx (*if `readxl` is installed*) files are
supported, with titles.csv being checked first.

         Data is expected to have `TABLE ID`, `IDENTIFIER`, and `TEXT` columns,
         where `IDENTIFIER` has the value `TITLE` for a title and `FOOT*` for
         footer materials where `*` is a positive integer. `TEXT` contains
         the value of the title/footer to be applied.
