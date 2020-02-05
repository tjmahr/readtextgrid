
<!-- README.md is generated from README.Rmd. Please edit that file -->

# readtextgrid <img src="man/figures/logo.png" width = "150" align="right" />

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/tjmahr/readtextgrid.svg?branch=master)](https://travis-ci.org/tjmahr/readtextgrid)
[![CRAN
status](https://www.r-pkg.org/badges/version/readtextgrid)](https://CRAN.R-project.org/package=readtextgrid)
<!-- badges: end -->

readtextgrid parses Praat textgrids into R dataframes.

## Installation

Install from CRAN:

``` r
install.packages("readtextgrid")
```

Install the development version from Github:

``` r
install.packages("remotes")
remotes::install_github("tjmahr/readtextgrid")
```

## Basic example

Here is the example textgrid created by Praat. It was created using `New
-> Create TextGrid...` with default settings in Praat.

<img src="man/figures/demo-textgrid.png" width="600" />

This textgrid is bundled with this R package. We can locate the file
with `example_textgrid()`. We read in the textgrid with
`read_textgrid()`.

``` r
library(readtextgrid)

# Locates path to an example textgrid bundled with this package
tg <- example_textgrid()

read_textgrid(path = tg)
#> # A tibble: 3 x 10
#>   file                    tier_num tier_name tier_type    tier_xmin tier_xmax
#>   <chr>                      <dbl> <chr>     <chr>            <dbl>     <dbl>
#> 1 Mary_John_bell.TextGrid        1 Mary      IntervalTier         0         1
#> 2 Mary_John_bell.TextGrid        2 John      IntervalTier         0         1
#> 3 Mary_John_bell.TextGrid        3 bell      TextTier             0         1
#>    xmin  xmax text  annotation_num
#>   <dbl> <dbl> <chr>          <int>
#> 1     0     1 ""                 1
#> 2     0     1 ""                 1
#> 3    NA    NA  <NA>             NA
```

The dataframe contains one row per annotation: one row for each interval
on an interval tier and one row for each point on a point tier. If a
point tier has no points, it is represented with single row with `NA`
values.

The columns encode the following information:

  - `file` filename of the textgrid. By default this column uses the
    filename in `path`. A user can override this value by setting the
    `file` argument in `read_textgrid(path, file)`, which can be useful
    if textgrids are stored in speaker-specific folders.
  - `tier_num` the number of the tier (as in the left margin of the
    textgrid editor)
  - `tier_name` the name of the tier (as in the right margin of the
    textgrid editor)
  - `tier_type` the type of the tier. `"IntervalTier"` for interval
    tiers and `"TextTier"` for point tiers (this is the terminology used
    inside of the textgrid file format).
  - `tier_xmin`, `tier_xmax` start and end times of the tier in seconds
  - `xmin`, `xmax` start and end times of the textgrid interval or point
    tier annotation in seconds
  - `text` the text in the annotation
  - `annotation_num` the number of that annotation in that tier (1 for
    the first annotation, etc.)

## Reading in a directory of textgrids

Suppose you have a folder of textgrids for each speaker. As an example,
this package has a folder called `speaker_data` bundled with it
representing 5 five textgrids from 2 speakers.

    speaker-data
    +-- speaker001
    |   +-- s2T01.TextGrid
    |   +-- s2T02.TextGrid
    |   +-- s2T03.TextGrid
    |   +-- s2T04.TextGrid
    |   \-- s2T05.TextGrid
    \-- speaker002
        +-- s2T01.TextGrid
        +-- s2T02.TextGrid
        +-- s2T03.TextGrid
        +-- s2T04.TextGrid
        \-- s2T05.TextGrid

First, we create a set of paths to read into R.

``` r
# Get the path of the folder bundled with the package
data_dir <- system.file(package = "readtextgrid", "speaker-data")

# Get the full paths to all the textgrids
paths <- list.files(
  path = data_dir, 
  pattern = "TextGrid$",
  full.names = TRUE, 
  recursive = TRUE
)
```

We can use `purrr::map_dfr()` to read all this into R, but note that
this way loses the speaker information.

``` r
library(purrr)

map_dfr(paths, read_textgrid)
#> # A tibble: 150 x 10
#>    file           tier_num tier_name tier_type    tier_xmin tier_xmax  xmin
#>    <chr>             <dbl> <chr>     <chr>            <dbl>     <dbl> <dbl>
#>  1 s2T01.TextGrid        1 words     IntervalTier         0      1.35 0    
#>  2 s2T01.TextGrid        1 words     IntervalTier         0      1.35 0.297
#>  3 s2T01.TextGrid        1 words     IntervalTier         0      1.35 0.522
#>  4 s2T01.TextGrid        1 words     IntervalTier         0      1.35 0.972
#>  5 s2T01.TextGrid        2 phones    IntervalTier         0      1.35 0    
#>  6 s2T01.TextGrid        2 phones    IntervalTier         0      1.35 0.297
#>  7 s2T01.TextGrid        2 phones    IntervalTier         0      1.35 0.36 
#>  8 s2T01.TextGrid        2 phones    IntervalTier         0      1.35 0.495
#>  9 s2T01.TextGrid        2 phones    IntervalTier         0      1.35 0.522
#> 10 s2T01.TextGrid        2 phones    IntervalTier         0      1.35 0.621
#>     xmax text    annotation_num
#>    <dbl> <chr>            <int>
#>  1 0.297 ""                   1
#>  2 0.522 "bird"               2
#>  3 0.972 "house"              3
#>  4 1.35  ""                   4
#>  5 0.297 "sil"                1
#>  6 0.36  "B"                  2
#>  7 0.495 "ER1"                3
#>  8 0.522 "D"                  4
#>  9 0.621 "HH"                 5
#> 10 0.783 "AW1"                6
#> # ... with 140 more rows
```

We can use `purrr::map2_dfr()` and some dataframe manipulation to add
the speaker information.

``` r
library(dplyr)

# This tells read_textgrid to set the file column to the full path
data <- map2_dfr(paths, paths, read_textgrid) %>% 
  mutate(
    # basename() removes the folder part from a path, 
    # dirname() removes the file part from a path
    speaker = basename(dirname(file)),
    file = basename(file),
  ) %>% 
  select(
    speaker, everything()
  )

data
#> # A tibble: 150 x 11
#>    speaker    file           tier_num tier_name tier_type    tier_xmin tier_xmax
#>    <chr>      <chr>             <dbl> <chr>     <chr>            <dbl>     <dbl>
#>  1 speaker001 s2T01.TextGrid        1 words     IntervalTier         0      1.35
#>  2 speaker001 s2T01.TextGrid        1 words     IntervalTier         0      1.35
#>  3 speaker001 s2T01.TextGrid        1 words     IntervalTier         0      1.35
#>  4 speaker001 s2T01.TextGrid        1 words     IntervalTier         0      1.35
#>  5 speaker001 s2T01.TextGrid        2 phones    IntervalTier         0      1.35
#>  6 speaker001 s2T01.TextGrid        2 phones    IntervalTier         0      1.35
#>  7 speaker001 s2T01.TextGrid        2 phones    IntervalTier         0      1.35
#>  8 speaker001 s2T01.TextGrid        2 phones    IntervalTier         0      1.35
#>  9 speaker001 s2T01.TextGrid        2 phones    IntervalTier         0      1.35
#> 10 speaker001 s2T01.TextGrid        2 phones    IntervalTier         0      1.35
#>     xmin  xmax text    annotation_num
#>    <dbl> <dbl> <chr>            <int>
#>  1 0     0.297 ""                   1
#>  2 0.297 0.522 "bird"               2
#>  3 0.522 0.972 "house"              3
#>  4 0.972 1.35  ""                   4
#>  5 0     0.297 "sil"                1
#>  6 0.297 0.36  "B"                  2
#>  7 0.36  0.495 "ER1"                3
#>  8 0.495 0.522 "D"                  4
#>  9 0.522 0.621 "HH"                 5
#> 10 0.621 0.783 "AW1"                6
#> # ... with 140 more rows
```

Another strategy would be to read the textgrid dataframes into a list
column and `unnest()` them.

``` r
# Read dataframes into a list column
data_nested <- tibble(
  speaker = basename(dirname(paths)),
  data = map(paths, read_textgrid)
)
data_nested
#> # A tibble: 10 x 2
#>    speaker    data              
#>    <chr>      <list>            
#>  1 speaker001 <tibble [13 x 10]>
#>  2 speaker001 <tibble [15 x 10]>
#>  3 speaker001 <tibble [16 x 10]>
#>  4 speaker001 <tibble [12 x 10]>
#>  5 speaker001 <tibble [19 x 10]>
#>  6 speaker002 <tibble [13 x 10]>
#>  7 speaker002 <tibble [15 x 10]>
#>  8 speaker002 <tibble [16 x 10]>
#>  9 speaker002 <tibble [12 x 10]>
#> 10 speaker002 <tibble [19 x 10]>

tidyr::unnest(data_nested, "data")
#> # A tibble: 150 x 11
#>    speaker file  tier_num tier_name tier_type tier_xmin tier_xmax  xmin  xmax
#>    <chr>   <chr>    <dbl> <chr>     <chr>         <dbl>     <dbl> <dbl> <dbl>
#>  1 speake~ s2T0~        1 words     Interval~         0      1.35 0     0.297
#>  2 speake~ s2T0~        1 words     Interval~         0      1.35 0.297 0.522
#>  3 speake~ s2T0~        1 words     Interval~         0      1.35 0.522 0.972
#>  4 speake~ s2T0~        1 words     Interval~         0      1.35 0.972 1.35 
#>  5 speake~ s2T0~        2 phones    Interval~         0      1.35 0     0.297
#>  6 speake~ s2T0~        2 phones    Interval~         0      1.35 0.297 0.36 
#>  7 speake~ s2T0~        2 phones    Interval~         0      1.35 0.36  0.495
#>  8 speake~ s2T0~        2 phones    Interval~         0      1.35 0.495 0.522
#>  9 speake~ s2T0~        2 phones    Interval~         0      1.35 0.522 0.621
#> 10 speake~ s2T0~        2 phones    Interval~         0      1.35 0.621 0.783
#> # ... with 140 more rows, and 2 more variables: text <chr>,
#> #   annotation_num <int>
```

## Other tips

### Helpful columns

These three columns are helpful:

  - `duration` of an interval
  - `xmid` midpoint of an interval
  - `total_annotations` total number of annotations on a tier

<!-- end list -->

``` r
data %>%
  # grouping needed for counting annotations per tier per file per speaker
  group_by(speaker, file, tier_num) %>%
  mutate(
    duration = xmax - xmin,
    xmid = xmin + (xmax - xmin) / 2,
    total_annotations = sum(!is.na(annotation_num))
  ) %>% 
  ungroup() %>% 
  glimpse()
#> Observations: 150
#> Variables: 14
#> $ speaker           <chr> "speaker001", "speaker001", "speaker001", "speake...
#> $ file              <chr> "s2T01.TextGrid", "s2T01.TextGrid", "s2T01.TextGr...
#> $ tier_num          <dbl> 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1...
#> $ tier_name         <chr> "words", "words", "words", "words", "phones", "ph...
#> $ tier_type         <chr> "IntervalTier", "IntervalTier", "IntervalTier", "...
#> $ tier_xmin         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...
#> $ tier_xmax         <dbl> 1.348571, 1.348571, 1.348571, 1.348571, 1.348571,...
#> $ xmin              <dbl> 0.000, 0.297, 0.522, 0.972, 0.000, 0.297, 0.360, ...
#> $ xmax              <dbl> 0.297000, 0.522000, 0.972000, 1.348571, 0.297000,...
#> $ text              <chr> "", "bird", "house", "", "sil", "B", "ER1", "D", ...
#> $ annotation_num    <int> 1, 2, 3, 4, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4...
#> $ duration          <dbl> 0.29700000, 0.22500000, 0.45000000, 0.37657143, 0...
#> $ xmid              <dbl> 0.148500, 0.409500, 0.747000, 1.160286, 0.148500,...
#> $ total_annotations <int> 4, 4, 4, 4, 9, 9, 9, 9, 9, 9, 9, 9, 9, 4, 4, 4, 4...
```

### Launching Praat

*This tip is written from the perspective of a Windows user who uses git
bash for a terminal*. To open textgrids in Praat, you can tell R to call
Praat from the command line. You have to know where the location of the
Praat binary is though. I like to keep a copy in my project directories,
so something like this would open 10 textgrids in Praat.

``` r
system2(
  command = "./Praat.exe",
  args = c("--open", paths),
  wait = FALSE
)
```

## Limitations

readtextgrid supports textgrids created by Praat by using `Save as text
file...`. It uses a parsing strategy based on regular expressions
targeting indentation patterns and text flags in the file format. The
[formal specification of the textgrid
format](http://www.fon.hum.uva.nl/praat/manual/TextGrid_file_formats.html),
however, is much more flexible. As a result, not every textgrid that
Praat can open—especially the minimal “short text” files—is compatible
with this package.

## Acknowledgments

readtextgrid was created to process data from the [WISC Lab
project](https://kidspeech.wisc.edu/). Thus, development of this package
was supported by NIH R01DC009411 and NIH R01DC015653.

-----

Please note that the ‘readtextgrid’ project is released with a
[Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md). By
contributing to this project, you agree to abide by its terms.
