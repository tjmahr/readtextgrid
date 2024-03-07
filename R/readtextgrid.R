
#' Read a textgrid file into a tibble
#'
#' @rdname read_textgrid
#' @param path a path to a textgrid
#' @param lines alternatively, the lines of a textgrid file
#' @param encoding the encoding of the textgrid. The default value `NULL` uses
#'   [readr::guess_encoding()] to guess the encoding of the textgrid. If an
#'   encoding is provided, it is forwarded to `[readr::locale()]` and
#'   `[readr::read_lines()]`.
#' @param file an optional value to use for the `file` column. For
#'   `read_textgrid()`, the default is the base filename of the input file. For
#'   `read_textgrid_lines()`, the default is `NA`.
#' @return a tibble with one row per textgrid annotation
#' @export
#' @examples
#' tg <- system.file("Mary_John_bell.TextGrid", package = "readtextgrid")
#' read_textgrid(tg)
read_textgrid <- function(path, file = NULL, encoding = NULL) {
  if (is.null(file)) {
    file <- basename(path)
  }

  if (is.null(encoding)) {
    encoding <- readr::guess_encoding(path)$encoding[1]
  }
  file_locale <- readr::locale(encoding = encoding)

  path |>
    readr::read_lines(locale = file_locale) |>
    read_textgrid_lines(file = file)
}

#' @rdname read_textgrid
#' @export
read_textgrid_lines <- function(lines, file = NULL) {
  if (is.null(file)) {
    file <- NA_character_
  }

  stopifnot(str_detect_any(lines, "ooTextFile"))

  lines |>
    parse_textgrid_lines() |>
    tibble::as_tibble() |>
    tibble::add_column(file = file, .before = 1)
}


#' Locate the path of an example textgrid file
#'
#' Locate the path of an example textgrid file
#'
#' @param which index of the textgrid to load
#' @return Path of `"Mary_John_bell.TextGrid"` bundled with the `readtextgrid`
#'   package.
#'
#' @details This function is a wrapper over [`system.file()`]  to locate the
#' paths to bundled textgrids. These files are used to test or demonstrate
#' functionality of the package.
#'
#' Two files are included:
#'
#' 1. `"Mary_John_bell.TextGrid"` - the default TextGrid created by Praat's
#'    Create TextGrid command. This file is saved as UTF-8 encoding.
#' 2. `"utf_16_be.TextGrid"` - a TextGrid with some IPA characters entered using
#'    Praat's IPA character selector. This file is saved with UTF-16 encoding.
#'
#' @export
example_textgrid <- function(which = 1) {
  basename <- c("Mary_John_bell.TextGrid", "utf_16_be.TextGrid")[which]
  system.file(basename, package = "readtextgrid")
}

parse_textgrid_lines <- function(lines) {
  lines |>
    slice_sections("item") |>
    purrr::map(parse_item_lines) |>
    plyr::ldply(as.data.frame, stringsAsFactors = FALSE)
}

slice_sections <- function(lines, section_head) {
  re <- sprintf("^\\s+%s \\[\\d+\\]:", section_head)
  starts <- stringr::str_which(lines, re)
  ends <- c(starts[-1] - 1, length(lines))
  purrr::map2(starts, ends, function(x, y) lines[seq(x, y, by = 1)])
}

parse_item_lines <- function(lines_items) {
  item_num <- lines_items[1] |>
    stringr::str_extract("\\d+") |>
    as.numeric()

  tier_type <- get_field(lines_items, "class")
  tier_name <- get_field(lines_items, "name")
  tier_xmin <- get_field_dbl(lines_items, "xmin")
  tier_xmax <- get_field_dbl(lines_items, "xmax")

  stopifnot(tier_type %in% c("IntervalTier", "TextTier"))

  if (tier_type == "IntervalTier") {
    df <- parse_interval_tier(lines_items)
  } else {
    df <- parse_point_tier(lines_items)
  }

  df[["xmin"]] <- as.numeric(df[["xmin"]])
  df[["xmax"]] <- as.numeric(df[["xmax"]])

  tibble::add_column(
    .data = df,
    tier_num  = item_num,
    tier_name = tier_name,
    tier_type = tier_type,
    tier_xmin = tier_xmin,
    tier_xmax = tier_xmax,
    .before = 1
  )
}

parse_interval_tier <- function(lines_interval_tier) {
  lines_interval_tier |>
    slice_sections("intervals") |>
    purrr::map(get_field_list, fields = c("xmin", "xmax", "text")) |>
    purrr::imap(add_annotation_num) |>
    plyr::ldply(as.data.frame, stringsAsFactors = FALSE)
}

parse_point_tier <- function(lines_point_tier) {
  no_points <- str_detect_any(lines_point_tier, "points: size = 0")

  if (!no_points) {
    df <- lines_point_tier |>
      slice_sections("points") |>
      purrr::map(get_field_list, fields = c("number", "mark")) |>
      purrr::imap(add_annotation_num) |>
      plyr::ldply(as.data.frame, stringsAsFactors = FALSE)

    # We treat points as zero-width intervals
    df[["xmin"]] <- df[["number"]]
    df[["xmax"]] <- df[["number"]]
    df[["text"]] <- df[["mark"]]
    df[["mark"]] <- NULL
    df[["number"]] <- NULL
  } else {
    # A point interval with no points should be represented in the results.
    df <- data.frame(
      xmin = NA,
      xmax = NA,
      text = NA_character_,
      annotation_num = NA,
      stringsAsFactors = FALSE
    )
  }

  df
}

add_annotation_num <- function(x, y) {
  x[["annotation_num"]] <- y
  x
}

get_field_list <- function(lines, fields) {
  stats::setNames(
    lapply(fields, function(x) get_field(lines, x)),
    fields
  )
}

# Find first match of "[field] = [value]", returning [value]
get_field <- function(lines, field) {
  re <- paste0("(?<=", field, " = ).+")

  lines |>
    stringr::str_extract(re) |>
    remove_na() |>
    utils::head(1) |>
    stringr::str_trim() |>
    str_unquote()
}

# Find first match of "[field] = [value]", returning [value]
get_field_dbl <- function(lines, field) {
  as.numeric(get_field(lines, field))
}

remove_na <- function(xs) {
  xs[!is.na(xs)]
}

str_unquote <- function(xs) {
  stringr::str_remove_all(xs, "^\"|\"$")
}

str_detect_any <- function(xs, pattern) {
    any(stringr::str_detect(xs, pattern))
}
