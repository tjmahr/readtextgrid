
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
#'
#' @details The `legacy_read_textgrid` functions are the original textgrid
#'   parsers provided by the package. They assume that the TextGrid file is a
#'   "long" format textgrid; this is the default format used by "Save a text
#'   file..." in Praat.
#'
#'   The current `read_textgrid()` functions are more
#'   flexible and can read in "short" format textgrids and textgrids with
#'   comments.
#'
#'   See <https://www.fon.hum.uva.nl/praat/manual/TextGrid_file_formats.html>
#'   for a description of the textgrid file format. Note that this package does
#'   not strictly adhere to format as described in this document. For example,
#'   the document says that numbers should be freestanding (surrounded by spaces
#'   or string boundaries), but Praat.exe can handle malformed numbers like
#'   `100ms`. Therefore, we tried to implement a parser that matched what Praat
#'   actually handles.
#'
#' @export
#' @order 1
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
#' @order 2
#' @export
read_textgrid_lines <- function(lines, file = NULL) {
  if (is.null(file)) {
    file <- NA_character_
  }

  stopifnot(str_detect_any(lines, "ooTextFile"))

  lines |>
    parse_textgrid_lines() |>
    tibble::add_column(file = file, .before = 1) |>
    tibble::as_tibble()
}

parse_textgrid_lines <- function(lines) {
  tg_text <- lines |>
    # collapse into one string
    stringr::str_c(collapse = "\n") |>
    # concat one trailing space
    stringr::str_c(" ")

  tg_tokens <- tokenize_textgrid(tg_text)
  tier_indices <- find_tier_boundaries(tg_tokens)
  tier_types <- tg_tokens[tier_indices$start] |> unlist()

  tier_info_df <- data.frame(
    tier_num = seq_along(tier_types),
    tier_type = tier_types,
    tier_start = tier_indices$start,
    tier_end = tier_indices$end
  )

  data <- tier_info_df |>
    split(~tier_num) |>
    lapply(parse_tier, tg_tokens = tg_tokens) |>
    dplyr::bind_rows()

  data[["tier_xmin"]] <- as.numeric(data[["tier_xmin"]])
  data[["tier_xmax"]] <- as.numeric(data[["tier_xmax"]])
  data[["xmin"]] <- as.numeric(data[["xmin"]])
  data[["xmax"]] <- as.numeric(data[["xmax"]])
  data[["tier_num"]] <- as.integer(data[["tier_num"]])
  data[["annotation_num"]] <- as.integer(data[["annotation_num"]])
  data[["text"]] <- as.character(data[["text"]])
  data
}


parse_tier <- function(tier_info, tg_tokens) {
  tier_tokens <- tg_tokens[tier_info$tier_start:tier_info$tier_end]

  # An empty Interval tier always has at least one interval. So it has that
  # at least 8 elements:
  # - (5) class, tier name, tier xmin, tier xmax, num intervals,
  # - (3) interval xmin, interval xmax, interval text
  # An empty Point tier has at least 5 elements
  # - (5) class, tier name, tier xmin, tier xmax, num points
  LENGTH_EMPTY_POINT_INTERVAL <- 5

  if (length(tier_tokens) == LENGTH_EMPTY_POINT_INTERVAL) {
    outer_df <- data.frame(
      tier_num = tier_info[["tier_num"]],
      tier_name = tier_tokens[[2]],
      tier_type = tier_tokens[[1]],
      tier_xmin = tier_tokens[[3]],
      tier_xmax = tier_tokens[[4]],
      xmin = NA_real_,
      xmax = NA_real_,
      text = NA_character_,
      annotation_num = NA_integer_
    )
    return(outer_df)
  }

  if (tier_info$tier_type == "IntervalTier") {
    marks_df <- make_intervals(tier_tokens, tg_tokens)
  }

  if (tier_info$tier_type == "TextTier") {
    marks_df <- make_points(tier_tokens, tg_tokens)
  }

  marks_df[["tier_num"]] <- tier_info[["tier_num"]]
  marks_df
}


make_intervals <- function(tier_tokens, tg_tokens) {
  # Skip first five elements (tier-level data)
  interval_data <- tier_tokens[-(1:5)]
  start_idx <- seq(1, length(interval_data) - 2, by = 3)

  data.frame(
    tier_num = NA_integer_,
    tier_name = tier_tokens[[2]],
    tier_type = tier_tokens[[1]],
    tier_xmin = tier_tokens[[3]],
    tier_xmax = tier_tokens[[4]],
    xmin = interval_data[start_idx] |> unlist(),
    xmax = interval_data[start_idx + 1] |> unlist(),
    text = interval_data[start_idx + 2] |> unlist(),
    annotation_num = seq_along(start_idx)
  )
}


make_points <- function(tier_tokens, tg_tokens) {
  # Skip first five elements (tier-level data)
  point_data <- tier_tokens[-(1:5)]
  start_idx <- seq(1, length(point_data) - 1, by = 2)

  data.frame(
    tier_num = NA_integer_,
    tier_name = tier_tokens[[2]],
    tier_type = tier_tokens[[1]],
    tier_xmin = tier_tokens[[3]],
    tier_xmax = tier_tokens[[4]],
    xmin = point_data[start_idx] |> unlist(),
    xmax = point_data[start_idx] |> unlist(),
    text = point_data[start_idx + 1] |> unlist(),
    annotation_num = seq_along(start_idx)
  )
}

tokenize_textgrid <- function(tg_text) {
  # .NUM_RE <- "^[+-]?\\d+(?:\\.\\d*)?(?:[eE][+-]?\\d+)?"

  # C++ scan for tokens
  res <- cpp_tg_scan_tokens(tg_text)
  toks <- res$tokens
  is_string <- res$is_string
  # Use R's number scanner to parse numbers
  res2 <- withr::with_locale(
    c(LC_NUMERIC = "C"),
    cpp_parse_praat_numbers(res$tokens)
  )
  numbers <- res2$value

  is_num <- !is.na(res2$value)
  keep <- is_num | is_string
  toks      <- toks[keep]
  is_string <- is_string[keep]
  numbers   <- numbers[keep & is_num]
  is_num    <- is_num[keep]

  if (!any(keep)) return(list())

  out <- vector("list", length(toks))
  out[is_num] <- numbers

  s <- toks[is_string]
  s <- substring(s, 2L, nchar(s) - 1L)
  s <- gsub('""', '"', s, fixed = TRUE)
  out[is_string] <- s

  out
}



find_tier_boundaries <- function(tg_tokens) {
  # TODO:
  # TextGrid_checkInvariants_e() in Praat source provides strong and weak
  # invariants
  # https://github.com/praat/praat.github.io/blob/master/fon/TextGrid.cpp#L1402


  # A textgrid interval might legitimately have the text "Tier" in it so
  # don't use regexes. Just consume tokens.
  num_tiers <- tg_tokens[[5]]
  tier_starts <- integer(num_tiers)
  tier_ends <- integer(num_tiers)
  tier_starts[1] <- 6L

  for (tier_i in seq_len(num_tiers)) {
    type <- tg_tokens[[tier_starts[tier_i]]]
    size <- tg_tokens[[tier_starts[tier_i] + 4]]
    # promote negative size to 0
    size <- max(c(0, size))

    if (type == "IntervalTier") {
      tier_end <- tier_starts[tier_i] + 4 + 3 * size
    } else {
      # 2 lines per point but they can have size 0
      tier_end <- tier_starts[tier_i] + 4 + 2 * size
    }

    if (tier_i != num_tiers) {
      tier_starts[tier_i + 1] <- tier_end + 1
    }
    tier_ends[tier_i] <- tier_end
  }

  tier_types <- tg_tokens[tier_starts] |> unlist()
  valid_tier_types <- tier_types |>
    is.element(c("IntervalTier", "TextTier")) |>
    all()

  if (!valid_tier_types) {
    rlang::abort("TextGrid appears misformatted")
  }

  list(start = tier_starts, end = tier_ends)
}


str_detect_any <- function(xs, pattern) {
  any(stringr::str_detect(xs, pattern))
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
#' 3. `"nested-intervals.TextGrid"` - A textgrid containing an `"utterance"`
#'    tier, a `"words"` tier, and a `"phones"` tier. This file is typical of
#'    forced alignment textgrids where utterances contain words which contain
#'    speech segments. In this case, alignment was made by hand so that word
#'    and phone boundaries do not correspond exactly.
#'
#' @export
example_textgrid <- function(which = 1) {
  choices <- c(
    "Mary_John_bell.TextGrid",
    "utf_16_be.TextGrid",
    "nested-intervals.TextGrid"
  )

  system.file(choices[which], package = "readtextgrid")
}
