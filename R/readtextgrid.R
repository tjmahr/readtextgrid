
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
  tg_characters <- lines |>
    # collapse into one string
    stringr::str_c(collapse = "\n") |>
    # concat one trailing space
    stringr::str_c(" ") |>
    # split into individual characters
    stringr::str_split("") |>
    unlist()

  tg_tokens <- tokenize_textgrid_chars(tg_characters)
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


tokenize_textgrid_chars <- function(all_char) {
  # The parser rules here follow the textgrid specifications
  # <https://www.fon.hum.uva.nl/praat/manual/TextGrid_file_formats.html> EXCEPT
  # when they contradict the behavior of Praat.exe. For example, the specs says
  # the main literals are freestanding strings and numbers, where freestanding
  # means that they have a whitespace or boundary (newline or file start/end).
  # But Praat.exe can handle numbers like "10.00!comment". So, this parser
  # gathers freestanding literals but only keeps ones that are strings or
  # start with a valid number (the non-numeric characters are lopped off.)

  in_strong_comment <- FALSE         # Comment mode: ! to new line \n
  in_string <- FALSE                 # String mode: "Quote to quote"
  in_escaped_quote <- FALSE          # Escaped quote: "" inside of a string

  token_start <- integer(0)          # Start of current token
  values <- vector(mode = "list")    # Collects completed values

  for (i in seq_along(all_char)) {
    cur_value_ready <- length(token_start) != 0
    c <- all_char[i]
    c_is_whitespace <- c %in% c(" ", "\n")
    c_starts_string <- c == "\""

    # Comments start with ! and end with \n. Skip characters in this mode.
    if (!in_string & c == "!") {
      in_strong_comment <- TRUE
      next
    }
    if (in_strong_comment) {
      if (c == "\n") in_strong_comment <- FALSE
      next
    }

    # Whitespace delimits values so collect values if we see whitespace
    if (c_is_whitespace & !in_string) {
      # Skip whitespace if no values collected so far
      if (!cur_value_ready) next

      total_value <- all_char[seq(token_start, i - 1)] |>
        paste0(collapse = "")
      is_string <- all_char[token_start] == "\"" && all_char[i - 1] == "\""

      # Collect only numbers and strings
      if (tg_parse_is_number(total_value)) {
        # Keep only the numeric part.
        total_value <- stringr::str_extract(total_value, "^-?\\d+(\\.\\d*)?")
        values <- c(values, total_value)
      } else if (is_string) {
        values <- c(values, total_value)
      }
      token_start <- integer(0)
      next
    }

    # Store character if ending an escaped quote
    if (in_escaped_quote) {
      in_escaped_quote <- !in_escaped_quote
      next
    }

    # Start or close string mode if we see "
    if (c_starts_string) {
      # Check for "" escapes
      peek_c <- all_char[i + 1]
      if (peek_c == "\"" & in_string) {
        in_escaped_quote <- TRUE
      } else {
        in_string <- !in_string
      }
    }

    if (!cur_value_ready) {
      token_start <- i
    }
  }

  values |>
    lapply(tg_parse_convert_value)
}

# A numeric token is:
# string start
# (optional minus sign)
# digit(s)
# (optional decimal point and digit(s))
tg_parse_is_number <- function(x) {
  stringr::str_detect(x, "^-?\\d+(\\.\\d*)?")
}

tg_parse_convert_value <- function(x) {
  v <- utils::type.convert(x, as.is = TRUE, tryLogical = FALSE)
  if (is.character(v)) {
    # unquote strings
    v <- substr(v, 2, nchar(v) - 1)
    # undo "" escapement
    v <- stringr::str_replace_all(v, "\"\"", "\"")
  }
  v
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






