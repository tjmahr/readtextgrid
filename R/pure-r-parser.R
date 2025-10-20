# Implementation of the textgrid parsing written in pure R.
# This was ported to C++ for speed but it's important to have around
# for unit tests as a reference implementation.

r_read_textgrid <- function(path, file = NULL, encoding = NULL) {
  if (is.null(file)) {
    file <- basename(path)
  }

  if (is.null(encoding)) {
    encoding <- readr::guess_encoding(path)$encoding[1]
  }
  file_locale <- readr::locale(encoding = encoding)

  path |>
    readr::read_lines(locale = file_locale) |>
    r_read_textgrid_lines(file = file)
}

r_read_textgrid_lines <- function(lines, file = NULL) {
  if (is.null(file)) {
    file <- NA_character_
  }

  stopifnot(str_detect_any(lines, "ooTextFile"))

  lines |>
    r_parse_textgrid_lines() |>
    tibble::add_column(file = file, .before = 1) |>
    tibble::as_tibble()
}

r_parse_textgrid_lines <- function(lines) {
  tg_characters <- lines |>
    # collapse into one string
    stringr::str_c(collapse = "\n") |>
    # concat one trailing space
    stringr::str_c(" ") |>
    # split into individual characters
    stringr::str_split("") |>
    unlist()

  tg_tokens <- r_tokenize_textgrid_chars(tg_characters)
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

r_tokenize_textgrid_chars <- function(all_char) {
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
      if (r_tg_parse_is_number(total_value)) {
        # Keep only the numeric part.
        total_value <- total_value |> r_tg_parse_extract_number()
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
    lapply(r_tg_parse_convert_value)
}

# A numeric token is:
# string start
# (optional minus sign)
# digit(s)
# (optional decimal point and digit(s))
r_tg_parse_is_number <- function(x) {
  .NUM_RE <- "^[+-]?\\d+(?:\\.\\d*)?(?:[eE][+-]?\\d+)?"
  stringr::str_detect(x, .NUM_RE)
}

r_tg_parse_extract_number <- function(x) {
  .NUM_RE <- "^[+-]?\\d+(?:\\.\\d*)?(?:[eE][+-]?\\d+)?"
  x |>
    stringr::str_extract(.NUM_RE) |>
    as.numeric()
}

r_tg_parse_convert_value <- function(x) {
  if (is.character(x)) {
    # unquote strings
    x <- substr(x, 2, nchar(x) - 1)
    # undo "" escapement
    x <- stringr::str_replace_all(x, "\"\"", "\"")
  }
  x
}
