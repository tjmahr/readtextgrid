
# library(readtextgrid)
devtools::load_all()


# Get the path of the folder bundled with the package
data_dir <- system.file(package = "readtextgrid", "speaker-data")

# Get the full paths to all the textgrids
paths <- list.files(
  path = data_dir,
  pattern = "TextGrid$",
  full.names = TRUE,
  recursive = TRUE
)

path <- testthat::test_path("test-data/quoted.TextGrid")

# b <- bench::mark(
#   new = read_textgrid(path, encoding = "UTF-8"),
#   new_guess = read_textgrid(path),
#   old = legacy_read_textgrid(path, encoding = "UTF-8"),
#   old_guess = legacy_read_textgrid(path),
#   min_iterations = 10,
#   check = TRUE
# )
# summary(b)
#




tg_characters <- readLines(path) |>
  # collapse into one string
  stringr::str_c(collapse = "\n") |>
  # concat one trailing space
  stringr::str_c(" ") |>
  # split into individual characters
  stringr::str_split("") |>
  unlist()

# readtextgrid:::tokenize_textgrid_chars(tg_characters)

tg_text <- readLines(path) |>
  # collapse into one string
  stringr::str_c(collapse = "\n") |>
  # concat one trailing space
  stringr::str_c(" ")
tokenize_textgrid_cpp(tg_text)


path <- testthat::test_path("test-data/nested-intervals.TextGrid")

bench::mark(
  read_textgrid(path),
  readtextgrid:::r_read_textgrid(path),
  legacy_read_textgrid(path),
  min_time = 1
)







tg_char_bounds <- tg_text |>
  stringi::stri_locate_all_boundaries(type = "character") |>
  _[[1]]

tg_chars <- tg_text |>
  stringi::stri_sub(
    from = tg_char_bounds[, 1],
    to = tg_char_bounds[, 2]
  )





tokenize_textgrid_chars_old <- function(all_char) {
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
    lapply(tg_parse_convert_value2)
}

# A numeric token is:
# string start
# (optional minus sign)
# digit(s)
# (optional decimal point and digit(s))
tg_parse_is_number <- function(x) {
  stringr::str_detect(x, "^-?\\d+(\\.\\d*)?")
}

tg_parse_convert_value2 <- function(x) {
  v <- utils::type.convert(x, as.is = TRUE, tryLogical = FALSE)
  if (is.character(v)) {
    # unquote strings
    v <- substr(v, 2, nchar(v) - 1)
    # undo "" escapement
    v <- stringr::str_replace_all(v, "\"\"", "\"")
  }
  v
}


tokenize_textgrid_chars_char_vec <- function(all_char) {
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
  values <- character(0)             # Collects completed values

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



tokenize_textgrid_chars_assign <- function(all_char) {
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

  token_start <- integer(0)             # Start of current token
  values <- vector(100, mode = "list")  # Collects completed values
  cur_value <- 0
  push <- FALSE

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
        push <- TRUE
      } else if (is_string) {
        push <- TRUE
      }

      if (push) {
        cur_value <- cur_value + 1
        if (cur_value > length(values)) {
          values <- c(values, vector(length(values), mode = "list"))
        }
        values[[cur_value]] <- total_value
        push <- FALSE
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

  values[seq_len(cur_value)] |>
    lapply(tg_parse_convert_value)
}


a <- tokenize_textgrid_chars(tg_text, tg_chars, tg_char_bounds)
b <- tokenize_textgrid_chars_old(tg_characters)

bench::mark(
  tokenize_textgrid_chars(tg_characters),
  tokenize_textgrid_chars_old(tg_characters),
  min_time = 1
)
