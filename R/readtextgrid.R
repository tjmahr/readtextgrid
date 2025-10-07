
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

#' @import rlang
parse_textgrid_lines <- function(lines) {

  # lines <- testthat::test_path("test-data/hard-to-parse.TextGrid") |> readLines()
  lines <- testthat::test_path("test-data/Mary_John_bell.TextGrid") |> readLines()

  tg_characters <- lines |>
    stringr::str_squish() |>
    # collapse into one string
    stringr::str_c(collapse = "\n") |>
    # concat one trailing space
    stringr::str_c(" ") |>
    # split into individual characters
    stringr::str_split("") |>
    unlist()

  tg_list <- char_to_value_list(tg_characters)
  tier_idces <- validate_tg_list(tg_list)
  tier_types <- tg_list[tier_idces] |> unlist()

  tier_df <- tibble::tibble(
    tier_type = tier_types,
    tier_start = tier_idces,
    tier_end = dplyr::lead(
      tier_idces - 1,
      default = length(tg_list)
    )
  ) |>
    dplyr::mutate(
      tier_num = dplyr::row_number(),
      .before = 1
    )

  tier_df |>
    tidyr::nest(.by = "tier_num", .key = "data") |>
    dplyr::mutate(
      marks = purrr::map(
        !!sym("data"),
        ~parse_tier(.x, tg_list)
      )
    ) |>
    tidyr::unnest(!!sym("marks")) |>
    dplyr::select(-!!sym("data"))
}

parse_tier <- function(tier_df, tg_list) {
  tier_list <- tg_list[tier_df$tier_start:tier_df$tier_end]
  outer_df <- tibble::tibble(
    tier_name = tier_list[[2]],
    tier_type = tier_list[[1]],
    tier_xmin = tier_list[[3]],
    tier_xmax = tier_list[[4]]
  )

  if (tier_df$tier_end - tier_df$tier_start < 5) {
    return(outer_df)
  }

  if (tier_df$tier_type == "IntervalTier") {
    marks_df <- make_intervals(tier_df, tg_list)
  }

  if (tier_df$tier_type == "TextTier") {
    marks_df <- make_points(tier_df, tg_list)
  }

  outer_df |>
    dplyr::cross_join(marks_df)
}

make_intervals <- function(tier_df, tg_list) {
  start_idx <- tier_df$tier_start + 5
  end_idx <- tier_df$tier_end - 2
  purrr::map(
    seq(start_idx, end_idx, by = 3),
    \(idx){
      tibble::tibble(
        xmin = tg_list[[idx]],
        xmax = tg_list[[idx + 1]],
        text = tg_list[[idx + 2]]
      )
    }
  ) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      annotation_num = dplyr::row_number()
    )
}

make_points <- function(tier_df, tg_list) {
  start_idx <- tier_df$tier_start + 5
  end_idx <- tier_df$tier_end - 1
  purrr::map(
    seq(start_idx, end_idx, by = 2),
    \(idx){
      tibble::tibble(
        xmin = tg_list[[idx]],
        text = tg_list[[idx + 1]]
      )
    }
  ) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      annotation_num = dplyr::row_number()
    )
}

#' @import rlang
char_to_value_list <- function(all_char, call = caller_env()) {
  all_char <- tg_characters
  in_char <- FALSE
  in_comment <- FALSE
  in_escaped_quote <- FALSE

  # Collects characters into values
  cur_value <- vector()
  # Collects completed values
  values <- vector(mode = "list")

  for (i in seq_along(all_char)) {
    c <- all_char[i]
    whitespace_c <- c %in% c(" ", "\n")

    # Comments start with ! and end with \n so we skip them
    if (!in_char & c == "!") {
      in_comment <- TRUE
      next
    }
    if (in_comment & c == "\n") {
      in_comment <- FALSE
      next
    }

    # Store character if ending an escaped quote
    if (in_escaped_quote) {
      cur_value <- c(cur_value, c)
      in_escaped_quote <- !in_escaped_quote
      next
    }

    # Start or close character mode if I see "
    if (c == "\"" & !in_comment) {
      # Check for "" escapes
      peek_c <- all_char[i + 1]
      if (peek_c == "\"" & in_char) {
        in_escaped_quote <- TRUE
      } else {
        # Store closing quote
        if (in_char) cur_value <- c(cur_value, c)
        in_char <- !in_char
      }
    }

    # Todo: Adjust the next two so they write out the buffer if character mode
    # is closing (I see a space and I just closed character mode) or if the
    # buffer has a number (I see a space and the buffer is only number
    # characters or a decimal point.)

    # Collect values if I see a space and I am not in character mode
    if (whitespace_c & !in_char & !in_comment & length(cur_value) > 0) {
      total_value <- stringr::str_c(cur_value, collapse = "")
      values <- c(values, total_value)
      cur_value <- vector()
      next
    }

    # Store characters if they are numeric
    if (!in_char & !in_comment & stringr::str_detect(c, r"{[\d\.]}")) {
      cur_value <- c(cur_value, c)
      next
    }

    # Store characters
    if (in_char) {
      cur_value <- c(cur_value, c)
      next
    }
  }
  # strip initial double quotes from strings
  # convert numbers to numbers
  values |>
    lapply(convert_value)

  # values |>
  #   lapply(function(x) if (startsWith(x, "\"")) x else as.numeric(x))

  # values |>
  #   purrr::map(
  #     ~ ifelse(
  #       stringr::str_sub(.x, 1, 1) != "\"",
  #       as.numeric(.x),
  #       stringr::str_sub(.x, 2, -1)
  #     )
  #   )
}

convert_value <- function(x) {
  v <- type.convert(x, as.is = TRUE, tryLogical = FALSE)
  if (is.character(v)) {
    # unquote strings
    v <- substr(v, 2, nchar(v) - 1)
    # undo "" escapement
    v <- stringr::str_replace_all(v, "\"\"", "\"")
  }
  v
}


#' @import rlang
validate_tg_list <- function(tg_list, call = caller_env()) {
  tier_idces <- tg_list |>
    purrr::map(
      ~ stringr::str_detect(.x, "Tier")
    ) |>
    unlist() |>
    which()


  if (min(tier_idces) != 6) {
    cli::cli_abort("TextGrid appears misformatted", call = call)
  }

  tier_types <- tg_list[tier_idces] |>
    unlist() |>
    stringr::str_remove("Tier")

  tier_idces2 <- c(tier_idces, length(tg_list) + 1)
  tier_length <- diff(tier_idces2) - 5

  correct_len <- purrr::map2(
    tier_types,
    tier_length,
    \(ttype, tlen){
      if (ttype == "Interval") {
        return(tlen %% 3 == 0)
      }
      if (ttype == "Text") {
        return(tlen %% 2 == 0)
      }
    }
  ) |>
    unlist()

  if (!all(correct_len)) {
    cli::cli_abort("TextGrid appears misformatted", call = call)
  }

  tier_idces
}


str_detect_any <- function(xs, pattern) {
  any(stringr::str_detect(xs, pattern))
}
