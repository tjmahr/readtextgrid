# Old version of parsing code


legacy_read_textgrid <- function(path, file = NULL, encoding = NULL) {
  if (is.null(file)) {
    file <- basename(path)
  }

  if (is.null(encoding)) {
    encoding <- readr::guess_encoding(path)$encoding[1]
  }
  file_locale <- readr::locale(encoding = encoding)

  path |>
    readr::read_lines(locale = file_locale) |>
    legacy_read_textgrid_lines(file = file)
}

legacy_read_textgrid_lines <- function(lines, file = NULL) {
  if (is.null(file)) {
    file <- NA_character_
  }

  stopifnot(str_detect_any(lines, "ooTextFile"))

  lines |>
    .v1_parse_textgrid_lines() |>
    tibble::as_tibble() |>
    tibble::add_column(file = file, .before = 1) |>
    dplyr::mutate(
      tier_name = .v1_str_unescape_quote(.data$tier_name),
      text = .v1_str_unescape_quote(.data$text)
    )
}

.v1_parse_textgrid_lines <- function(lines) {
  lines |>
    .v1_slice_sections("item") |>
    purrr::map(.v1_parse_item_lines) |>
    dplyr::bind_rows()
}

.v1_slice_sections <- function(lines, section_head) {
  re <- sprintf("^\\s+%s ?\\[\\d+\\]:?", section_head)
  starts <- stringr::str_which(lines, re)
  ends <- c(starts[-1] - 1, length(lines))
  purrr::map2(starts, ends, function(x, y) lines[seq(x, y, by = 1)])
}

.v1_parse_item_lines <- function(lines_items) {
  item_num <- lines_items[1] |>
    stringr::str_extract("\\d+") |>
    as.numeric()

  tier_type <- .v1_get_field(lines_items, "class")
  tier_name <- .v1_get_field(lines_items, "name")
  tier_xmin <- .v1_get_field_dbl(lines_items, "xmin")
  tier_xmax <- .v1_get_field_dbl(lines_items, "xmax")

  stopifnot(tier_type %in% c("IntervalTier", "TextTier"))

  if (tier_type == "IntervalTier") {
    df <- .v1_parse_interval_tier(lines_items)
  } else {
    df <- .v1_parse_point_tier(lines_items)
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

.v1_parse_interval_tier <- function(lines_interval_tier) {
  lines_interval_tier |>
    .v1_slice_sections("intervals") |>
    purrr::map(.v1_combine_text_lines) |>
    purrr::map(.v1_get_field_list, fields = c("xmin", "xmax", "text")) |>
    purrr::imap(.v1_add_annotation_num) |>
    dplyr::bind_rows()
}

# If the text field spans multiple lines, combine them into one string
.v1_combine_text_lines <- function(lines_annotation) {
  loc_mark_start <- lines_annotation |> .v1_which_field("mark")
  loc_text_start <- lines_annotation |> .v1_which_field("text")
  loc_text_start <- c(loc_text_start, loc_mark_start)

  if (loc_text_start != length(lines_annotation)) {
    loc_text_rest <- seq(loc_text_start + 1, length(lines_annotation), by = 1)
    loc_text_full <- c(loc_text_start, loc_text_rest)
    lines_annotation[loc_text_start] <- lines_annotation[loc_text_full] |>
      paste0(collapse = "\n")
    lines_annotation <- lines_annotation[-loc_text_rest]
  }
  lines_annotation
}

.v1_parse_point_tier <- function(lines_point_tier) {
  no_points <- .v1_str_detect_any(lines_point_tier, "points: size = 0")

  if (!no_points) {
    df <- lines_point_tier |>
      .v1_slice_sections("points") |>
      purrr::map(.v1_get_field_list, fields = c("number", "mark")) |>
      purrr::imap(.v1_add_annotation_num) |>
      dplyr::bind_rows()

    # We treat points as zero-width intervals
    df[["xmin"]] <- df[["number"]]
    df[["xmax"]] <- df[["number"]]
    df[["text"]] <- df[["mark"]]
    df[["mark"]] <- NULL
    df[["number"]] <- NULL
    df <- df[c("xmin", "xmax", "text", "annotation_num")]
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

.v1_add_annotation_num <- function(x, y) {
  x[["annotation_num"]] <- y
  x
}

.v1_get_field_list <- function(lines, fields) {
  stats::setNames(
    lapply(fields, function(x) .v1_get_field(lines, x)),
    fields
  )
}

# Find first match of "[field] = [value]", returning [value]
.v1_get_field <- function(lines, field) {
  re <- paste0("(?<=", field, " = ).+") |>
    # "text = .*" needs to capture newlines too
    stringr::regex(dotall = TRUE)

  lines |>
    stringr::str_extract(re) |>
    .v1_remove_na() |>
    utils::head(1) |>
    stringr::str_trim() |>
    .v1_str_unquote()
}

# Find first match of "[field] = [value]", returning [value]
.v1_which_field <- function(lines, field) {
  re <- paste0("(?<=", field, " = ).+")
  lines |>
    stringr::str_which(re)
}

# Find first match of "[field] = [value]", returning [value]
.v1_get_field_dbl <- function(lines, field) {
  as.numeric(.v1_get_field(lines, field))
}

.v1_remove_na <- function(xs) {
  xs[!is.na(xs)]
}

.v1_str_unquote <- function(xs) {
  gsub("^\"|\"$", "", xs)
}

.v1_str_unescape_quote <- function(xs) {
  gsub('""', '"', xs, perl = TRUE)
}

.v1_str_detect_any <- function(xs, pattern) {
  any(stringr::str_detect(xs, pattern))
}
