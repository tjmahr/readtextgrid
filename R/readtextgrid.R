
#' Read a textgrid file into a tibble
#'
#' @rdname read_textgrid
#' @param path a path to a dataframe
#' @param lines alternatively, the lines of a textgrid file
#' @return a tibble with one row per textgrid annotation
#' @export
read_textgrid <- function(path) {
  path %>%
    readr::read_lines() %>%
    read_textgrid_lines(file = basename(path))
}

#' @rdname read_textgrid
#' @export
read_textgrid_lines <- function(lines, file = NA_character_) {
  lines %>%
    parse_textgrid_lines() %>%
    tibble::add_column(file = file, .before = 1)
}

parse_textgrid_lines <- function(lines) {
  lines %>%
    slice_sections("item") %>%
    purrr::map_dfr(parse_item_lines)
}

slice_sections <- function(lines, section_head) {
  re <- sprintf("^\\s+%s \\[\\d+\\]:", section_head)
  starts <- stringr::str_which(lines, re)
  ends <- c(starts[-1] - 1, length(lines))
  purrr::map2(starts, ends, function(x, y) lines[seq(x, y, by = 1)]
  )
}


parse_item_lines <- function(lines) {
  item_num <- lines[1] %>%
    stringr::str_extract("\\d+") %>%
    as.numeric()

  tier_type <- lines %>% get_field("class")
  tier_name <- lines %>% get_field("name")
  tier_xmin <- lines %>% get_field("xmin") %>% as.numeric()
  tier_xmax <- lines %>% get_field("xmax") %>% as.numeric()

  stopifnot(tier_type %in% c("IntervalTier", "TextTier"))

  if (tier_type == "IntervalTier") {
    df <- lines %>%
      slice_sections("intervals") %>%
      purrr::map(get_field_list, fields = c("xmin", "xmax", "text")) %>%
      purrr::imap_dfr(add_annotation_num)
  } else {
    no_points <- lines %>%
      stringr::str_detect("points: size = 0") %>%
      any()

    if (!no_points) {
      df <- lines %>%
        slice_sections("points") %>%
        purrr::map(get_field_list, fields = c("number", "mark")) %>%
        purrr::imap_dfr(add_annotation_num)

      df[["xmin"]] <- df[["number"]]
      df[["xmax"]] <- df[["number"]]
      df[["text"]] <- df[["mark"]]
      df[["mark"]] <- NULL
      df[["number"]] <- NULL
    } else {
      df <- tibble::tibble(
        xmin = NA,
        xmax = NA,
        text = NA_character_,
        annotation_num = NA
      )
    }
  }

  df[["xmin"]] <- as.numeric(df[["xmin"]])
  df[["xmax"]] <- as.numeric(df[["xmax"]])

  df %>%
    tibble::add_column(
      tier_num  = item_num,
      tier_name = tier_name,
      tier_type = tier_type,
      tier_xmin = tier_xmin,
      tier_xmax = tier_xmax,
      .before = 1
    )
}

add_annotation_num <- function(x, y) {
  x[["annotation_num"]] <- y
  x
}

get_field_list <- function(lines, fields) {
  fields %>%
    lapply(function(x) get_field(lines, x)) %>%
    stats::setNames(fields)
}

# Find first match of "[field] = [value]", returning [value]
get_field <- function(lines, field) {
  re <- paste0("(?<=", field, " = ).+")

  lines %>%
    stringr::str_extract(re) %>%
    remove_na() %>%
    utils::head(1) %>%
    stringr::str_trim() %>%
    str_unquote()
}

remove_na <- function(xs) xs[!is.na(xs)]
str_unquote <- function(xs) stringr::str_remove_all(xs, "^\"|\"$")
