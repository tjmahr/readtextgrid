
#' Pivot a textgrid into wide format, respecting nested tiers
#'
#' @param data a textgrid dataframe created with [read_textgrid()]
#' @param tiers character vector of tiers to pivot into wide format. When
#'   `tiers` has more than 1 element, the tiers are treated as nested. For
#'   example, if `tiers` is `c("utterance", "word", "phone")`, where
#'   `"utterance"` intervals contain `"word"` intervals which in turn contain
#'   `"phone"` intervals, the output will have one row per `"phone"` interval
#'   and include `utterance_*` and `word_*` columns for the utterance and word
#'   intervals that contain each phone interval. `tiers` should be ordered from
#'   broadest to narrowest (e.g, `"word"` preceding `"phone"`).
#' @param join_cols character vector of the columns that will uniquely identify
#'   a textgrid file. Defaults to `"file"` because
#'   these columns have identical values for tiers read from the same textgrid
#'   file.
#' @return a dataframe with just the intervals from tiers named in `tiers`
#'   converted into a wide format. Columns are renamed so that the `text` column
#'   is pivot into columns named after the tier names. For example, the `text`
#'   column in a `words` tier is renamed to `words`. The `xmax`, `xmin`,
#'   `annotation_num`, `tier_num`, `tier_type` are also prefixed with the tier
#'   name. For example, the `xmax` column in a `words` tier is renamed to
#'   `words_xmax`. An additional helper column `xmid` is added and prefixed
#'   appropriately. See examples below.
#' @export
#'
#' @details
#' For the joining nested intervals, two intervals *a* and *b* are combined into
#' the same row if they match on the values in the `join_cols` columns and if
#' the `a$xmin <= b$xmid` and `b$xmid <= a$xmax`. That is, if the midpoint of
#' *b* is contained inside the interval *a*.
#'
#'
#' @examples
#' data <- example_textgrid(3) |>
#'   read_textgrid()
#' data
#'
#' # With a single tier, we get just that tier with the columns prefixed with
#' # the tier_name
#' pivot_textgrid_tiers(data, "utterance")
#' pivot_textgrid_tiers(data, "words")
#'
#' # With multiple tiers, intervals in one tier that contain intervals in
#' # another tier are combined into the same row.
#' a <- pivot_textgrid_tiers(data, c("utterance", "words"))
#' cols <- c(
#'   "utterance", "utterance_xmin", "utterance_xmax",
#'   "words", "words_xmin", "words_xmax"
#' )
#' a[cols]
#'
#' a <- pivot_textgrid_tiers(data, c("utterance", "words", "phones"))
#' cols <- c(cols, "phones", "phones_xmin", "phones_xmax")
#' a[cols]
pivot_textgrid_tiers <- function(
    data,
    tiers,
    join_cols = "file"
) {
  {
    stopifnot(
      `tier names must be used in textgrid` =
        all(tiers %in% unique(data[["tier_name"]]))
    )

    # todo
    # allow only point tiers "TextTier" at last point in nesting

    # tier_types <- data[c("tier_name", "tier_type")]
  }
  tiers <- unique(tiers)
  data <- data[data[["tier_name"]] %in% tiers, ]

  join_cols <- join_cols |>
    c("tier_xmin", "tier_xmax") |>
    unique()

  f <- function(x, y) left_join_nested_tiers(x, y, join_cols)

  l <- data |>
    split(~tier_name) |>
    _[tiers] |>
    lapply(pivot_single_tier, join_cols) |>
    Reduce(f, x = _)

  l[["tier_name"]] <- NULL
  l
}

pivot_single_tier <- function(data, join_cols) {
  tier_name <- unique(data[["tier_name"]])
  stopifnot(length(tier_name) == 1)

  data[["xmid"]] <- data[["xmin"]] + (data[["xmax"]] - data[["xmin"]]) / 2

  names_end <- c("tier_name", "tier_xmin", "tier_xmax")
  names_front <- setdiff(join_cols, names_end)
  names_mid <- c(
    "xmin", "xmax", "xmid",
    "annotation_num", "tier_num", "tier_type"
  )
  name_ordering <- c(names_front, "text", names_mid, names_end)
  names_new <- c(
    names_front, tier_name,
    paste0(tier_name, "_", names_mid),
    names_end
  )

  data <- data[name_ordering] |>
    stats::setNames(names_new)

  data
}

# For the dplyr::join_by() syntax
utils::globalVariables(c("x", "y"))

left_join_nested_tiers <- function(data_parent, data_child, join_cols) {
  x_names <- data_parent[["tier_name"]][1] |>
    paste0("_", c("xmin", "xmax"))

  y_names <- data_child[["tier_name"]][1] |>
    paste0("_", "xmid")

  e <- rlang::expr(
    dplyr::between(
      `$`(y, !! y_names),
      `$`(x, !! x_names[1]),
      `$`(x, !! x_names[2])
    )
  )

  data_parent[["tier_name"]] <- NULL

  dplyr::left_join(
    data_parent,
    data_child,
    dplyr::join_by(!!! join_cols, !! e),
    relationship = "one-to-many"
  )
}
