test_that("reading in point tiers", {
  path <- testthat::test_path("test-data/points.TextGrid")
  tg <- read_textgrid(path)
  expect_equal(nrow(tg), 3)
})

test_that("reading in empty point tiers", {
  path <- testthat::test_path("test-data/Mary_John_bell.TextGrid")
  tg <- read_textgrid(path)
  expect_equal(nrow(tg), 3)
})

test_that("result is a tibble", {
  path <- testthat::test_path("test-data/Mary_John_bell.TextGrid")
  tg <- read_textgrid(path)
  testthat::expect_s3_class(tg, "tbl")
})

test_that("example_textgrid works", {
  path <- example_textgrid()
  tg <- read_textgrid(path)
  expect_equal(nrow(tg), 3)
})

test_that("encoding support", {
  example_textgrid(1) |>
    read_textgrid(encoding = "UTF-8") |>
    nrow() |>
    expect_equal(3)

  example_textgrid(1) |>
    read_textgrid() |>
    nrow() |>
    expect_equal(3)

  example_textgrid(2) |>
    read_textgrid(encoding = "UTF-16") |>
    nrow() |>
    expect_equal(3)

  example_textgrid(2) |>
    read_textgrid() |>
    nrow() |>
    expect_equal(3)
})

test_that("reading in ELAN-generated textgrids (#11)", {
  path <- testthat::test_path("test-data/elan.TextGrid")

  path |>
    read_textgrid() |>
    nrow() |>
    expect_equal(5)
})

test_that("pivoting words on a single tier", {
  path <- testthat::test_path("test-data/nested-intervals.TextGrid")

  data <- path |>
    read_textgrid()

  p1 <- data |> pivot_textgrid_tiers("utterance")
  expect_equal(p1$utterance, "hug daddy")

  p1 |>
    hasName(c("utterance_xmin", "utterance_xmid", "utterance_xmax")) |>
    all() |>
    expect_true()

  data |>
    pivot_textgrid_tiers("fake name") |>
    expect_error("must be used")
})

test_that("pivoting works with multiple tiers", {
  path <- testthat::test_path("test-data/nested-intervals.TextGrid")
  phones <- c("sil", "HH", "AH1", "G", "sp", "D", "AE1", "D", "IY0", "sp", "")
  words <- rep(c("", "hug", "", "daddy", ""), c(1, 3, 1, 4, 2))

  data <- path |>
    read_textgrid()

  p2 <- data |> pivot_textgrid_tiers(c("words", "phones"))

  p2$words |>
    expect_equal(words)

  p2$phones |>
    expect_equal(phones)

  p2 |>
    hasName(c("words_xmin", "words_xmid", "words_xmax")) |>
    all() |>
    expect_true()

  p2 |>
    hasName(c("phones_xmin", "phones_xmid", "phones_xmax")) |>
    all() |>
    expect_true()

})
