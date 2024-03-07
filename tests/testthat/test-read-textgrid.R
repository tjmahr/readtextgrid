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
