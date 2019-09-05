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
