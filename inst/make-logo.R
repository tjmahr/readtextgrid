# install.packages("hexSticker")
library(hexSticker)
library(ggplot2)

# dir.create("man/figures")

df <- tibble::tibble(
  row = 2,
  x = .5,
  label = "read"
)

df2 <- tibble::tibble(
  row = 1,
  x = seq(0, 1, length.out = 8),
  label = c("t", "e", "x", "t", "g", "r", "i", "d")
)

lines <- purrr::map2_dbl(
  df2$x[1:7],
  df2$x[2:8],
  function(x, y) median(c(x, y))
)

# # from actual praat
# yellow <- "#FADF28"
# red <- "#DE0805"
# blue <- "#0000D3"

text <- "#404e4d"
line <- "#747e7d"
yellow <- "#fde74c"
red <- "#c3423f"
blue <- "#4D85BD"

grid_min <- -.143 / 2
grid_max <- 1 + .143 / 2
text_size <- 15

p <- ggplot(df) +
  aes(x = x, y = row) +
  geom_segment(
    aes(x = x, y = y, xend = xend, yend = yend),
    data = tibble::tibble(
      x = grid_min,
      xend = grid_max,
      y = c(.5, 1.5, 2.5),
      yend = y
    ),
    color = line
  ) +
  geom_segment(
    aes(x = x, y = y, xend = xend, yend = yend),
    data = tibble::tibble(
      x = c(grid_min, grid_max),
      xend = x,
      y = .5,
      yend = 2.5
    ),
    size = 2,
    lineend = "round",
    color = blue
  ) +
  geom_ribbon(
    aes(x = x, ymax = ymax, ymin = ymin),
    data = tibble::tibble(
      x = lines[5:6],
      ymin = .5,
      ymax = 1.5,
      row = .5
    ),
    fill = yellow
  ) +
  geom_segment(
    aes(x = x, y = y, xend = xend, yend = yend),
    data = tibble::tibble(
      x = lines[-5],
      xend = x,
      y = .5,
      yend = 1.5
    ),
    size = 2,
    lineend = "round",
    color = blue
  ) +
  geom_segment(
    aes(x = x, y = y, xend = xend, yend = yend),
    data = tibble::tibble(
      x = lines[5],
      xend = x,
      y = .5,
      yend = 1.5
    ),
    size = 2,
    lineend = "round",
    color = red
  ) +
  geom_text(aes(label = label), size = text_size, color = text) +
  geom_text(
    aes(label = label),
    data = df2[-6, ],
    size = text_size,
    color = text
  ) +
  geom_text(
    aes(label = label),
    data = df2[6, ],
    size = text_size,
    color = red
  ) +
  theme_void() +
  theme_transparent()

sticker(
  p,
  package = "",
  s_x = 1,
  s_y = 1,
  s_width = 1.65,
  s_height = 1,
  filename = "man/figures/logo.png",
  h_fill = "white",
  h_color = red
)

system2("open", "man/figures/logo.png")
