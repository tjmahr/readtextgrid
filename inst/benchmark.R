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

path <- paths[10]

b <- bench::mark(
  new = read_textgrid(path, encoding = "UTF-8"),
  new_guess = read_textgrid(path),
  old = legacy_read_textgrid(path, encoding = "UTF-8"),
  old_guess = legacy_read_textgrid(path),
  min_iterations = 5,
  check = TRUE
)
summary(b)
