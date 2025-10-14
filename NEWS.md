# readtextgrid dev version

* New function `pivot_textgrid_tiers()` to pivot out nested textgrid 
  intervals into a wide dataframe. For example, if a forced aligner has a 
  `words` and `phones` tier, we can pivot the `words` and `phones` 
  intervals into a dataframe with one row per `phones` intervals and with 
  columns `words`, `words_xmin`, `words_xmax`, `phones`, `phones_xmin`, 
  `phones_xmax`, etc. (#10, request of @stefanocoretta)
* `read_textgrid()` now manually parses textgrids and can handle short-format 
  textgrids. (#4, #16, initial parser by @JoFrhwld). Part of the parsing is done
  in C++ so the new manual parser is faster than the legacy version.
* Original package functions are available in `legacy_read_textgrid()`.
* `legacy_read_textgrid()` un-escapes `A ""quote"" word` to `A "quoted" word`.
* `legacy_read_textgrid()` can handle interval text with line breaks in them.
* Testing suite includes a short-format textgrid, a short-format textgrid 
  with inline comments, and a textgrid with escaped `"` characters. (@JoFrhwld) 
* Testing suit includes an adversarial textgrid to challenge parsing.
* Support ELAN-generated textgrids. (#11, @djvill)
* Raised required R version to 4.3.0. (April 2023)

# readtextgrid 0.1.2

* Add `encoding` argument to `read_textgrid()`. (#7, #8, #9, @jonorthwash)
* Include example UTF-16 textgrid for testing.
* Remove magrittr dependency. (#5, @JoFrhwld)


# readtextgrid 0.1.1

* Fixes for CRAN resubmission.


# readtextgrid 0.1.0

* Initial release.
