% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readtextgrid.R
\name{example_textgrid}
\alias{example_textgrid}
\title{Locate the path of an example textgrid file}
\usage{
example_textgrid(which = 1)
}
\arguments{
\item{which}{index of the textgrid to load}
}
\value{
Path of \code{"Mary_John_bell.TextGrid"} bundled with the \code{readtextgrid}
package.
}
\description{
Locate the path of an example textgrid file
}
\details{
This function is a wrapper over \code{\link[=system.file]{system.file()}}  to locate the
paths to bundled textgrids. These files are used to test or demonstrate
functionality of the package.

Two files are included:
\enumerate{
\item \code{"Mary_John_bell.TextGrid"} - the default TextGrid created by Praat's
Create TextGrid command. This file is saved as UTF-8 encoding.
\item \code{"utf_16_be.TextGrid"} - a TextGrid with some IPA characters entered using
Praat's IPA character selector. This file is saved with UTF-16 encoding.
\item \code{"nested-intervals.TextGrid"} - A textgrid containing an \code{"utterance"}
tier, a \code{"words"} tier, and a \code{"phones"} tier. This file is typical of
forced alignment textgrids where utterances contain words which contain
speech segments. In this case, alignment was made by hand so that word
and phone boundaries do not correspond exactly.
}
}
