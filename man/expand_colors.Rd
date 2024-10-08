% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resize-colors.R
\name{expand_colors}
\alias{expand_colors}
\title{Create nested color palette}
\usage{
expand_colors(
  colors,
  n = NULL,
  names = NULL,
  keep_original = FALSE,
  property = "lightness",
  direction = NULL,
  range = NULL,
  ...
)
}
\arguments{
\item{colors}{Vector of starting colors}

\item{n}{The number of expanded colors to generate for each starting color,
can be one of the following:
- A single integer
- A vector the same length as `colors` indicating the number
  of expanded colors to generate for each starting color}

\item{names}{A list of vectors the same length as `colors`,
with each vector containing names to assign to the expanded colors.
Each original color is expanded based on the number of provided names.}

\item{keep_original}{If `TRUE` original colors will be included in final
palette}

\item{property}{Color property to adjust when expanding colors.}

\item{direction}{Direction to use for expanding colors when `keep_original`
is `TRUE`, can be `1` or `-1` to increase or decrease values of `property`.
If `NULL` this will be automatically selected based on the starting colors.}

\item{range}{A vector containing the minimum and maximum values to use when
adjusting colors.}

\item{...}{Additional arguments to pass to `spruce_colors()`.}
}
\description{
Create nested color palette
}
