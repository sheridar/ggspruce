% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare-colors.R
\name{plot_colors}
\alias{plot_colors}
\title{Plot color palette}
\usage{
plot_colors(colors, filter = NULL, label_size = 14, label_color = "white", ...)
}
\arguments{
\item{colors}{Character vector of colors to plot.}

\item{filter}{Filter to apply when plotting colors,
possible values include,
- "deutan"
- "protan"
- "tritan"}

\item{label_size}{Size of labels}

\item{label_color}{Color of labels}

\item{...}{Additional arguments to pass to `ggplot2::geom_bar()`}
}
\description{
Plot color palette
}
