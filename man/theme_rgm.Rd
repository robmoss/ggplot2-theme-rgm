% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/theme_rgm.R
\name{theme_rgm}
\alias{theme_rgm}
\title{Define the plotting theme.}
\usage{
theme_rgm(base.size = 16, legend.bg = NA, legend.border = NA,
  legend.position = c(0, 1), key.box = 1.25, key.label = NULL,
  facet.label = 1.25, axis.line = 0.5, axis.label = 1.0,
  axis.title=1.25, axis.black = TRUE,
  axis.x = c("normal", "simple", "hide"),
  axis.y = c("normal", "simple", "hide"), hide.title = TRUE,
  CairoFonts = TRUE)
}
\arguments{
\item{base.size}{The base font size.}

\item{legend.bg}{The legend background colour.}

\item{legend.border}{The legend border colour.}

\item{legend.position}{The legend position *and* justification.}

\item{key.box}{The relative size of legend key boxes.}

\item{key.label}{The relative size of legend key labels.}

\item{facet.label}{The relative size of facet labels.}

\item{axis.line}{The width of axis lines.}

\item{axis.label}{The relative size of axis text.}

\item{axis.title}{The relative size of axis titles.}

\item{axis.black}{Draw axes, ticks and labels in black, not grey.}

\item{axis.x}{Hide ticks ('simple') or the entire axis ('hide').}

\item{axis.y}{Hide ticks ('simple') or the entire axis ('hide').}

\item{hide.title}{Whether to display plot titles.}

\item{CairoFonts}{Configure Cairo to use the Open Sans font family.}
}
\value{
The custom ggplot2 theme.
}
\description{
Define the plotting theme.
}
\examples{
\dontrun{theme_set(theme_rgm(axis.black = TRUE, CairoFonts = FALSE))}
\dontrun{p <- ggplot(...) + ... + theme_rgm(...)}
}
