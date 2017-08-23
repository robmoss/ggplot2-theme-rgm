## A simple plotting theme for ggplot2

This package provides the "theme_rgm" plotting theme for ggplot2.
By default, it uses the freely-available
[Open Sans](http://www.google.com/fonts/specimen/Open+Sans) font when printing
plots with [Cairo](http://cran.r-project.org/web/packages/Cairo/).

## License

This work is made available under the BSD 2-Clause license (see `LICENSE`).

## Usage

    library(themergm)

    # Add the theme to an existing plot.
    p <- ggplot(...) + ... + theme_rgm()

    # Set the theme as the default theme.
    theme_set(theme_rgm())

## Install

Clone the repository (into the directory `ggplot2-theme-rgm`) and run:

    R CMD INSTALL ggplot2-theme-rgm

Note that this package requires `ggplot2 >= 2.2.0`.
