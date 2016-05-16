##
## theme_rgm.R
## Robert Moss, 2015-2016
##
## This file provides the "theme_rgm" plotting theme for ggplot2.
##
## 1.7 2016/05/16
##   Apply common settings to each axis, to override any default settings.
##
## 1.6 2016/05/11
##   Correct the ggplot2 dependency to 2.0.0 or later, since this package
##   relies on "element_text()" accepting a "margins" argument.
##
## 1.5 2016/03/11
##   Add margins to axis tick labels and axis titles.
##   Increase the default size of axis tick labels and axis titles.
##   Option "axis.black" is now TRUE by default.
##
## 1.4 2016/01/06
##   Fix several warnings raised by `R CMD check`.
##   Correctly document the "axis.title" option.
##
## 1.3 2015/06/22
##   Option "axis.title" controls the relative size of axis titles.
##   Warn if "CairoFonts" is true but the Cairo package is not installed.
##
## 1.2 2015/06/17
##   Convert from a single source file into an installable R package.
##
## 1.1 2015/05/28
##   Option "axis.black" draws axes, ticks and labels in black, not grey.
##   Option "legend.position" controls legend position *and* justification.
##
## 1.0 2015/03/11
##   Initial version of the theme.
##
## License:
##   This software may be modified and distributed under the terms of the
##   BSD 2-Clause license (http://opensource.org/licenses/BSD-2-Clause).
##

#' Define the plotting theme.
#'
#' @param base.size        The base font size.
#' @param legend.bg        The legend background colour.
#' @param legend.border    The legend border colour.
#' @param legend.position  The legend position *and* justification.
#' @param key.box          The relative size of legend key boxes.
#' @param key.label        The relative size of legend key labels.
#' @param facet.label      The relative size of facet labels.
#' @param axis.line        The width of axis lines.
#' @param axis.label       The relative size of axis text.
#' @param axis.title       The relative size of axis titles.
#' @param axis.black       Draw axes, ticks and labels in black, not grey.
#' @param axis.x           Hide ticks ('simple') or the entire axis ('hide').
#' @param axis.y           Hide ticks ('simple') or the entire axis ('hide').
#' @param hide.title       Whether to display plot titles.
#' @param CairoFonts       Configure Cairo to use the Open Sans font family.
#'
#' @return The custom ggplot2 theme.
#'
#' @examples
#' \dontrun{theme_set(theme_rgm(axis.black = TRUE, CairoFonts = FALSE))}
#' \dontrun{p <- ggplot(...) + ... + theme_rgm(...)}
#'
#' @export
theme_rgm <- function(base.size=16, legend.bg=NA, legend.border=NA,
                      legend.position=c(0, 1),
                      key.box=1.25, key.label=NULL, facet.label=1.25,
                      axis.line=0.5, axis.label=1.0,
                      axis.title=1.25, axis.black=TRUE,
                      axis.x=c('normal', 'simple', 'hide'),
                      axis.y=c('normal', 'simple', 'hide'),
                      hide.title=TRUE, CairoFonts=TRUE) {
    if (CairoFonts) {
        ## Attempt to load the Cairo library, as specified.
        if (! requireNamespace("Cairo", quietly = TRUE)) {
            warning("Cairo is not installed", call. = FALSE)
        } else {
            ## Use Open Sans, since it includes the Greek alphabet.
            font.light <- "Open Sans:style=Light"
            font.heavy <- "Open Sans:style=Regular"
            Cairo::CairoFonts(regular = font.light, italic = font.light,
                              bold = font.heavy, bolditalic=font.heavy,
                              symbol=font.light)
        }
    }

    ## Determine the size of the legend key labels.
    ## By default, assume the same scaling as for the legend key boxes.
    if (is.null(key.label)) {
        key.label <- key.box
    }

    ## Determine how to display each axis.
    axis.x <- match.arg(axis.x)
    axis.y <- match.arg(axis.y)
    if (axis.black) {
        axis.colour <- "black"
    } else {
        axis.colour <- "grey50"
    }

    ## Load the default ggplot2 theme and modify as desired.
    plot.theme <- theme_grey(base.size)

    ##
    ## Remove the grey plot background and grid lines.
    ##
    plot.theme$panel.background <- element_rect(fill = NA, colour = NA)
    plot.theme$plot.background <- element_rect(fill = NA, colour = NA)
    plot.theme$panel.grid.major <- element_line(colour = NA, size = 0)
    plot.theme$panel.grid.minor <- element_line(colour = NA, size = 0)

    ##
    ## Define margins for axis tick labels and titles.
    ##
    margin.text.x <- unit(base.size * c(0.25, 0, 0, 0), "pt")
    margin.text.y <- unit(base.size * c(0, 0.25, 0, 0), "pt")
    margin.title.x <- unit(base.size * c(0.5, 0, 0, 0), "pt")
    margin.title.y <- unit(base.size * c(0, 0.5, 0, 0), "pt")

    ##
    ## Draw axes with thin grey lines and axis values in small grey text.
    ##
    plot.theme$axis.line <- element_line(colour = axis.colour,
                                         size = axis.line)
    plot.theme$axis.ticks <- element_line(colour = axis.colour,
                                          size = axis.line)
    plot.theme$axis.text.x <- element_text(colour = axis.colour,
                                           size = base.size * axis.label,
                                           margin = margin.text.x,
                                           vjust = 1, lineheight = 0.9)
    plot.theme$axis.text.y <- element_text(colour = axis.colour,
                                           size = base.size * axis.label,
                                           margin = margin.text.y,
                                           hjust = 1, lineheight = 0.9)
    plot.theme$axis.title.x <- element_text(colour = axis.colour,
                                            size = base.size * axis.title,
                                            margin = margin.title.x,
                                            vjust = -0.5, lineheight = 0.9)
    plot.theme$axis.title.y <- element_text(colour = axis.colour,
                                            size = base.size * axis.title,
                                            margin = margin.title.y,
                                            angle = 90, vjust = 1, lineheight = 0.9)

    ##
    ## Apply common settings to each axis, to override any default settings.
    ##
    plot.theme$axis.line.x <- plot.theme$axis.line
    plot.theme$axis.line.y <- plot.theme$axis.line
    plot.theme$axis.ticks.x <- plot.theme$axis.ticks
    plot.theme$axis.ticks.y <- plot.theme$axis.ticks

    ##
    ## Simplify or hide the axes, as specified.
    ##
    if (axis.x == 'simple') {
        plot.theme$axis.text.x <- element_blank()
        plot.theme$axis.ticks.x <- element_blank()
    } else if (axis.x == 'hide') {
        plot.theme$axis.line.x <- element_blank()
        plot.theme$axis.text.x <- element_blank()
        plot.theme$axis.ticks.x <- element_blank()
    }
    if (axis.y == 'simple') {
        plot.theme$axis.text.y <- element_blank()
        plot.theme$axis.ticks.y <- element_blank()
    } else if (axis.y == 'hide') {
        plot.theme$axis.line.y <- element_blank()
        plot.theme$axis.text.y <- element_blank()
        plot.theme$axis.ticks.y <- element_blank()
    }

    ##
    ## Don't draw a box around the plot area; this allows individual plot axes
    ## (i.e., on the bottom and left of the plot) to be either shown or hidden.
    ##
    plot.theme$panel.border <- element_rect(fill = NA, colour = NA,
                                            size = axis.line)

    ##
    ## Leave some space between facetted plots.
    ##
    plot.theme$panel.margin <- unit(2, "lines")

    ##
    ## Apply common settings to each axis, to override any default settings.
    ##
    plot.theme$panel.margin.x <- plot.theme$panel.margin
    plot.theme$panel.margin.y <- plot.theme$panel.margin

    ##
    ## Display facet titles in large black text, to emphasise the facetting.
    ##
    plot.theme$strip.background <- element_rect(colour = NA, fill = NA)
    plot.theme$strip.text.x <- element_text(colour = "black", face="bold",
                                            size = base.size * facet.label,
                                            vjust = 1)
    plot.theme$strip.text.y <- element_text(colour = "black", face="bold",
                                            size = base.size * facet.label,
                                            angle = -90, vjust = 1)

    ##
    ## By default, don't draw a border or background for plot legends.
    ##
    plot.theme$legend.background <- element_rect(colour = legend.border,
                                                 fill = legend.bg)

    ##
    ## Enlarge legend keys and labels, to emphasise each plot series.
    ##
    plot.theme$legend.key.size <- unit(key.box, "cm")
    plot.theme$legend.text <- element_text(colour="black",
                                           size = base.size * key.label,
                                           vjust = 0, hjust = 3)

    ##
    ## Set the legend position and justification, if provided.
    ##
    if (! (is.null(legend.position) || is.na(legend.position))) {
        plot.theme$legend.position = legend.position
        ## Don't set justification if position is, e.g., "top".
        if (is.numeric(legend.position)) {
            plot.theme$legend.justification = legend.position
        }
    }

    ##
    ## Don't draw a shaded box around the key for each line series.
    ##
    plot.theme$legend.key <- element_rect(fill = NA, colour = NA)

    if (hide.title) {
        ## Hide plot titles; figure captions should contain pertinent details.
        plot.theme$legend.title <- element_blank()
    }

    return(plot.theme)
}
