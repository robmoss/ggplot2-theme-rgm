1.9 (2019/03/29)
----------------

* Correct the alignment of legend labels, and define their margins directly
  rather than by misusing their 'hjust' and 'vjust' attributes.


1.8 (2017/08/23)
----------------

* Increase the ggplot2 dependency to 2.2.0 or later, "panel.margin" is now
  deprecated and has been superseded by "panel.spacing".

* Ensure that horizontal facet labels have sufficient vertical spacing.

* Correct the alignment of vertical facet labels.


1.7 (2016/05/16)
----------------

* Apply common settings to each axis, to override any default settings.


1.6 (2016/05/11)
----------------

* Correct the ggplot2 dependency to 2.0.0 or later, since this package relies
  on "element_text()" accepting a "margins" argument.


1.5 (2016/03/11)
----------------

* Add margins to axis tick labels and axis titles.

* Increase the default size of axis tick labels and axis titles.

* Option "axis.black" is now TRUE by default.


1.4 (2016/01/06)
----------------

* Fix several warnings raised by `R CMD check`.

* Correctly document the "axis.title" option.


1.3 (2015/06/22)
----------------

* Option "axis.title" controls the relative size of axis titles.

* Warn if "CairoFonts" is true but the Cairo package is not installed,
  rather than stopping execution (as was previously the case).


1.2 (2015/06/17)
----------------

* Convert from a single source file into an installable R package.


1.1 (2015/05/28)
----------------

* Option "axis.black" draws axes, ticks and labels in black, not grey.

* Option "legend.position" controls legend position *and* justification.


1.0 (2015/03/11)
----------------

* Initial version of the theme.
