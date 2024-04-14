# AcceptReject 0.1.0

* Initial CRAN submission.

# AcceptReject 0.1.1

* Now it is possible to specify a different base density/probability mass function than the uniform one. If none is specified, the uniform density (either discrete or continuous) is assumed for the case of discrete or continuous random variables, respectively;

* In generating observations of continuous random variables, using histogram with the same breaks as the R graphics `hist()` function, in the histogram created by **ggplot2**;

* Providing alerts regarding the limits passed to the `xlim` argument of the `accept_reject()` function. If a significant density/probability mass is present, a warning will be issued. The alert can be omitted by setting `warning = FALSE`;

* Improved performance;

* In the `plot.accept_reject()` function, there's an additional argument `hist = TRUE` (default). If `hist = TRUE`, a histogram is plotted along with the base density, in the case of generating pseudo-random observations of a continuous random variable. If `hist = FALSE`, the theoretical density is plotted alongside the observed density;

* The `print.accept_reject()` function now informs whether the case is discrete or continuous and the `xlim`;

* Bug fix.
