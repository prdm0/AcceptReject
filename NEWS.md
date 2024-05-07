# AcceptReject 0.1.0

* Initial CRAN submission.

# AcceptReject 0.1.1

* Improved performance in serial and parallel processing with Rcpp and RcppArmadillo;

* Now it is possible to specify a different base density/probability mass function than the uniform one. If none is specified, the uniform density (either discrete or continuous) is assumed for the case of discrete or continuous random variables, respectively;

* Now the function `inspect()` is available, allowing you to compare the base probability density function with the theoretical density function. The `inspect()` function is useful for finding a reasonable base density function. It returns an object of the classes gg and ggplot with the density curves, the intersection area, and the value of the intersection. Users are not obligated to use the `inspect()` function since the `accept_reject()` function already takes care of a lot. However, for the continuous case, providing the f_base argument to the `accept_reject()` function with a good candidate base density function can be a good idea;

* In generating observations of continuous random variables, using histogram with the same breaks as the R graphics `hist()` function, in the histogram created by **ggplot2**;

* Providing alerts regarding the limits passed to the `xlim` argument of the `accept_reject()` function. If a significant density/probability mass is present, a warning will be issued. The alert can be omitted by setting `warning = FALSE`;

* In the `plot.accept_reject()` function, there's an additional argument `hist = TRUE` (default). If `hist = TRUE`, a histogram is plotted along with the base density, in the case of generating pseudo-random observations of a continuous random variable. If `hist = FALSE`, the theoretical density is plotted alongside the observed density;

* The `print.accept_reject()` function now informs whether the case is discrete or continuous and the `xlim`;

* Putting the order of the specifications of the arguments of the exported functions in the order of the arguments of the functions;

* The warning messages have been improved;

* Bug fix.

# AcceptReject 0.1.2

* The performance of the `one_step()` function, an internal function used in the implementation of C++ using Rcpp, has been improved;

* The method `qqplot.accept_reject()` has been added, which constructs the QQ-Plot of an object of class `accept_reject` returned by the function `accept_reject()`;

* The `qqplot.accept_reject()` function utilizes the [**scattermore**](https://github.com/exaexa/scattermore) package if the point density is high, i.e., above 10 thousand observations;

* The function `accept_reject()` now has the argument cores, which allows the user to control the number of cores that will be used if `parallel = TRUE`. The default, `cores = NULL`, means that all processor cores will be used. If `parallel = FALSE`, the cores argument is ignored;

* The [DESCRIPTION](https://raw.githubusercontent.com/prdm0/AcceptReject/main/DESCRIPTION) file was edited;

* Another bibliographic reference was added to the `accept_reject()` function;

* The dependency on the `lbfgs` package has been removed;

* New unit tests have been introduced;

* Bug fix.
