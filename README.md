
<!-- README.md is generated from README.Rmd. Please edit that file -->

# AcceptReject <img src="logo.png" align="right" />

<!-- badges: start -->
<!-- [![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/purrr)](https://cran.r-project.org/package=purrr) -->
<!-- [![Codecov test coverage](https://codecov.io/gh/tidyverse/purrr/branch/master/graph/badge.svg)](https://app.codecov.io/gh/tidyverse/purrr?branch=master) -->

[![R-CMD-check](https://github.com/prdm0/AcceptReject/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/prdm0/AcceptReject/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Generating pseudo-random observations from a probability distribution is
a common task in statistics. Being able to generate pseudo-random
observations from a probability distribution is useful for simulating
scenarios, in
[Monte-Carlo](https://en.wikipedia.org/wiki/Monte_Carlo_method) methods,
which are useful for evaluating various statistical models.

The inversion method is a common way to do this, but it is not always
possible to find a closed-form formula for the inverse function of the
cumulative distribution function of a random variable $X$, that is,
$q(u) = F^{-1}(u) = x$ (quantile function), where $F$ is the cumulative
distribution function of $X$ and $u$ is a uniformly distributed random
variable in the interval $(0, 1)$.

Whenever possible, it is preferable to use the inversion method to
generate pseudo-random observations from a probability distribution.
However, when it is not possible to find a closed-form formula for the
inverse function of the cumulative distribution function of a random
variable, it is necessary to resort to other methods. One way to do this
is through the [acceptance-rejection
method](https://en.wikipedia.org/wiki/Rejection_sampling), which is a
Monte-Carlo procedure. This package aims to provide a function that
implements the Acceptance and Rejection method for generating
pseudo-random observations from probability distributions that are
difficult to sample directly.

The package [AcceptReject](https://github.com/prdm0/AcceptReject)
provides the `AcceptReject::accept_reject()` function that implements
the acceptance-rejection method in an optimized manner to generate
pseudo-random observations for discrete or continuous random variables.
The `AcceptReject::accept_reject()` function operates in parallel on
Unix-based operating systems such as Linux and MacOS and operates
sequentially on Windows-based operating systems; however, it still
exhibits good performance. By default, on Unix-based systems,
observations are generated sequentially, but it is possible to generate
observations in parallel if desired, by using the `parallel = TRUE`
argument.

The `AcceptReject::accept_reject()` function, by default, attempts to
maximize the probability of acceptance of the pseudo-random observations
generated. Suppose $X$ and $Y$ are random variables with probability
density function (pdf) or probability function (pf) $f$ and $g$,
respectively. Furthermore, suppose there exists a constant $c$ such that

$$\frac{f_X(x)}{g_Y(y)} \leq c.$$

By default, the accept_reject function attempts to find the value of $c$
that maximizes the probability of acceptance of the pseudo-random
observations generated. However, it is possible to provide a value of
$c$ to the `AcceptReject::accept_reject()` function through the argument
`c`, where $Y$ is a random variable for which we know how to generate
observations. For the `AcceptReject::accept_reject()` function, it is
not necessary to specify the probability function or probability density
function of $Y$ to generate observations of $X$ for discrete and
continuous cases, respectively. For the discrete and continuous cases,
$Y$ follows the discrete uniform distribution function and continuous
uniform distribution function, respectively.

Since the probability of acceptance is $1/c$, the
`AcceptReject::accept_reject()` function attempts to find the minimum
value of $c$ that satisfies the description above. Unless you have
compelling reasons to provide a value for the `c` argument of the
`AcceptReject::accept_reject()` function, it is recommended to use
`c = NULL` (default), allowing a value of $c$ to be automatically
determined.

## Installation

The package is being versioned on GitHub. You can install the
development version of
[AcceptReject](https://github.com/prdm0/AcceptReject), and to do this,
you must first install the
[remotes](https://CRAN.R-project.org/package=remotes) package and then
run the following command:

``` r
# install.packages("remotes")
# or remotes::install_github("prdm0/AcceptReject", force = TRUE)
library(AcceptReject)
```

The `force = TRUE` argument is not necessary. It is only needed in
situations where you have already installed the package and want to
reinstall it to have a new version.

## Examples

Please note the examples below on how to use the
`AcceptReject::accept_reject()` function to generate pseudo-random
observations of discrete and continuous random variables. For further
details, refer to the function’s documentation
[**Reference**](https://prdm0.github.io/AcceptReject/reference/) and the
[**Vignette**](https://prdm0.github.io/AcceptReject/articles/accept_reject.html).

### Generating discrete observations

As an example, let $X \sim Poisson(\lambda = 0.7)$. We will generate
$n = 1000$ observations of $X$ using the acceptance-rejection method,
using the `AcceptReject::accept_reject()` function. Note that it is
necessary to provide the `xlim` argument. Try to set an upper limit
value for which the probability of $X$ assuming that value is zero or
very close to zero. In this case, we choose `xlim = c(0, 20)`, where
`dpois(x = 20, lambda = 0.7)` is very close to zero (1.6286586^{-22}).

``` r
library(AcceptReject)
library(patchwork) # install.packages("patchwork")
# Ensuring Reproducibility
set.seed(0) 

simulation <- function(n){
  AcceptReject::accept_reject(
    n = 1000L,
    f = dpois,
    continuous = FALSE,
    args_f = list(lambda = 0.7),
    xlim = c(0, 20),
    parallel = TRUE
  )
}

p1 <- simulation(25L) |> plot()
p2 <- simulation(250L) |> plot()
p3 <- simulation(2500L) |> plot()
p4 <- simulation(25000L) |> plot()

p1 + p2 + p3 + p4
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

## Generating continuous observations

To expand beyond examples of generating pseudo-random observations of
discrete random variables, consider now that we want to generate
observations from a random variable
$X \sim \mathcal{N}(\mu = 0, \sigma^2 = 1)$. We chose the normal
distribution because we are familiar with its form, but you can choose
another distribution if desired. Below, we will generate `n = 2000`
observations using the acceptance-rejection method. Note that
`continuous = TRUE`.

``` r
library(AcceptReject)
library(patchwork)

# Ensuring reproducibility
set.seed(0) 

simulation <- function(n){
  AcceptReject::accept_reject(
    n = n,
    f = dnorm,
    continuous = TRUE,
    args_f = list(mean = 0, sd = 1),
    xlim = c(-4, 4),
    parallel = TRUE
  )
}
# Inspecting
p1 <- simulation(n = 250L) |> plot()
p2 <- simulation(n = 2500L) |> plot()
p3 <- simulation(n = 25000L) |> plot()
p4 <- simulation(n = 250000L) |> plot()

p1 + p2 + p3 + p4
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />
