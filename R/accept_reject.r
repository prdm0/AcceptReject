#' Acceptance-Rejection Method
#'
#' This function implements the acceptance-rejection method for generating random numbers from a given probability density function (pdf).
#'
#' @param n The number of random numbers to generate.
#' @param continuous A logical value indicating whether the pdf is continuous or discrete. Default is \code{TRUE}.
#' @param f The probability density function (`continuous = TRUE`), in the continuous case or the probability mass function, in the discrete case (`continuous = FALSE`).
#' @param args_f A list of arguments to be passed to the `f` function. It refers to the list of arguments of the target distribution.
#' @param f_base Base probability density function (for continuous case).If `f_base = NULL`,
#' a uniform distribution will be used. In the discrete case, this argument is ignored,
#' and a uniform probability mass function will be used as the base.
#' @param random_base Random number generation function for the base distribution passed as an argument to `f_base`.
#' If `random_base = NULL` (default), the uniform generator will be used. In the discrete case, this argument is
#' disregarded, and the uniform random number generator function will be used.
#' @param args_f_base A list of arguments for the base distribution. This refers to the list of arguments that will be passed to the function `f_base`.
#' It will be disregarded in the discrete case.
#' @param xlim A vector specifying the range of values for the random numbers in the form `c(min, max)`. Default is \code{c(0, 100)}.
#' @param c A constant value used in the acceptance-rejection method. If \code{NULL}, it will be estimated using the [lbfgs::lbfgs()] optimization algorithm. Default is \code{NULL}.
#' @param linesearch_algorithm The linesearch algorithm to be used in the [lbfgs::lbfgs()] optimization. Default is \code{"LBFGS_LINESEARCH_BACKTRACKING_ARMIJO"}.
#' @param max_iterations The maximum number of iterations for the [lbfgs::lbfgs()] optimization. Default is \code{1000}.
#' @param epsilon The convergence criterion for the [lbfgs::lbfgs()] optimization. Default is \code{1e-6}.
#' @param start_c The initial value for the constant \code{c} in the [lbfgs::lbfgs()] optimization. Default is \code{25}.
#' @param parallel A logical value indicating whether to use parallel processing for generating random numbers. Default is \code{FALSE}.
#' @param warning A logical value indicating whether to show warnings. Default is \code{TRUE}.
#' @param ... Additional arguments to be passed to the [lbfgs::lbfgs()] optimization algorithm. For details, see [lbfgs::lbfgs()].
#'
#' @return A vector of random numbers generated using the acceptance-rejection method.
#' The return is an object of `class accept_reject`, but it can be treated as an atomic vector.
#'
#' @details
#' In situations where we cannot use the inversion method (situations where it is not possible to obtain the quantile function) and we do not know a transformation that involves a random variable from which we can generate observations, we can use the acceptance and rejection method.
#' Suppose that \eqn{X} and \eqn{Y} are random variables with probability density function (pdf) or probability function (pf) \eqn{f} and \eqn{g}, respectively. In addition, suppose that there is a constant \eqn{c} such that
#'
#' \deqn{f(x) \leq c \cdot g(x), \quad \forall x \in \mathbb{R}.}
#'
#' for all values of \eqn{t}, with \eqn{f(t)>0}. To use the acceptance and rejection method to generate observations from the random variable \eqn{X}, using the algorithm below, first find a random variable \eqn{Y} with pdf or pf \eqn{g}, that satisfies the above condition.
#'
#' Algorithm of the Acceptance and Rejection Method:
#'
#' 1 - Generate an observation \eqn{y} from a random variable \eqn{Y} with pdf/pf \eqn{g};
#'
#' 2 - Generate an observation \eqn{u} from a random variable \eqn{U\sim \mathcal{U} (0, 1)};
#'
#' 3 - If \eqn{u < \frac{f(y)}{cg(y)}} accept \eqn{x = y}; otherwise reject \eqn{y} as an observation of the random variable \eqn{X} and return to step 1.
#'
#' Proof: Let's consider the discrete case, that is, \eqn{X} and \eqn{Y} are random variables with pf's \eqn{f} and \eqn{g}, respectively. By step 3 of the above algorithm, we have that \eqn{{accept} = {x = y} = u < \frac{f(y)}{cg(y)}}. That is,
#'
#' \eqn{P(accept | Y = y) = \frac{P(accept \cap {Y = y})}{g(y)} = \frac{P(U \leq f(y)/cg(y)) \times g(y)}{g(y)} = \frac{f(y)}{cg(y)}.}
#'
#' Hence, by the Total Probability Theorem, we have that:
#'
#' \eqn{P(accept) = \sum_y P(accept|Y=y)\times P(Y=y) = \sum_y \frac{f(y)}{cg(y)}\times g(y) = \frac{1}{c}.}
#'
#' Therefore, by the acceptance and rejection method we accept the occurrence of $Y$ as being an occurrence of \eqn{X} with probability \eqn{1/c}. In addition, by Bayes' Theorem, we have that
#'
#' \eqn{P(Y = y | accept) = \frac{P(accept|Y = y)\times g(y)}{P(accept)} = \frac{[f(y)/cg(y)] \times g(y)}{1/c} = f(y).}
#'
#' The result above shows that accepting \eqn{x = y} by the procedure of the algorithm is equivalent to accepting a value from \eqn{X} that has pf \eqn{f}.
#'
#' The argument `c = NULL` is the default. Thus, the function [accept_reject()] estimates the value of c using the optimization algorithm [lbfgs::lbfgs()]. For more details, see [lbfgs::lbfgs()].
#' If a value of `c` is provided, the function [accept_reject()] will use this value to generate the random observations. An inappropriate choice of c can lead to low efficiency of the acceptance and rejection method.
#'
#' In Unix-based operating systems, the function [accept_reject()] can be executed in parallel. To do this, simply set the argument `parallel = TRUE`.
#' The function [accept_reject()] utilizes the [parallel::mclapply()] function to execute the acceptance and rejection method in parallel.
#' On Windows operating systems, the code will not be parallelized even if `parallel = TRUE` is set.
#'
#' For the continuous case, a base density function can be used, where the arguments
#' `f_base`, `random_base` and `args_f_base` need to be passed. If at least one of
#' them is `NULL`, the function will assume a uniform density function over the
#' interval `xlim`.
#'
#' For the discrete case, the arguments `f_base`, `random_base` and `args_f_base`
#' should be `NULL`, and if they are passed, they will be disregarded, as for
#' the discrete case, the discrete uniform distribution will always be
#' considered as the base. Sampling from the discrete uniform distribution
#' has shown good performance for the discrete case.
#'
#' @seealso [parallel::mclapply()] and [lbfgs::lbfgs()].
#'
#' @references CASELLA, George; ROBERT, Christian P.; WELLS, Martin T. Generalized accept-reject sampling schemes. Lecture Notes-Monograph Series, p. 342-347, 2004.
#' @references NEAL, Radford M. Slice sampling. The annals of statistics, v. 31, n. 3, p. 705-767, 2003.
#' @references BISHOP, Christopher. 11.4: Slice sampling. Pattern Recognition and Machine Learning. Springer, 2006.
#'
#' @examples
#' set.seed(0) # setting a seed for reproducibility
#'
#' x <- accept_reject(
#'   n = 2000L,
#'   f = dbinom,
#'   continuous = FALSE,
#'   args_f = list(size = 5, prob = 0.5),
#'   xlim = c(0, 10)
#' )
#' plot(x)
#'
#' y <- accept_reject(
#'   n = 1000L,
#'   f = dnorm,
#'   continuous = TRUE,
#'   args_f = list(mean = 0, sd = 1),
#'   xlim = c(-4, 4)
#' )
#' plot(y)
#'
#' @import rlang
#' @importFrom lbfgs lbfgs
#' @importFrom purrr partial map_dbl
#' @importFrom numDeriv grad
#' @importFrom parallel detectCores mclapply
#' @importFrom stats dunif runif dweibull
#' @importFrom utils capture.output
#' @importFrom assertthat assert_that
#' @importFrom cli cli_alert_danger cli_alert_warning
#' @importFrom glue glue
#' @useDynLib AcceptReject, .registration = TRUE
#' @importFrom Rcpp evalCpp
#'
#' @export
accept_reject <-
  function(n = 1L,
           continuous = TRUE,
           f = NULL,
           args_f = NULL,
           f_base = NULL,
           random_base = NULL,
           args_f_base = NULL,
           xlim = NULL,
           c = NULL,
           linesearch_algorithm = "LBFGS_LINESEARCH_BACKTRACKING_ARMIJO",
           max_iterations = 0L,
           epsilon = 1e-5,
           start_c = 25,
           parallel = FALSE,
           warning = TRUE,
           ...) {
    assertthat::assert_that(
      !is.null(f),
      msg = cli::cli_alert_danger("You need to pass the argument f referring to the probability density or mass function that you want to generate observations.")
    )

    assertthat::assert_that(
      !is.null(args_f),
      msg = cli::cli_alert_danger("You need to pass args_f with the parameters that index f.")
    )

    assertthat::assert_that(
      !is.null(xlim),
      msg = cli::cli_alert_danger("You must provide the vector xlim argument (generation support).")
    )

    f <- purrr::partial(.f = f, !!!args_f)

    if (warning && f(xlim[2L]) > 0.01) {
      cli::cli_alert_warning(
        glue::glue("Warning: f({xlim[2L]}) is {f(xlim[2L])}. If f is defined for x >= f({xlim[2L]}), trying a upper limit might be better.")
      )
    }

    if (warning && xlim[1L] < 0 && f(xlim[1L]) > 0.01) {
      cli::cli_alert_warning(
        glue::glue("Warning: f({xlim[1L]}) is {f(xlim[1L])}. If f is defined for x <= f({xlim[1L]}), trying a lower limit might be better.")
      )
    }

    # Uniform distribution will be used if not all information from the base
    # distribution is provided.
    any_null <- any(is.null(c(f_base, random_base, args_f_base)))
    if (continuous && any_null) {
      f_base <- purrr::partial(.f = dunif, min = xlim[1L], max = xlim[2L])
      random_base <- purrr::partial(.f = runif, min = xlim[1L], max = xlim[2L])
    }

    # Is it a discrete random variable?
    if (!continuous) {
      f_base <- function(x) dunif(x, min = xlim[1L], max = xlim[2L])
      random_base <- function(n) sample(x = xlim[1L]:xlim[2L], size = n, replace = TRUE)
    } else if (continuous && !any_null) {
      if (xlim[1L] == 0) xlim[1L] <- .Machine$double.xmin
      f_base <- purrr::partial(.f = f_base, !!!args_f_base)
      random_base <- purrr::partial(.f = random_base, !!!args_f_base)
    }

    objective_c <- function(c) {
      y <- round(mean(xlim), digits = 0L)

      differences <- log(f(y)) - (log(c) + f_base(y))

      if (is.infinite(differences) && continuous) {
        return(.Machine$double.xmax)
      } else {
        return(exp(differences))
      }
    }

    gradient_objective_c <- function(c) {
      numDeriv::grad(
        func = objective_c, # function(c) objective_c(c = c, y = y),
        x = c
      )
    }

    try_gradient_objective_c <- function(c) {
      tryCatch(
        gradient_objective_c(c),
        error = function(e) NaN
      )
    }

    if (is.null(c)) {
      c <-
        lbfgs::lbfgs(
          call_eval = objective_c,
          call_grad = try_gradient_objective_c,
          vars = start_c,
          invisible = 1L,
          max_iterations = max_iterations,
          epsilon = epsilon,
          linesearch_algorithm = linesearch_algorithm,
          ...
        )$par[1L]
    }

    if (parallel && .Platform$OS.type == "unix") {
      n_cores <- parallel::detectCores()
      n_per_core <- n %/% n_cores
      remainder <- n %% n_cores
      n_each_core <-
        c(
          rep(n_per_core, n_cores - remainder),
          rep(n_per_core + 1L, remainder)
        )
      r <- unlist(parallel::mclapply(
        X = n_each_core,
        FUN = function(n) one_step(n, f, f_base, random_base, c),
        mc.cores = n_cores
      ))
    } else {
      r <- one_step(n, f, f_base, random_base, c)
    }

    class(r) <- "accept_reject"
    attr(r, "f") <- f
    attr(r, "args_f") <- args_f
    attr(r, "c") <- c
    attr(r, "continuous") <- continuous
    attr(r, "xlim") <- xlim
    return(r)
  }
