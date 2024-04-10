#' Acceptance-Rejection Method
#'
#' This function implements the acceptance-rejection method for generating random numbers from a given probability density function (pdf).
#'
#' @param n The number of random numbers to generate.
#' @param continuous A logical value indicating whether the pdf is continuous or discrete. Default is \code{TRUE}.
#' @param f The probability density function (`continuous = TRUE`), in the continuous case or the probability mass function, in the discrete case (`continuous = FALSE`).
#' @param args_f A list of arguments to be passed to the pdf function.
#' @param xlim A vector specifying the range of values for the random numbers in the form `c(min, max)`. Default is \code{c(0, 100)}.
#' @param c A constant value used in the acceptance-rejection method. If \code{NULL}, it will be estimated using the [lbfgs::lbfgs()] optimization algorithm. Default is \code{NULL}.
#' @param linesearch_algorithm The linesearch algorithm to be used in the [lbfgs::lbfgs()] optimization. Default is \code{"LBFGS_LINESEARCH_BACKTRACKING_ARMIJO"}.
#' @param max_iterations The maximum number of iterations for the [lbfgs::lbfgs()] optimization. Default is \code{1000}.
#' @param epsilon The convergence criterion for the [lbfgs::lbfgs()] optimization. Default is \code{1e-6}.
#' @param start_c The initial value for the constant \code{c} in the [lbfgs::lbfgs()] optimization. Default is \code{25}.
#' @param parallel A logical value indicating whether to use parallel processing for generating random numbers. Default is \code{FALSE}.
#' @param ... Additional arguments to be passed to the [lbfgs::lbfgs()] optimization algorithm. For details, see [lbfgs::lbfgs()].
#'
#' @return A vector of random numbers generated using the acceptance-rejection method.
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
#' On Windows operating systems, the code will be seral even if `parallel = TRUE` is set.
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
#' accept_reject(
#'  n = 2000L,
#'  f = dbinom,
#'  continuous = FALSE,
#'  args_f = list(size = 5, prob = 0.5),
#'  xlim = c(0, 10)
#' ) |> plot()
#'
#' accept_reject(
#'  n = 1000L,
#'  f = dnorm,
#'  continuous = TRUE,
#'  args_f = list(mean = 0, sd = 1),
#'  xlim = c(-4, 4)
#' ) |> plot()
#'
#' @import rlang
#' @importFrom lbfgs lbfgs
#' @importFrom purrr partial map_dbl
#' @importFrom pbmcapply pbmclapply
#' @importFrom numDeriv grad
#' @importFrom parallel detectCores
#' @importFrom stats dunif runif dweibull
#' @importFrom utils capture.output
#'
#' @export
accept_reject <-
  function(
      n = 1L,
      continuous = TRUE,
      f = dweibull,
      args_f = list(shape = 1, scale = 1),
      xlim = c(0, 100),
      c = NULL,
      linesearch_algorithm = "LBFGS_LINESEARCH_BACKTRACKING_ARMIJO",
      max_iterations = 1000L,
      epsilon = 1e-6,
      start_c = 25,
      parallel = FALSE,
      ...) {

    pdf <- purrr::partial(.f = f, !!!args_f)
    if(xlim[1L] == 0 && continuous) xlim[1L] <- .Machine$double.xmin

    if (continuous) {
      step <- 1e-5
      pdf_base <- purrr::partial(.f = dunif, min = xlim[1L], max = xlim[2L])
      base_generator <- purrr::partial(.f = runif, min = xlim[1L], max = xlim[2L])
    } else {
      step <- 1L
      pdf_base <- \(x) 1/ (xlim[2L] - xlim[1L] + 1)
      base_generator <- \(n) sample(x = xlim[1L]:xlim[2L], size = n, replace = TRUE)
    }

    x <- seq(from = xlim[1L], to = xlim[2L], by = step)

    a <- purrr::map_dbl(.x = x, .f = pdf)
    b <- purrr::map_dbl(.x = x, .f = pdf_base)

    x_max <- x[which.max((a/b)[!is.infinite(a/b)])]

    objective_c <- function(c) {
      differences <-
      (pdf(x_max) - c * pdf_base(x_max))^2

      if(is.infinite(differences)) return(.Machine$double.xmax)
      else return(differences)
    }

    gradient_objective_c <- function(c) {
      numDeriv::grad(
        func = objective_c,
        x = c
      )
    }

    try_gradient_objective_c <- function(c) {
      tryCatch(
        gradient_objective_c(c = c),
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
        )$par
    }
    one_step <- function(i) {
      repeat{
        x <- base_generator(n = 1L)
        u <- runif(n = 1L)
        if (u <= pdf(x = x) / (c * pdf_base(x = x))) {
          return(x)
        }
      }
    }

    if (parallel && Sys.info()["sysname"] %in% c("Linux", "Darwin")) {
      capture.output(
        r <-
          pbmcapply::pbmclapply(
            X = 1L:n,
            FUN = one_step,
            mc.cores = parallel::detectCores()
          ) |> unlist()
      )
    } else {
      r <- purrr::map_dbl(1L:n, ~one_step(i = .))
    }
    class(r) <- "accept_reject"
    attr(r, "f") <- f
    attr(r, "args_f") <- args_f
    attr(r, "c") <- c
    attr(r, "continuous") <- continuous
    return(r)
  }
