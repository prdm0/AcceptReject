#' @title Print method for accept_reject objects
#' @description Print method for accept_reject objects.
#' @param x An accept_reject object.
#' @param n_min Minimum number of observations to print.
#' @param ... Additional arguments.
#'
#' @details
#' The function [print.accept_reject()] is responsible for printing an object of
#' class `accept_reject` in a formatted manner, providing some information
#' about the `accept_reject` object, including the number of observations, the
#' value of the constant \eqn{c} that maximizes acceptance, and the acceptance
#' probability \eqn{1/c}. Additionally, it prints the first generated
#' observations. The function [print.accept_reject()] delivers formatted output
#' when executing an object of class `accept_reject` in the console or when
#' executing the function [print()] on an object of class `accept_reject`,
#' returned by the function [accept_reject()].

#' @return
#' An object of class `character`, providing a formatted output with some
#' information about the `accept_reject` object, including the number of
#' observations, the value of the constant \eqn{c} that maximizes acceptance, and
#' the acceptance probability \eqn{1/c}. Additionally, it prints the first
#' generated observations. The function [print.accept_reject()] enables
#' formatting when executing an object of class 'accept_reject' in the console
#' or when executing the function [print()] on an object of class
#' `accept_reject`, returned by the function [accept_reject()].
#'
#' @seealso [accept_reject()] and [plot.accept_reject()].
#'
#' @importFrom cli cli_h1 cli_alert_success
#' @importFrom glue glue
#' @examples
#' set.seed(0) # setting a seed for reproducibility
#' x = accept_reject(
#'    n = 2000L,
#'    f = dbinom,
#'    continuous = FALSE,
#'    args_f = list(size = 10, prob = 0.5),
#'    xlim = c(0, 10)
#' )
#' print(x)
#' @export
print.accept_reject <- function(x, n_min = 10L, ...) {
  cli::cli_h1("Accept-Reject Samples")
  cat('\n')
  cli::cli_alert_info("It's not necessary, but if you want to extract the observations, use as.vector().")
  cat('\n')
  n <- length(x)

  case <- if (attr(x, "continuous")) "continuous" else "discrete"

  cli_alert_success(glue("Case: {case}"))
  cli_alert_success(glue("Number of observations: {n}"))
  cli_alert_success(glue("c: {round(attr(x, 'c'), digits = 4L)}"))
  cli_alert_success(glue("Probability of acceptance (1/c): {round(1/attr(x, 'c'), digits = 4L)}"))
  if (n <= n_min) {
    cli_alert_success(glue("Observations: {paste(round(x[1L:n], 4L), collapse = ' ')}"))
  } else {
    cli_alert_success(glue("Observations: {paste(round(x[1L:n_min], 4L), collapse = ' ')}..."))
  }

  limits <- attr(x, "xlim")

  if(limits[1L] >= 0 && limits[1L] < 1e-10) limits[1L] <- 0

  cli_alert_success(glue("xlim = {paste(limits, collapse = ' ')}"))
  cli_h1("")
}
