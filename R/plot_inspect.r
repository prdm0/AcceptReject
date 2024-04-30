#' @title Inspecting the theoretical density with the base density
#'
#' @description
#' Inspect the probability density function used as the base with the theoretical
#' density function from which observations are desired.
#'
#' @param f Theoretical density function.
#' @param args_f List of arguments for the theoretical density function.
#' @param f_base Base density function.
#' @param args_f_base List of arguments for the base density function.
#' @param xlim The range of the x-axis.
#' @param c A constant that covers the base density function, with \eqn{c \geq 1}. The
#' default value is 1.
#' @param alpha The transparency of the base density function. The default value
#' is 0.4
#' @param color_intersection Color of the intersection between the base density function
#' and theoretical density functions.
#' @param color_f Color of the base density function.
#' @param color_f_base Color of the theoretical density function.
#'
#' @import rlang
#' @importFrom ggplot2 ggplot geom_area geom_line scale_fill_manual
#' scale_colour_manual labs theme annotate element_blank
#' @importFrom purrr partial
#' @importFrom utils head tail
#'
#' @return An object of the `gg` and `ggplot` class comparing the theoretical
#' density function with the base density function. The object shows the
#' compared density functions, the intersection area between them, and the
#' value of the area.
#'
#' @details
#' The function [inspect()] returns an object of the `gg` and `ggplot` class that
#' compares the probability density of two functions and is not useful for the
#' discrete case, only for the continuous one. Finding the parameters of the
#' base distribution that best approximate the theoretical distribution and the
#' smallest value of `c` that can cover the base distribution is a great strategy.

#' Something important to note is that the plot provides the value of the area of
#' intersection between the theoretical probability density function we want to
#' generate observations from and the probability density function used as the
#' base. It's desirable for this value to be as close to 1 as possible, ideally
#' 1. When the intersection area between the probability density functions is 1,
#' it means that the base probability density function passed to the `f_base`
#' argument overlaps the theoretical density function passed to the `f` argument.
#' This is crucial in the acceptance-rejection method.

#' However, even if you don't use the [inspect()] function to find a suitable
#' distribution, by finding viable args_base (list of arguments passed to f_base)
#' and the value of `c` so that the intersection area is 1, the [accept_reject()]
#' function already does this for you.

#' The [inspect()] function is helpful for finding a suitable base distribution,
#' which increases the probability of acceptance, further reducing computational
#' cost. Therefore, inspecting is a good practice.
#'
#' If you use the [accept_reject()] function, even with parallelism enabled by
#' specifying `parallel = TRUE` in [accept_reject()] and find that the generation
#' time is high for your needs, consider inspecting the base distribution.
#'
#' @seealso [accept_reject()], [print.accept_reject()] and [plot.accept_reject()].
#'
#' @examples
#' # Considering c = 1 (default)
#' inspect(
#'    f = dweibull,
#'    f_base = dgamma,
#'    xlim = c(0,5),
#'    args_f = list(shape = 2, scale = 1),
#'    args_f_base = list(shape = 2.1, rate = 2),
#'    c = 1
#' )
#'
#' # Considering c = 1.35.
#' inspect(
#'    f = dweibull,
#'    f_base = dgamma,
#'    xlim = c(0,5),
#'    args_f = list(shape = 2, scale = 1),
#'    args_f_base = list(shape = 2.1, rate = 2),
#'    c = 1.35
#' )
#'
#' # Plotting f equal to f_base. This would be the best-case scenario, which,
#' # in practice, is unlikely.
#' inspect(
#'    f = dgamma,
#'    f_base = dgamma,
#'    xlim = c(0,5),
#'    args_f = list(shape = 2.1, rate = 2),
#'    args_f_base = list(shape = 2.1, rate = 2),
#'    c = 1
#' )
#'
#' @export
inspect <-
  function(
    f,
    args_f,
    f_base,
    args_f_base,
    xlim,
    c = 1,
    alpha = 0.4,
    color_intersection = "#BB9FC9",
    color_f = "#F890C2", #"#FE4F0E",
    color_f_base = "#7BBDB3"
  ) {

  if(xlim[1L] == 0) xlim[1L] <- .Machine$double.xmin

  f <- purrr::partial(f, !!!args_f)
  f_base <- purrr::partial(f_base, !!!args_f_base)

  x <- seq(xlim[1], xlim[2], length.out = 1000)

  df <- data.frame(x = x, f = f(x), f_base = c * f_base(x))

  inter <- base::pmin(df$f, df$f_base)

  # Calculate the area under the intersection curve using the trapezoid rule
  area <- sum(diff(df$x) * (head(inter, -1) + tail(inter, -1))) / 2

  ggplot(df, aes(x = x)) +
    geom_area(aes(y = inter, fill = "Intersection"), alpha = alpha) +
    geom_line(aes(y = f, colour = "True density")) +
    geom_line(aes(y = f_base, colour = "Base density")) +
    scale_fill_manual("", values = color_intersection) +
    scale_colour_manual("", values = c("True density" = color_f, "Base density" = color_f_base)) +
    labs(
      x = "x",
      y = "f(x) vs f_base(x)",
      title = "Probability density functions",
      subtitle = "True x Base - Overlay"
    ) +
    annotate("text", x = mean(xlim), y = max(df$f, df$f_base), label = paste("Intersection area: ", round(area, 2))) +
    theme(
      axis.title = element_text(face = "bold"),
      title = element_text(face = "bold"),
      legend.title = element_blank(),
      plot.subtitle = element_text(face = "plain")
    )
}
