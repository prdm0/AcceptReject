.onLoad <- function(libname, pkgname) {
  utils::globalVariables("prop")
}
#' @title Plot Accept-Reject
#'
#' @description
#' Inspects the probability function (discrete case) or probability density
#' (continuous case) by comparing the theoretical case with the observed one.
#'
#' @param x An object of class `accept reject`
#' @param color_observed_density Observed density color (continuous case).
#' @param color_true_density True histogram density color (continuous case)
#' @param color_bar Bar chart fill color (discrete case)
#' @param color_observable_point Color of generated points (discrete case)
#' @param color_real_point Color of real probability points (discrete case)
#' @param alpha Bar chart transparency (discrete case) and observed density
#' (continuous case)
#' @param hist If TRUE, a histogram will be plotted in the continuous case,
#' comparing the theoretical density with the observed one. If FALSE,
#' [ggplot2::geom_density()] will be used instead of the histogram.
#'
#' @param ... Additional arguments.
#'
#' @details
#' The function [plot.accept_reject()] is responsible for plotting the
#' probability function (in the discrete case) or the probability density (in
#' the continuous case), comparing the theoretical case with the observed one.
#' It is useful, therefore, for inspecting the quality of the samples generated
#' by the acceptance-rejection method. The returned plot is an object of classes
#' `gg` and `ggplot`. Easily, you can further customize the plot.
#'
#' The function [plot.accept_reject()], or simply [plot()], constructs the plot
#' for inspection and expects an object of class `accept_reject` as an argument.
#'
#' @return An object of class `gg` and `ggplot` from the package \pkg{ggplot2}.
#' The function [plot.accept_reject()] expects an object of class
#' `accept_reject` as an argument.
#'
#' @seealso [accept_reject()] and [print.accept_reject()].
#'
#' @importFrom ggplot2 ggplot aes after_stat geom_line geom_point geom_histogram
#' geom_bar labs scale_color_manual theme element_text
#' @importFrom glue glue
#' @importFrom rlang list2
#' @importFrom cli cli_alert_success cli_alert_info
#' @importFrom scales percent
#' @importFrom stats density
#' @importFrom graphics hist
#' @import rlang
#'
#' @examples
#' x <- accept_reject(
#'    n = 1000L,
#'    f = dbinom,
#'    continuous = FALSE,
#'    args_f = list(size = 5, prob = 0.5),
#'    xlim = c(0, 10)
#' )
#' plot(x)
#'
#' y <- accept_reject(
#'   n = 500L,
#'   f = dnorm,
#'   continuous = TRUE,
#'   args_f = list(mean = 0, sd = 1),
#'   xlim = c(-4, 4)
#' )
#' plot(y)
#'
#' @export
plot.accept_reject <-
  function(
    x,
    color_observed_density = "#BB9FC9", #"#E65A65", # "#FBBA78",
    color_true_density = "#FE4F0E",
    color_bar = "#BB9FC9", #"#E65A65", #"#FCEFC3",
    color_observable_point = "#7BBDB3",
    color_real_point = "#FE4F0E",
    alpha = .3,
    hist = TRUE,
    ...
  ){

  y <- do.call(attr(x, "f"), list(as.vector(x)))
  data <- data.frame(x = as.vector(x), y = y)

  graphic <- function(x){
    if(attr(x, "continuous")){
      if(hist){
        p <-
          ggplot2::ggplot(data, ggplot2::aes(x = x)) +
          ggplot2::geom_histogram(ggplot2::aes(y = after_stat(density), color = "Observed density"), fill = color_observed_density, alpha = alpha, breaks = hist(data$x, plot = FALSE)$breaks)
      } else {
        p <- ggplot2::ggplot(data, ggplot2::aes(x = x)) +
          ggplot2::geom_density(ggplot2::aes(y = after_stat(density), color = "Observed density"), position = "stack", fill = color_observed_density, alpha = alpha)
      }
      p <-
        p +
        # ggplot2::ggplot(data, ggplot2::aes(x = x)) +
        ggplot2::geom_line(aes(y = y, color = "True density")) +
        ggplot2::scale_color_manual(values = c("True density" = color_true_density, "Observed density" = color_observed_density)) +
        ggplot2::labs(
          x = "x",
          y = "f(x)",
          title = "Probability density function",
          subtitle = "True x Observed",
          color = "Legend"
        )
    } else {
      p <-
        ggplot2::ggplot(data, ggplot2::aes(x = x)) +
        ggplot2::geom_bar(aes(y = after_stat(prop), group = 1L), fill = color_bar, alpha = alpha) +
        ggplot2::geom_line(aes(y = y), linetype = "dotted") +
        ggplot2::geom_point(aes(y = y, color = "Observable Probability")) +
        ggplot2::geom_point(aes(y = after_stat(prop), group = 1L, color = "True Probability"), stat = "count") +
        ggplot2::scale_color_manual(values = c("Observable Probability" = color_observable_point, "True Probability" = color_real_point)) +
        ggplot2::scale_y_continuous(labels = scales::percent) +
        ggplot2::labs(
          x = "x",
          y = "P(X = x)",
          title = "Probability Function",
          subtitle = "True x Observed",
          color = "Legend"
        )
    }
    p <- p +
      ggplot2::theme(
        axis.title = ggplot2::element_text(face = "bold"),
        title = ggplot2::element_text(face = "bold"),
        legend.title = ggplot2::element_text(face = "bold"),
        plot.subtitle = ggplot2::element_text(face = "plain")
      )
    return(p)
  }
  graphic(x)
}
