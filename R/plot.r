.onLoad <- function(libname, pkgname) {
  utils::globalVariables("prop")
}

#' @title Plot Accept-Reject
#' @description
#' Inspects the probability function (discrete case) or probability density
#' (continuous case) by comparing the theoretical case with the observed one.
#'
#' @param x An object of class `accept reject`
#' @param color_observed_density Observed density color (continuous case)
#' @param color_true_density True density color (continuous case)
#' @param color_bar Bar chart fill color (discrete case)
#' @param color_observable_point Color of generated points (discrete case)
#' @param color_real_point Color of real probability points (discrete case)
#' @param alpha Bar chart transparency (discrete case)
#' @param ... Additional arguments
#'
#' @importFrom ggplot2 ggplot aes after_stat geom_line geom_freqpoly geom_point geom_bar labs scale_color_manual theme element_text
#' @importFrom glue glue
#' @importFrom rlang list2
#' @importFrom cli cli_alert_success cli_alert_info
#' @importFrom scales percent
#' @importFrom stats density
#'
#' @examples
#'
#' accept_reject(
#'    n = 2000L,
#'    f = dbinom,
#'    continuous = FALSE,
#'    args_f = list(size = 5, prob = 0.5),
#'    xlim = c(0, 10)
#' ) |> plot()
#'
#' accept_reject(
#'   n = 1000L,
#'   f = dnorm,
#'   continuous = TRUE,
#'   args_f = list(mean = 0, sd = 1),
#'   xlim = c(-4, 4)
#' ) |> plot()
#'
#' @import rlang
#' @export
plot.accept_reject <-
  function(
    x,
    color_observed_density = "blue",
    color_true_density = "black",
    color_bar = "blue",
    color_observable_point = "red",
    color_real_point = "green",
    alpha = .3,
    ...
  ){

  y <-
    do.call(
      attr(x, "f"),
      rlang::list2(
        as.vector(x),
        !!!attr(x, "args_f")
      )
    )

  data <- data.frame(x = as.vector(x), y = y)

  graphic <- function(x){
    if(attr(x, "continuous")){
      capture.output(
        p <-
          ggplot2::ggplot(data, ggplot2::aes(x = x)) +
          ggplot2::geom_line(aes(y = y, color = "True density")) +
          ggplot2::geom_freqpoly(aes(y = after_stat(density), color = "Observed density"), bins = 30) +
          ggplot2::scale_color_manual(values = c("True density" = color_true_density, "Observed density" = color_observed_density)) +
          ggplot2::labs(
            x = "x",
            y = "f(x)",
            title = "Probability density function",
            subtitle = "Real x Observed",
            color = "Legenda"
          ) +
          ggplot2::theme(
            axis.title = ggplot2::element_text(face = "bold"),
            title = ggplot2::element_text(face = "bold"),
            legend.title = ggplot2::element_text(face = "bold"),
            plot.subtitle = ggplot2::element_text(face = "plain")
          )
      )
      return(p)
    } else {
      capture.output(
        p <- ggplot2::ggplot(data, ggplot2::aes(x = x)) +
          ggplot2::geom_line(aes(y = y)) +
          ggplot2::geom_point(aes(y = y, color = "Observable Probability")) +
          ggplot2::geom_bar(aes(y = after_stat(prop), group = 1), fill = color_bar, alpha = alpha) +
          ggplot2::geom_point(aes(y = after_stat(prop), group = 1, color = "Real Probability"), stat = "count") +
          ggplot2::scale_color_manual(values = c("Observable Probability" = color_observable_point, "Real Probability" = color_real_point)) +
          ggplot2::scale_y_continuous(labels = scales::percent) +
          ggplot2::labs(
            x = "x",
            y = "P(X = x)",
            title = "Probability Function",
            subtitle = "Real x Observed",
            color = "Legend"
          ) +
          ggplot2::theme(
            axis.title = ggplot2::element_text(face = "bold"),
            title = ggplot2::element_text(face = "bold"),
            legend.title = ggplot2::element_text(face = "bold"),
            plot.subtitle = ggplot2::element_text(face = "plain")
          )
      )
      return(p)
    }
  }
  graphic(x)
}
