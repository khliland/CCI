#' Plot for CCI testing
#'
#' @param x Object of class 'CCI'
#' @param fill_color Color for the histogram fill
#' @param axis.text.x Size of x-axis text
#'
#' @param ... Additional arguments to ggplot2
#'
#' @import ggplot2
#' @importFrom stats density
#' @return A plot of the null distribution and the test statistic in ggplot2 format.
#' @seealso \code{\link{print.CCI}}, \code{\link{summary.CCI}},
#' \code{\link{plot.CCI}}, \code{\link{perm.test}}
#' @export
#'
#' @examples
#' dat <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = rnorm(100))
#' cci <- CCI.test(y ~ x1 + x2, data = dat, interaction = FALSE)
#' plot(cci)


plot.CCI <- function(x, fill_color = "lightblue", axis.text.x = 17, axis.text.y = 17, strip.text.x = 17, strip.text.7 = 17, legend.text = 17, legend.title = 17, ...) {
  if (!inherits(x, "CCI")) {
    stop("Object must be of class 'CCI'")
  }

  # Extracting the null distribution and test statistic
  null_dist <- unlist(x$null.distribution)
  test_stat <- unlist(x$test.statistic)

  # Create a data frame for ggplot2
  df <- data.frame(null_dist = null_dist)

  # Generate the plot
  plot <- ggplot2::ggplot(df, ggplot2::aes(x = null_dist)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            bins = 10,
                            fill = fill_color,
                            color = "black",
                            alpha = 0.7) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = test_stat), color = "black", linetype = "dashed", linewidth = 1) +
    ggplot2::labs(title = "Null distribution with test statistic",
         x = "Value",
         y = "Freq.") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = element_text(size = axis.text.x),
                   axis.text.y = element_text(size = axis.text.y),
                   strip.text.x = element_text(size = strip.text.x),
                   strip.text.y = element_text(size = strip.text.y),
                   legend.text = element_text(size = legend.text),
                   legend.title = element_text(size = legend.title), legend.position = 'none')

  if (!missing(...)) {
    additional_layers <- list(...)
    for (layer in additional_layers) {
      plot <- plot + layer
    }
  }

  print(plot)
  return(plot)
}

