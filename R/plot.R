#' Plot for CCI testing
#'
#' @param x Object of class 'CCI'
#' @param fill_color Color for the histogram fill
#' @param axis.text.x Size of x-axis text
#' @param axis.text.y Size of y-axis text
#' @param strip.text.x Size of x-axis strip text
#' @param strip.text.y Size of y-axis strip text
#' @param legend.text Size of legend text
#' @param legend.title Size of legend title
#' @param axis.title.x Size of x-axis title
#' @param axis.title.y Size of y-axis title
#' @param ... Additional arguments to ggplot2
#'
#' @import ggplot2
#' @importFrom stats density
#' @return A plot of the null distribution and the test statistic in ggplot2 format.
#' @seealso \code{\link{print.CCI}}, \code{\link{summary.CCI}},
#' \code{\link{plot.CCI}}, \code{\link{perm.test}}
#' @method plot CCI
#' @export
#'
#' @examples
#' dat <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = rnorm(100))
#' cci <- CCI.test(y ~ x1 + x2, data = dat, interaction = FALSE)
#' plot(cci)


plot.CCI <- function(x, 
                     fill_color = "lightblue", 
                     axis.text.x = 13, 
                     axis.text.y = 13, 
                     strip.text.x = 13, 
                     strip.text.y = 13, 
                     legend.text = 13, 
                     legend.title = 13,
                     axis.title.x = 13,
                     axis.title.y = 13,
                     ...) {
  if (!inherits(x, "CCI")) {
    stop("Object must be of class 'CCI'")
  }
  if (is.null(x$null.distribution)) stop("Missing null distribution")
  if (is.null(x$test.statistic)) stop("Missing test statistic")
  # Extracting the null distribution and test statistic
  null_dist <- unlist(x$null.distribution)
  test_stat <- unlist(x$test.statistic)
  xTitle <- x$metric
  # Create a data frame for ggplot2
  df <- data.frame(null_dist = null_dist)

  # Generate the plot
  plot <- ggplot2::ggplot(df, ggplot2::aes(x = null_dist)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            bins = 10,
                            fill = fill_color,
                            color = "black") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = test_stat), color = "black", linetype = "dashed", linewidth = 1) +
    ggplot2::labs(title = "Null distribution with test statistic",
         x = xTitle,
         y = "Freq.") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = element_text(size = axis.text.x),
                   axis.text.y = element_text(size = axis.text.y),
                   strip.text.x = element_text(size = strip.text.x),
                   strip.text.y = element_text(size = strip.text.y),
                   legend.text = element_text(size = legend.text),
                   legend.title = element_text(size = legend.title),
                   axis.title.x = element_text(size = axis.title.x),
                   axis.title.y = element_text(size = axis.title.y),
                   legend.position = 'none')

  additional_layers <- list(...)
  if (all(vapply(additional_layers, inherits, logical(1), what = "gg"))) {
    for (layer in additional_layers) {
      plot <- plot + layer
    }
  }
  print(plot)
  return(plot)
}

