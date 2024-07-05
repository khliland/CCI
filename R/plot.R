#' Plot of null distribution and observed value
#'
#' @param x Object of class 'CCI'
#' @param y Bin width for ggplot2's histogram
#' @param overlay Overlay curve options: 'none', 'norm', 'density'
#' @param ... Additional arguments to be passed to ggplot2
#'
#' @return A plot of the null distribution and observed value in ggplot2 format.
#' @importFrom ggplot2 geom_histogram ggplot
#' @seealso \code{\link{print.CCI}}, \code{\link{summary.CCI}},
#' \code{\link{perm.test}}, \code{\link{QQplot}}
#' @export
#'
#' @examples
#' dat <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = rnorm(100))
#' cci <- perm.test(y ~ x1 + x2, data = dat)
#' plot(cci)
plot.CCI <- function(x, y = x$bin.width,
                     overlay = c("none", "norm", "density"), ...){
  # Overloading of plot function for CCI objects, i.e., plot(CCI)
  # ggplot2 histogram
  # Return plot
  ggobj <- "Not implemented yet!"
  return(ggobj)
}
