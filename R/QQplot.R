#' QQ-plot for multiple testing in CCI
#'
#' @param object Object of class 'CCI'
#' @param n Number of samplings
#' @param ... Additional arguments to ggplot2
#'
#' @importFrom ggplot2 geom_qq
#' @return A QQ-plot of the p-values in ggplot2 format.
#' @seealso \code{\link{print.CCI}}, \code{\link{summary.CCI}},
#' \code{\link{plot.CCI}}, \code{\link{perm.test}}
#' @export
#'
#' @examples
#' dat <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = rnorm(100))
#' cci <- perm.test(y ~ x1 + x2, data = dat)
#' QQplot(cci, 1000)
QQplot <- function(object, n, ...){
  # Named QQplot to avoid overwriting qqplot
  if(!inherits(object, "CCI"))
    stop("Object must be of class 'CCI'")
  # Perform sampling and ML
  # Create QQ-plot
  # Return plot
  ggobj <- "Not implemented yet!"
  return(ggobj)
}
