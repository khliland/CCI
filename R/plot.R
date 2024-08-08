#' Plot for CCI testing 
#'
#' @param object Object of class 'CCI'
#' @param ... Additional arguments to ggplot2
#'
#' @importFrom ggplot2 geom_qq
#' @return A plot of the null distribution and the test statistic in ggplot2 format.
#' @seealso \code{\link{print.CCI}}, \code{\link{summary.CCI}},
#' \code{\link{plot.CCI}}, \code{\link{perm.test}}
#' @export
#'
#' @examples
#' dat <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = rnorm(100))
#' cci <- perm.test(y ~ x1 + x2, data = dat)
#' plot(cci)


plot_null_distribution <- function(obj) {
  if (!inherits(obj, "CCI")) {
    stop("Object must be of class 'CCI'")
  }
  
  # Extracting the null distribution and test statistic
  null_dist <- unlist(obj$null.distribution)
  test_stat <- unlist(obj$test.statistic)
  
  # Create a data frame for ggplot2
  df <- data.frame(null_dist = null_dist)
  
  # Generate the plot
  p <- ggplot(df, aes(x = null_dist)) +
    geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "lightblue", color = "black", alpha = 0.7) +
    geom_density(color = "blue", linewidth = 1) +
    geom_vline(aes(xintercept = test_stat), color = "red", linetype = "dashed", linewidth = 1) +
    labs(title = "Null Distribution with Test Statistic",
         x = "Null Distribution",
         y = "Density") +
    theme_minimal()
  
  return(p)
}
