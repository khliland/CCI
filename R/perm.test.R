#' Permutation test for conditional independence
#'
#' @param formula Model formula or DAGitty object
#' @param data Data frame
#' @param MLfunc Model fitting function similar to \code{lm} (default)
#' @param nperm Number of permutations
#' @param dag_n Which test to perform if using a DAGitty object
#' @param ... Additional arguments to pass to \code{MLfunc}
#'
#' @return An object of class 'CCI' containing a null distribution,
#' observed value, p-values, the ML model, and the data.
#' @importFrom stats lm rnorm
#' @export
#' @seealso \code{\link{print.CCI}}, \code{\link{summary.CCI}},
#' \code{\link{plot.CCI}}, \code{\link{QQplot}}
#'
#' @examples
#' dat <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = rnorm(100))
#' perm.test(y ~ x1 + x2, data = dat)
perm.test <- function(formula, data, MLfunc = lm, nperm = 1000, dag_n = 1, ...) {
  # Check if formula is a DAGitty object
  # Take appropriate action
  # Create null distribution using MLfunc
  # Calculate empirical p-value
  # Calculate parametric p-value(s)
  # Gather everything in "obj"
  obj <- "Not implemented yet!"
  class(obj) <- c("CCI", "list")
  obj
}
