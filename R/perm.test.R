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
#' @importFrom stats lm rnorm predict
#' @export
#' @seealso \code{\link{print.CCI}}, \code{\link{summary.CCI}},
#' \code{\link{plot.CCI}}, \code{\link{QQplot}}
#'
#' @examples
#' set.seed(123)
#' dat <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = rnorm(100))
#' perm.test(y ~ x1 + x2, data = dat)
perm.test <- function(formula, data, MLfunc = lm, nperm = 1000, dag_n = 1, ...) {
  # Check if formula is a DAGitty object
  # Take appropriate action
  # Create null distribution using MLfunc
  # Calculate empirical p-value
  # Calculate parametric p-value(s)

  # Example of using the MLfunc parameter
  preds <- predict(MLfunc(formula = formula, data = data, ...))

  # Gather everything in "obj"
  obj <- list(status  = "Not implemented yet!",
              MLfunc  = MLfunc,
              MLname  = deparse(substitute(MLfunc)),
              data    = data,
              formula = formula,
              dag_n   = dag_n,
              predictions = preds)
  class(obj) <- c("CCI", "list")
  obj
}
