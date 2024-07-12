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
#' perm.test(y ~ x1 | x2, data = dat)
perm.test <- function(formula = NA, data, MLfunc = lm, nperm = 500, dag = NA, dag_n = 1, ...) {
  if (is.null(data)) {
    stop("Please provide some data")
  }
  
  if (is.na(formula) & is.na(dag)) {
    stop("Formula and dag object is missing")
  } 

  if (is.na(formula) & class(dag) != 'dagitty') {
    stop("DAG needs to be of class dagitty.")
  } 
  
  if (class(dag) == 'dagitty' & is.na(formula)) {
    ci_statement <- impliedConditionalIndependencies(dag)[dag_n]
    names(ci_statement)[names(ci_statement) == dag_n] <- "CI"
    formula <- paste(ci_statement$CI$Y, " ~ ", ci_statement$CI$X, "|", paste(ci_statement$CI$Z, collapse = ", "))
    
  } else if (!is.na(formula)) {
      formula = formula
  }
  check_formula(formula)
  
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
