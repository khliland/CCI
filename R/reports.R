#' @aliases reports print.CCI summary.CCI print.summary.CCI
#' @title Print and summary methods for the CCI class
#'
#' @param x Object of class 'CCI'
#' @param object Object of class 'CCI'
#' @param ... Additional arguments to print/summary
#'
#' @return The print methods have no return value, the summary methods return
#' an object of class 'summary.CCI'.
#' @rdname reports
#' @seealso \code{\link{perm.test}},
#' \code{\link{plot.CCI}}, \code{\link{QQplot}}
#' @export
print.CCI <- function(x, ...){
  # Print method for the CCI class
  cat(paste0("Conditional independence test using '", x$MLname, "'.\n"))
  cat("Not implemented yet!\n")
}

#' @export
#' @rdname reports
summary.CCI <- function(object, ...){
  # An extended overview of the results of the permutation testing
  # Returns important statistics etc.
  obj <- object # Or a subset or after doing some calculations
  class(obj) <- "summary.CCI"
  obj
}

#' @export
#' @rdname reports
print.summary.CCI <- function(x, ...){
  # Print method for the summary.CCI class
  cat(paste0("Conditional independence test using '", x$MLname, "'.\n"))
  if(inherits(x$formula, "formula"))
    cat("Formula: ", deparse(x$formula), "\n")
  else
    cat("DAGitty test: ", x$dag_n, "\n") # Change x$dag_n to something more informative
  cat("Not implemented yet!\n")
}
