#' @aliases reports print.CCI summary.CCI print.summary.CCI
#' @title Print and summary methods for the CCI class
#'
#' @param x Object of class 'CCI'
#' @param ... Additional arguments to print/summary
#'
#' @return The print methods have no return value, the summary methods return
#' an object of class 'summary.CCI'.
#' @rdname reports
#' @seealso \code{\link{perm.test}},
#' \code{\link{plot.CCI}}, \code{\link{QQplot}}
#' @export
#' @export
print.summary.CCI <- function(x, ...) {
  cat("\nComputational Conditional Independence Test\n")
  cat("--------------------------------------------\n")
  cat("Method:   ", x$method, "\n")
  cat("Formula:  ", x$data.name, "\n")
  cat("Permutations: ", x$parameter, "\n")
  cat("Metric:   ", x$metric, "\n")
  cat("Tail:     ", x$tail, "\n")
  cat("Statistic:", format(x$statistic, digits = 4), "\n")
  cat("P-value:  ", format.pval(x$p.value, digits = 4), "\n\n")
  cat("Subsample:  ", format(x$subsample, digits = 2), "\n")
  
  invisible(x)
}

#' @export
summary.CCI <- function(object, ...) {
  # Make summary mimic htest structure
  summary_list <- list(
    statistic = object$test.statistic,
    parameter = object$nperm,   # could also be 'NULL' if not applicable
    p.value   = object$p.value,
    method    = paste("CCI test using", object$MLfunc),
    data.name = deparse(object$formula),
    metric    = object$metric,
    tail      = object$tail,
    null.distribution = object$null.distribution,
    subsample = object$subsample
  )
  class(summary_list) <- c("summary.CCI", "htest")
  return(summary_list)
}

