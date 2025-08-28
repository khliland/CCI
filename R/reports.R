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
print.summary.CCI <- function(x, ...) {

  cat(paste0("Computational conditional independence test using '", x$method, "'.\n")) #Fix so compatible with custom made functions
  cat("Null hypothesis: ", deparse(x$formula), "\n")
  cat("Number of Monte Carlo samples: ", x$nperm, "\n")
  cat("Performance Metric: ", x$metric, "\n")
  cat("Test Statistic: ", format(unlist(x$test.statistic), digits = 3), "\n")
  cat("P-value: ", format(x$p.value, digits = 5), "\n")
  cat("Tail: ", x$tail, "\n")

  invisible(x)
}


#' @export
#' @rdname reports

summary.CCI <- function(object, ...) {
  summary_list <- list(
    method = object$MLfunc,
    formula = object$formula,
    nperm = object$nperm,
    tail = object$tail,
    p.value = object$p.value,
    metric = object$metric,
    null.distribution = object$null.distribution,
    test.statistic = object$test.statistic
  )
  class(summary_list) <- "summary.CCI"
  return(summary_list)
}
