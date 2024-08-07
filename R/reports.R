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
  cat(paste0("Computational conditional independence test using '", x$method, "'.\n"))
  
  if (!is.null(x$formula) && is.character(x$formula)) {
    cat("Formula: ", x$formula, "\n")
  } else if (!is.null(x$dag_n)) {
    cat("DAGitty test: ", x$dag_n, "\n")
  }
  
  cat("Number of Monte Carlo samples: ", x$nperm, "\n")
  cat("Test Statistic: ", unlist(x$test.statistic), "\n")
  cat("P-value: ", x$p.value, "\n")
  cat("Summary of Null Distribution:\n")
  print(summary(unlist(x$null.distribution)))
  
  invisible(x)
}


#' @export
#' @rdname reports

summary.CCI <- function(object, ...) {
  summary_list <- list(
    method = object$MLfunc,
    formula = object$formula,
    nperm = object$nperm,
    dag = object$dag,
    dag_n = object$dag_n,
    p.value = object$p.value,
    null.distribution = object$null.distribution,
    test.statistic = object$test.statistic
  )
  class(summary_list) <- "summary.CCI"
  return(summary_list)
}
