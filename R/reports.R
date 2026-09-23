#' @aliases reports print.CCI summary.CCI print.summary.CCI
#' @title Print and summary methods for the CCI class
#'
#' @description \code{print()} gives a short overview of a CCI test: the hypothesis tested, the
#' learner and metric, the test statistic and the p-value. \code{summary()} gives the same with the
#' number of permutations, the tail of the test and the share of data used in each Monte Carlo sample
#' (\code{MC_sample}) on separate lines.
#'
#' When \code{choose_direction = TRUE} changed the direction of the test (e.g. from \code{Y ~ X | Z}
#' to \code{X ~ Y | Z}), the formula that was tested is shown, together with the formula as given.
#'
#' @param x Object of class 'CCI' (for \code{print}) or 'summary.CCI' (for \code{print.summary.CCI})
#' @param object Object of class 'CCI'
#' @param digits Number of significant digits for the test statistic and p-value. Default is 4.
#' @param ... Additional arguments to print/summary
#'
#' @return The print methods return their argument invisibly, the summary method returns
#' an object of class 'summary.CCI'.
#' @rdname reports
#' @seealso \code{\link{perm.test}},
#' \code{\link{plot.CCI}}, \code{\link{QQplot}}
#' @export
#'
#' @examples
#' set.seed(1)
#' dat <- data.frame(Z = rnorm(100), X = rnorm(100))
#' dat$Y <- dat$Z + rnorm(100)
#' res <- CCI.test(Y ~ X | Z, data = dat, nperm = 20, progress = FALSE)
#' res
#' summary(res)
print.CCI <- function(x, digits = 4, ...) {
  cat("\nComputational Conditional Independence Test\n\n")
  cat("Formula:  ", formula_label(x$tested_formula %||% x$formula), "\n")
  if (direction_changed(x)) {
    cat("          (direction chosen by choose_direction; given as ", formula_label(x$formula), ")\n", sep = "")
  }
  cat("Method:   ", x$MLfunc, "with metric", x$metric, "and", x$nperm, "permutations\n")
  cat("Statistic:", format(unlist(x$test.statistic), digits = digits),
      "  P-value:", format.pval(x$p.value, digits = digits),
      if (isTRUE(x$parametric)) "(parametric)" else "", "\n\n")
  invisible(x)
}

#' @rdname reports
#' @export
print.summary.CCI <- function(x, ...) {
  cat("\nComputational Conditional Independence Test\n")
  cat("--------------------------------------------\n")
  cat("Method:   ", x$method, "\n")
  cat("Formula:  ", x$data.name, "\n")
  if (!is.null(x$given_formula)) {
    cat("Direction: chosen by choose_direction; given as", x$given_formula, "\n")
  }
  cat("Permutations: ", x$parameter, "\n")
  cat("Metric:   ", x$metric, "\n")
  cat("Tail:     ", x$tail, "\n")
  cat("Statistic:", format(x$statistic, digits = 4), "\n")
  cat("P-value:  ", format.pval(x$p.value, digits = 4), "\n\n")
  cat("MC sample:  ", format(x$MC_sample, digits = 2), "\n")

  invisible(x)
}

#' @rdname reports
#' @export
summary.CCI <- function(object, ...) {
  # Make summary mimic htest structure
  summary_list <- list(
    statistic = unlist(object$test.statistic),
    parameter = object$nperm,   # could also be 'NULL' if not applicable
    p.value   = object$p.value,
    method    = paste("CCI test using", object$MLfunc),
    data.name = formula_label(object$tested_formula %||% object$formula),
    given_formula = if (direction_changed(object)) formula_label(object$formula) else NULL,
    metric    = object$metric,
    tail      = object$tail,
    null.distribution = object$null.distribution,
    MC_sample = object$MC_sample %||% object$subsample   # objects from CCI < 0.3.7 store subsample
  )
  class(summary_list) <- c("summary.CCI", "htest")
  return(summary_list)
}

#' A formula as a single line of text
#' @noRd
formula_label <- function(f) paste(trimws(deparse(f)), collapse = " ")

#' Whether choose_direction swapped Y and X
#' @noRd
direction_changed <- function(object) {
  !is.null(object$tested_formula) && !is.null(object$formula) &&
    all.vars(object$tested_formula)[1] != all.vars(object$formula)[1]
}
