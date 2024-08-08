#' Wrapper function for performing the CCI test
#'
#' @param formula Model formula or DAGitty object
#' @param data Data frame
#' @param plot Logical, indicating if a plot of null distribution with test statistic should be generated (default: TRUE)
#' @param p Proportion of data used for training
#' @param nperm Number of permutations
#' @param dag DAGitty object for conditional independence testing
#' @param dag_n Which test to perform if using a DAGitty object
#' @param data_type Type of data: "continuous", "binary", or "categorical"
#' @param method Method for modeling: "lm", "xgboost", "rf", etc.
#' @param parametric Logical, indicating if parametric p-value should be computed
#' @param ... Additional arguments to pass to \code{perm.test}
#' @return Invisibly returns the result of \code{perm.test}
#' @export
CCI.test <- function(formula = NA,
                     data,
                     plot = TRUE,
                     p = 0.825,
                     nperm = 500,
                     dag = NA,
                     dag_n = 1,
                     data_type = "continuous",
                     method = 'rf',
                     parametric = FALSE,
                     ...) {

  result <- perm.test(formula = formula,
                      data = data,
                      p = p,
                      nperm = nperm,
                      dag = dag,
                      dag_n = dag_n,
                      data_type = data_type,
                      method = method,
                      parametric = parametric,
                      ...)


  print.summary.CCI(result)

  if (plot) {
  plot_null_distribution(result)
  cat("Plot generated.\n")
  } else {
    cat("No plot generated.\n")
  }

  invisible(result)
}
