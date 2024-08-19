#' Wrapper Function for Performing the CCI Test
#'
#' The `CCI.test` function performs a conditional independence test using a specified machine learning model or a custom model provided by the user. It calculates the test statistic, generates a null distribution via permutations, computes p-values, and optionally generates a plot of the null distribution with the observed test statistic.
#'
#' @param formula Model formula or a DAGitty object specifying the relationship between dependent and independent variables.
#' @param data A data frame containing the variables specified in the formula.
#' @param plot Logical, indicating if a plot of the null distribution with the test statistic should be generated. Default is TRUE.
#' @param p Numeric. Proportion of data used for training the model. Default is 0.825.
#' @param nperm Integer. The number of permutations to perform. Default is 500.
#' @param dag An optional DAGitty object for specifying a Directed Acyclic Graph (DAG) to use for conditional independence testing. Default is NA.
#' @param dag_n Integer. If a DAGitty object is provided, specifies which conditional independence test to perform. Default is 1.
#' @param data_type Character. Specifies the type of data: "continuous", "binary", or "categorical". Default is "continuous".
#' @param method Character. Specifies the machine learning method to use. Supported methods include "lm", "rf", "xgboost", etc. Default is "rf".
#' @param metricfunc Optional custom function for calculating a performance metric based on the model's predictions. Default is NULL.
#' @param mlfunc Optional custom machine learning function to use instead of the predefined methods. Default is NULL.
#' @param parametric Logical, indicating whether to compute a parametric p-value in addition to the empirical p-value. Default is FALSE.
#' @param tail Character. Specifies whether to calculate left-tailed or right-tailed p-values, depending on the performance metric used. Only applicable if using `metricfunc` or `mlfunc`. Default is NA.
#' @param seed Optional integer. Specifies a seed for random number generation to ensure reproducibility. Default is NULL.
#' @param ... Additional arguments to pass to the \code{perm.test} function.
#'
#' @return Invisibly returns the result of \code{perm.test}, which is an object of class 'CCI' containing the null distribution, observed test statistic, p-values, the machine learning model used, and the data.
#' @export
#'
#' @seealso \code{\link{perm.test}}, \code{\link{print.summary.CCI}}, \code{\link{plot.CCI}}, \code{\link{QQplot}}
#'
#' @examples
#' set.seed(123)
#' # Basic use
#' data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = rnorm(100))
#' result <- CCI.test(y ~ x1 | x2, data = data, nperm = 1000)


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
                     seed = NULL,
                     metricfunc = NULL,
                     mlfunc = NULL,
                     tail = NA,
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
                      seed = seed,
                      tail = tail,
                      ...)


  print.summary.CCI(result)

  if (plot) {
  plot(result)
  cat("Plot generated.\n")
  } else {
    cat("No plot generated.\n")
  }

  return(result)
}
