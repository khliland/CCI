#' Wrapper Function for Performing the CCI Test
#'
#' The `CCI.test` function performs a conditional independence test using a specified machine learning model or a custom model provided by the user. It calculates the test statistic, generates a null distribution via permutations, computes p-values, and optionally generates a plot of the null distribution with the observed test statistic.
#' The 'CCI.test' function serves as a wrapper around the 'perm.test' function
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
#' @examples
#' set.seed(123)
#'
#' # Example 1: Basic use with a continuous outcome
#' data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = rnorm(100))
#' result <- CCI.test(y ~ x1 | x2, data = data, nperm = 1000)
#'
#' # Example 2: Using a binary outcome with logistic regression (method = "lm")
#' data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = rbinom(100, 1, 0.5))
#' result <- CCI.test(y ~ x1 | x2, data = data, method = "lm", nperm = 500, data_type = "binary", family = binomial())
#'
#' # Example 3: Using xgboost with categorical data
#' data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100), y = sample(1:3, 100, replace = TRUE) - 1)
#' result <- CCI.test(y ~ x1 | x2 + x3, data = data, method = "xgboost", data_type = "categorical", nperm = 200, num_class = 3)
#'
#' # Example 4: Using a custom machine learning function
#' custom_ml_func <- function(formula, data, train_indices, test_indices, ...) {
#'   model <- lm(formula, data = data[train_indices, ])
#'   predictions <- predict(model, newdata = data[test_indices, ])
#'   actual <- data[test_indices, ][[all.vars(formula)[1]]]
#'   metric <- sqrt(mean((predictions - actual)^2)) # RMSE
#'   return(metric)
#' }
#' data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = rnorm(100))
#' result <- CCI.test(y ~ x1 | x2, data = data, nperm = 1000, mlfunc = custom_ml_func)
#'
#' # Example 5: Using a custom performance metric function
#' custom_metric <- function(data, model, test_indices) {
#'   predictions <- predict(model, newdata = data[test_indices, ])
#'   actual <- data[test_indices, ][[all.vars(formula)[1]]]
#'   sst <- sum((actual - mean(actual))^2)
#'   ssr <- sum((actual - predictions)^2)
#'   rsq <- 1 - (ssr / sst)
#'   return(rsq) # R-squared
#' }
#' result <- CCI.test(y ~ x1 | x2, data = data, nperm = 1000, metricfunc = custom_metric, method = "lm")

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

  metric <- if (!is.null(metricfunc)) {
    deparse(substitute(metricfunc))
  } else if (!is.null(mlfunc)) {
    "custom"
  } else {
    if (data_type %in% "continuous") {
      "RMSE"
    } else {
      "Kappa Score"
    }
  }

  method <- if (!is.null(mlfunc)) {
    deparse(substitute(mlfunc))
  } else {
    method
  }

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
                      metricfunc = metricfunc,
                      mlfunc = mlfunc,
                      ...)
  result$metric <- metric
  print.summary.CCI(result)

  if (plot) {
  plot(result)
  cat("Plot generated.\n")
  } else {
    cat("No plot generated.\n")
  }

  return(result)
}
