#' CCI tuner function for CCI test
#'
#' The `CCI.tuner` function performs a grid search over parameters for a conditional independence test using a specified machine learning model or a custom model provided by the user. It calculates the test statistic, generates a null distribution via permutations, computes p-values, and optionally generates a plot of the null distribution with the observed test statistic.
#'
#'
#' @param formula Model formula or a DAGitty object specifying the relationship between dependent and independent variables.
#' @param data A data frame containing the variables specified in the formula.
#' @param method Character. Specifies the machine learning method to use. Supported methods include generlaized linear models "lm", random forest "rf", and extreme gradient boosting "xgboost", etc. Default is "rf".
#' @param random_grid Logical. If TRUE, a random grid search is performed. If FALSE, a full grid search is performed. Default is TRUE.
#' @param metric Character. Specifies the performance metric to optimize during the tuning process. Default is "RMSE".
#' @param folds Integer. The number of folds for cross-validation during the tuning process. Default is 5.
#' @param metricfunc Optional custom function for calculating a performance metric based on the model's predictions. Default is NULL.
#' @param mlfunc Optional custom machine learning function to use instead of the predefined methods. Default is NULL.
#' @param ml_param List. A list of parameters to be tuned when using the mlfunc. Default is NULL.
#' @param ... Additional arguments to pass to the \code{CCI.tuner} function.
#'
#' @importFrom caret
#'
#' @return Returns tuned parameters values for the predictive function
#' @aliases tuner
#' @export
#'
#' @seealso \code{\link{CCI.test} \link{perm.test}}, \code{\link{print.summary.CCI}}, \code{\link{plot.CCI}}, \code{\link{QQplot}}
#'

CCI.tuner <- function(formula,
                      data,
                      method = "rf",
                      random_grid = TRUE,
                      metric = "RMSE",
                      folds = 5,
                      metricfunc = NULL,
                      mlfunc = NULL,
                      ml_param = NULL,
                      ...) {
  caret::control <- trainControl(method = "cv", number = folds)
  if (random_grid) {
    tune_grid <- expand.grid(
      mtry = seq(1, ncol(data) - 1, by = 1),
      splitrule = c("variance", "extratrees"),
      min.node.size = seq(1, 10, by = 1)
    )
  } else {
    tune_grid <- expand.grid(
      mtry = seq(1, ncol(data) - 1, by = 1),
      splitrule = c("variance", "extratrees"),
      min.node.size = seq(1, 10, by = 1)
    )
    }

}
