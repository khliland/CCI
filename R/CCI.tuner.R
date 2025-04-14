#' CCI tuner function for CCI test
#'
#' The `CCI.tuner` function performs a grid search over parameters for a conditional independence test using machine learning model supported by CCI.test. The tuner use the caret package for tuning.
#'
#'
#' @param formula Model formula or a DAGitty object specifying the relationship between dependent and independent variables.
#' @param data A data frame containing the variables specified in the formula.
#' @param tune_length Integer. The number of parameter combinations to try during the tuning process. Default is 10.
#' @param method Character. Specifies the machine learning method to use. Supported methods are random forest "rf", extreme gradient boosting "xgboost", neural-net "nnet, Gaussian Process Regression "gpr" and Support Vector Machine "svm".
#' @param random_grid Logical. If TRUE, a random grid search is performed. If FALSE, a full grid search is performed. Default is TRUE.
#' @param folds Integer. The number of folds for cross-validation during the tuning process. Default is 10.
#' @param seed Integer. The seed for random number generation. Default is 1984.
#' @param metric Character. The performance metric to optimize during tuning. Default is NULL, which will be set to "Accuracy" for classification tasks and "RMSE" for regression tasks.
#' @param ... Additional arguments to pass to the \code{CCI.tuner} function.
#'
#' @importFrom caret train trainControl
#'
#' @return Returns tuned parameters values for the predictive function
#' @aliases tuner
#' @export
#'
#' @seealso \code{\link{CCI.test} \link{perm.test}}, \code{\link{print.summary.CCI}}, \code{\link{plot.CCI}}, \code{\link{QQplot}}
#'

CCI.pretuner <- function(formula,
                     data,
                     method = "rf",
                     folds = 10,
                     tune_length = 5,
                     seed = 1984,
                     metric = NULL,
                     random_grid = TRUE,
                     ...) {

  set.seed(seed)

  # Default metric
  if (is.null(metric)) {
    metric <- "Accuracy"
    if (is.numeric(data[[all.vars(formula)[1]]])) {
      metric <- "RMSE"
    }
  }
  if (random_grid) {
    search <- "random"
  } else {
    search <- "grid"
  }
  # Check if the formula is a valid formula
  formula <- clean_formula(formula)
  check_formula(formula, data)

  caret_method <- switch(method,
                         rf = "rf",
                         xgboost = "xgbTree",
                         nnet = "nnet",
                         gpr = "gaussprRadial",
                         svm = "svmRadial",
                         stop("Unsupported method"))

  ctrl <- trainControl(method = "cv", number = folds, search = search,  verboseIter = TRUE)

  # Run tuning
  tuned_model <- train(
    formula,
    data = data,
    method = caret_method,
    trControl = ctrl,
    tuneLength = tune_length,
    metric = metric,
    ...
  )

  best <- tuned_model$bestTune
  best$method <- method
  return(best)
}
