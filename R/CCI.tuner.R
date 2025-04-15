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
#' @param verboseIter Logical. If TRUE, the function will print the tuning process. Default is FALSE.
#' @param ... Additional arguments to pass to the \code{CCI.tuner} function.
#'
#' @importFrom caret train trainControl
#' @importFrom dplyr %>%
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
                     folds = 5,
                     tune_length = 10,
                     seed = 1984,
                     metric = 'RMSE',
                     random_grid = TRUE,
                     data_type = "continuous",
                     verboseIter = FALSE,
                     ...) {

  set.seed(seed)

  if (random_grid) {
    search <- "random"
  } else {
    search <- "grid"
  }

  outcome_name <- all.vars(formula)[1]

  if (data_type %in% c("categorical", "binary")) {
    Y <- as.factor(data[[outcome_name]])
  } else {
    Y <- data[[outcome_name]]
  }

  X <- model.matrix(formula, data = data)[, -1, drop = FALSE]

  check_formula(formula, data)

  caret_method <- switch(method,
                         rf = "rf",
                         xgboost = "xgbTree",
                         nnet = "nnet",
                         gpr = "gaussprRadial",
                         svm = "svmRadial",
                         stop("Unsupported method"))

  if (!random_grid) {
    cat("Performing grid search...\n")
  } else {
    cat("Performing random grid search...\n")
  }

  ctrl <- caret::trainControl(method = "cv", number = folds, search = search, verboseIter = verboseIter)

  tuned_model <- caret::train(
    x = X,
    y = Y,
    method = caret_method,
    trControl = ctrl,
    tuneLength = tune_length,
    ...
  )

  best <- tuned_model$bestTune
  best$method <- method
  return(c(best))
}
