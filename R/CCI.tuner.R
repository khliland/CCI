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
#' @param samples Integer. The number of random samples to take from the grid. Default is 30.
#' @param data_type Character. Specifies the type of data of dependent variable: "continuous", "binary", or "categorical". Default is "continuous".
#' @param folds Integer. The number of folds for cross-validation during the tuning process. Default is 10.
#' @param seed Integer. The seed for random number generation. Default is 1984.
#' @param metric Character. The performance metric to optimize during tuning. Default is 'RMSE'.
#' @param verboseIter Logical. If TRUE, the function will print the tuning process. Default is FALSE.
#' @param size Integer. The size of the neural network. Default is 1:5.
#' @param decay Numeric. The decay parameter for the neural network. Default is c(0, 0.01, 0.1).
#' @param mtry Integer. The number of variables randomly sampled as candidates at each split for random forest. Default is 1:5.
#' @param nrounds Integer. The number of rounds (trees) for methods such as xgboost and random forest. Default is seq(50, 200, by = 25).
#' @param eta Numeric. The learning rate for xgboost. Default is seq(0.01, 0.3, by = 0.05).
#' @param max_depth Integer. The maximum depth of the tree for xgboost. Default is 1:6.
#' @param subsample Numeric. The subsample ratio of the training instances for xgboost. Default is seq(0.5, 1, by = 0.1).
#' @param gamma Numeric. The minimum loss reduction required to make a further partition on a leaf node for xgboost. Default is seq(0, 5, by = 1).
#' @param colsample_bytree Numeric. The subsample ratio of columns when constructing each tree for xgboost. Default is seq(0.5, 1, by = 0.1).
#' @param min_child_weight Integer. The minimum sum of instance weight (hessian) needed in a child for xgboost. Default is 1:5.
#' @param sigma Numeric. The standard deviation of the Gaussian kernel for Gaussian Process Regression. Default is seq(0.1, 2, by = 0.3).
#' @param C Numeric. The regularization parameter for Support Vector Machine. Default is seq(0.1, 2, by = 0.5).
#' @param ... Additional arguments to pass to the \code{CCI.tuner} function.
#'
#' @importFrom caret train trainControl
#' @importFrom dplyr %>%
#' @importFrom pbapply pblapply
#' @importFrom stats model.matrix
#' @importFrom lattice levelplot
#'
#' @return Returns tuned parameters values for the predictive function
#' @aliases tuner
#' @export
#'
#' @seealso \code{\link{CCI.test} \link{perm.test}}, \code{\link{print.summary.CCI}}, \code{\link{plot.CCI}}, \code{\link{QQplot}}
#'
#' @examples
#' set.seed(123)
#'
#'
#' data <- data.frame(x1 = rnorm(500), x2 = rnorm(500), y = rnorm(500))
#'
#' CCI.pretuner(formula = y ~ x1 | x2, data = data, seed = 192, samples = 100,method = 'xgboost')

CCI.pretuner <- function(formula,
                         data,
                         method = "rf",
                         folds = 5,
                         tune_length = 10,
                         seed = 1984,
                         metric = 'RMSE',
                         random_grid = TRUE,
                         samples = 30,
                         data_type = "continuous",
                         verboseIter = FALSE,

                         size = 1:5,
                         decay = c(0.001, 0.01, 0.1),
                         mtry = 1:5,
                         nrounds = seq(50, 200, by = 25),
                         eta = seq(0.01, 0.3, by = 0.05),
                         max_depth = 1:6,
                         subsample = seq(0.5, 1, by = 0.1),
                         gamma = seq(0, 5, by = 1),
                         colsample_bytree = seq(0.5, 1, by = 0.1),
                         min_child_weight = 1:5,
                         sigma = seq(0.1, 2, by = 0.3),
                         C = seq(0.1, 2, by = 0.5),
                         ...) {

  set.seed(seed)

  if (!is.null(list(...)$tuneGrid)) {
    tuneGrid <- list(...)$tuneGrid
  }

  if (random_grid) {
    search <- "random"
  } else {
    search <- "grid"
  }

  check_formula(formula, data)

  outcome_name <- all.vars(formula)[1]

  if (data_type %in% c("categorical", "binary")) {
    Y <- as.factor(data[[outcome_name]])
    metric <- "Accuracy"
  } else {
    Y <- data[[outcome_name]]
  }

  X <- model.matrix(formula, data = data)[, -1, drop = FALSE]


  caret_method <- switch(method,
                         rf = "rf",
                         xgboost = "xgbTree",
                         nnet = "nnet",
                         gpr = "gaussprRadial",
                         svm = "svmRadial",
                         stop("Unsupported method"))


  ctrl <- caret::trainControl(method = 'cv', number = folds, search = search, verboseIter = verboseIter)

  tuneGrid <- switch(method,
                     nnet = expand.grid(size = size, decay = decay),
                     rf   = expand.grid(mtry = mtry),
                     xgboost = expand.grid(
                       nrounds = nrounds,
                       eta = eta,
                       max_depth = max_depth,
                       subsample = subsample,
                       gamma = gamma,
                       colsample_bytree = colsample_bytree,
                       min_child_weight = min_child_weight
                     ),
                     gpr = expand.grid(sigma = sigma),
                     svm = expand.grid(sigma = sigma, C = C),
                     stop("Unsupported method.")
  )


  if (random_grid) {
    total <- nrow(tuneGrid)
    sample_n <- min(samples, total)
    cat("Total combinations in grid:", total, "\n")
    cat("Randomly sampling", sample_n, "combinations...\n\n")
    tuneGrid <- tuneGrid[sample(seq_len(total), sample_n), , drop = FALSE]
  }

  # Create progress bar loop for grid search
  results <- pbapply::pblapply(seq_len(nrow(tuneGrid)), function(i) {
    row <- tuneGrid[i, , drop = FALSE]
    model <- train(
      x = X,
      y = Y,
      method = caret_method,
      trControl = ctrl,
      tuneGrid = row,
      metric = metric,
      trace = FALSE,
      ...
    )
    cbind(row, model$results[1, ])
  })

  # Combine results
  results_df <- do.call(rbind, results)

  # Pick best by metric
  best_idx <- if (metric == "RMSE") which.min(results_df$RMSE) else which.max(results_df[[metric]])
  best <- results_df[best_idx, ]
  best$method <- method

  cat("\n Tuning complete. Best model found.\n")
  return(list(best_param = best, tuning_result = results_df))
}
