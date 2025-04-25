#' CCI tuner function for CCI test
#'
#' The `CCI.tuner` function performs a grid search over parameters for a conditional independence test using machine learning model supported by CCI.test. The tuner use the caret package for tuning.
#'
#'
#' @param formula Model formula specifying the relationship between dependent and independent variables.
#' @param data A data frame containing the variables specified in the formula.
#' @param tune_length Integer. The number of parameter combinations to try during the tuning process. Default is 10.
#' @param method Character. Specifies the machine learning method to use. Supported methods are random forest "rf", extreme gradient boosting "xgboost", neural-net "nnet, Gaussian Process Regression "gpr" and Support Vector Machine "svm".
#' @param validation_method Character. Specifies the resampling method. Default is "cv".
#' @param training_share Numeric. For leave-group out cross-validation: the training percentage. Default is 0.7.
#' @param random_grid Logical. If TRUE, a random grid search is performed. If FALSE, a full grid search is performed. Default is TRUE.
#' @param samples Integer. The number of random samples to take from the grid. Default is 30.
#' @param data_type Character. Specifies the type of data of dependent variable: "continuous", "binary", or "categorical". Default is "continuous".
#' @param poly Logical. If TRUE, polynomial terms of the conditional variables are included in the model. Default is TRUE.
#' @param degree Integer. The degree of polynomial terms to include if poly is TRUE. Default is 3.
#' @param interaction Logical. If TRUE, interaction terms of the conditional variables are included in the model. Default is TRUE.
#' @param folds Integer. The number of folds for cross-validation during the tuning process. Default is 10.
#' @param seed Integer. The seed for random number generation. Default is 1984.
#' @param metric Character. The performance metric to optimize during tuning. Defaults to 'RMSE' for continuous data. Automatically set to 'Accuracy' for binary or categorical data.
#' @param verboseIter Logical. If TRUE, the function will print the tuning process. Default is FALSE.
#' @param trace Logical. If TRUE, the function will print the tuning process. Default is FALSE.
#' @param include_explanatory Logical. If TRUE, given the condition Y _||_ X |  Z, the function will include explanatory variable X in the model for Y. Default is FALSE
#' @param verbose Logical. If TRUE, the function will print the tuning process. Default is FALSE.
#' @param parallel Logical. If TRUE, the function will use parallel processing. Default is TRUE.
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
#' @param num_leaves Integer. The number of leaves in the tree for LightGBM. Default is c(20, 31, 40).
#' @param learning_rate Numeric. The learning rate for LightGBM. Default is c(0.01, 0.1, 0.3).
#' @param feature_fraction Numeric. The fraction of features to be used for LightGBM. Default is seq(0.1, 1, by = 0.1).
#' @param bagging_fraction Numeric. The fraction of data to be used for LightGBM. Default is seq(0.1, 1, by = 0.1).
#' @param ... Additional arguments to pass to the \code{CCI.tuner} function.
#'
#' @importFrom caret train trainControl nearZeroVar
#' @importFrom lightgbm lgb.Dataset lgb.cv
#' @importFrom dplyr %>%
#' @importFrom pbapply pblapply
#' @importFrom stats model.matrix var
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{best_param}: A data frame with the best parameters and their performance metric.
#'   \item \code{tuning_result}: A data frame with all tested parameter combinations and their performance metrics.
#'   \item \code{warnings}: A character vector of warnings issued during tuning.
#' }
#' @aliases tuner
#' @export
#'
#' @seealso \code{\link{CCI.test} \link{perm.test}}, \code{\link{print.summary.CCI}}, \code{\link{plot.CCI}}, \code{\link{QQplot}}
#'
#' @examples
#' set.seed(123)
#' data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = rnorm(100))
#' # Tune random forest parameters
#' result <- CCI.pretuner(formula = y ~ x1 | x2, data = data, seed = 192, samples = 5, folds = 3, method = "rf")
#' # Returns a list with best parameters and tuning results
#' if (requireNamespace("xgboost", quietly = TRUE)) {
#'   # Tune xgboost parameters
#'   result_xgb <- CCI.pretuner(formula = y ~ x1 | x2, data = data, seed = 192, samples = 5, folds = 3, method = "xgboost")
#' }

CCI.pretuner <- function(formula,
                         data,
                         method = "rf",
                         validation_method = 'cv',
                         folds = 5,
                         training_share = 0.7,
                         tune_length = 3,
                         seed = 1984,
                         metric = 'RMSE',
                         random_grid = TRUE,
                         samples = 30,
                         data_type = "continuous",
                         poly = TRUE,
                         degree = 3,
                         interaction = TRUE,
                         verboseIter = FALSE,
                         trace = FALSE,
                         include_explanatory = FALSE,
                         verbose = FALSE,
                         parallel = FALSE,
                         size = 1:5,
                         decay = c(0.001, 0.01, 0.1, 0.2, 0.5, 1),
                         mtry = 1:10,
                         nrounds = seq(50, 500, by = 50),
                         eta = seq(0.01, 0.3, by = 0.05),
                         max_depth = 1:6,
                         subsample = seq(0.5, 1, by = 0.1),
                         gamma = seq(0, 5, by = 1),
                         colsample_bytree = seq(0.5, 1, by = 0.1),
                         min_child_weight = 1:5,
                         sigma = seq(0.1, 2, by = 0.3),
                         C = seq(0.1, 2, by = 0.5),
                         num_leaves = c(20, 31, 40),
                         learning_rate = c(0.01, 0.1, 0.3),
                         feature_fraction =  seq(0.1, 1, by = 0.1),
                         bagging_fraction = seq(0.1, 1, by = 0.1),
                         min_data_in_leaf = c(5, 10, 20, 30),
                         ...) {

  set.seed(seed)
  if (!is.data.frame(data) || nrow(data) == 0) {
    stop("The 'data' argument must be a non-empty data frame.")
  }
  if (!is.numeric(tune_length) || tune_length < 1) {
    stop("tune_length must be a positive integer.")
  }
  if (!method %in% c("rf", "xgboost", "nnet", "gpr", "svm")) {
    stop("method must be one of 'rf', 'xgboost', 'nnet', 'gpr', or 'svm'.")
  }
  if (!data_type %in% c("continuous", "binary", "categorical")) {
    stop("data_type must be one of 'continuous', 'binary', or 'categorical'.")
  }
  if (poly && degree < 1) {
    stop("Degree of 0 or less is not allowed")
  }
  formula_vars <- all.vars(formula)
  if (any(sapply(data[formula_vars], function(x) !is.numeric(x) && !is.factor(x)))) {
    stop("All formula variables must be numeric or factors.")
  }

  if (data_type == "continuous" && stats::var(data[[formula_vars[1]]]) < 1e-10) {
    warning("Response variable has near-zero variance (", stats::var(data[[formula_vars[1]]]), "). R-squared may be unreliable.")
  }
  nzv <- caret::nearZeroVar(data[formula_vars[-1]], saveMetrics = TRUE)
  if (any(nzv$nzv)) {
    warning("Predictors with near-zero variance detected: ",
            paste(names(data)[formula_vars[-1]][nzv$nzv], collapse = ", "))
  }

  if (method == "nnet") {
    trace <- trace
  } else {
    trace <- NULL
  }

  org_formula <- formula # Store the original formula for later use
  formula <- clean_formula(formula)

  Y = formula[[2]]
  X = formula[[3]][[2]]
  Z = unlist(strsplit(deparse(formula[[3]][[3]]), split = " \\+ "))
  if (any(sapply(data[Z], is.factor))) {
    warning("Polynomial terms are not supported for categorical variables. Polynomial terms will not be included.")
    poly <- FALSE
  }
  poly_result <- add_poly_terms(data, Z, degree = degree, poly = poly)
  data <- poly_result$data
  poly_terms <- poly_result$new_terms

  if (interaction) {
    interaction_result <- add_interaction_terms(data, Z)
    data <- interaction_result$data
    interaction_terms <- interaction_result$interaction_terms
  } else {
    interaction_terms <- NULL
  }

  formula <- build_formula(formula, poly_terms, interaction_terms)

  n_predictors <- length(all.vars(formula[[3]]))

  if (n_predictors > 100) {
    warning("Formula includes ", n_predictors, " predictors including polynomials and interactions. Tuning may be slow. Consider reducing folds or samples.")
  }

  if (method == "rf") {
    mtry <- seq(1, min(n_predictors, max(5, ceiling(n_predictors / 2))), by = 1)
  }
  tuneGrid <- NULL
  dots <- list(...)

  if (!is.null(dots$tuneGrid)) {
    tuneGrid <- dots$tuneGrid
    if (!is.data.frame(tuneGrid) && !is.matrix(tuneGrid)) {
      stop("Custom tuneGrid must be a data frame or matrix.")
    }
  }


  if (random_grid) {
    search <- "random"
  } else {
    search <- "grid"
  }

  outcome_name <- all.vars(formula)[1]

  if (data_type %in% c("binary", "categorical") && metric != "Accuracy") {
    warning("For binary or categorical data, metric is set to 'Accuracy'.")
    metric <- "Accuracy"
  }

  if (data_type %in% c("categorical", "binary")) {
    Y <- as.factor(data[[outcome_name]])
    metric <- "Accuracy"
  } else {
    Y <- data[[outcome_name]]
  }

  if (include_explanatory) {
    formula <- formula
  } else {
    formula <- as.formula(paste(all.vars(formula)[1], "~", paste(all.vars(formula[[3]])[-1], collapse = " + ")))  # Building new formula, keeping "X" out
  }

  check_formula(formula, data)
  X <- model.matrix(formula, data = data)[, -1, drop = FALSE]

  if (ncol(X) == 0) {
    stop("The formula produced an empty design matrix. Check variable types and formula specification.")
  }


  if (method == "lightgbm") {
    if (!requireNamespace("lightgbm", quietly = TRUE)) {
      stop("Package 'lightgbm' is required for method 'lightgbm'")
    }
    tuneGrid <- expand.grid(
      num_leaves = num_leaves,
      learning_rate = learning_rate,
      feature_fraction = feature_fraction,
      bagging_fraction = bagging_fraction,,
      min_data_in_leaf = min_data_in_leaf
    )
    if (random_grid) {
      total <- nrow(tuneGrid)
      sample_n <- min(samples, total)
      cat("Total combinations in grid:", total, "\n")
      cat("Randomly sampling", sample_n, "combinations...\n\n")
      tuneGrid <- tuneGrid[sample(seq_len(total), sample_n), , drop = FALSE]
    }

    warning_log <- character()
    results <- lapply(seq_len(nrow(tuneGrid)), function(i) {
      row <- tuneGrid[i, , drop = FALSE]
      if (verbose) {
        cat("Training LightGBM with parameters:", paste(names(row), row, sep = "=", collapse = ", "), "\n")
      }
      dtrain <- lightgbm::lgb.Dataset(data = X, label = as.numeric(Y))
      cv_result <- tryCatch({
        lightgbm::lgb.cv(
          params = list(
            objective = switch(data_type,
                               continuous = "regression",
                               binary = "binary",
                               categorical = "multiclass"),
            num_leaves = row$num_leaves,
            learning_rate = row$learning_rate,
            feature_fraction = row$feature_fraction,
            bagging_fraction = row$bagging_fraction,
            min_data_in_leaf = row$min_data_in_leaf,
            num_threads = if (parallel) max(1, parallel::detectCores() - 1) else 1,
            ...
          ),
          data = dtrain,
          nfold = folds,
          nrounds = max(nrounds),
          early_stopping_rounds = 10,
          verbose = if (verbose) 1 else -1
        )
      }, error = function(e) {
        warning_log <<- c(warning_log, paste("Error for parameters ",
                                             paste(names(row), row, sep = "=", collapse = ", "),
                                             ": ", conditionMessage(e)))
        NULL
      })
      if (is.null(cv_result)) return(NULL)
      metric_value <- if (data_type == "continuous") {
        cv_result$best_score  # RMSE
      } else {
        cv_result$best_score  # Accuracy for binary/categorical
      }
      res <- data.frame(
        RMSE = if (data_type == "continuous") metric_value else NA,
        Accuracy = if (data_type %in% c("binary", "categorical")) metric_value else NA,
        nrounds = cv_result$best_iter
      )
      cbind(row, res)
    })

    results <- results[!sapply(results, is.null)]
    if (length(results) == 0) {
      stop("No LightGBM models were successfully trained. Check parameter ranges and data.")
    }
    results_df <- do.call(rbind, results)
    best_idx <- if (metric == "RMSE") which.min(results_df$RMSE) else which.max(results_df$Accuracy)
    best <- results_df[best_idx, ]
    best$method <- method
    if (length(warning_log) > 0) {
      warning("Tuning completed with ", length(warning_log), " warnings. Check result$warnings for details.")
    }
    cat("\n Tuning complete. Best model found.\n")
    return(list(best_param = best, tuning_result = results_df))
  }

  caret_method <- switch(method,
                         rf = "rf",
                         xgboost = "xgbTree",
                         nnet = "nnet",
                         gpr = "gaussprRadial",
                         svm = "svmRadial",
                         stop("Unsupported method"))


  ctrl <- caret::trainControl(method = validation_method,
                              p = training_share,
                              number = folds,
                              search = search,
                              verboseIter = verboseIter,
                              allowParallel = parallel,
                              summaryFunction = if (data_type == "continuous") {
                                function(data, lev = NULL, model = NULL) {
                                  obs <- data$obs
                                  pred <- data$pred
                                  if (verbose) {
                                    cat("Summary function called. Variance of predictions:", var(pred), "\n")
                                    cat("Variance of observed:", var(obs), "\n")
                                    cat("Correlation:", cor(obs, pred, use = "pairwise.complete.obs"), "\n")
                                  }
                                  if (stats::var(pred) < 1e-10 || stats::var(obs) < 1e-10) {
                                    return(c(RMSE = sqrt(mean((obs - pred)^2)), Rsquared = NA, MAE = mean(abs(obs - pred))))
                                  }
                                  out <- caret::defaultSummary(data, lev, model)
                                  if (is.nan(out["Rsquared"]) || is.na(out["Rsquared"])) {
                                    out["Rsquared"] <- NA
                                  }
                                  out
                                }
                              } else caret::defaultSummary
  )

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
                     lightgbm = expand.grid(
                       num_leaves = c(20, 31, 40),
                       learning_rate = c(0.01, 0.1, 0.3),
                       feature_fraction = c(0.6, 0.8, 1.0)
                     ),
                     stop("Unsupported method.")
  )


  if (random_grid) {
    total <- nrow(tuneGrid)
    sample_n <- min(samples, total)
    cat("Total combinations in grid:", total, "\n")
    cat("Randomly sampling", sample_n, "combinations...\n\n")
    tuneGrid <- tuneGrid[sample(seq_len(total), sample_n), , drop = FALSE]
  }

  warning_log <- character()

  # Progress bar with pbapply
  if (!requireNamespace("pbapply", quietly = TRUE)) {
    warning("The 'pbapply' package is not installed. Progress bars will not be shown.")
    warning_log <- c(warning_log, "The 'pbapply' package is not installed. Progress bars will not be shown.")

    results <- lapply(seq_len(nrow(tuneGrid)), function(i) {
      row <- tuneGrid[i, , drop = FALSE]
      if (verbose) {
        cat("Training model with parameters:", paste(names(row), row, sep = "=", collapse = ", "), "\n")
      }
      train_args <- list(
        x = X,
        y = Y,
        method = caret_method,
        trControl = ctrl,
        tuneGrid = row,
        metric = metric,
        ...
      )
      if (caret_method == "nnet") {
        train_args$trace <- trace
      }
      model <- tryCatch(
        {
          withCallingHandlers(
            do.call(caret::train, train_args),
            warning = function(w) {
              warning_log <<- c(warning_log, paste("Warning for parameters ",
                                                   paste(names(row), row, sep = "=", collapse = ", "),
                                                   ": ", conditionMessage(w)))
              invokeRestart("muffleWarning")
            }
          )
        },
        error = function(e) {
          warning_log <<- c(warning_log, paste("Error for parameters ",
                                               paste(names(row), row, sep = "=", collapse = ", "),
                                               ": ", conditionMessage(e)))
          NULL
        }
      )
      if (is.null(model)) return(NULL)
      res <- model$results[1, ]
      res[] <- lapply(res, function(x) if (is.numeric(x)) replace(x, is.nan(x), NA) else x)
      if (verbose) {
        cat("Results for parameters:", paste(names(row), row, sep = "=", collapse = ", "), "\n")
        # print(res)
      }
      cbind(row, res)
    })
  } else {
    results <- pbapply::pblapply(seq_len(nrow(tuneGrid)), function(i) {
      row <- tuneGrid[i, , drop = FALSE]
      if (verbose) {
        cat("Training model with parameters:", paste(names(row), row, sep = "=", collapse = ", "), "\n")
      }
      train_args <- list(
        x = X,
        y = Y,
        method = caret_method,
        trControl = ctrl,
        tuneGrid = row,
        metric = metric,
        ...
      )
      if (caret_method == "nnet") {
        train_args$trace <- trace
      }
      model <- tryCatch(
        {
          withCallingHandlers(
            do.call(caret::train, train_args),
            warning = function(w) {
              warning_log <<- c(warning_log, paste("Warning for parameters ",
                                                   paste(names(row), row, sep = "=", collapse = ", "),
                                                   ": ", conditionMessage(w)))
              invokeRestart("muffleWarning")
            }
          )
        },
        error = function(e) {
          warning_log <<- c(warning_log, paste("Error for parameters ",
                                               paste(names(row), row, sep = "=", collapse = ", "),
                                               ": ", conditionMessage(e)))
          NULL
        }
      )
      if (is.null(model)) return(NULL)
      res <- model$results[1, ]
      res[] <- lapply(res, function(x) if (is.numeric(x)) replace(x, is.nan(x), NA) else x)
      if (verbose) {
        cat("Results for parameters:", paste(names(row), row, sep = "=", collapse = ", "), "\n")
        print(res)
      }
      cbind(row, res)
    })
  }

  results <- results[!sapply(results, is.null)]
  if (length(results) == 0) {
    stop("No models were successfully trained. Check parameter ranges and data.")
  }

  results_df <- do.call(rbind, results)

  best_idx <- if (metric == "RMSE") which.min(results_df$RMSE) else which.max(results_df[[metric]])
  best <- results_df[best_idx, ]
  best$method <- method
  formula <- org_formula # Restore the original formula

  if (length(warning_log) > 0) {
    warning("Tuning completed with ", length(warning_log), " warnings. Check result$warnings for details.")
  }

  cat("\n Tuning complete. Best model found.\n")
  return(list(best_param = best, tuning_result = results_df))
}
