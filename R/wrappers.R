#' Extreme Gradient Boosting wrapper for CCI
#'
#' Fits an xgboost model on the training rows and returns its performance on the test rows.
#'
#' The type of task is decided from the metric and the response: \code{"RMSE"} gives regression,
#' \code{"Kappa"} and \code{"LogLoss"} give classification (binary or multiclass). With a custom
#' \code{metricfunc}, a numeric response gives regression and a factor, character or logical
#' response gives classification. Classes are encoded as 0..K-1 internally, so any class labels work.
#'
#' A custom \code{metricfunc} is called as \code{metricfunc(actual, predictions, ...)} (with the
#' \code{...} arguments only if the function accepts them), where
#' \code{actual} has the type of the response (numeric for regression, factor for classification) and
#' \code{predictions} is a numeric vector for regression, the probability of the second class level for
#' binary classification, and an n x K probability matrix with the class levels as column names for
#' multiclass classification.
#'
#' @param formula Model formula
#' @param data Data frame
#' @param train_indices Indices for training data
#' @param test_indices Indices for test data
#' @param nrounds Number of boosting rounds
#' @param metric Type of metric ("RMSE", "Kappa" or "LogLoss"), or the name of a custom metric when \code{metricfunc} is given
#' @param metricfunc Optional user-defined function \code{function(actual, predictions, ...)} returning a numeric performance value. See Details.
#' @param nthread Integer. Number of threads to use for parallel computation during model training in XGBoost. Default is 1.
#' @param eps Small value to avoid log(0) in LogLoss calculations. Default is 1e-15.
#' @param MC_sample Not used by the model. The share of data per Monte Carlo sample is applied in \code{\link{test.gen}} before the wrapper is called. (xgboost's own row subsampling can be set with \code{subsample} in \code{...}.)
#' @param ... Additional arguments passed to \code{xgb.train} as parameters (e.g. \code{eta}, \code{max_depth}, \code{objective}).
#'
#' @importFrom xgboost xgb.DMatrix xgb.train
#' @importFrom stats model.matrix predict
#' @importFrom caret confusionMatrix
#' @importFrom rlang %||%
#' @importFrom utils modifyList
#'
#' @return Performance metric
#' @export

wrapper_xgboost <- function(formula,
                            data,
                            train_indices,
                            test_indices,
                            metric,
                            nrounds = 500,
                            metricfunc = NULL,
                            nthread = 1,
                            eps = 1e-15,
                            MC_sample = 1,
                            ...) {

  independent <- all.vars(formula)[-1]
  dependent <- all.vars(formula)[1]
  y <- data[[dependent]]

  # ---- Type of task ----
  if (metric %in% c("Kappa", "LogLoss")) {
    classification <- TRUE
  } else if (metric == "RMSE") {
    classification <- FALSE
  } else {
    # Custom metric: decided by the type of the response
    classification <- is.factor(y) || is.character(y) || is.logical(y)
  }

  if (classification) {
    # Encode classes as 0..K-1, whatever the original labels are
    y <- droplevels(factor(y))
    lev <- levels(y)
    num_class <- length(lev)
    if (num_class < 2) stop("The response must have at least two classes for classification.")
    if (num_class > 6) {
      warning("More than 6 classes detected. Consider RMSE for continuous targets, or be cautious with classification metrics.")
    }
    label <- as.integer(y) - 1L
    data_type <- if (num_class == 2) "binary" else "categorical"
  } else {
    if (!is.numeric(y)) stop("Metric '", metric, "' requires a numeric response.")
    label <- as.numeric(y)
    data_type <- "continuous"
  }

  # ---- Features: one design matrix, so train and test get the same columns ----
  X <- data[independent]
  if (all(vapply(X, is.numeric, logical(1)))) {
    features <- as.matrix(X)
  } else {
    features <- stats::model.matrix(~ . - 1, data = X)
  }

  dtrain <- xgboost::xgb.DMatrix(data = features[train_indices, , drop = FALSE], label = label[train_indices])
  test_features <- features[test_indices, , drop = FALSE]

  # ---- Fit ----
  params <- list(
    objective = switch(data_type,
                       continuous  = "reg:squarederror",
                       binary      = "binary:logistic",
                       categorical = "multi:softprob"),
    eval_metric = switch(data_type,
                         continuous  = "rmse",
                         binary      = "error",
                         categorical = "merror"),
    nthread = nthread
  )
  params <- utils::modifyList(params, list(...))
  if (data_type == "categorical") params$num_class <- num_class
  params <- params[!vapply(params, is.null, logical(1))]

  model <- xgboost::xgb.train(
    data    = dtrain,
    params  = params,
    nrounds = nrounds,
    verbose = 0
  )
  predictions <- stats::predict(model, newdata = test_features)

  # ---- Evaluate ----
  if (data_type == "continuous") {
    actual <- label[test_indices]
    keep <- is.finite(predictions) & is.finite(actual)
    predictions <- predictions[keep]
    actual <- actual[keep]
    if (!is.null(metricfunc)) return(call_metricfunc(metricfunc, actual, predictions, ...))
    return(sqrt(mean((predictions - actual)^2)))
  }

  actual <- y[test_indices]
  if (data_type == "binary") {
    # Probability of the second class level
    P <- as.numeric(predictions)
    P <- cbind(1 - P, P)
  } else {
    P <- if (is.matrix(predictions)) predictions else matrix(predictions, ncol = num_class, byrow = TRUE)
  }
  colnames(P) <- lev

  if (!is.null(metricfunc)) {
    pred_out <- if (data_type == "binary") P[, 2] else P
    return(call_metricfunc(metricfunc, actual, pred_out, ...))
  }

  if (metric == "LogLoss") {
    P <- pmin(pmax(P, eps), 1 - eps)
    p_true <- P[cbind(seq_along(actual), as.integer(actual))]
    return(-mean(log(p_true)))
  }

  # Kappa
  pred_class <- factor(lev[max.col(P, ties.method = "first")], levels = lev)
  cm <- caret::confusionMatrix(pred_class, actual)
  unname(cm$overall["Kappa"])
}



#' Random Forest wrapper for CCI
#'
#' @param formula Model formula specifying the dependent and independent variables.
#' @param data Data frame containing the dataset to be used for training and testing the model.
#' @param train_indices A vector of indices specifying the rows in `data` to be used as the training set.
#' @param test_indices A vector of indices specifying the rows in `data` to be used as the test set.
#' @param metric Type of metric ("RMSE", "Kappa" or "Log Loss")
#' @param metricfunc Optional user-defined function \code{function(actual, predictions, ...)} returning a numeric performance value. For a factor response, \code{predictions} are the predicted classes. The \code{...} arguments are only passed on if the function accepts them.
#' @param nthread Integer. The number of threads to use for parallel processing. Default is 1.
#' @param mtry Integer. The number of variables to possibly split at in each node. Default is the square root of the number of columns in `data`.
#' @param num.trees Integer. The number of trees to grow in the random forest. 
#' @param eps Small value to avoid log(0) in LogLoss calculations. Default is 1e-15.
#' @param ... Additional arguments passed to the `ranger` function.
#'
#' @importFrom ranger ranger
#' @importFrom stats predict
#' @importFrom caret confusionMatrix
#'
#' @return A numeric value representing the performance metric of the model on the test set.
#' @export

wrapper_ranger <- function(formula,
                           data,
                           train_indices,
                           test_indices,
                           metric,
                           metricfunc = NULL,
                           nthread = 1,
                           mtry = NULL,
                           num.trees,
                           eps = 1e-15,
                           ...) {

  # ranger does not accept a character response or character predictors
  data <- characters_to_factors(data, all.vars(formula))

  if (metric %in% c("Kappa", "LogLoss")) {
    dependent <- all.vars(formula)[1]
    testing <- data[test_indices, ]
    test_label <- testing[[dependent]]
    model <- ranger::ranger(formula, data = data[train_indices, ], mtry = mtry, probability = TRUE, num.threads = nthread, num.trees = num.trees, ...)
  } else if (metric == "RMSE") {
    model <- ranger::ranger(formula, data = data[train_indices, ], mtry = mtry, num.threads = nthread, num.trees = num.trees, ...)
  } else {
    model <- ranger::ranger(formula, data = data[train_indices, ], mtry = mtry, num.threads = nthread, num.trees = num.trees, ...)
  }

  predictions <- stats::predict(model, data = data[test_indices, ])$predictions
  actual <- data[test_indices, ][[all.vars(formula)[1]]]

  bad_idx <- is.infinite(predictions) | is.infinite(actual)
  if (any(bad_idx)) {
    predictions <- predictions[!bad_idx]
    actual <- actual[!bad_idx]
  }
  
  if (!is.null(metricfunc)) {
    metric_value <- call_metricfunc(metricfunc, actual, predictions, ...)
  } else if (metric %in% c("Kappa")) {
    # Probability columns are named by class, but not necessarily in the order of levels(factor(actual))
    classes <- colnames(predictions)
    if (is.null(classes)) classes <- as.character(model$forest$class.values)
    lev <- union(levels(factor(actual)), classes)
    pred_class <- factor(classes[max.col(predictions, ties.method = "first")], levels = lev)
    cm <- caret::confusionMatrix(pred_class, factor(actual, levels = lev))
    metric_value <- cm$overall["Kappa"]
    } else if (metric == "RMSE") {
      metric_value <- sqrt(mean((predictions - actual)^2))
    } else if (metric == "LogLoss") {
      
      # Ensure actual is a factor
      actual <- factor(actual)
      lev <- levels(actual)
      
      eps <- 1e-15
      
      # ranger classification predictions should be a matrix (n x K)
      # but be defensive if a vector/list arrives
      if (is.list(predictions)) {
        # ranger sometimes returns a list with $predictions depending on context;
        # if you already extracted $predictions this likely won't happen, but handle it.
        if (!is.null(predictions$predictions)) predictions <- predictions$predictions
        else stop("Unexpected prediction format (list) for LogLoss.", call. = FALSE)
      }
      
      # Binary case: predictions may be n x 2 (probabilities) OR a vector
      if (nlevels(actual) == 2) {
        
        # Ensure we have a probability vector for the positive class
        pos <- lev[2]
        
        if (is.matrix(predictions)) {
          if (!is.null(colnames(predictions)) && pos %in% colnames(predictions)) {
            p <- predictions[, pos]
          } else {
            p <- predictions[, 2]
          }
        } else {
          # if predictions is already a probability vector
          p <- as.numeric(predictions)
        }
        
        p <- pmin(pmax(p, eps), 1 - eps)
        y01 <- as.integer(actual == pos)
        
        metric_value <- -mean(y01 * log(p) + (1 - y01) * log(1 - p))
        
      } else {
        
        # Multiclass: predictions must be n x K
        if (!is.matrix(predictions)) {
          stop("For multiclass LogLoss, predictions must be an n x K matrix.", call. = FALSE)
        }
        
        # Align columns to class levels if column names exist
        if (!is.null(colnames(predictions))) {
          # Ensure all levels are present as columns
          if (!all(lev %in% colnames(predictions))) {
            stop("Prediction matrix column names do not match class levels in 'actual'.", call. = FALSE)
          }
          P <- predictions[, lev, drop = FALSE]
        } else {
          P <- predictions
        }
        
        P <- pmax(P, eps)
        
        idx <- cbind(seq_along(actual), match(actual, lev))
        p_true <- P[idx]
        
        metric_value <- -mean(log(p_true))
      }
    } else {
      stop("Unsupported metric for Ranger wrapper.")
    }

  return(metric_value)
}

#' SVM wrapper for CCI
#'
#' @param formula Model formula
#' @param data Data frame
#' @param train_indices Indices for training data
#' @param test_indices Indices for testing data
#' @param metric Type of metric ("RMSE", "Kappa" or "Log Loss")
#' @param metricfunc Optional user-defined function to calculate a custom performance metric.
#' @param eps Small value to avoid log(0) in LogLoss calculations. Default is 1e-15.
#' @param ... Additional arguments passed to e1071::svm
#'
#' @importFrom e1071 svm
#' @importFrom caret confusionMatrix
#' @return Performance metric (RMSE for continuous, Kappa for classification)
#' @export

wrapper_svm <- function(formula,
                        data,
                        train_indices,
                        test_indices,
                        metric,
                        metricfunc = NULL,
                        eps = 1e-15,
                        ...) {
  y_name <- all.vars(formula)[1]
  data <- characters_to_factors(data, all.vars(formula))

  # Ensure factor outcome for classification metrics
  if (metric %in% c("Kappa", "LogLoss")) {
    data[[y_name]] <- as.factor(data[[y_name]])
  }
  
  model <- e1071::svm(
    formula = formula,
    data = data[train_indices, , drop = FALSE],
    probability = TRUE,
    ...
  )
  
  predictions <- stats::predict(
    model,
    newdata = data[test_indices, , drop = FALSE],
    probability = TRUE
  )
  
  actual <- data[test_indices, , drop = FALSE][[y_name]]
  
  # Remove problematic rows for numeric regression metrics
  bad_idx <- is.infinite(predictions) | is.infinite(actual)
  if (any(bad_idx)) {
    predictions <- predictions[!bad_idx]
    actual <- actual[!bad_idx]
  }
  
  if (!is.null(metricfunc)) {
    metric_value <- call_metricfunc(metricfunc, actual, predictions, ...)
  } else if (metric == "RMSE") {
    metric_value <- sqrt(mean((predictions - actual)^2))
  } else if (metric == "Kappa") {
    pred_class <- factor(predictions, levels = levels(factor(actual)))
    cm <- caret::confusionMatrix(pred_class, factor(actual))
    metric_value <- unname(cm$overall["Kappa"])
  } else if (metric == "LogLoss") {
    # Extract probability matrix from svm predictions
    prob_mat <- attr(predictions, "probabilities")
    if (is.null(prob_mat)) {
      stop("LogLoss requires probability=TRUE in svm() and probabilities in predict().")
    }
    
    actual <- factor(actual)
    lvls <- levels(actual)
    
    # Align probability columns to actual levels when possible
    common <- intersect(lvls, colnames(prob_mat))
    if (length(common) == 0L) {
      stop("Could not align probability columns with outcome class levels for LogLoss.")
    }
    
    # If predict() returns fewer columns than levels, subset levels to common
    actual2 <- factor(actual, levels = common)
    prob_mat2 <- prob_mat[, common, drop = FALSE]
    
    # Clip probabilities for numerical stability
    prob_mat2 <- pmin(pmax(prob_mat2, eps), 1 - eps)
    
    if (nlevels(actual2) == 2L) {
      # Binary log loss: use probability of the second level as "positive"
      pos <- levels(actual2)[2]
      p <- prob_mat2[, pos]
      y <- as.integer(actual2 == pos)
      metric_value <- -mean(y * log(p) + (1 - y) * log(1 - p))
    } else {
      # Multiclass log loss: -mean(log p_trueclass)
      idx <- cbind(seq_along(actual2), as.integer(actual2))
      p_true <- prob_mat2[idx]
      metric_value <- -mean(log(p_true))
    }
  } else {
    stop("Unsupported metric for SVM wrapper.")
  }
  
  return(metric_value)
}

#' k-Nearest Neighbors (KNN) wrapper for CCI (kknn-based)
#'
#' @param formula Model formula
#' @param data Data frame
#' @param train_indices Indices for training rows
#' @param test_indices Indices for test rows
#' @param metric Performance metric: "RMSE" (regression), "Kappa" (classification), or "LogLoss" (classification), or the name of a custom metric when \code{metricfunc} is given
#' @param metricfunc Optional custom metric function \code{function(actual, predictions, ...)}. A numeric response gives regression (numeric predictions); a factor, character or logical response gives classification (predicted classes as a factor). The \code{...} arguments are only passed on if the function accepts them.
#' @param k Integer, number of neighbors (default 15)
#' @param eps Small value to avoid log(0) in LogLoss calculations. Default is 1e-15.
#' @param positive Character. The positive class label for binary classification (used in LogLoss). Default is NULL.
#' @param kernel Character. Weighting kernel for kknn. Default "optimal".
#' @param distance Numeric. Minkowski distance parameter. 2 = Euclidean. Default 2.
#' @param ... Additional arguments passed to kknn::kknn (e.g., ykernel, na.action)
#'
#' @importFrom kknn kknn 
#' @importFrom stats model.matrix fitted
#' @importFrom caret confusionMatrix
#'
#' @return Numeric performance metric
#' @export

# NOTE! This function was written by AI after seeing the other wrapper functions!!

wrapper_knn <- function(formula,
                        data,
                        train_indices,
                        test_indices,
                        metric,
                        metricfunc = NULL,
                        k = 15,
                        eps = 1e-15,
                        positive = NULL,
                        kernel = "optimal",
                        distance = 2,
                        ...) {
  # Parse vars
  y_name <- all.vars(formula)[1]
  x_names <- all.vars(formula)[-1]
  
  # Type of task: from the metric, or from the response when a custom metric is used
  y <- data[[y_name]]
  if (metric %in% c("Kappa", "LogLoss")) {
    classification <- TRUE
  } else if (metric == "RMSE") {
    classification <- FALSE
  } else if (!is.null(metricfunc)) {
    classification <- is.factor(y) || is.character(y) || is.logical(y)
  } else {
    stop("metric must be 'RMSE' (regression), 'Kappa' (classification), or 'LogLoss' (classification), ",
         "or a custom metricfunc must be given.")
  }
  if (classification) {
    y <- as.factor(y)
  } else {
    if (!is.numeric(y)) stop("Metric '", metric, "' requires a numeric response.")
    y <- as.numeric(y)
  }
  
  # Design matrices using one-hot for factors; build ONCE to keep same columns
  X_all <- if (length(x_names) > 0) {
    stats::model.matrix(~ . - 1, data = data[x_names])
  } else {
    stop("No predictors provided in formula.")
  }
  
  # Split
  X_train <- X_all[train_indices, , drop = FALSE]
  X_test  <- X_all[test_indices, , drop = FALSE]
  y_train <- y[train_indices]
  y_test  <- y[test_indices]
  
  # Remove rows with Inf in features or outcomes (defensive)
  bad_train <- rowSums(!is.finite(X_train)) > 0 | !is.finite(if (classification) as.numeric(y_train) else y_train)
  bad_test  <- rowSums(!is.finite(X_test))  > 0 | !is.finite(if (classification) as.numeric(y_test) else y_test)
  if (any(bad_train)) {
    X_train <- X_train[!bad_train, , drop = FALSE]
    y_train <- y_train[!bad_train]
  }
  if (any(bad_test)) {
    X_test <- X_test[!bad_test, , drop = FALSE]
    y_test <- y_test[!bad_test]
  }
  
  # Sanity checks
  if (nrow(X_train) < 1L || nrow(X_test) < 1L) stop("Empty train/test after filtering non-finite values.")
  if (k > nrow(X_train)) {
    warning(sprintf("k=%d > n_train=%d; reducing k to n_train.", k, nrow(X_train)))
    k <- nrow(X_train)
  }
  
  # Helper: build data frames for kknn from model.matrix (keeps encoding consistent)
  train_df <- data.frame(y = y_train, X_train, check.names = FALSE)
  test_df  <- data.frame(y = y_test,  X_test,  check.names = FALSE)
  
  # Fit kknn model
  # Note: kknn uses "kmax"; setting kmax = k yields exactly k neighbors.
  fit <- kknn::kknn(
    formula = y ~ .,
    train   = train_df,
    test    = test_df,
    k    = k,
    kernel  = kernel,
    distance = distance,
    ...
  )
  
  # Predictions
  if (!classification) {
    preds <- as.numeric(fitted(fit))
  } else {
    preds <- as.character(fitted(fit))  # class labels
    preds <- factor(preds, levels = levels(factor(y_train)))
  }

  # Custom metric function override
  if (!is.null(metricfunc)) {
    return(call_metricfunc(metricfunc, y_test, preds, ...))
  }
  
  # Built-in metrics
  if (metric == "RMSE") {
    keep <- is.finite(preds) & is.finite(y_test)
    return(sqrt(mean((preds[keep] - y_test[keep])^2)))
    
  } else if (metric == "Kappa") {
    y_test_f <- factor(y_test, levels = levels(preds))
    cm <- caret::confusionMatrix(preds, y_test_f)
    return(unname(cm$overall["Kappa"]))
    
  } else if (metric == "LogLoss") {
    y_train <- droplevels(as.factor(y_train))
    y_test  <- factor(y_test, levels = levels(y_train))
    
    prob_mat <- fit$prob
    if (is.null(prob_mat)) stop("kknn did not return class probabilities; cannot compute LogLoss.")
    
    # Align columns with training levels
    lvls <- levels(y_train)
    if (!all(lvls %in% colnames(prob_mat))) {
      common <- intersect(lvls, colnames(prob_mat))
      if (length(common) < 2L) stop("Could not align probability columns with class levels for LogLoss.")
      lvls <- common
      y_test <- factor(y_test, levels = lvls)
      prob_mat <- prob_mat[, lvls, drop = FALSE]
    } else {
      prob_mat <- prob_mat[, lvls, drop = FALSE]
    }
    
    # Clip probabilities for stability
    prob_mat <- pmin(pmax(prob_mat, eps), 1 - eps)
    
    if (nlevels(y_train) == 2L) {
      pos <- if (!is.null(positive)) {
        if (!positive %in% lvls) stop("`positive` must be one of: ", paste(lvls, collapse = ", "))
        positive
      } else {
        lvls[2]
      }
      p_pos <- prob_mat[, pos]
      y_bin <- as.integer(y_test == pos)
      return(-mean(y_bin * log(p_pos) + (1 - y_bin) * log(1 - p_pos)))
    } else {
      # Multiclass log loss: -mean(log p_trueclass)
      idx <- cbind(seq_along(y_test), as.integer(y_test))
      p_true <- prob_mat[idx]
      return(-mean(log(p_true)))
    }
  }
  
  stop("Unsupported metric.")
}

