#' Extreme Gradient Boosting wrapper for CCI
#'
#' @param formula Model formula
#' @param data Data frame
#' @param train_indices Indices for training data
#' @param test_indices Indices for training data
#' @param nrounds Number of boosting rounds
#' @param metric Type of metric ("RMSE", "Kappa" or "Log Loss")
#' @param metricfunc A user specific metric function which have the arguments data, model test_indices and test_matrix and returns a numeric value
#' @param nthread Integer. Number of threads to use for parallel computation during model training in XGBoost. Default is 1.
#' @param eps Small value to avoid log(0) in LogLoss calculations. Default is 1e-15.
#' @param ... Additional arguments passed to xgb.train
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
                            subsample = 1,
                            ...) {
  
  independent <- all.vars(formula)[-1]
  dependent <- all.vars(formula)[1]
  
  # Keep original labels for metrics; but xgboost needs numeric labels
  y_orig <- data[[dependent]]
  
  # If dependent is factor, encode to numeric 0..K-1 for xgboost
  if (is.factor(data[[dependent]])) {
    data[[dependent]] <- as.numeric(data[[dependent]]) - 1
  }
  
  training <- data[train_indices, ]
  testing  <- data[test_indices, ]
  
  if (any(sapply(training[independent], is.factor))) {
    train_features <- stats::model.matrix(~ . - 1, data = training[independent])
    test_features  <- stats::model.matrix(~ . - 1, data = testing[independent])
  } else {
    train_features <- as.matrix(training[independent])
    test_features  <- as.matrix(testing[independent])
  }
  
  train_label <- training[[dependent]]
  test_label  <- testing[[dependent]]
  
  dtrain <- xgboost::xgb.DMatrix(data = train_features, label = as.numeric(train_label))
  dtest  <- test_features
  
  # For classification metrics, we prefer to work with factor levels
  # If original response was factor, preserve its level ordering
  y_test_factor <- NULL
  if (is.factor(y_orig)) {
    y_test_factor <- factor(y_orig[test_indices], levels = levels(y_orig))
  } else {
    # numeric case: will treat as factor for multi-class logloss if needed
    y_test_factor <- factor(test_label)
  }
  
  # Determine data_type (minimal change: extend for LogLoss)
  if (is.numeric(train_label) && length(unique(train_label)) > 2 && metric == "RMSE") {
    data_type <- "continuous"
  } else if (is.numeric(train_label) && length(unique(train_label)) == 2 && metric %in% c("Kappa", "LogLoss")) {
    data_type <- "binary"
  } else if (is.numeric(train_label) && length(unique(train_label)) == 2 && metric == "RMSE") {
    data_type <- "continuous"
  } else if (is.numeric(train_label) && length(unique(train_label)) > 2 && metric %in% c("Kappa", "LogLoss")) {
    data_type <- "categorical"
  } else if (metric == "RMSE") {
    data_type <- "continuous"
  } else if (is.numeric(train_label) && length(unique(train_label)) == 1) {
    data_type <- "categorical"
  } else if (length(unique(train_label)) > 6 && metric %in% c("Kappa", "LogLoss")) {
    data_type <- "categorical"
    warning("More than 6 classes detected. Consider RMSE for continuous targets, or be cautious with classification metrics.")
  } else {
    data_type <- "categorical"
  }
  
  
  if (data_type == "categorical") {
    num_class <- length(unique(data[[dependent]]))
  } else {
    num_class <- NULL
  }
  dots <- list(...)
  
  
  if (!"objective" %in% names(args)) {
    params <- list(
      objective = switch(data_type,
                         continuous   = "reg:squarederror",
                         binary       = "binary:logistic",
                         categorical  = "multi:softprob"),
      eval_metric = switch(data_type,
                           continuous   = "rmse",
                           binary       = "error",
                           categorical  = "merror"),
      nthread   = nthread
    )
  } else {
    params <- list(
      objective = args$objective,
      eval_metric = switch(data_type,
                           continuous   = "rmse",
                           binary       = "error",
                           categorical  = "merror"),
      nthread   = nthread,
      subsample = subsample
      )
  }
  
  
  params <- utils::modifyList(params, dots)
  params <- utils::modifyList(params, list(num_class = num_class))
  params <- params[!sapply(params, is.null)]
  
  model <- xgboost::xgb.train(
    data      = dtrain,
    params    = params,
    nrounds   = nrounds,
    verbose   = 0
  )
  
  predictions <- stats::predict(model, newdata = dtest)
  
  # Remove non-finite entries safely
  # (predictions may be vector; actual may be matrix)
  eps_bad <- !is.finite(predictions)
  if (any(eps_bad)) predictions <- predictions[!eps_bad]
  
  if (!is.null(metricfunc)) {
    metric_value <- metricfunc(y_test_factor, predictions, ...)
    return(metric_value)
  }
  
  # --- Metrics ---
  if (params$objective %in% c("reg:squarederror", "reg:squaredlogerror", "reg:pseudohubererror")) {
    
    actual <- as.numeric(test_label)
    metric_value <- sqrt(mean((predictions - actual)^2))
    
  } else if (params$objective %in% "binary:logistic") {
    
    if (metric == "LogLoss") {
      # Binary log loss
      eps <- eps
      p <- pmin(pmax(as.numeric(predictions), eps), 1 - eps)
      
      # Need y in {0,1} with 1 as "positive"
      # If original was factor, treat 2nd level as positive by convention
      if (!is.null(y_test_factor) && nlevels(y_test_factor) == 2) {
        pos <- levels(y_test_factor)[2]
        y01 <- as.integer(y_test_factor == pos)
      } else {
        y01 <- as.integer(test_label == 1)
      }
      
      metric_value <- -mean(y01 * log(p) + (1 - y01) * log(1 - p))
      
    } else {
      # Kappa (your existing behavior)
      pred_class <- ifelse(predictions > 0.5, 1, 0)
      conf_matrix <- try(
        caret::confusionMatrix(
          factor(pred_class, levels = levels(factor(test_label))),
          factor(test_label)
        ),
        silent = TRUE
      )
      metric_value <- conf_matrix$overall[2]
    }
    
  } else if (params$objective %in% "multi:softprob") {
    
    # reshape to n x K
    P <- matrix(predictions, ncol = num_class, byrow = TRUE)
    
    if (metric == "LogLoss") {
      # Multiclass log loss: -log p_true
      eps <- 1e-15
      P <- pmax(P, eps)
      
      # True class indices must be 1..K
      # test_label is 0..K-1
      true_idx <- as.integer(test_label) + 1L
      
      p_true <- P[cbind(seq_along(true_idx), true_idx)]
      metric_value <- -mean(log(p_true))
      
    } else {
      # Kappa (your existing behavior)
      lev <- levels(factor(train_label))
      pred_class <- max.col(P) - 1
      conf_matrix <- try(
        caret::confusionMatrix(
          factor(pred_class, levels = lev),
          factor(test_label, levels = lev)
        ),
        silent = TRUE
      )
      metric_value <- conf_matrix$overall[2]
    }
    
  } else {
    stop("Objective function for XGBoost is not supported")
  }
  
  return(metric_value)
}


#' Random Forest wrapper for CCI
#'
#' @param formula Model formula specifying the dependent and independent variables.
#' @param data Data frame containing the dataset to be used for training and testing the model.
#' @param train_indices A vector of indices specifying the rows in `data` to be used as the training set.
#' @param test_indices A vector of indices specifying the rows in `data` to be used as the test set.
#' @param metric Type of metric ("RMSE", "Kappa" or "Log Loss")
#' @param metricfunc Optional user-defined function to calculate a custom performance metric. This function should take the arguments `data`, `model`, and `test_indices`, and return a numeric value representing the performance metric.
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
  if (metric %in% c("Kappa", "LogLoss")) {
    dependent <- all.vars(formula)[1]
    testing <- data[test_indices, ]
    test_label <- testing[[dependent]]
    model <- ranger::ranger(formula, data = data[train_indices, ], mtry = mtry, probability = TRUE, num.threads = nthread, num.trees = num.trees, ...)
  } else if (metric == "RMSE") {
    model <- ranger::ranger(formula, data = data[train_indices, ], mtry = mtry, num.threads = nthread, num.trees = num.trees)
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
    metric_value <- metricfunc(actual, predictions, ...)
  } else if (metric %in% c("Kappa")) {
    if (nlevels(factor(actual)) > 2) {
      pred_class <- apply(predictions, 1, which.max)
      pred_class <- factor(pred_class, levels = 1:nlevels(factor(actual)), labels = levels(factor(actual)))
      cm <- caret::confusionMatrix(pred_class, factor(actual))
      metric_value <- cm$overall["Kappa"]
    } else {
      pred_class <- ifelse(predictions[, 2] > 0.5, 1, 0)
      conf_matrix <- try(caret::confusionMatrix(factor(pred_class, levels = levels(factor(test_label))), factor(test_label)), silent = TRUE)
      metric_value <- conf_matrix$overall["Kappa"]
      }
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
    metric_value <- metricfunc(actual, predictions, ...)
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
#' @param metric Performance metric: "RMSE" (regression), "Kappa" (classification), or "LogLoss" (classification)
#' @param metricfunc Optional custom metric function: function(actual, predictions, ...)
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
  
  # Outcome: coerce type to match metric
  y <- data[[y_name]]
  if (metric %in% c("Kappa", "LogLoss")) {
    y <- as.factor(y)
  } else if (metric %in% "RMSE") {
    y <- as.numeric(y)
  } else {
    stop("metric must be 'RMSE' (regression), 'Kappa' (classification), or 'LogLoss' (classification).")
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
  bad_train <- rowSums(!is.finite(X_train)) > 0 | !is.finite(if (metric == "RMSE") y_train else as.numeric(y_train))
  bad_test  <- rowSums(!is.finite(X_test))  > 0 | !is.finite(if (metric == "RMSE") y_test  else as.numeric(y_test))
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
  if (metric == "RMSE") {
    preds <- as.numeric(fitted(fit))
  } else {
    preds <- as.character(fitted(fit))  # class labels
    preds <- factor(preds, levels = levels(factor(y_train)))
  }
  
  # Custom metric function override
  if (!is.null(metricfunc)) {
    return(metricfunc(y_test, preds, ...))
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

