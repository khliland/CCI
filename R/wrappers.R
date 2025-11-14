#' Extreme Gradient Boosting wrapper for CCI
#'
#' @param formula Model formula
#' @param data Data frame
#' @param train_indices Indices for training data
#' @param test_indices Indices for training data
#' @param nrounds Number of boosting rounds
#' @param metric Type of performance metric
#' @param metricfunc A user specific metric function which have the arguments data, model test_indices and test_matrix and returns a numeric value
#' @param nthread Integer. Number of threads to use for parallel computation during model training in XGBoost. Default is 1.
#' @param subsample Proportion of the data to be used. Default is 1 (no subsampling).
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
                            subsample = 1,
                            ...) {


  independent <- all.vars(formula)[-1]
  dependent <- all.vars(formula)[1]
  # if dependent is a factor variable encode to numeric
  if (is.factor(data[[dependent]])) {
    data[[dependent]] <- as.numeric(data[[dependent]]) - 1
  }
  training <- data[train_indices, ]
  testing <- data[test_indices, ]

  if (any(sapply(training[independent], is.factor))) {
    train_features <- model.matrix(~ . - 1, data = training[independent])
    test_features <- model.matrix(~ . - 1, data = testing[independent])
  } else {
    train_features <- as.matrix(training[independent])
    test_features <- as.matrix(testing[independent])
  }

  train_label <- training[[dependent]]
  test_label <- testing[[dependent]]

  dtrain <- xgboost::xgb.DMatrix(data = train_features, label = as.numeric(train_label))
  dtest <- test_features

  y_test <- as.matrix(testing[dependent])

  if (is.numeric(train_label) && length(unique(train_label)) > 2 && metric == 'RMSE') {
    data_type <- "continuous"
  } else if (is.numeric(train_label) && length(unique(train_label)) == 2 && metric == 'Kappa') {
    data_type <- "binary"
  } else if (is.numeric(train_label) && length(unique(train_label)) == 2 && metric == 'RMSE'){
    data_type <- "continuous"
  } else if (is.numeric(train_label) && length(unique(train_label)) > 2 && metric == 'Kappa') {
    data_type <- "categorical"
  } else if (metric == "RMSE") {
    data_type <- "continuous"
  } else if (is.numeric(train_label) && length(unique(train_label)) == 1) {
    data_type <- "categorical"
  } else if (length(unique(train_label)) > 6 && metric == 'Kappa') {
    data_type <- "categorical"
    warning("More than 6 classes detected. It might be better to use RMSE as the metric for this data.")
  }
  else {
    data_type <- "categorical"
  }
  if (data_type == "categorical") {
    num_class <- length(unique(data[[all.vars(formula)[1]]]))
  } else {
    num_class <- NULL
  }
 
  args <- list(...)
  if (!"objective" %in% names(args)) {
  params <- list(
    objective = switch(data_type,
                       continuous = "reg:squarederror",
                       binary = "binary:logistic",
                       categorical = "multi:softprob"),
    eval_metric = switch(data_type,
                         continuous = "rmse",
                         binary = "error",
                         categorical = "merror"))
  } else {
    params <- list(objective = args$object,
                   eval_metric = switch(data_type,
                                        continuous = "rmse",
                                        binary = "error",
                                        categorical = "merror"))
  }

  dots <- list(...)
  params <- utils::modifyList(params, dots)
  params <- utils::modifyList(params, list(num_class = num_class))

  params <- params[!sapply(params, is.null)]

  model <- xgboost::xgb.train(data = dtrain,
                              params = params,
                              nthread = nthread,
                              nrounds = nrounds,
                              subsample = subsample,
                              verbose = 0)

  predictions <- stats::predict(model, newdata = dtest)
  actual <- y_test
  
  bad_idx <- is.infinite(predictions) | is.infinite(actual)
  if (any(bad_idx)) {
    predictions <- predictions[!bad_idx]
    actual <- actual[!bad_idx]
  }
  
  if (!is.null(metricfunc)) {
    metric_value <- metricfunc(actual, predictions, ...)
  } else if (params$objective %in% c("reg:squarederror", "reg:squaredlogerror", "reg:pseudohubererror")) {
    metric_value <- sqrt(mean((predictions - actual)^2))
  } else if (params$objective %in% "binary:logistic") {
    pred_class <- ifelse(predictions > 0.5, 1, 0)
    conf_matrix <- try(caret::confusionMatrix(factor(pred_class, levels = levels(factor(test_label))), factor(test_label)), silent = TRUE)
    metric_value <- conf_matrix$overall[2]
  } else if (params$objective %in% "multi:softprob") {
    levels <- levels(factor(train_label))
    predictions <- matrix(predictions, ncol=num_class, byrow=TRUE)
    pred_class <- max.col(predictions) - 1
    conf_matrix <- try(caret::confusionMatrix(factor(pred_class, levels = levels), factor(test_label, levels = levels)), silent = TRUE)
    metric_value <- conf_matrix$overall[2]
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
#' @param metric Character string indicating the type of performance metric. Can be "RMSE" for regression, "Kappa" for binary classification, or multiclass classification.
#' @param metricfunc Optional user-defined function to calculate a custom performance metric. This function should take the arguments `data`, `model`, and `test_indices`, and return a numeric value representing the performance metric.
#' @param nthread Integer. The number of threads to use for parallel processing. Default is 1.
#' @param num.trees Integer. The number of trees to grow in the random forest. 
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
                           num.trees,
                           ...) {
  if (metric %in% c("Kappa")) {
    model <- ranger::ranger(formula, data = data[train_indices, ], probability = TRUE, num.threads = nthread, num.trees = num.trees, ...)
  } else if (metric %in% "RMSE") {
    model <- ranger::ranger(formula, data = data[train_indices, ], probability = FALSE, num.threads = nthread, num.trees = num.trees, ...)
  } else {
    model <- ranger::ranger(formula, data = data[train_indices, ], num.threads = nthread, num.trees = num.trees, ...)
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
      cm <- caret::confusionMatrix(factor(pred_class), factor(actual))
      metric_value <- cm$overall["Kappa"]
      }
    } else if (metric == "RMSE") {
      metric_value <- sqrt(mean((predictions - actual)^2))
  }

  return(metric_value)
}

#' SVM wrapper for CCI
#'
#' @param formula Model formula
#' @param data Data frame
#' @param train_indices Indices for training data
#' @param test_indices Indices for testing data
#' @param metric Type of metric ("RMSE" or "Kappa")
#' @param metricfunc Optional user-defined function to calculate a custom performance metric.
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
                        ...) {
  y_name <- all.vars(formula)[1]

  if (metric %in% c("Kappa")) {
    data[[y_name]] <- as.factor(data[[y_name]])
  }


  model <- e1071::svm(formula = formula, data = data[train_indices, ], probability = TRUE, ...)

  predictions <- stats::predict(model, newdata = data[test_indices, ], probability = TRUE)
  actual <- data[test_indices, ][[all.vars(formula)[1]]]
  
  bad_idx <- is.infinite(predictions) | is.infinite(actual)
  if (any(bad_idx)) {
    predictions <- predictions[!bad_idx]
    actual <- actual[!bad_idx]
  }
  
  if (!is.null(metricfunc)) {
    metric_value <- metricfunc(actual, predictions, ...)
  } else if (metric == "RMSE") {
    metric_value <- sqrt(mean((predictions - actual)^2))
  } else if (metric %in% c("Kappa")) {
    pred_class <- factor(predictions, levels = levels(factor(actual)))
    cm <- caret::confusionMatrix(pred_class, factor(actual))
    metric_value <- cm$overall["Kappa"]
  } else {
    stop("Unsupported metric for SVM wrapper.")
  }

  return(metric_value)
}

#' k-Nearest Neighbors (KNN) wrapper for CCI
#'
#' @param formula Model formula
#' @param data Data frame
#' @param train_indices Indices for training rows
#' @param test_indices Indices for test rows
#' @param metric Performance metric: "RMSE" (regression) or "Kappa" (classification)
#' @param metricfunc Optional custom metric function: function(actual, predictions, ...)
#' @param k Integer, number of neighbors (default 15)
#' @param center Logical, center features using training means (default TRUE)
#' @param scale. Logical, scale features using training sds (default TRUE)
#' @param ... Ignored (for API consistency)
#'
#' @importFrom class knn
#' @importFrom FNN knn.reg
#' @importFrom stats model.matrix
#' @importFrom caret confusionMatrix
#'
#' @return Numeric performance metric
#' @export
wrapper_knn <- function(formula,
                        data,
                        train_indices,
                        test_indices,
                        metric,
                        metricfunc = NULL,
                        k = 15,
                        center = TRUE,
                        scale. = TRUE,
                        ...) {
  # Parse vars
  y_name <- all.vars(formula)[1]
  x_names <- all.vars(formula)[-1]
  
  # Outcome: coerce type to match metric
  y <- data[[y_name]]
  if (metric %in% "Kappa") {
    y <- as.factor(y)
  } else if (metric %in% "RMSE") {
    y <- as.numeric(y)
  } else {
    stop("metric must be 'RMSE' (regression) or 'Kappa' (classification).")
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
  
  # Optional standardization using TRAIN stats
  if (isTRUE(center) || isTRUE(scale.)) {
    # compute train means/sds (avoid zero sd)
    cm <- colMeans(X_train, na.rm = TRUE)
    cs <- apply(X_train, 2, sd)
    cs[cs == 0 | is.na(cs)] <- 1
    
    if (isTRUE(center)) {
      X_train <- sweep(X_train, 2, cm, FUN = "-")
      X_test  <- sweep(X_test,  2, cm, FUN = "-")
    }
    if (isTRUE(scale.)) {
      X_train <- sweep(X_train, 2, cs, FUN = "/")
      X_test  <- sweep(X_test,  2, cs, FUN = "/")
    }
  }
  
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
  
  # Fit/predict + metric
  if (!is.null(metricfunc)) {
    # Let user compute with raw predictions we produce below
    if (metric == "RMSE") {
      preds <- FNN::knn.reg(train = X_train, test = X_test, y = as.numeric(y_train), k = k)$pred
      return(metricfunc(y_test, preds, ...))
    } else { # Kappa
      preds <- class::knn(train = X_train, test = X_test, cl = y_train, k = k, prob = TRUE)
      return(metricfunc(y_test, preds, ...))
    }
  } else if (metric == "RMSE") {
    preds <- FNN::knn.reg(train = X_train, test = X_test, y = as.numeric(y_train), k = k)$pred
    # defensive: drop any non-finite pairs
    keep <- is.finite(preds) & is.finite(y_test)
    rmse <- sqrt(mean((preds[keep] - y_test[keep])^2))
    return(rmse)
  } else if (metric %in% "Kappa") {
    preds <- class::knn(train = X_train, test = X_test, cl = y_train, k = k, prob = TRUE)
    # Ensure factors share the same levels
    preds_f <- factor(preds, levels = levels(y_train))
    y_test_f <- factor(y_test, levels = levels(y_train))
    cm <- caret::confusionMatrix(preds_f, y_test_f)
    return(unname(cm$overall["Kappa"]))
  } else {
    stop("Unsupported metric.")
  }
}

#' CatBoost wrapper for CCI
#'
#' @param formula Model formula
#' @param data Data frame
#' @param train_indices Integer vector: row indices for training
#' @param test_indices Integer vector: row indices for testing
#' @param metric Performance metric: "RMSE" (regression) or "Kappa" (classification)
#' @param metricfunc Optional custom metric function: function(actual, predictions, ...)
#' @param iterations Number of boosting iterations (default 500)
#' @param learning_rate Learning rate (default 0.1)
#' @param depth Tree depth (default 6)
#' @param l2_leaf_reg L2 regularization (default 3)
#' @param thread_count Threads for CatBoost (default 1)
#' @param loss_function Optional CatBoost loss_function override (e.g., "RMSE","Logloss","MultiClass")
#' @param random_seed Random seed for reproducibility (default 1)
#' @param ... Additional CatBoost params passed into `params`
#'
#' @importFrom catboost catboost.load_pool catboost.train catboost.predict
#' @importFrom caret confusionMatrix
#' @importFrom stats model.matrix
#'
#' @return Numeric performance metric
#' @export
wrapper_catboost <- function(formula,
                             data,
                             train_indices,
                             test_indices,
                             metric,
                             metricfunc = NULL,
                             iterations = 500,
                             learning_rate = 0.1,
                             depth = 6,
                             l2_leaf_reg = 3,
                             thread_count = 1,
                             loss_function = NULL,
                             random_seed = 1,
                             ...) {
  # --- Parse variables ---
  y_name <- all.vars(formula)[1]
  x_names <- all.vars(formula)[-1]
  if (length(x_names) == 0) stop("No predictors provided in formula.")
  
  # Subset design frames once; keep factors for CatBoost native handling
  X_all <- data[, x_names, drop = FALSE]
  # Identify categorical feature indices (1-based within x_names)
  cat_idx <- which(sapply(X_all, is.factor))
  
  # Outcome handling
  if (metric == "Kappa") {
    y_factor <- factor(data[[y_name]])
    class_levels <- levels(y_factor)
    y_all <- as.integer(y_factor) - 1L  # CatBoost expects 0...(K-1)
    n_class <- length(class_levels)
    is_binary <- n_class == 2L
  } else if (metric == "RMSE") {
    y_all <- as.numeric(data[[y_name]])
    n_class <- NULL
    is_binary <- FALSE
  } else {
    stop("metric must be 'RMSE' (regression) or 'Kappa' (classification).")
  }
  
  # Train/test split
  X_train <- X_all[train_indices, , drop = FALSE]
  X_test  <- X_all[test_indices,  , drop = FALSE]
  y_train <- y_all[train_indices]
  y_test  <- y_all[test_indices]
  
  # Replace Inf with NA in features; CatBoost can handle NA but not Inf
  fix_inf <- function(df) {
    for (j in seq_along(df)) {
      if (is.numeric(df[[j]])) {
        df[[j]][is.infinite(df[[j]])] <- NA_real_
      }
    }
    df
  }
  X_train <- fix_inf(X_train)
  X_test  <- fix_inf(X_test)
  
  # Pools
  pool_train <- catboost::catboost.load_pool(
    data = X_train,
    label = y_train,
    cat_features = if (length(cat_idx)) cat_idx else NULL,
    thread_count = thread_count
  )
  pool_test <- catboost::catboost.load_pool(
    data = X_test,
    label = y_test,
    cat_features = if (length(cat_idx)) cat_idx else NULL,
    thread_count = thread_count
  )
  
  # --- CatBoost params ---
  # Choose loss if not provided
  if (is.null(loss_function)) {
    loss_function <- if (metric == "RMSE") {
      "RMSE"
    } else if (metric == "Kappa" && is_binary) {
      "Logloss"
    } else {
      "MultiClass"
    }
  }
  
  # eval_metric default aligned with loss_function
  eval_metric <- switch(loss_function,
                        "RMSE" = "RMSE",
                        "Logloss" = "Logloss",
                        "CrossEntropy" = "Logloss",
                        "MultiClass" = "MultiClass",
                        "MultiClassOneVsAll" = "MultiClassOneVsAll",
                        # fallback
                        loss_function)
  
  params <- utils::modifyList(
    list(
      loss_function = loss_function,
      eval_metric = eval_metric,
      iterations = iterations,
      learning_rate = learning_rate,
      depth = depth,
      l2_leaf_reg = l2_leaf_reg,
      thread_count = thread_count,
      random_seed = random_seed,
      # Enable best model if you wish early stopping by passing od_params via ...
      # use_best_model = TRUE
    ),
    list(...)
  )
  
  # Train
  model <- catboost::catboost.train(
    learn_pool = pool_train,
    test_pool = NULL,
    params = params
  )
  
  # Predict
  if (metric == "RMSE") {
    preds <- catboost::catboost.predict(model, pool_test, prediction_type = "RawFormulaVal")
    actual <- as.numeric(y_test)
    keep <- is.finite(preds) & is.finite(actual)
    if (!any(keep)) stop("No finite predictions/actuals for RMSE.")
    if (!is.null(metricfunc)) {
      return(metricfunc(actual[keep], preds[keep], ...))
    } else {
      return(sqrt(mean((preds[keep] - actual[keep])^2)))
    }
  } else { # Kappa
    if (is_binary) {
      # Probability of class 1
      p1 <- catboost::catboost.predict(model, pool_test, prediction_type = "Probability")
      pred_class_int <- as.integer(p1 > 0.5) # 0/1
      # Map back to factor levels for confusionMatrix
      pred_fac <- factor(class_levels[pred_class_int + 1L], levels = class_levels)
      actual_fac <- factor(class_levels[y_test + 1L], levels = class_levels)
      if (!is.null(metricfunc)) {
        return(metricfunc(actual_fac, pred_fac, ...))
      } else {
        cm <- caret::confusionMatrix(pred_fac, actual_fac)
        return(unname(cm$overall["Kappa"]))
      }
    } else {
      # Multiclass: matrix of class probabilities
      prob_mat <- catboost::catboost.predict(model, pool_test, prediction_type = "Probability")
      if (is.vector(prob_mat)) {
        # Some versions return vector even for multiclass; coerce
        prob_mat <- matrix(prob_mat, ncol = n_class, byrow = TRUE)
      }
      pred_class_int <- max.col(prob_mat) - 1L
      pred_fac <- factor(class_levels[pred_class_int + 1L], levels = class_levels)
      actual_fac <- factor(class_levels[y_test + 1L], levels = class_levels)
      if (!is.null(metricfunc)) {
        return(metricfunc(actual_fac, pred_fac, ...))
      } else {
        cm <- caret::confusionMatrix(pred_fac, actual_fac)
        return(unname(cm$overall["Kappa"]))
      }
    }
  }
}
