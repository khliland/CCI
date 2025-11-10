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

