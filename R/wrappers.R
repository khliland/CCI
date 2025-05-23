#' Extreme Gradient Boosting wrapper for CCI
#'
#' @param formula Model formula
#' @param data Data frame
#' @param train_indices Indices for training data
#' @param test_indices Indices for training data
#' @param nrounds Number of boosting rounds
#' @param data_type Type of data (continuous, binary or categorical)
#' @param num_class Number of categorical classes
#' @param metricfunc A user specific metric function which have the arguments data, model test_indices and test_matrix and returns a numeric value
#' @param nthread Integer. Number of threads to use for parallel computation during model training in XGBoost. Default is 1.
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
                            data_type,
                            nrounds = 150,
                            num_class = NULL,
                            metricfunc = NULL,
                            nthread = 1,
                            ...) {


  independent <- all.vars(formula)[-1]
  dependent <- all.vars(formula)[1]
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

  params <- list(
    objective = switch(data_type,
                       continuous = "reg:squarederror",
                       binary = "binary:logistic",
                       categorical = "multi:softprob"),
    eval_metric = switch(data_type,
                         continuous = "rmse",
                         binary = "error",
                         categorical = "merror"),
    num_class = if (data_type == "categorical") length(unique(train_label)) else NULL
  )

  dots <- list(...)
  params <- utils::modifyList(params, dots)

  params <- params[!sapply(params, is.null)]


  model <- xgboost::xgb.train(data = dtrain,
                              params = params,
                              nthread = nthread,
                              nrounds = nrounds,
                              verbose = 0)

  pred <- predict(model, newdata = dtest)
  actual <- y_test
  if (!is.null(metricfunc)) {
    data_type <- "custom"
    metric <- metricfunc(actual, predictions, ...)
  } else if (params$objective %in% c("reg:squarederror", "reg:squaredlogerror", "reg:pseudohubererror")) {
    metric <- sqrt(mean((pred - actual)^2))
  } else if (params$objective %in% "binary:logistic") {
    pred_class <- ifelse(pred > 0.5, 1, 0)
    conf_matrix <- try(caret::confusionMatrix(factor(pred_class, levels = levels(factor(test_label))), factor(test_label)), silent = TRUE)
    metric <- conf_matrix$overall[2]
  } else if (params$objective %in% "multi:softprob") {
    levels <- levels(factor(train_label))
    pred <- matrix(pred, ncol=num_class, byrow=TRUE)
    pred_class <- max.col(pred) - 1
    conf_matrix <- try(caret::confusionMatrix(factor(pred_class, levels = levels), factor(test_label, levels = levels)), silent = TRUE)
    metric <- conf_matrix$overall[2]
  } else {
    stop("Objective function for XGBoost is not supported by perm.test()")
  }
  return(metric)
}

#' Random Forest wrapper for CCI
#'
#' @param formula Model formula specifying the dependent and independent variables.
#' @param data Data frame containing the dataset to be used for training and testing the model.
#' @param train_indices A vector of indices specifying the rows in `data` to be used as the training set.
#' @param test_indices A vector of indices specifying the rows in `data` to be used as the test set.
#' @param data_type Character string indicating the type of data. Can be "continuous" for regression, "binary" for binary classification, or "categorical" for multiclass classification.
#' @param metricfunc Optional user-defined function to calculate a custom performance metric. This function should take the arguments `data`, `model`, and `test_indices`, and return a numeric value representing the performance metric.
#' @param nthread Integer. The number of threads to use for parallel processing. Default is 1.
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
                           data_type,
                           metricfunc = NULL,
                           nthread = 1,
                           ...) {
  if (data_type %in% c("binary", "categorical")) {
    model <- ranger::ranger(formula, data = data[train_indices, ], probability = TRUE, num.threads = nthread, ...)
  } else if (data_type %in% "continuous") {
    model <- ranger::ranger(formula, data = data[train_indices, ], probability = FALSE, num.threads = nthread, ...)
  }

  predictions <- predict(model, data = data[test_indices, ])$predictions
  actual <- data[test_indices, ][[all.vars(formula)[1]]]

  if (!is.null(metricfunc)) {
    metric <- metricfunc(actual, predictions, ...)
  } else if (data_type %in% c("binary", "categorical")) {
    if (nlevels(factor(actual)) > 2) {
      pred_class <- apply(predictions, 1, which.max)
      pred_class <- factor(pred_class, levels = 1:nlevels(factor(actual)), labels = levels(factor(actual)))
      cm <- caret::confusionMatrix(pred_class, factor(actual))
      metric <- cm$overall["Kappa"]
    } else {
      pred_class <- ifelse(predictions[, 2] > 0.5, 1, 0)
      cm <- caret::confusionMatrix(factor(pred_class), factor(actual))
      metric <- cm$overall["Kappa"]
      }
    } else if (data_type == "continuous") {
    metric <- sqrt(mean((predictions - actual)^2))
  }

  return(metric)
}

#' SVM wrapper for CCI
#'
#' @param formula Model formula
#' @param data Data frame
#' @param train_indices Indices for training data
#' @param test_indices Indices for testing data
#' @param data_type Type of data ("continuous", "binary", or "categorical")
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
                        data_type,
                        metricfunc = NULL,
                        ...) {
  y_name <- all.vars(formula)[1]

  if (data_type %in% c("binary", "categorical")) {
    data[[y_name]] <- as.factor(data[[y_name]])
  }


  model <- e1071::svm(formula = formula, data = data[train_indices, ], probability = TRUE, ...)

  predictions <- predict(model, newdata = data[test_indices, ], probability = TRUE)
  actual <- data[test_indices, ][[all.vars(formula)[1]]]

  if (!is.null(metricfunc)) {
    metric <- metricfunc(actual, predictions, ...)
  } else if (data_type == "continuous") {
    metric <- sqrt(mean((predictions - actual)^2))
  } else if (data_type %in% c("binary", "categorical")) {
    pred_class <- factor(predictions, levels = levels(factor(actual)))
    cm <- caret::confusionMatrix(pred_class, factor(actual))
    metric <- cm$overall["Kappa"]
  } else {
    stop("Unsupported data_type for SVM wrapper.")
  }

  return(metric)
}

