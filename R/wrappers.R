#' Wrapper function for GLM model training and evaluation
#'
#' @param formula Model formula
#' @param data Data frame
#' @param train_indices Indices for training data
#' @param test_indices Indices for testing data
#' @param family Family for GLM
#' @param data_type Type of data (continuous or binary)
#' @param metricfunc A user-specified function which calculates a metric
#' @param ... Additional arguments passed to glm
#'
#' @importFrom stats glm predict
#' @importFrom caret confusionMatrix
#' @return Performance metric (defaults are RMSE for continuous, Kappa for binary)
#' @export

glm_wrapper <- function(formula,
                        data,
                        train_indices,
                        test_indices,
                        family,
                        data_type,
                        metricfunc = NULL,
                        ...) {
  model <- stats::glm(formula = formula, data = data, family = family, subset = train_indices, ...)

    if (!is.null(metricfunc)) {
    data_type <- "custom"
    metric <- metricfunc(data, model, test_indices)
  } else if (data_type %in% "continuous") {
    pred <- stats::predict.glm(model, newdata = data[test_indices,])
    actual <- data[test_indices,][[all.vars(formula)[1]]]
    metric <- sqrt(mean((pred - actual)^2))
  } else if (data_type %in% "binary") {
    levels <- levels(factor(data[[all.vars(formula)[1]]]))
    pred <- predict.glm(model, newdata = data[test_indices,], type = "response")
    actual <- data[test_indices, ][[all.vars(formula)[1]]]
    pred_class <- ifelse(pred > 0.5, 1, 0)
    cm <- caret::confusionMatrix(factor(pred_class, levels = levels), factor(actual, levels = levels))
    metric <- cm$overall["Kappa"]
  }
  return(metric)
}

#' Wrapper function for multinomial regression model training and evaluation
#'
#' @param formula Model formula
#' @param data Data frame
#' @param train_indices Indices for training data
#' @param test_indices Indices for testing data
#' @param metricfunc A user-specified metric function which has the arguments data, model, and test_indices, and returns a numeric value
#' @param ... Additional arguments passed to \code{nnet::multinom}
#'
#' @importFrom nnet multinom
#' @importFrom stats predict
#' @importFrom caret confusionMatrix
#' @return Performance metric (Kappa for classification tasks)
#' @export
multinom_wrapper <- function(formula,
                             data,
                             train_indices,
                             test_indices,
                             metricfunc = NULL,
                             ...) {
  model <- nnet::multinom(formula = Y ~ X + Z1 + Z2, data = data, subset = train_indices, trace = FALSE, ...)

  if (!is.null(metricfunc)) {
    data_type <- "custom"
    metric <- metricfunc(data, model, test_indices)
  } else {
    pred <- predict(model, newdata = data[test_indices,])
    actual <- data[test_indices,][[all.vars(formula)[1]]]
    pred <- as.factor(pred)
    cm <- caret::confusionMatrix(pred, factor(actual))
    metric <- cm$overall["Kappa"]
  }
  return(metric)
}

#' Wrapper function for XGBoost model training and evaluation
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
#' @return Performance metric
#' @export

xgboost_wrapper <- function(formula,
                            data,
                            train_indices,
                            test_indices,
                            data_type,
                            nrounds,
                            num_class = NULL,
                            metricfunc = NULL,
                            nthread = 1,
                            ...) {
  args <- list(...)
  if (!("objective" %in% names(args))) {
    if (data_type == "continuous") {
      args$objective <- "reg:squarederror"
    } else if (data_type %in% "binary") {
      args$objective <- "binary:logistic"
    } else if (data_type %in% "categorical") {
      args$objective <- "multi:softprob"
    }
  } else {
    args$objective <- args$objective
  }
  independent <- all.vars(formula)[-1]
  dependent <- update(formula, . ~ .)[[2]]
  training <- data[train_indices,]
  testing <- data[test_indices,]

  if (any(sapply(training, is.factor))) { # Only check if training data contains factor variables, assuming that testing contains the same
    train_features <- model.matrix(~ . - 1, data = training[independent])
    train_label <- training[[dependent]]

    test_features <- model.matrix(~ . - 1, data = testing[independent])
    test_label <- testing[[dependent]]

    train_matrix <- xgboost::xgb.DMatrix(data = as.matrix(train_features), label = as.matrix(train_label))
    test_matrix <- xgboost::xgb.DMatrix(data = as.matrix(test_features), label = as.matrix(test_label))
  } else {
    train_features <- training[independent]
    train_label <- training[[dependent]]

    test_features <- testing[independent]
    test_label <- testing[[dependent]]

    train_matrix <- xgboost::xgb.DMatrix(data = as.matrix(train_features), label = as.matrix(train_label))
    test_matrix <- xgboost::xgb.DMatrix(data = as.matrix(test_features), label = as.matrix(test_label))
  }


  if (!is.null(num_class) && args$objective == "multi:softprob") {
    args$num_class <- num_class
  }

  model <- xgboost::xgb.train(data = train_matrix,
                              nrounds = nrounds,
                              params = args,
                              nthread = nthread,
                              verbose = 0)

  pred <- predict(model, newdata = test_matrix)
  if (!is.null(metricfunc)) {
    data_type <- "custom"
    metric <- metricfunc(data, model, test_indices, test_matrix)
  } else if (args$objective %in% c("reg:squarederror", "reg:squaredlogerror", "reg:pseudohubererror")) {
    actual <- testing[[dependent]]
    metric <- sqrt(mean((pred - actual)^2))
  } else if (args$objective %in% "binary:logistic") {
    pred_class <- ifelse(pred > 0.5, 1, 0)
    conf_matrix <- try(caret::confusionMatrix(factor(pred_class, levels = levels(factor(test_label))), factor(test_label)), silent = TRUE)
    metric <- conf_matrix$overall[2]
  } else if (args$objective %in% "multi:softprob") {
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

#' Wrapper function for Ranger model training and evaluation
#'
#' @param formula Model formula specifying the dependent and independent variables.
#' @param data Data frame containing the dataset to be used for training and testing the model.
#' @param train_indices A vector of indices specifying the rows in `data` to be used as the training set.
#' @param test_indices A vector of indices specifying the rows in `data` to be used as the test set.
#' @param data_type Character string indicating the type of data. Can be "continuous" for regression, "binary" for binary classification, or "categorical" for multiclass classification.
#' @param num.trees Integer specifying the number of trees to grow in the random forest. Default is 500.
#' @param metricfunc Optional user-defined function to calculate a custom performance metric. This function should take the arguments `data`, `model`, and `test_indices`, and return a numeric value representing the performance metric.
#' @param ... Additional arguments passed to the `ranger` function.
#'
#' @importFrom ranger ranger
#' @importFrom stats predict
#' @importFrom caret confusionMatrix
#'
#' @return A numeric value representing the performance metric of the model on the test set.
#' @export

ranger_wrapper <- function(formula,
                           data,
                           train_indices,
                           test_indices,
                           data_type,
                           num.trees = 500,
                           metricfunc = NULL,
                           ...) {
  if (data_type %in% c("binary", "categorical")) {
    model <- ranger::ranger(formula, data = data[train_indices, ], num.trees, probability = TRUE, ...)
  } else if (data_type %in% "continuous") {
    model <- ranger::ranger(formula, data = data[train_indices, ], num.trees, probability = FALSE, ...)
  }

  predictions <- predict(model, data = data[test_indices, ])$predictions
  actual <- data[test_indices, ][[all.vars(formula)[1]]]

  if (!is.null(metricfunc)) {
    metric <- metricfunc(data, model, test_indices)
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

