#' Wrapper function for GLM model training and evaluation
#'
#' @param formula Model formula
#' @param data Data frame
#' @param train_indices Indices for training data
#' @param test_indices Indices for testing data
#' @param iteration Current iteration index
#' @param family Family for GLM
#' @param data_type Type of data (continuous or binary)
#' @param ... Additional arguments passed to glm
#'
#' @return Performance metric (RMSE for continuous, Kappa for binary)
#' @export
glm_wrapper <- function(formula,
                        data,
                        train_indices,
                        test_indices,
                        family,
                        data_type,
                        ...) {
  model <- stats::glm(formula = formula, data = data, family = family, subset = train_indices, ...)
  if (data_type %in% "continuous") {
    pred <- predict.glm(model, newdata = data[test_indices,])
    actual <- data[test_indices,][[all.vars(formula)[1]]]
    metric <- sqrt(mean((pred - actual)^2))
  } else if (data_type %in% "binary") {
    pred <- predict.glm(model, newdata = data[test_indices,], type = "response")
    actual <- data[test_indices, ][[all.vars(formula)[1]]]
    pred_class <- ifelse(pred > 0.5, 1, 0)
    cm <- caret::confusionMatrix(factor(pred_class), factor(actual))
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
#' @param iteration Current iteration index
#' @param ... Additional arguments passed to multinom
#'
#' @return Performance metric (Kappa for binary)
#' @export
multinom_wrapper <- function(formula,
                             data,
                             train_indices,
                             test_indices,
                             ...) {
  model <- nnet::multinom(formula, data = data, subset = train_indices, ...)
  pred <- predict(model, newdata = data[test_indices,])
  actual <- data[test_indices[iteration,],][[all.vars(formula)[1]]]
  cm <- caret::confusionMatrix(factor(pred), factor(actual))
  metric <- cm$overall["Kappa"]
  return(metric)
}

# Wrapper function for XGBoost model training and evaluation
#'
#' @param formula Model formula
#' @param data Data frame
#' @param train_indices Indices for training data
#' @param test_indices Indices for training data
#' @param iteration Current iteration index
#' @param nrounds Number of boosting rounds
#' @param objective Objective function for XGBoost
#' @param ... Additional arguments passed to xgb.train
#'
#' @return Performance metric (RMSE for continuous, Kappa for binary, Kappa for categorical)
#' @export
xgboost_wrapper <- function(formula,
                            data,
                            train_indices,
                            test_indices,
                            nrounds,
                            objective,
                            ...) {
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
  params <- list(objective = objective,
                 ...)
  model <- xgboost::xgb.train(data = train_matrix,
                              params = params,
                              nrounds = nrounds,
                              verbose = 0)
  pred <- predict(model, newdata = test_matrix)
  if (objective %in% "reg:squarederror") {
    actual <- testing[[dependent]]
    metric <- sqrt(mean((pred - actual)^2))
  } else if (objective %in% "binary:logistic") {
    pred_class <- ifelse(pred > 0.5, 1, 0)
    conf_matrix <- try(caret::confusionMatrix(factor(pred_class, levels = levels(factor(test_label))), factor(test_label)), silent = TRUE)
    metric <- conf_matrix$overall[2]
  } else if (objective %in% "multi:softprob") {
    pred <- matrix(pred, ncol=num_class, byrow=TRUE)
    pred_class <- max.col(pred) - 1
    conf_matrix <- try(caret::confusionMatrix(factor(pred_class, levels = levels(factor(test_label))), factor(test_label)), silent = TRUE)
    metric <- conf_matrix$overall[2]
  } else {
    stop("Objective function for XGBoost is not supported by perm.test()")
  }
  return(metric)
}

# Wrapper function for Ranger model training and evaluation
#'
#' @param formula Model formula
#' @param data Data frame
#' @param train_indices Indices for training data
#' @param test_indices Indices for training data
#' @param iteration Current iteration index
#' @param num.trees Number of boosting rounds
#' @param probability Grow a probability forest for binary or categorical outcomes
#' @param ... Additional arguments passed to ranger
#'
#' @return Performance metric (RMSE for continuous, Kappa for binary, Kappa for categorical)
#' @export
ranger_wrapper <- function(formula,
                           data,
                           train_indices,
                           test_indices,
                           probability = FALSE,
                           num.trees,
                           ...) {
  training <- data[train_indices, ]
  testing <- data[test_indices, ]
  model <- ranger::ranger(formula, data = training, num.trees, probability = probability, ...)

  predictions <- predict(model, data = testing)$predictions
  actual <- testing[[all.vars(formula)[1]]]

  if (probability) {
    pred_class <- ifelse(predictions[, 2] > 0.5, 1, 0)
    cm <- caret::confusionMatrix(factor(pred_class), factor(actual))
    metric <- cm$overall["Kappa"]
  } else {

    metric <- sqrt(mean((predictions - actual)^2))
  }

  return(metric)
}

