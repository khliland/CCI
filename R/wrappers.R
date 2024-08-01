#' Wrapper function for GLM model training and evaluation
#' 
#' @param formula Model formula
#' @param data Data frame
#' @param train_indices Indices for training data
#' @param test_indices Indices for testing data
#' @param iteration Current iteration index
#' @param lm_family Family for GLM
#' @param data_type Type of data (continuous or binary)
#' @param ... Additional arguments passed to glm
#' 
#' @return Performance metric (RMSE for continuous, Kappa for binary)
#' @export
glm_wrapper <- function(formula, data, train_indices, test_indices, iteration, lm_family, ...) {
  model <- glm(formula = formula, data = data, family = lm_family, subset = train_indices[iteration,], ...)
  if (data_type == "continuous") {
    pred <- predict.glm(model, newdata = data[test_indices[iteration,],])
    actual <- data[test_indices[iteration,],][[all.vars(formula)[1]]]  
    metric <- sqrt(mean((pred - actual)^2))
  } else if (data_type == "binary") {
    pred <- predict.glm(model, newdata = data[test_indices[iteration,],], type = "response")
    actual <- data[test_indices[iteration,],][[all.vars(formula)[1]]]
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
multinom_wrapper <- function(formula, data, train_indices, test_indices, iteration, ...) {
  model <- nnet::multinom(formula, data = data, subset = train_indices[iteration,], ...)
  pred <- predict(model, newdata = data[test_indices[iteration,],])
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
#' @param ... Additional arguments passed to xgb.train
#' 
#' @return Performance metric (RMSE for continuous, Kappa for binary, Kappa for categorical)
#' @export
xgboost_wrapper <- function(formula, 
                            data, 
                            train_indices, 
                            test_indices, 
                            iteration, 
                            nrounds, 
                            objective = c("reg:squarederror", "binary:logistic", "multi:softprob"), 
                            ...) {
  independent <- all.vars(formula)[-1]
  dependent <- update(formula, . ~ .)[[2]]
  training <- data[train_indices[iteration,],]
  testing <- data[test_indices[iteration,],]

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
  model <- xgboost::xgb.train(data = train_matrix,
                              objective = objective,
                              nrounds = nrounds,
                              verbose = 0,
                              ...)
  pred <- predict(model, newdata = test_matrix)
  if (objective == "reg:squarederror") 
  {
    
  } else if (objective == "binary:logistic")
  pred_class <- ifelse(predictions > 0.5, 1, 0)
  conf_matrix <- try(caret::confusionMatrix(factor(pred_class, levels = levels(factor(test_label))), factor(test_label)), silent = TRUE)
  metric <- conf_matrix$overall[2]
}

