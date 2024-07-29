lmNullGen <- function(Y, 
               X, 
               Z, 
               data,
               p,
               data_type,
               eps = 1e-15,
               ...){
  # data type either "continous", "binary", "categorical"
  if (is.null(data)) {
    stop("Please provide some data")
  }
  if (is.null(p)) {
    stop("Please provide the parameter p (size of training set)")
  }
  if (is.null(Y) || is.null(X)){
    stop("X or Y is missing.")
  }
  if (data_type %in% c('binary', 'categorical')){
    all_levels <- unique(data[[Y]])
  }
  data[X[[1]]] <- sample(data[[X[1]]]) #Shuffling the X variable 
  
  if (data_type %in% "continous") {
    inTraining <- sample(1:nrow(data), size = floor(p * nrow(data)))
    training <- data[inTraining, ]
    testing <- data[-inTraining, ]
  } else if (data_type %in% c('binary', 'categorical')) {
    inTraining <- caret::createDataPartition(y = factor(data[[Y]]), p = p, list = FALSE)
    training <- data[inTraining, ]
    testing <- data[-inTraining, ]
  }
  
  if (data_type %in% "continous") {
    model <- lm(formula, data = training)
    predictions <- predict(model, newdata = testing)
    metric1 <- Metrics::rmse(test_label, predictions)
    metric2 <- Metrics::mse(test_label, predictions)
  } else if (data_type %in% "binary") {
    model <- stats::glm(formula, data = training, family = binomial)
    predictions <- predict(model, newdata = testing, type = "response")
    log_loss <- -mean(test_label*log(predictions + eps) + (1 - test_label) * log(1 - predictions + eps))
    metric1 <- log_loss
    pred_class <- ifelse(predictions > 0.5, 1, 0)
    conf_matrix <- try(caret::confusionMatrix(factor(pred_class, levels = levels(factor(test_label))), factor(test_label)), silent = TRUE)
    if (inherits(conf_matrix, "try-error")) {
      metric2 <- NA
    } else {
      metric2 <- conf_matrix$overall[2]
    }
  } else {
    stop("Unsupported objective for linear regression.")
  }
}