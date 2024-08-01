#' Generate null distribution using permutation
#'
#' @param Y Dependent variable
#' @param X Independent variable to be permuted
#' @param Z Conditioning variables
#' @param data Data frame
#' @param data_type Type of data: "continuous", "binary", or "multinomial"
#' @param method Method for modeling: "lm", "xgboost", "ranger", etc.
#' @param nperm Number of permutations
#' @param p Proportion of data used for training
#' @param N Number of observations
#' @param poly Logical, whether to include polynomial terms
#' @param degree Degree of polynomial terms
#' @param nrounds Number of rounds (trees) for xgboost and ranger
#' @param lm_family Family for glm
#' @param ... Additional arguments to pass to the modeling wrapper function
#'
#' @return A list containing the null distribution and the model
#' @export

null.gen <- function(Y, X, Z, data, data_type = "continuous", method = "xgboost", nperm = 500, p = 0.825, N = nrow(data), poly = TRUE, degree = 3, nrounds = 120, lm_family ,...) {
  
  # Create formula for the prediction 
  if (poly == TRUE & degree < 1) {
    stop("Degree of 0 or less is not allowed")
  }
  # Setting 'poly == true' creates nth degree terms of conditional variables
  if (poly == TRUE & degree > 1){
    transformations <- lapply(2:degree, function(d) {
      eval(parse(text = paste0("~ .^", d)))
    })
    names(transformations) <- paste0("d_", 2:degree)
    
    data <- data %>%
      mutate(across(all_of(Z), transformations, .names = "{col}_{fn}"))
    
    new_terms <- unlist(lapply(Z, function(var) {
      sapply(2:degree, function(d) {
        paste0(var, "_d_", d)
      })
    }))
    formula <- as.formula(paste(Y, " ~ ", X, " + ", paste(Z, collapse = " + "), " + ",paste(new_terms, collapse = " + ")))
  } 
  else {
    formula <- as.formula(paste(Y, " ~ ", X, " + ", paste(Z, collapse = "+")))
  }
  
  # Create nperm number of Monte Carlo Cross Validation sets
  train_indices <- matrix(NA, nrow = nperm, ncol = round(p * N))
  test_indices <- matrix(NA, nrow = nperm, ncol = round((1-p) * N))
  
  for (i in 1:nperm) {
    if (data_type == "continuous") {
      inTraining <- sample(1:nrow(data), size = floor(p * N), replace = F)
      train_indices[i, ]  <- inTraining
      test_indices[i, ] <. -inTraining
    } else if (data_type %in% c('binary', 'categorical')) {
      inTraining <- caret::createDataPartition(y = factor(data[[Y]]), p = p, list = FALSE)
      train_indices[i, ]  <- inTraining
      test_indices[i, ] <. -inTraining
    }
  }
  # Initialize a matrix for storing of results
  null <- matrix(NA, nrow = nperm, ncol = 1)
  
  # If statement of all the ML methods one can use to do computational test 
  for (iteration in 1:nperm) {
    if (method == "lm" & data_type == "continuous") { # Parametric linear model with continous outcome 
      resampled_data <- data %>% mutate(!!X := sample(!!sym(X)))    
      null[iteration] <- glm_wrapper(formula, resampled_data, train_indices, test_indices, iteration, lm_family,  ...)
    } 
    else if (method == "lm" & data_type == "binary") { # Parametric model (logistic) with binary outcome
      resampled_data <- data %>% mutate(!!X := sample(!!sym(X)))    
      null[iteration] <- glm_wrapper(formula, resampled_data, train_indices, test_indices, iteration, lm_family, ...)
    } 
    else if (method == "lm" & data_type == "categorical") { # Parametric model (logistic) with categorical outcome
      resampled_data <- data %>% mutate(!!X := sample(!!sym(X)))    
      null[iteration] <-  multinom_wrapper(formula, resampled_data, train_indices, test_indices, iteration, ...)
    } else if (method == "xgboost" & data_type == "continuous") { # XGBoost with continous outcome
      
      resampled_data <- data %>% mutate(!!X := sample(!!sym(X)))    
      null[iteration] <- xgboost_wrapper(formula, resampled_data, train_indices, test_indices, iteration, nrounds, ...)
        
    } else if (method == "xgboost" & data_type == "binary") { # XGBoost with binary outcome
      resampled_data <- data %>% mutate(!!X := sample(!!sym(X)))    
      
      independent <- all.vars(formula)[-1]
      dependent <- update(formula, . ~ .)[[2]]
      
      training <- resampled_data[train_indices[iteration,],]
      testing <- resampled_data[-train_indices[iteration,],]
      
      if (any(sapply(training, is.factor))) {
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
      params <- list(...)
      
      model <- xgboost::xgb.train(data = train_matrix,
                                  objective = "binary:logistic",
                                  params = params,
                                  nrounds = nrounds,
                                  verbose = 0)
      
      predictions <- predict(model, test_matrix)
      pred_class <- ifelse(predictions > 0.5, 1, 0)
      conf_matrix <- try(caret::confusionMatrix(factor(pred_class, levels = levels(factor(test_label))), factor(test_label)), silent = TRUE)
      null[iteration] <- conf_matrix$overall[2]
    } else if (method == "xgboost" & data_type == "categorical") { # XGBoost with categorical outcome
      if (is.null(num_class)) {
        stop("num_class needs to be set.")
      }
      resampled_data <- data %>% mutate(!!X := sample(!!sym(X)))    
      
      independent <- all.vars(formula)[-1]
      dependent <- update(formula, . ~ .)[[2]]
      
      training <- resampled_data[train_indices[iteration,],]
      testing <- resampled_data[-train_indices[iteration,],]
      
      if (any(sapply(training, is.factor))) {
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
      params <- list(...)
      
      model <- xgboost::xgb.train(data = train_matrix,
                                  objective = "multi:softprob",
                                  params = params,
                                  nrounds = nrounds,
                                  num_class = num_class,
                                  verbose = 0)
      
      predictions <- predict(model, test_matrix)
      pred <- matrix(predictions, ncol=num_class, byrow=TRUE)
      pred_class <- max.col(pred) - 1
      conf_matrix <- try(caret::confusionMatrix(factor(pred_class, levels = levels(factor(test_label))), factor(test_label)), silent = TRUE)
      null[iteration] <- conf_matrix$overall[2]
    } else if (method == "RandomForest" & data_type == "continuous") { # Random Forest with continuous outcome
      resampled_data <- data %>% mutate(!!X := sample(!!sym(X)))    
      model <- ranger::ranger(formula, data = resampled_data[train_indices[iteration,],], num.trees = nrounds, ...)
      testing <- resampled_data[-train_indices[iteration,],]
      pred <- predict(model, data = testing)$predictions
      actual <- testing[[all.vars(formula)[1]]]
      null[iteration] <- sqrt(mean((pred - actual)^2))  
    } else if (method == "RandomForest" & data_type %in% c("binary", "categorical")) { # Random Forest with binary or categorical outcome
      resampled_data <- data %>% mutate(!!X := sample(!!sym(X)))    
      model <- ranger(formula, data = resampled_data[train_indices[iteration,],], probability = TRUE, num.trees = nrounds, ...)
      testing <- resampled_data[-train_indices[iteration,],]
      predictions <- predict(model, data = testing)$predictions
      actual <- testing[[all.vars(formula)[1]]]
      if (data_type == "binary") {
        pred_class <- ifelse(predictions[, 2] > 0.5, 1, 0)
        # Calculate Kappa score
        cm <- caret::confusionMatrix(factor(pred_class), factor(actual))
        null[iteration] <- cm$overall["Kappa"]   
      } else {
        pred_class <- apply(predictions, 1, which.max)
        # Calculate Kappa score
        cm <- caret::confusionMatrix(factor(pred_class), factor(actual))
        null[iteration] <- cm$overall["Kappa"]  
      } 
    } else {
      stop("Method choosen is not supported by the null.gen() function")
    }
    # Calculate the percentage finished
    percentage <- (iteration / nperm) * 100
    # Print the progress
    cat(sprintf("Creating null distribution: %d%% complete\r", round(percentage)))
    flush.console()
  }
  # Naming the result in the null matrix
  if (data_type == "continuous") {
    colnames(null) <- "RMSE"  
  }
  else {
    colnames(null) <- "Kappa score"
  } 
  
  null_object <- list(distribution  = null,
                      MLmodel  = model)
  
  return(null_object)
}


