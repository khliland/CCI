#' Generate null distribution using permutation
#'
#' @param Y Dependent variable
#' @param X Independent variable to be permuted
#' @param Z Conditioning variables
#' @param data Data frame
#' @param data_type Type of data: "continuous", "binary", or "multinomial"
#' @param method Method for modeling: "lm", "xgboost", "rf"
#' @param nperm Number of permutations
#' @param p Proportion of data used for training
#' @param N Number of observations
#' @param poly Logical, whether to include polynomial terms
#' @param degree Degree of polynomial terms
#' @param nrounds Number of rounds (trees) for xgboost and ranger
#' @param lm_family Family for glm
#' @param objective Objective function for xgboost
#' @importFrom stats glm predict update as.formula
#' @importFrom caret createDataPartition confusionMatrix
#' @importFrom dplyr mutate across all_of sym
#' @importFrom nnet multinom
#' @importFrom xgboost xgb.train xgb.DMatrix
#' @importFrom ranger ranger
#' @return A list containing the null distribution and the model
#' @export

null.gen <- function(Y, 
                     X, 
                     Z, 
                     data, 
                     data_type = "continuous", 
                     method = "rf",
                     nperm = 100, 
                     p = 0.825, 
                     N = nrow(data), 
                     poly = TRUE, 
                     degree = 3, 
                     nrounds = 120,
                     lm_family,
                     objective = "reg:squarederror",
                     probability = FALSE,
                     ...) {
  
  # Create formula for the prediction 
  if (poly & degree < 1) {
    stop("Degree of 0 or less is not allowed")
  }
  if (method %in% "xgboost" & data_type %in% "categorical" & !exists("num_class")) {
    stop("num_class needs to be set.")
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
  } else {
    formula <- as.formula(paste(Y, " ~ ", X, " + ", paste(Z, collapse = "+")))
  }
  
  # Initialize a matrix for storing of results
  null <- matrix(NA, nrow = nperm, ncol = 1)
  
  # If statement of all the ML methods one can use to do computational test 
  for (iteration in 1:nperm) {
    if (data_type %in% "continuous") {
      inTraining <- sample(1:nrow(data), size = floor(p * N), replace = F)
      train_indices  <- inTraining
      test_indices <- setdiff(1:nrow(data), inTraining)
    } else if (data_type %in% c('binary', 'categorical')) {
      inTraining <- caret::createDataPartition(y = factor(data[[Y]]), p = p, list = FALSE)
      train_indices  <- inTraining
      test_indices <- setdiff(1:nrow(data), inTraining)
    }
    if (method %in% "lm" & data_type %in% "continuous")  { # Parametric linear model  
      resampled_data <- data %>% mutate(!!X := sample(!!sym(X)))    
      null[iteration] <- glm_wrapper(formula, 
                                     resampled_data, 
                                     train_indices, 
                                     test_indices, 
                                     lm_family,
                                     data_type,
                                     ...)
    } else if (method %in% "lm" & data_type %in% "binary"){
      resampled_data <- data %>% mutate(!!X := sample(!!sym(X)))    
      null[iteration] <- glm_wrapper(formula, 
                                     resampled_data, 
                                     train_indices, 
                                     test_indices, 
                                     lm_family,  
                                     data_type,
                                     ...)
    } else if (method %in% "lm" & data_type %in% "categorical") { # Parametric model (logistic) with categorical outcome
      resampled_data <- data %>% mutate(!!X := sample(!!sym(X)))    
      null[iteration] <-  multinom_wrapper(formula, 
                                           resampled_data, 
                                           train_indices, 
                                           test_indices,
                                           ...)
    } 
    else if (method %in% "xgboost") {
      resampled_data <- data %>% mutate(!!X := sample(!!sym(X)))
      if (data_type %in% c("binary")) {
        objective <-  "binary:logistic"
      } else if (data_type %in% c("categorical")) {
        objective <- "multi:softprob"
      } else {
        objective <- "reg:squarederror"
      }
      null[iteration] <- xgboost_wrapper(formula, 
                                         data, 
                                         train_indices, 
                                         test_indices,
                                         nrounds, 
                                         objective,
                                         ...)
    } 
    else if (method %in% "rf") { # Random Forest with continuous outcome
      resampled_data <- data %>% mutate(!!X := sample(!!sym(X)))
      null[iteration] <- ranger_wrapper(formula, 
                                        resampled_data, 
                                        train_indices, 
                                        test_indices, 
                                        num.trees = nrounds, 
                                        probability,
                                        ...)
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
  if (data_type %in% "continuous") {
    colnames(null) <- "RMSE"
  } else {
    colnames(null) <- "Kappa score"
  }

  null_object <- list(distribution  = null)
  
  return(null_object)
}


