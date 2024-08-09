#' Generate the test statistic or null distribution using permutation
#'
#' @param Y Dependent variable
#' @param X Independent variable to be permuted
#' @param Z Conditioning variables
#' @param data Data frame
#' @param data_type Type of data: "continuous", "binary", or "multinomial"
#' @param method Method for modeling: "lm", "xgboost", "rf"
#' @param nperm Number of generated samples
#' @param p Proportion of data used for training
#' @param N Number of observations
#' @param poly Logical, whether to include polynomial terms
#' @param degree Degree of polynomial terms
#' @param nrounds Number of rounds (trees) for xgboost and ranger
#' @param family Family for glm
#' @param objective Objective function for xgboost
#' @param probability Logical, whether the ranger_wrapper should do classification
#' @param permutation Logical, whether the perform permutation to generat a null distribution
#' @param mlfunc custom ML function provided by the user, the function must have the arguments: formula, resampled_data, test_indicies
#' @importFrom stats glm predict update as.formula
#' @importFrom caret createDataPartition confusionMatrix
#' @importFrom dplyr mutate across all_of sym
#' @importFrom nnet multinom
#' @importFrom xgboost xgb.train xgb.DMatrix
#' @importFrom ranger ranger
#' @return A list containing the test distribution
#' @export
test.gen <- function(Y,
                     X,
                     Z,
                     data,
                     data_type = "continuous",
                     method = "rf",
                     nperm = 100,
                     p,
                     N = nrow(data),
                     poly = TRUE,
                     degree = 3,
                     nrounds = 120,
                     family,
                     objective = "reg:squarederror",
                     probability = FALSE,
                     permutation = FALSE,
                     mlfunc = NULL,
                     ...) {

  if (poly & degree < 1) {
    stop("Degree of 0 or less is not allowed")
  }
  if (method %in% "xgboost" & data_type %in% "categorical" & !exists("num_class")) {
    stop("num_class needs to be set.")
  }

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

  # Initialize a matrix for storing results
  null <- matrix(NA, nrow = nperm, ncol = 1)

  # If statement of all the ML methods one can use to do computational test
  for (iteration in 1:nperm) {
    if (data_type %in% "continuous") {
      inTraining <- sample(1:nrow(data), size = floor(p * N), replace = FALSE)
      train_indices  <- inTraining
      test_indices <- setdiff(1:nrow(data), inTraining)
    } else if (data_type %in% c('binary', 'categorical')) {
      inTraining <- caret::createDataPartition(y = factor(data[[Y]]), p = p, list = FALSE)
      train_indices  <- inTraining
      test_indices <- setdiff(1:nrow(data), inTraining)
    }

    resampled_data <- data
    if (permutation) {
      resampled_data <- data %>% mutate(!!X := sample(!!sym(X)))
    }

    if (!is.null(mlfunc)) {
      null[iteration] <- mlfunc(formula, resampled_data, train_indices, test_indices, ...)
    } else if (method %in% "lm" & data_type %in% "continuous")  { # Parametric linear model
      null[iteration] <- glm_wrapper(formula,
                                     resampled_data,
                                     train_indices,
                                     test_indices,
                                     family,
                                     data_type,
                                     ...)
    } else if (method %in% "lm" & data_type %in% "binary"){
      null[iteration] <- glm_wrapper(formula,
                                     resampled_data,
                                     train_indices,
                                     test_indices,
                                     family,
                                     data_type,
                                     ...)
    } else if (method %in% "lm" & data_type %in% "categorical") { # Parametric model (logistic) with categorical outcome
      null[iteration] <-  multinom_wrapper(formula,
                                           resampled_data,
                                           train_indices,
                                           test_indices,
                                           ...)
    }
    else if (method %in% "xgboost") {
      if (data_type %in% c("binary")) {
        objective <-  "binary:logistic"
      } else if (data_type %in% c("categorical")) {
        objective <- "multi:softprob"
      } else {
        objective <- "reg:squarederror"
      }
      null[iteration] <- xgboost_wrapper(formula,
                                         resampled_data,
                                         train_indices,
                                         test_indices,
                                         nrounds,
                                         objective,
                                         ...)
    }
    else if (method %in% "rf") { # Random Forest with continuous outcome
      null[iteration] <- ranger_wrapper(formula,
                                        resampled_data,
                                        train_indices,
                                        test_indices,
                                        num.trees = nrounds,
                                        probability,
                                        ...)
    } else {
      stop("Method chosen is not supported by the test.gen() function")
    }

    percentage <- (iteration / nperm) * 100

    if (permutation == TRUE) {
      cat(sprintf("Creating null distribution: %d%% complete\r", round(percentage)))
      flush.console()
    } else if (permutation == FALSE){
      cat(sprintf("Creating test distribution: %d%% complete\r", round(percentage)))
      flush.console()
    }

  }


  null_object <- list(distribution  = null)
  return(null_object)
}
