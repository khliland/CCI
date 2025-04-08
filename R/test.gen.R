#' Generate the Test Statistic or Null Distribution Using Permutation
#'
#' This function generates the test statistic or a null distribution through permutation for conditional independence testing.
#' It supports various machine learning methods, including linear models, random forests, and gradient boosting, and allows for custom metric functions and model fitting functions.
#'
#' @param Y Character. The name of the dependent (response) variable in the data.
#' @param X Character. The name of the independent (predictor) variable to be permuted.
#' @param Z Character vector. The names of the conditioning variables to be included in the model.
#' @param data Data frame. The data containing the variables used in the analysis.
#' @param data_type Character. The type of data of the Y parameter: can be "continuous", "binary", or "multinomial".
#' @param method Character. The modeling method to be used. Options include "lm" for linear models, "xgboost" for gradient boosting, or "rf" for random forests.
#' @param nperm Integer. The number of generated samples or permutations. Default is 100.
#' @param p Numeric. The proportion of the data to be used for training. The remaining data will be used for testing. Default is 0.85.
#' @param N Integer. The total number of observations in the data. Default is the number of rows in the data frame.
#' @param poly Logical. Whether to include polynomial terms of the conditioning variables. Default is TRUE.
#' @param degree Integer. The degree of polynomial terms to be included if \code{poly} is TRUE. Default is 3.
#' @param nrounds Integer. The number of rounds (trees) for methods like xgboost and ranger. Default is 120.
#' @param family Family object. The family parameter for generalized linear models (e.g., \code{gaussian()} for linear regression).
#' @param num_class Integer. The number of classes for multinomial classification (used in xgboost with \code{objective = "multi:softprob"}). Default is NULL.
#' @param nthread Integer. The number of threads to use for parallel processing. Default is 1.
#' @param permutation Logical. Whether to perform permutation to generate a null distribution. Default is FALSE.
#' @param metricfunc Function. A custom metric function provided by the user. The function must take arguments: \code{data}, \code{model}, \code{test_indices}, and \code{test_matrix}, and return a single value performance metric. Default is NULL.
#' @param mlfunc Function. A custom machine learning function provided by the user. The function must have the arguments: \code{formula}, \code{data}, \code{train_indices}, \code{test_indices}, and \code{...}, and return a single value performance metric. Default is NULL.
#' @param ... Additional arguments to pass to the machine learning wrapper functions \code{glm_wrapper}, \code{multinom_wrapper}, \code{xgboost_wrapper}, \code{ranger_wrapper}, or to a custom-built wrapper function.
#'
#' @return A list containing the test distribution.
#' @importFrom stats glm predict update as.formula
#' @importFrom caret createDataPartition confusionMatrix
#' @importFrom nnet multinom
#' @importFrom xgboost xgb.train xgb.DMatrix
#' @importFrom ranger ranger
#' @importFrom data.table :=
#' @importFrom utils flush.console
#' @importFrom dplyr mutate across all_of
#' @export
#' @examples
#' set.seed(123)
#' data <- data.frame(x1 = rnorm(100),
#' x2 = rnorm(100),
#' x3 = rnorm(100),
#' x4 = rnorm(100),
#' y = rnorm(100))
#' result <- test.gen(Y = "y", X = "x1", Z = c("x2", "x3", "x4"), data = data)

test.gen <- function(Y,
                     X,
                     Z,
                     data,
                     data_type = "continuous",
                     method = "rf",
                     nperm = 100,
                     p = 0.85,
                     N = nrow(data),
                     poly = TRUE,
                     degree = 3,
                     nrounds = 120,
                     family,
                     num_class = NULL,
                     nthread = 1,
                     permutation = FALSE,
                     metricfunc = NULL,
                     mlfunc = NULL,
                     ...) {

  if (permutation && nperm < 10) {
    stop("nperm can't be less than 10")
  }
  if (poly && degree < 1) {
    stop("Degree of 0 or less is not allowed")
  }
  if (method %in% "xgboost" && data_type %in% "categorical" && !exists("num_class")) {
    stop("num_class needs to be set.")
  }

  if (any(sapply(data[Z], is.factor))) {
    warning("Polynomial terms are not supported for categorical variables. Polynomial terms will not be included.")
    poly <- FALSE
  }

  if (poly && degree > 1){
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

  null <- matrix(NA, nrow = nperm, ncol = 1)

  # If statement of all the ML methods one can use to do computational test
  for (iteration in 1:nperm) {
    if (data_type %in% c("continuous", "custom")) {
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
      data_type <- "custom"
      null[iteration] <- mlfunc(formula, data = resampled_data, train_indices, test_indices, ...)
    } else if (method %in% "lm" & data_type %in% c("continuous", "binary"))  { # Parametric linear model
      null[iteration] <- wrapper_glm(formula,
                                     resampled_data,
                                     train_indices,
                                     test_indices,
                                     family,
                                     data_type,
                                     metricfunc = metricfunc,
                                     ...)

    } else if (method %in% "lm" & data_type %in% "categorical") { # Parametric model (logistic) with categorical outcome
      null[iteration] <-  wrapper_multinom(formula,
                                           resampled_data,
                                           train_indices,
                                           test_indices,
                                           metricfunc = metricfunc,
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
      null[iteration] <- wrapper_xgboost(formula,
                                         resampled_data,
                                         train_indices,
                                         test_indices,
                                         data_type,
                                         nrounds = nrounds,
                                         num_class,
                                         metricfunc = metricfunc,
                                         ...)
    }
    else if (method %in% "rf") { # Random Forest with continuous outcome
      null[iteration] <- wrapper_ranger(formula,
                                        resampled_data,
                                        train_indices,
                                        test_indices,
                                        data_type,
                                        num.trees = nrounds,
                                        metricfunc = metricfunc,
                                        ...)
    } else if (method %in% "svm") {
      null[iteration] <- wrapper_svm(formula,
                                     resampled_data,
                                     train_indices,
                                     test_indices,
                                     data_type,
                                     metricfunc = metricfunc,
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
