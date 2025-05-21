#' Generate the Test Statistic or Null Distribution Using Permutation
#'
#' This function generates the test statistic or a null distribution through permutation for conditional independence testing.
#' It supports various machine learning methods, including random forests, extreme gradient boosting, and allows for custom metric functions and model fitting functions.
#'
#' @param formula Formula specifying the relationship between dependent and independent variables.
#' @param data Data frame. The data containing the variables used.
#' @param data_type Character. The type of data of the Y parameter: can be "continuous", "binary", or "categorical".
#' @param method Character. The modeling method to be used. Options include "xgboost" for gradient boosting, or "rf" for random forests or '"svm" for Support Vector Machine.
#' @param nperm Integer. The number of generated Monte Carlo samples. Default is 60.
#' @param p Numeric. The proportion of the data to be used for training. The remaining data will be used for testing. Default is 0.8.
#' @param N Integer. The total number of observations in the data. Default is the number of rows in the data frame.
#' @param poly Logical. Whether to include polynomial terms of the conditioning variables. Default is TRUE.
#' @param interaction Logical. Whether to include interaction terms of the conditioning variables. Default is TRUE.
#' @param degree Integer. The degree of polynomial terms to be included if \code{poly} is TRUE. Default is 3.
#' @param nrounds Integer. The number of rounds (trees) for methods like xgboost, ranger, and lightgbm. Default is 500.
#' @param num_class Integer. The number of classes for categorical data (used in xgboost and lightgbm). Default is NULL.
#' @param nthread Integer. The number of threads to use for parallel processing. Default is 1.
#' @param permutation Logical. Whether to perform permutation to generate a null distribution. Default is FALSE.
#' @param metricfunc Function. A custom metric function provided by the user. The function must take arguments: \code{data}, \code{model}, \code{test_indices}, and \code{test_matrix}, and return a single value performance metric. Default is NULL.
#' @param mlfunc Function. A custom machine learning function provided by the user. The function must have the arguments: \code{formula}, \code{data}, \code{train_indices}, \code{test_indices}, and \code{...}, and return a single value performance metric. Default is NULL.
#' @param ... Additional arguments to pass to the machine learning wrapper functions \code{xgboost_wrapper}, \code{ranger_wrapper}, \code{lightgbm_wrapper}, or to a custom-built wrapper function.
#'
#' @return A list containing the test distribution.
#' @importFrom stats predict update as.formula
#' @importFrom caret createDataPartition confusionMatrix
#' @importFrom xgboost xgb.train xgb.DMatrix
#' @importFrom ranger ranger
#' @importFrom data.table :=
#' @importFrom utils flush.console
#' @importFrom dplyr mutate across all_of sym
#' @importFrom utils combn
#' @export
#' @examples
#' set.seed(123)
#' data <- data.frame(x1 = rnorm(100),
#' x2 = rnorm(100),
#' x3 = rnorm(100),
#' x4 = rnorm(100),
#' y = rnorm(100))
#' result <- test.gen(formula = y ~ x1 | x2 + x3 + x4, data = data, method = "rf", degree = 4)
#' result <- test.gen(formula = Y ~ X | Z1 + Z2,
#'                    data = data,
#'                    nperm = 40,
#'                    method = "xgboost",
#'                    data_type = "binary",
#'                    permutation = FALSE,
#'                    poly = TRUE,
#'                    degree = 4)
test.gen <- function(formula,
                     data,
                     data_type = "continuous",
                     method = "rf",
                     nperm = 60,
                     p = 0.8,
                     N = nrow(data),
                     poly = TRUE,
                     interaction = TRUE,
                     degree = 3,
                     nrounds = 500,
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
  if (method == "xgboost" && data_type == "categorical" && is.null(num_class)) {
    stop("num_class needs to be set for xgboost with categorical data")
  }
  if (method == "lightgbm" && data_type == "categorical" && is.null(num_class)) {
    stop("num_class needs to be set for lightgbm with categorical data")
  }

  # Parse formula
  Y <- all.vars(formula)[1]
  X <- all.vars(formula[[3]])[1]
  Z <- all.vars(formula[[3]])[-1]
  if (length(Z) == 0) {
    Z <- NULL
  }
  if (!is.null(Z) && any(sapply(data[Z], is.factor))) {
    warning("Polynomial terms are not supported for categorical variables. Polynomial terms will not be included.")
    poly <- FALSE
  }

  # Add polynomial and interaction terms
  poly_result <- add_poly_terms(data, Z, degree = degree, poly = poly)
  data <- poly_result$data
  poly_terms <- poly_result$new_terms

  if (interaction && !is.null(Z)) {
    interaction_result <- add_interaction_terms(data, Z)
    data <- interaction_result$data
    interaction_terms <- interaction_result$interaction_terms
  } else {
    interaction_terms <- NULL
  }

  formula <- build_formula(formula, poly_terms, interaction_terms)

  null <- matrix(NA, nrow = nperm, ncol = 1)

  for (iteration in 1:nperm) {
    # Split data
    if (data_type %in% c("continuous", "custom")) {
      inTraining <- sample(1:nrow(data), size = floor(p * N), replace = FALSE)
      train_indices <- inTraining
      test_indices <- setdiff(1:nrow(data), inTraining)
    } else if (data_type %in% c("binary", "categorical")) {
      inTraining <- caret::createDataPartition(y = factor(data[[Y]]), p = p, list = FALSE)
      train_indices <- inTraining
      test_indices <- setdiff(1:nrow(data), inTraining)
    }

    resampled_data <- data
    if (permutation) {
      resampled_data <- data %>% mutate(!!X := sample(!!sym(X)))
    }

    # Apply machine learning method
    null[iteration] <- tryCatch({
      if (!is.null(mlfunc)) {
        mlfunc(formula, data = resampled_data, train_indices, test_indices, data_type = data_type, num_class = num_class, ...)
      } else if (method == "xgboost") {
        if (!"objective" %in% names(list(...))) {
          objective <- switch(data_type,
                              binary = "binary:logistic",
                              categorical = "multi:softprob",
                              continuous = "reg:squarederror")
        } else {
          objective <- list(...)$objective
        }
        wrapper_xgboost(
          formula,
          resampled_data,
          train_indices,
          test_indices,
          data_type = data_type,
          num_class = num_class,
          metricfunc = metricfunc,
          nrounds = nrounds,
          objective = objective,
          ...
        )
      } else if (method == "rf") {
        wrapper_ranger(
          formula,
          resampled_data,
          train_indices,
          test_indices,
          data_type = data_type,
          metricfunc = metricfunc,
          num.trees = nrounds,
          ...
        )
      } else if (method == "svm") {
        wrapper_svm(
          formula,
          resampled_data,
          train_indices,
          test_indices,
          data_type = data_type,
          metricfunc = metricfunc,
          ...
        )
      } else {
        stop("Method chosen is not supported: ", method)
      }
    }, error = function(e) {
      warning("Error in iteration ", iteration, ": ", conditionMessage(e))
      NA
    })

    step <- max(1, ceiling(nperm / 100))
    if (iteration %% step == 0 && permutation) {
      percentage <- (iteration / nperm) * 100
      cat(sprintf("%s: %d%% complete\r", "Creating null distribution", round(percentage)))
      flush.console()
    } else if (iteration %% step == 0 && !permutation && nperm > 9) {
      percentage <- (iteration / nperm) * 100
      cat(sprintf("%s: %d%% complete\r", "Creating test statistic distribution", round(percentage)))
      flush.console()
    }
  }


  null_object <- list(distribution = null)
  return(null_object)
}
