#' Generate the Test Statistic or Null Distribution Using Permutation
#'
#' This function generates the test statistic or a null distribution through permutation for conditional independence testing.
#' It supports various machine learning methods, including random forests, extreme gradient boosting, and allows for custom metric functions and model fitting functions.
#'
#' @param formula Formula specifying the relationship between dependent and independent variables.
#' @param data Data frame. The data containing the variables used.
#' @param method Character. The modeling method to be used. Options include "xgboost" for gradient boosting, or "rf" for random forests or "svm" for Support Vector Machine.
#' @param metric Character. The type of metric: can be "RMSE", "Kappa" or "LogLoss". Default is 'RMSE'
#' @param nperm Integer. The number of generated Monte Carlo samples. Default is 160.
#' @param subsample Numeric. The proportion of the data to be used for subsampling. Default is 1 (no subsampling).
#' @param p Numeric. The proportion of the data to be used for training. The remaining data will be used for testing. Default is 0.5.
#' @param nrounds Integer. The number of rounds (trees) for methods like 'xgboost' and 'rf'. Default is 600.
#' @param mtry Integer. The number of variables to possibly split at in each node for method 'rf'. Default is the rounded down square root of numbers of columns in data.
#' @param nthread Integer. The number of threads to use for parallel processing. Only relevant for methods 'rf' and 'xgboost'. Default is 1.
#' @param permutation Logical. Whether to perform permutation of the 'X' variable. Used to generate a null distribution. Default is FALSE.
#' @param robust Logical. If TRUE, automatically performs stratified permutation if all conditional variables are factor or categorical. Default is TRUE.
#' @param metricfunc Function. A custom metric function provided by the user. It must take arguments: \code{actual}, \code{predictions}, and optionally \code{...}, and return a single numeric performance value.
#' @param mlfunc Function. A custom machine learning function provided by the user. The function must have the arguments: \code{formula}, \code{data}, \code{train_indices}, \code{test_indices}, and \code{...}, and return a single value performance metric. Default is NULL.
#' @param progress Logical. A logical value indicating whether to show a progress bar during when building the null distribution. Default is TRUE.
#' @param center Logical. If TRUE, the data is centered before model fitting. Default is TRUE.
#' @param scale. Logical. If TRUE, the data is scaled before model fitting. Default is TRUE.
#' @param k Integer. The number of nearest neighbors for the "KNN" method. Default is 15.
#' @param eps Numeric. A small value added to avoid division by zero. Only relevant for method 'KNN'. Default is 1e-15.
#' @param positive Character vector. Only relevant for method 'KNN'. Specifies which levels of a factor variable should be treated as positive class in classification tasks. Default is NULL.
#' @param kernel Character. Only relevant for method 'KNN'. Specifies the kernel type for method option "KNN" . Possible choices are "rectangular" (which is standard unweighted knn), "triangular", "epanechnikov" (or beta(2,2)), "biweight" (or beta(3,3)), "triweight" (or beta(4,4)), "cos", "inv", "gaussian" and "optimal". Default is "optimal".
#' @param distance Numeric. Parameter of Minkowski distance for the "KNN" method. Default is 2.
#' @param ... Additional arguments to pass to the machine learning wrapper functions \code{wrapper_xgboost}, \code{wrapper_ranger}, \code{wrapper_knn} and  \code{wrapper_svm}, or to a custom-built wrapper function.
#'
#' @return A list containing the test distribution.
#' @importFrom stats predict update as.formula
#' @importFrom caret createDataPartition confusionMatrix
#' @importFrom xgboost xgb.train xgb.DMatrix
#' @importFrom ranger ranger
#' @importFrom data.table :=
#' @importFrom utils flush.console combn
#' @importFrom dplyr mutate across all_of sym
#' @importFrom progress progress_bar
#' @export
#' @examples
#' set.seed(123)
#' data <- data.frame(x1 = rnorm(100),
#' x2 = rnorm(100),
#' x3 = rnorm(100),
#' x4 = rnorm(100),
#' y = rnorm(100))
#' result <- test.gen(formula = y ~ x1 | x2 + x3 + x4,
#'                    metric = "RMSE",
#'                    data = data)
#' hist(result$distribution)


test.gen <- function(formula,
                     data,
                     method = "rf",
                     metric = 'RMSE',
                     nperm = 160,
                     subsample = 1,
                     p = 0.5,
                     nrounds = 600,
                     mtry =  NULL,
                     nthread = 1,
                     permutation = FALSE,
                     robust = TRUE,
                     metricfunc = NULL,
                     mlfunc = NULL,
                     progress = TRUE,
                     center = TRUE,
                     scale. = TRUE,
                     eps = 1e-15,
                     k = 15,
                     positive = NULL,
                     kernel = "optimal",
                     distance = 2,
                     ...) {
  
  if (permutation && nperm < 10) {
    stop("nperm can't be less than 10")
  }

  # Prompt progress bar message
  pb_message <- if (permutation) {
    "Creating null distribution"
  } else {
    "Creating test statistic distribution"
  }

  if (progress) {
    pb <- progress::progress_bar$new(
      format = paste0(pb_message, " [:bar] :percent (:current/:total)"),
      total = nperm,
      clear = FALSE,
      width = 60
    )
  }
  
  # Scaling and centering data 
  x_names <- all.vars(formula)[-1]
  num_x <- x_names[sapply(data[, x_names, drop = FALSE],
                          function(z) is.numeric(z) && !is.factor(z))]
  if (isTRUE(center) || isTRUE(scale.)) {
    
    if (length(num_x) > 0L) {
      
      X_mat <- data[, num_x, drop = FALSE]
      
      if (isTRUE(center)) {
        cm <- colMeans(X_mat, na.rm = TRUE)
        X_mat <- sweep(X_mat, 2, cm, "-")
      }
      
      if (isTRUE(scale.)) {
        cs <- apply(X_mat, 2, sd, na.rm = TRUE)
        cs[cs == 0 | is.na(cs)] <- 1
        X_mat <- sweep(X_mat, 2, cs, "/")
      }
      
      data[, num_x] <- X_mat
    }
  }
  

  null <- matrix(NA, nrow = nperm, ncol = 1)

  for (iteration in 1:nperm) {
    if (subsample <= 0 || subsample > 1) {
      stop("Subsample must be between 0 and 1.")
    } else if (subsample < 1) {
      sub_data <- data[sample(nrow(data), size = round(nrow(data) * subsample)), ]
      N <- nrow(sub_data)
    } else  {
      sub_data <- data
      N <- nrow(sub_data)
    }

   
    # Permute X if generating null distribution
    Z <- all.vars(formula[[3]])[-1] # Use this for stratification
    X <- all.vars(formula[[3]])[1]
    if (permutation) {
      if (robust && is_categorical_Z_any(sub_data, Z)) { 
        # Permute within strata of Z 
        strata <- make_strata_from_categorical_Z(sub_data, Z)
        X_star <- permute_within_strata(sub_data$X, strata)
        # Replace X with permuted version
        resampled_data <- sub_data %>% dplyr::mutate(!!X := X_star)
      } else {
        resampled_data <- sub_data %>%
          dplyr::mutate(!!X := sample(.data[[X]], size = dplyr::n(), replace = FALSE))
      }
    } else {
      resampled_data <- sub_data
    }
    
    
    # Create training and testing indices
    Y <- all.vars(formula[[2]])
    if (metric %in% c("Kappa", "LogLoss")) {
      inTraining <- caret::createDataPartition(y = factor(sub_data[[Y]]), p = p, list = FALSE)
      train_indices <- inTraining
      test_indices <- setdiff(1:nrow(sub_data), inTraining)
    } else {
      inTraining <- sample(1:nrow(sub_data), size = floor(p * N), replace = FALSE)
      train_indices <- inTraining
      test_indices <- setdiff(1:nrow(sub_data), inTraining)
    }

    
    # Apply machine learning method
    null[iteration] <- tryCatch({
      if (!is.null(mlfunc)) {
        mlfunc(formula,
               data = resampled_data,
               train_indices,
               test_indices, ...)
      } else if (method == "xgboost") {
        wrapper_xgboost(
          formula,
          resampled_data,
          train_indices,
          test_indices,
          metric = metric,
          metricfunc = metricfunc,
          nrounds = nrounds,
          nthread = nthread,
          subsample = subsample,
          ...
        )
      } else if (method == "rf") {
        formula <- unclean_formula(formula)
        wrapper_ranger(
          formula,
          resampled_data,
          train_indices,
          test_indices,
          metric = metric,
          metricfunc = metricfunc,
          num.trees = nrounds,
          nthread = nthread,
          mtry = mtry,
          ...
        )
      } else if (method == "svm") {
        wrapper_svm(
          formula,
          resampled_data,
          train_indices,
          test_indices,
          metric = metric,
          metricfunc = metricfunc,
          ...
        )
      } else if (method == "KNN") {
        wrapper_knn(
          formula,
          resampled_data,
          train_indices,
          test_indices,
          metric = metric,
          metricfunc = metricfunc,
          k = k,
          eps = eps,
          positive = positive,
          kernel = kernel,
          distance = distance,
          ...
        )
      } else {
        stop("Method chosen is not supported: ", method)
      }
    }, error = function(e) {
      warning("Error in iteration ", iteration, ": ", conditionMessage(e))
      NA
    })
  if (progress) {
    pb$tick()
  }

  }


  return(list(distribution = null))
}
