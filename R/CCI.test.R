#' Computational test for conditional independence based on ML and Monte Carlo Cross Validation
#'
#' The `CCI.test` function performs a conditional independence test using a specified machine learning model or a custom model provided by the user. It calculates the test statistic, generates a null distribution via permutations, computes p-values, and optionally generates a plot of the null distribution with the observed test statistic.
#' The 'CCI.test' function serves as a wrapper around the 'perm.test' function
#'
#' @param formula Model formula specifying the relationship between dependent and independent variables. (Ex: Y ~ X | Z1 + Z2 for Y _||_ X | Z1, Z2)  
#' @param data A data frame containing the variables specified in the formula.
#' @param p Numeric. Proportion of data used for training the model. Default is 0.5.
#' @param nperm Integer. The number of permutations to perform. Default is 60.
#' @param mtry Number of variables to possibly split at in each node for method 'rf'. Default is NULL (sqrt of number of variables).
#' @param nrounds Integer. The number of rounds (trees) for methods 'xgboost' and 'rf' Default is 600.
#' @param metric Character. Specifies the type of data: "Auto", "RMSE" or "Kappa". Default is "Auto".
#' @param choose_direction Logical. If TRUE, the function will choose the best direction for testing. Default is FALSE.
#' @param method Character. Specifies the machine learning method to use. Supported methods are random forest "rf", extreme gradient boosting "xgboost", support vector machine 'svm' and K-nearest neighbour 'KNN'. Default is "rf".
#' @param poly Logical. If TRUE, polynomial terms of the conditional variables are included in the model. Default is TRUE.
#' @param degree Integer. The degree of polynomial terms to include if poly is TRUE. Default is 3.
#' @param subsample Character. Specifies whether to use automatic subsampling based on sample size ("Auto"), user-defined subsampling ("Yes"), or no subsampling ("No"). Default is "Auto"
#' @param subsample_set Numeric. If `subsample` is set to "Yes", this parameter defines the proportion of data to use for subsampling. Default is NA.
#' @param robust Logical. If TRUE, uses a robust method for permutation. Default is TRUE.
#' @param min_child_weight Numeric. The minimum sum of instance weight (hessian) needed in a child for methods like xgboost. Default is 1.
#' @param colsample_bytree Numeric. The subsample ratio of columns when constructing each tree for methods like xgboost. Default is 1.
#' @param eta Numeric. The learning rate for methods like xgboost. Default is 0.3.
#' @param gamma Numeric. The minimum loss reduction required to make a further partition on a leaf node of the tree for methods like xgboost. Default is 0.
#' @param max_depth Integer. The maximum depth of the trees for methods like xgboost. Default is 6.
#' @param interaction Logical. If TRUE, interaction terms of the conditional variables are included in the model. Default is TRUE.
#' @param mode Character. Specifies the mode of operation: "numeric_only" or "mixed". Default is "numeric_only".
#' @param metricfunc Optional the user can pass a custom function for calculating a performance metric based on the model's predictions. Default is NULL.
#' @param mlfunc Optional the user can pass a custom machine learning wrapper function to use instead of the predefined methods. Default is NULL.
#' @param parametric Logical, indicating whether to compute a parametric p-value instead of the empirical p-value. A parametric p-value assumes that the null distribution is gaussian. Default is FALSE.
#' @param tail Character. Specifies whether to calculate left-tailed or right-tailed p-values, depending on the performance metric used. Only applicable if using `metricfunc` or `mlfunc`. Default is NA.
#' @param tune Logical. If TRUE, the function will perform hyperparameter tuning for the specified machine learning method. Default is FALSE.
#' @param folds Integer. The number of folds for cross-validation during the tuning process. Default is 5.
#' @param tune_length Integer. The number of parameter combinations to try during the tuning process. Default is 10.
#' @param k Integer. The number of nearest neighbors to use for KNN method. Default is 15.
#' @param center Logical. If TRUE, the data will be centered before fitting the model
#' @param scale Logical. If TRUE, the data will be scaled before fitting the model. Default is TRUE.
#' @param eps Numeric. A small value to avoid division by zero in some calculations.
#' @param positive Character. The name of the positive class (KNN) in the data, used for classification tasks. Default is NULL.
#' @param kernel Character. The kernel type to use for KNN method. Default is "optimal".
#' @param samples Integer. Number of hyperparameter combinations used in tuning. Default is 35.
#' @param distance Numeric. Parameter of Minkowski distance for the "KNN" method. Default is 2.
#' @param seed Integer. Set the seed for reproducing results. Default is NA.
#' @param random_grid Logical. If TRUE, a random grid search is performed. If FALSE, a full grid search is performed. Default is TRUE.
#' @param nthread Integer. The number of threads to use for parallel processing. Default is 1.
#' @param verbose Logical. If TRUE, additional information is printed during the execution of the function. Default is FALSE.
#' @param progress Logical. If TRUE, a progress bar is displayed during the permutation process. Default is TRUE.
#' @param ... Additional arguments to pass to the \code{perm.test} function.
#'
#' @importFrom dplyr %>%
#' @importFrom caret train trainControl createDataPartition
#'
#' @return Invisibly returns the result of \code{perm.test}, which is an object of class 'CCI' containing the null distribution, observed test statistic, p-values, the machine learning model used, and the data.
#' @aliases CCI
#' @export
#'
#' @seealso \code{\link{perm.test}}, \code{\link{print.summary.CCI}}, \code{\link{plot.CCI}}, \code{\link{CCI.pretuner}}, \code{\link{QQplot}}
#'
#' @examples
#' set.seed(123)
#' data <- data.frame(x1 = stats::rnorm(100), x2 = stats::rnorm(100), y = stats::rnorm(100))
#' result <- CCI.test(y ~ x1 | x2, data = data, nperm = 25, interaction = FALSE)
#' summary(result)

CCI.test <- function(formula = NULL,
                     data,
                     p = 0.5,
                     nperm = 160,
                     nrounds = 600,
                     mtry = NULL,
                     metric = "Auto",
                     method = 'rf',
                     choose_direction = FALSE,
                     parametric = FALSE,
                     poly = TRUE,
                     degree = 3,
                     robust = TRUE,
                     subsample = "Auto",
                     subsample_set,
                     min_child_weight = 1,
                     colsample_bytree = 1,
                     eta = 0.3,
                     gamma = 0,
                     max_depth = 6,
                     interaction = TRUE,
                     mode = "numeric_only",
                     metricfunc = NULL,
                     mlfunc = NULL,
                     tail = NA,
                     tune = FALSE,
                     samples = 35,
                     folds = 5,
                     tune_length = 10,
                     k = 15,
                     center = TRUE,
                     scale = TRUE,
                     eps = 1e-15,
                     positive = NULL,
                     kernel = "optimal",
                     distance = 2,
                     seed = NA,
                     random_grid = TRUE,
                     nthread = 2,
                     verbose = FALSE,
                     progress = TRUE,
                     ...) {


  if (!is.na(seed)) {
    set.seed(seed)
  }
  if (is.null(data)) {
    stop("Please provide some data")
  }
  if ((!is.null(metricfunc) | !is.null(mlfunc)) && is.na(tail)) {
    stop("tail parameter must be either 'left' or 'right'")
  }
  if (is.null(formula)) {
    stop("Formula is missing")
  }
  
  if (tune && (folds < 1 || tune_length < 1)) {
    stop("folds and tune_length must be positive integers.")
  }
  if (!is.null(mlfunc) && !is.null(metricfunc)) {
    stop("You can only use one of mlfunc or metricfunc.")
  }

  # Set subsample as a function of sample size, starting when sample size > 1000
  if (subsample == "Auto") {
    n <- nrow(data)
    if (n > 900) {
      subsample <- 1 / ((n / 900) ^ 0.75)
    } else {
      subsample <- 1
    }
  } else if (subsample == "Yes") {
      subsample <- subsample_set
    } else if (subsample == "No") {
      subsample <- 1
    } else {
      stop("Invalid subsample option. Use 'Auto', 'Yes' or 'No'.")
    }
    if (verbose) {
      cat("Subsample set to: ", subsample, "\n")
    }
  if (poly && degree < 1) {
    stop("Degree of 0 or less is not allowed")
  }
  
  # Parse formula
  original_formula <- formula
  Y <- all.vars(formula)[1]
  X <- all.vars(formula[[3]])[1]
  Z <- all.vars(formula[[3]])[-1]
  if (length(Z) == 0) {
    Z <- NULL
  }
  if (!is.null(Z) && any(sapply(data[Z], is.factor))) {
    poly <- FALSE
  }
  
  # Add polynomial and interaction terms
  poly_result <- add_poly_terms(data, Z, degree = degree, poly = poly)
  data <- poly_result$data
  poly_terms <- poly_result$new_terms
  
  if (interaction && !is.null(Z)) {
    interaction_result <- add_interaction_terms(data, Z, mode = mode)
    data <- interaction_result$data
    interaction_terms <- interaction_result$interaction_terms
  } else {
    interaction_terms <- NULL
  }
  
  formula <- build_formula(formula, poly_terms, interaction_terms)
  
  
  formula = as.formula(formula)
  check_formula(formula, data)
  
  formula <- clean_formula(formula)
  
  
  if (verbose) {
    cat("Using formula: ", deparse(formula), "\n")
  }

  if (!is.null(metricfunc)) {
    metric <- deparse(substitute(metricfunc))
  } else if (!is.null(mlfunc)) {
    metric <- deparse(substitute(mlfunc))
  } else if (metric == "Auto") {
    response_var <- all.vars(formula)[1]
    y <- data[[response_var]]
      if (is.numeric(y)) {
        metric <- "RMSE"
      } else if (is.factor(y) || is.character(y)) {
          metric <- "Kappa"
      } else {
          stop("Could not determine an appropriate metric automatically. Please specify the 'metric' explicitly.")
    }
  } else if (metric == "RMSE" || metric == "Kappa" || metric == "LogLoss" ) {
    metric <- metric
  } else {
    stop("Invalid metric specified. Use 'Auto', 'RMSE', 'Kappa' or 'LogLoss' (or set a custom metricfunc).")
  }


  if (choose_direction) {
    formula <- CCI.direction(
      formula = formula,
      data = data,
      method = method,
      nrounds = nrounds,
      max_depth = max_depth,
      eta = eta,
      gamma = gamma,
      colsample_bytree = colsample_bytree,
      min_child_weight = min_child_weight,
      subsample = subsample,
      folds = 4,
      verbose = verbose
    )
  }
  if (tune && is.null(mlfunc)) {

    best_params <- CCI.pretuner(formula = formula,
                                data = data,
                                method = method,
                                subsample = subsample,
                                folds = folds,
                                tune_length = tune_length,
                                random_grid = random_grid,
                                metric = metric,
                                samples = samples,
                                verbose = verbose)
    params <- get_tuned_params(best_params$best_param)
    tune_warning <- best_params$warnings
  } else if (tune && !is.null(mlfunc)) {
    stop("Tuning parameters is not available when using a custom ML function.")
  } else if (method == "xgboost") {
    params <- list(max_depth = max_depth,
                   eta = eta,
                   gamma = gamma,
                   colsample_bytree = colsample_bytree,
                   min_child_weight = min_child_weight)
  }
  else {
    params <- list()
  }
  samples <- NULL

  method <- if (!is.null(mlfunc)) {
    deparse(substitute(mlfunc))
  } else {
    method
  }

  result <- perm.test(
    formula = formula,
    data = data,
    p = p,
    nperm = nperm,
    nrounds = nrounds,
    mtry = mtry,
    metric = metric,
    method = method,
    parametric = parametric,
    tail = tail,
    robust = robust,
    metricfunc = metricfunc,
    mlfunc = mlfunc,
    subsample = subsample,
    progress = progress,
    nthread = nthread,
    k = k,
    center = center,
    scale = scale,
    eps = eps,
    positive = positive,
    kernel = kernel,
    distance = distance,
    params,
    ...
  )
  if (!is.null(metricfunc)) {
    result$metric <- deparse(substitute(metricfunc))
  }
  
  if (tune) {
    result$warnings <- tune_warning
  }
  result$formula <- original_formula
  result$ext_formula <- formula
  result$poly <- poly
  result$degree <- degree
  result$interaction <- interaction
  
  pvalue <- result$p.value

  if (verbose) {
    cat("\n")
    cat("p-value: ", pvalue, "\n")
  }

  return(invisible(result))
}
