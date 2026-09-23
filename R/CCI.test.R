#' Computational test for conditional independence based on ML and Monte Carlo Cross Validation
#'
#' The `CCI.test` function performs a conditional independence test using a specified machine learning model or a custom model provided by the user. It calculates the test statistic, generates a null distribution via permutations, computes p-values, and optionally generates a plot of the null distribution with the observed test statistic.
#' The 'CCI.test' function serves as a wrapper around the 'perm.test' function
#'
#' @param formula Model formula specifying the relationship between dependent and independent variables. (Ex: Y ~ X | Z1 + Z2 for Y _||_ X | Z1, Z2). For an unconditional test of Y _||_ X, write Y ~ X | 1 or Y ~ X + 1; Y ~ X alone gives an error.
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
#' @param MC_sample Character. The share of the data used in each Monte Carlo sample: "Auto" uses \eqn{(900/n)^{0.75}} of the data when n > 900 and all data otherwise, "Yes" uses the share given in `MC_sample_set`, and "No" always uses all data. A different random sample is drawn in each Monte Carlo sample. Default is "Auto".
#' @param MC_sample_set Numeric between 0 and 1. The share of the data used in each Monte Carlo sample when `MC_sample = "Yes"`.
#' @param subsample Deprecated, use `MC_sample`.
#' @param subsample_set Deprecated, use `MC_sample_set`.
#' @param robust Logical. If TRUE and the conditioning set Z contains any categorical variables (factor, character or logical), X is permuted within the groups defined by the categorical variables in Z (stratified permutation). This keeps the relationship between X and the categorical part of Z under the null hypothesis. If FALSE, X is always permuted over all observations. Default is TRUE.
#' @param min_child_weight Numeric. The minimum sum of instance weight (hessian) needed in a child for methods like xgboost. Default is 1.
#' @param colsample_bytree Numeric. The subsample ratio of columns when constructing each tree for methods like xgboost. Default is 1.
#' @param eta Numeric. The learning rate for methods like xgboost. Default is 0.3.
#' @param gamma Numeric. The minimum loss reduction required to make a further partition on a leaf node of the tree for methods like xgboost. Default is 0.
#' @param max_depth Integer. The maximum depth of the trees for methods like xgboost. Default is 6.
#' @param interaction Logical. If TRUE, interaction terms of the conditional variables are included in the model. Default is TRUE.
#' @param mode Character. Specifies the mode of operation: "numeric_only" or "mixed". Default is "numeric_only".
#' @param metricfunc Optional custom performance metric: a function \code{function(actual, predictions, ...)} returning a single number. Set \code{tail} to "right" if higher values mean better predictions and "left" if lower values do. \code{actual} is numeric for a numeric Y and a factor for a categorical Y. For a numeric Y, \code{predictions} is numeric. For a categorical Y, \code{predictions} are the predicted classes (a factor) for methods "rf", "svm" and "KNN", and class probabilities for "xgboost": the probability of the second class level for two classes, or an n x K matrix with the class levels as column names for more classes. The \code{...} arguments are only passed on if the function accepts them. Default is NULL.
#' @param mlfunc Optional the user can pass a custom machine learning wrapper function to use instead of the predefined methods. Default is NULL.
#' @param parametric Logical, indicating whether to compute a parametric p-value instead of the empirical p-value. A parametric p-value assumes that the null distribution is gaussian. Default is FALSE.
#' @param tail Character. Specifies whether to calculate left-tailed or right-tailed p-values, depending on the performance metric used. Only applicable if using `metricfunc` or `mlfunc`. Default is NA.
#' @param tune Logical. If TRUE, hyperparameters for the specified machine learning method are tuned with \code{\link{CCI.pretuner}} before testing, and the best parameters are used in the test. Available for methods 'rf', 'xgboost' and 'svm'. Default is FALSE.
#' @param folds Integer. The number of folds for cross-validation during the tuning process. Default is 5.
#' @param tune_length Deprecated and ignored. Use `samples` to control the number of parameter combinations tried in tuning.
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
                     MC_sample = "Auto",
                     MC_sample_set = NULL,
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
                     tune_length = NULL,
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
                     subsample = NULL,
                     subsample_set = NULL,
                     ...) {

  MC_sample <- deprecated_arg(MC_sample, subsample, "subsample", "MC_sample")
  MC_sample_set <- deprecated_arg(MC_sample_set, subsample_set, "subsample_set", "MC_sample_set")

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
  
  if (tune && folds < 2) {
    stop("folds must be an integer of at least 2.")
  }
  if (!is.null(tune_length)) {
    warning("'tune_length' is deprecated and ignored. Use 'samples' to control the number of combinations tried.")
  }
  if (!is.null(mlfunc) && !is.null(metricfunc)) {
    stop("You can only use one of mlfunc or metricfunc.")
  }

  # Share of the data used in each Monte Carlo sample; "Auto" starts reducing it when n > 900
  if (identical(MC_sample, "Auto")) {
    n <- nrow(data)
    if (n > 900) {
      MC_sample <- 1 / ((n / 900) ^ 0.75)
    } else {
      MC_sample <- 1
    }
  } else if (identical(MC_sample, "Yes")) {
    if (is.null(MC_sample_set) || !is.numeric(MC_sample_set) || MC_sample_set <= 0 || MC_sample_set > 1) {
      stop("With MC_sample = 'Yes', set MC_sample_set to the share of data to use (between 0 and 1).")
    }
    MC_sample <- MC_sample_set
  } else if (identical(MC_sample, "No")) {
    MC_sample <- 1
  } else {
    stop("Invalid MC_sample option. Use 'Auto', 'Yes' or 'No'.")
  }
  if (verbose) {
    cat("MC sample share set to: ", MC_sample, "\n")
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
  # Character and logical variables are categorical: treat them as factors in every step
  data <- characters_to_factors(data, c(Y, X, Z))

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


  # The hypothesis as tested, without polynomial and interaction terms (shown by print and summary)
  tested_formula <- original_formula
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
      MC_sample = MC_sample,
      mtry = mtry,
      nthread = nthread,
      k = k,
      kernel = kernel,
      distance = distance,
      folds = 4,
      verbose = verbose
    )
    if (all.vars(formula)[1] != Y) {
      Z_part <- if (is.null(Z)) "1" else paste(Z, collapse = " + ")
      tested_formula <- stats::as.formula(paste(X, "~", Y, "|", Z_part))
    }
  }
  if (tune && is.null(mlfunc)) {
    if (!method %in% c("rf", "xgboost", "svm")) {
      stop("Tuning is only available for methods 'rf', 'xgboost' and 'svm'.")
    }
    # A custom metricfunc can not be tuned on directly, so fall back to the default metric for the response type
    tune_metric <- if (metric %in% c("RMSE", "Kappa", "LogLoss")) {
      metric
    } else if (is.numeric(data[[all.vars(formula)[1]]])) {
      "RMSE"
    } else {
      "Kappa"
    }
    # Polynomial and interaction terms are already added to data and formula
    best_params <- CCI.pretuner(formula = formula,
                                data = data,
                                method = method,
                                metric = tune_metric,
                                folds = folds,
                                random_grid = random_grid,
                                samples = samples,
                                poly = FALSE,
                                interaction = FALSE,
                                # Unconditional test: nothing to tune on without X
                                include_explanatory = is.null(Z),
                                verbose = verbose,
                                progress = progress)
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

  perm_args <- list(
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
    MC_sample = MC_sample,
    progress = progress,
    nthread = nthread,
    k = k,
    center = center,
    scale = scale,
    eps = eps,
    positive = positive,
    kernel = kernel,
    distance = distance
  )
  # Model parameters (tuned or user given) must be passed as named arguments to reach the model
  perm_args <- utils::modifyList(perm_args, params)
  result <- do.call(perm.test, c(perm_args, list(...)))
  if (!is.null(metricfunc)) {
    result$metric <- deparse(substitute(metricfunc))
  }
  
  if (tune) {
    result$warnings <- tune_warning
  }
  result$formula <- original_formula
  result$tested_formula <- tested_formula
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
