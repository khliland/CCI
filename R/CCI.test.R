#' Computational test for conditional independence based on ML and Monte Carlo Cross Validation
#'
#' The `CCI.test` function performs a conditional independence test using a specified machine learning model or a custom model provided by the user. It calculates the test statistic, generates a null distribution via permutations, computes p-values, and optionally generates a plot of the null distribution with the observed test statistic.
#' The 'CCI.test' function serves as a wrapper around the 'perm.test' function
#'
#' @param formula Model formula or a DAGitty object specifying the relationship between dependent and independent variables.
#' @param data A data frame containing the variables specified in the formula.
#' @param plot Logical, indicating if a plot of the null distribution with the test statistic should be generated. Default is TRUE.
#' @param p Numeric. Proportion of data used for training the model. Default is 0.5.
#' @param nperm Integer. The number of permutations to perform. Default is 500.
#' @param nrounds Integer. The number of rounds (trees) for methods such as xgboost and random forest. Default is 120.
#' @param dag An optional DAGitty object for specifying a Directed Acyclic Graph (DAG) to use for conditional independence testing. Default is NA.
#' @param dag_n Integer. If a DAGitty object is provided, specifies which conditional independence test to perform. Default is 1.
#' @param data_type Character. Specifies the type of data: "continuous", "binary", or "categorical". Default is "continuous".
#' @param choose_direction Logical. If TRUE, the function will choose the best direction for testing. Default is FALSE.
#' @param print_result Logical. If TRUE, the function will print the result of the test. Default is TRUE.
#' @param method Character. Specifies the machine learning method to use. Supported methods include generlaized linear models "lm", random forest "rf", and extreme gradient boosting "xgboost", etc. Default is "rf".#'
#' @param poly Logical. If TRUE, polynomial terms of the conditional variables are included in the model. Default is TRUE.
#' @param degree Integer. The degree of polynomial terms to include if poly is TRUE. Default is 3.
#' @param interaction Logical. If TRUE, interaction terms of the conditional variables are included in the model. Default is TRUE.
#' @param metricfunc Optional custom function for calculating a performance metric based on the model's predictions. Default is NULL.
#' @param mlfunc Optional custom machine learning function to use instead of the predefined methods. Default is NULL.
#' @param parametric Logical, indicating whether to compute a parametric p-value instead of the empirical p-value. A parametric p-value assumes that the null distribution is gaussian. Default is FALSE.
#' @param tail Character. Specifies whether to calculate left-tailed or right-tailed p-values, depending on the performance metric used. Only applicable if using `metricfunc` or `mlfunc`. Default is NA.
#' @param tune Logical. If TRUE, the function will perform hyperparameter tuning for the specified machine learning method. Default is FALSE.
#' @param folds Integer. The number of folds for cross-validation during the tuning process. Default is 5.
#' @param tune_length Integer. The number of parameter combinations to try during the tuning process. Default is 10.
#' @param samples Integer. The number of samples to use for tuning. Default is 35.
#' @param seed Integer. The seed for tuning. Default is NA.
#' @param random_grid Logical. If TRUE, a random grid search is performed. If FALSE, a full grid search is performed. Default is TRUE.
#' @param nthread Integer. The number of threads to use for parallel processing. Default is 1.
#' @param ... Additional arguments to pass to the \code{perm.test} function.
#'
#' @importFrom stats lm predict density gaussian predict.glm rexp runif sd rpois
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
#'
#' # Example: Basic use with a continuous outcome. The tests if y is independent of x1 given x2.
#' data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = rnorm(100))
#' result <- CCI.test(y ~ x1 | x2, data = data, nperm = 500)
#'
#'
#' # Example: Using xgboost when y is categorical
#' data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100),
#'                    y = sample(1:3, 100, replace = TRUE) - 1)

#' result <- CCI.test(y ~ x1 | x2 + x3, data = data, method = "xgboost",
#'                    data_type = "categorical", nperm = 50, num_class = 3)
#'
#' # Example: Again we can switch y and x1 (still using xgboost)
#' data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100),
#'                    y = sample(1:3, 100, replace = TRUE) - 1)
#' result <- CCI.test(x1 ~ y | x2 + x3, data = data, method = "xgboost", nperm = 200, seed = 1)
#'
#' # Example:
#' custom_ml_func <- function(formula, data, train_indices, test_indices, ...) {
#'   model <- lm(formula, data = data[train_indices, ])
#'   predictions <- predict(model, newdata = data[test_indices, ])
#'   actual <- data[test_indices, ][[all.vars(formula)[1]]]
#'   metric <- sqrt(mean((predictions - actual)^2)) # RMSE
#'   return(metric)
#' }
#'
#' result <- CCI.test(y ~ x1 | x2, data = data, nperm = 1000,
#'                    mlfunc = custom_ml_func, tail = "right")
#'
#' # Example: Using a custom performance metric function
#' data_generator <-  function(N){
#' Z1 <- rnorm(N,0,1)
#' Z2 <- rnorm(N,0,1)
#' X <- rnorm(N, Z1 + Z2, 1)
#' Y <- rnorm(N, Z1 + Z2, 1)
#' df <- data.frame(Z1, Z2, X, Y)
#' return(df)
#' }
#'
#' data <- data_generator(500)
#' custom_metric <- function(data, model, test_indices) {
#'  predictions <- predict(model, data = data[test_indices, ])$predictions
#'  actual <- data[test_indices, ][["Y"]]
#'  sst <- sum((actual - mean(actual))^2)
#'  ssr <- sum((actual - predictions)^2)
#'  rsq <- 1 - (ssr / sst)
#'  return(rsq) # R-squared
#' }
#' correct_test <- CCI.test(Y ~ X | Z1 + Z2, data = data, nperm = 100,
#'                          metricfunc = custom_metric, tail = "right")
#' false_test <- CCI.test(Y ~ X | Z1, data = data, nperm = 100,
#'                        metricfunc = custom_metric, tail = "right")

CCI.test <- function(formula = NA,
                     data,
                     plot = TRUE,
                     p = 0.5,
                     nperm = 60,
                     nrounds = 250,
                     dag = NA,
                     dag_n = 1,
                     data_type = "continuous",
                     choose_direction = FALSE,
                     print_result = TRUE,
                     method = 'rf',
                     parametric = FALSE,
                     poly = TRUE,
                     degree = 3,
                     interaction = TRUE,
                     metricfunc = NULL,
                     mlfunc = NULL,
                     tail = NA,
                     tune = FALSE,
                     samples = 35,
                     folds = 5,
                     tune_length = 10,
                     seed = NA,
                     random_grid = TRUE,
                     nthread = 1,
                     ...) {


  if (!is.na(seed)) {
    set.seed(seed)
  }

  if (!inherits(formula, "formula") && !inherits(formula, "dagitty")) {
    stop("The 'formula' must be a formula object or a dagitty object.")
  }

  if (tune && (folds < 1 || tune_length < 1)) {
    stop("folds and tune_length must be positive integers.")
  }

  metric <- if (!is.null(metricfunc)) {
    deparse(substitute(metricfunc))
  } else if (!is.null(mlfunc)) {
    "custom"
  } else {
    if (data_type %in% "continuous") {
      "RMSE"
    } else {
      "Kappa Score"
    }
  }
  if (choose_direction) {
    formula <- CCI.direction(
      formula = formula,
      data = data,
      method = method,
      folds = 4,
      data_type = data_type,
      poly = poly,
      degree = degree,
      interaction = interaction
    )
  }
  if (tune && is.null(mlfunc)) {

    best_params <- CCI.pretuner(formula = formula,
                                data = data,
                                method = method,
                                folds = folds,
                                tune_length = tune_length,
                                metric = metric,
                                random_grid = random_grid,
                                data_type = data_type,
                                interaction = interaction,
                                poly = poly,
                                degree = degree,
                                samples = samples,
                                ...)
    params <- get_tuned_params(best_params$best_param)
    tune_warning <- best_params$warnings
  } else if (tune && !is.null(mlfunc)) {
    stop("Tuning parameters is not available when using a custom ML function.")
  } else {
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
    dag = dag,
    dag_n = dag_n,
    nrounds = nrounds,
    data_type = data_type,
    degree = degree,
    poly = poly,
    interaction = interaction,
    method = method,
    parametric = parametric,
    tail = tail,
    metricfunc = metricfunc,
    mlfunc = mlfunc,
    params,
    ...
  )
  if (tune) {
    result$warnings <- tune_warning
  }

  if (print_result) {
    print.summary.CCI(result)
  }

  if (plot) {
    plot(result)
    cat("Plot generated.\n")
  }

  return(invisible(result))
}
