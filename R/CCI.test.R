#' Computational test for conditional independence based on ML and Monte Carlo Cross Validation
#'
#' The `CCI.test` function performs a conditional independence test using a specified machine learning model or a custom model provided by the user. It calculates the test statistic, generates a null distribution via permutations, computes p-values, and optionally generates a plot of the null distribution with the observed test statistic.
#' The 'CCI.test' function serves as a wrapper around the 'perm.test' function
#'
#' @param formula Model formula or a DAGitty object specifying the relationship between dependent and independent variables.
#' @param data A data frame containing the variables specified in the formula.
#' @param plot Logical, indicating if a plot of the null distribution with the test statistic should be generated. Default is TRUE.
#' @param p Numeric. Proportion of data used for training the model. Default is 0.5.
#' @param nperm Integer. The number of permutations to perform. Default is 600.
#' @param nrounds Integer. The number of rounds (trees) for methods 'xgboost' and 'rf' Default is 600.
#' @param dag An optional DAGitty object for specifying a Directed Acyclic Graph (DAG) to use for conditional independence testing. Default is NA.
#' @param dag_n Integer. If a DAGitty object is provided, specifies which conditional independence test to perform. Default is 1.
#' @param metric Character. Specifies the type of data: "Auto", "RMSE" or "Kappa". Default is "Auto".
#' @param choose_direction Logical. If TRUE, the function will choose the best direction for testing. Default is FALSE.
#' @param print_result Logical. If TRUE, the function will print the result of the test. Default is TRUE.
#' @param method Character. Specifies the machine learning method to use. Supported methods include generlaized linear models "lm", random forest "rf", and extreme gradient boosting "xgboost", etc. Default is "rf".#'
#' @param poly Logical. If TRUE, polynomial terms of the conditional variables are included in the model. Default is TRUE.
#' @param degree Integer. The degree of polynomial terms to include if poly is TRUE. Default is 3.
#' @param subsample Numeric. The proportion of data to use for subsampling. Default is 1 (no subsampling).
#' @param min_child_weight Numeric. The minimum sum of instance weight (hessian) needed in a child for methods like xgboost. Default is 1.
#' @param colsample_bytree Numeric. The subsample ratio of columns when constructing each tree for methods like xgboost. Default is 1.
#' @param eta Numeric. The learning rate for methods like xgboost. Default is 0.3.
#' @param gamma Numeric. The minimum loss reduction required to make a further partition on a leaf node of the tree for methods like xgboost. Default is 0.
#' @param max_depth Integer. The maximum depth of the trees for methods like xgboost. Default is 6.
#' @param num_class Integer. The number of classes for categorical data (used in xgboost). Default is NULL.
#' @param interaction Logical. If TRUE, interaction terms of the conditional variables are included in the model. Default is TRUE.
#' @param metricfunc Optional the user can pass a custom function for calculating a performance metric based on the model's predictions. Default is NULL.
#' @param mlfunc Optional the user can pass a custom machine learning wrapper function to use instead of the predefined methods. Default is NULL.
#' @param parametric Logical, indicating whether to compute a parametric p-value instead of the empirical p-value. A parametric p-value assumes that the null distribution is gaussian. Default is FALSE.
#' @param tail Character. Specifies whether to calculate left-tailed or right-tailed p-values, depending on the performance metric used. Only applicable if using `metricfunc` or `mlfunc`. Default is NA.
#' @param tune Logical. If TRUE, the function will perform hyperparameter tuning for the specified machine learning method. Default is FALSE.
#' @param folds Integer. The number of folds for cross-validation during the tuning process. Default is 5.
#' @param tune_length Integer. The number of parameter combinations to try during the tuning process. Default is 10.
#' @param samples Integer. The number of samples to use for tuning. Default is 35.
#' @param seed Integer. The seed for tuning. Default is NA.
#' @param random_grid Logical. If TRUE, a random grid search is performed. If FALSE, a full grid search is performed. Default is TRUE.
#' @param nthread Integer. The number of threads to use for parallel processing. Default is 1.
#' @param verbose Logical. If TRUE, additional information is printed during the execution of the function. Default is FALSE.
#' @param progress Logical. If TRUE, a progress bar is displayed during the permutation process. Default is TRUE.
#' @param ... Additional arguments to pass to the \code{perm.test} function.
#'
#' @importFrom dplyr %>%
#' @importFrom dagitty impliedConditionalIndependencies
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
                     plot = TRUE,
                     p = 0.5,
                     nperm = 60,
                     nrounds = 600,
                     dag = NULL,
                     dag_n = 1,
                     metric = "Auto",
                     method = 'rf',
                     choose_direction = FALSE,
                     print_result = TRUE,
                     parametric = FALSE,
                     poly = TRUE,
                     degree = 3,
                     subsample = 1,
                     min_child_weight = 1,
                     colsample_bytree = 1,
                     eta = 0.3,
                     gamma = 0,
                     max_depth = 6,
                     num_class = NULL,
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
  if (is.null(formula) & is.null(dag)) {
    status <- "Error: Formula or DAG are missing"
    stop("Formula or dagitty object is missing")
  }
  if (!is.null(dag) & !inherits(dag, "dagitty")) {
    stop("DAG needs to be of class dagitty.")
  }
  if (tune && (folds < 1 || tune_length < 1)) {
    stop("folds and tune_length must be positive integers.")
  }
  if (!is.null(mlfunc) && !is.null(metricfunc)) {
    stop("You can only use one of mlfunc or metricfunc.")
  }
  if (!is.null(dag)) {
    if (!requireNamespace("dagitty", quietly = TRUE)) {
      stop("Package 'dagitty' is required for this function. Please install it.")
    }
  }
  if (is.null(num_class) && metric == "Kappa" && !is.null(mlfunc)) {
    num_class <- unique(data[[all.vars(formula)[1]]])
  } else {
    num_class <- num_class
  }

  if (!is.null(dag)) {
    if (!is.null(formula)) {
      formula = as.formula(formula)
    } else if (is.null(formula)) {
      ci_statement <- dagitty::impliedConditionalIndependencies(dag)[dag_n]
      names(ci_statement)[names(ci_statement) == dag_n] <- "CI"
      formula <- as.formula(paste(ci_statement$CI$X, " ~ ", ci_statement$CI$Y, "|", paste(ci_statement$CI$Z, collapse = "+ ")))
    }
  }
  formula <- clean_formula(formula)
  check_formula(formula, data)


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
  } else if (metric == "RMSE" || metric == "Kappa" || metric == "Custom") {
    metric <- metric
  } else {
    stop("Invalid metric specified. Use 'Auto', 'RMSE', 'Kappa' or 'Custom'")
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
      poly = poly,
      degree = degree,
      interaction = interaction,
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
                                interaction = interaction,
                                poly = poly,
                                degree = degree,
                                samples = samples,
                                verbose = verbose,
                                ...)
    params <- get_tuned_params(best_params$best_param)
    tune_warning <- best_params$warnings
  } else if (tune && !is.null(mlfunc)) {
    stop("Tuning parameters is not available when using a custom ML function.")
  } else {
    params <- list(max_depth = max_depth,
                   eta = eta,
                   gamma = gamma,
                   colsample_bytree = colsample_bytree,
                   min_child_weight = min_child_weight)
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
    dag_n = dag_n,
    nrounds = nrounds,
    metric = metric,
    degree = degree,
    poly = poly,
    interaction = interaction,
    method = method,
    parametric = parametric,
    tail = tail,
    metricfunc = metricfunc,
    mlfunc = mlfunc,
    subsample = subsample,
    num_class = num_class,
    progress = progress,
    nthread = nthread,
    params,
    ...
  )
  result$metric <- metric
  if (tune) {
    result$warnings <- tune_warning
  }

  pvalue <- result$p.value

  if (verbose) {
    cat("\n")
    cat("p-value: ", pvalue, "\n")
  }

  return(invisible(result))
}
