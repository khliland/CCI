#' Permutation Test for Conditional Independence
#'
#' @param formula Model formula or DAGitty object specifying the relationship between dependent and independent variables.
#' @param data A data frame containing the variables specified in the formula.
#' @param p Proportion of data to use for training the model. Default is 0.825.
#' @param nperm Number of permutations to perform. Default is 500.
#' @param subsample The proportion of the data to be used. Default is 1 (no subsampling).
#' @param metric Type of metric: "RMSE" or "Kappa". Default is 'RMSE'.
#' @param method The machine learning method to use. Supported methods include "rf", "xgboost", etc. Default is "rf".
#' @param nrounds Number of rounds (trees) for methods such as xgboost and random forest. Default is 120.
#' @param parametric Logical. If TRUE, a parametric p-value is calculated in addition to the empirical p-value. Default is FALSE.
#' @param poly Logical. If TRUE, polynomial terms of the conditional variables are included in the model. Default is TRUE.
#' @param interaction Logical. If TRUE, interaction terms of the conditional variables are included in the model. Default is TRUE.
#' @param degree The degree of polynomial terms to include if poly is TRUE. Default is 3.
#' @param tail Specifies whether the test is one-tailed ("left" or "right") or two-tailed. Default is NA.
#' @param metricfunc An optional custom function to calculate the performance metric based on the model's predictions. Default is NULL.
#' @param mlfunc An optional custom machine learning function to use instead of the predefined methods. Default is NULL.
#' @param nthread Integer. The number of threads to use for parallel processing. Default is 1.
#' @param progress Logical. If TRUE, a progress bar is displayed during the permutation process. Default is TRUE.
#' @param k Integer. The number of nearest neighbors for the "KNN" method. Default is 15.
#' @param center Logical. If TRUE, the data is centered before model fitting. Default is TRUE.
#' @param scale. Logical. If TRUE, the data is scaled before model fitting. Default is TRUE.
#' @param eps Numeric. A small value added to avoid division by zero. Default is 1e-15.
#' @param positive Character vector. Specifies which levels of a factor variable should be treated as positive class in classification tasks. Default is NULL.
#' @param kernel Character string specifying the kernel type for method option "KNN" . Possible choices are "rectangular" (which is standard unweighted knn), "triangular", "epanechnikov" (or beta(2,2)), "biweight" (or beta(3,3)), "triweight" (or beta(4,4)), "cos", "inv", "gaussian" and "optimal". Default is "optimal".
#' @param distance Numeric. Parameter of Minkowski distance for the "KNN" method. Default is 2.
#' @param ... Additional arguments to pass to the machine learning model fitting function.
#'
#' @return An object of class 'CCI' containing the null distribution, observed test statistic, p-values, the machine learning model used, and the data.
#' @importFrom dplyr mutate
#' @export
#' @seealso \code{\link{print.CCI}}, \code{\link{summary.CCI}},
#' \code{\link{plot.CCI}}, \code{\link{QQplot}}
#'
#' @examples
#' set.seed(123)
#' dat <- data.frame(x1 = rnorm(100),
#' x2 = rnorm(100),
#' x3 = rnorm(100),
#' x4 = rnorm(100),
#' y = rnorm(100))
#' perm.test(y ~ x1 | x2 + x3 + x4, data = dat, nperm = 25)

perm.test <- function(formula,
                      data,
                      p = 0.7,
                      nperm = 600,
                      subsample = 1,
                      metric = 'RMSE',
                      method = "rf",
                      nrounds = 120,
                      parametric = FALSE,
                      poly = TRUE,
                      interaction = TRUE,
                      degree = 3,
                      tail = NA,
                      metricfunc = NULL,
                      mlfunc = NULL,
                      nthread = 1,
                      progress = TRUE,
                      k = 15,
                      center = TRUE,
                      scale. = TRUE,
                      eps = 1e-15,
                      positive = NULL,
                      kernel = "optimal",
                      distance = 2,
                      ...) {



  # Creating the null distribution
  dist <- test.gen(formula = formula,
                   metric = metric,
                   data = data,
                   method = method,
                   nperm = nperm,
                   poly = poly,
                   degree = degree,
                   interaction = interaction,
                   nrounds = nrounds,
                   p = p,
                   permutation = TRUE,
                   mlfunc = mlfunc,
                   metricfunc = metricfunc,
                   subsample = subsample,
                   progress = progress,
                   k = k,
                   center = center,
                   scale. = scale,
                   eps = eps,
                   positive = positive,
                   kernel = kernel,
                   distance = distance,
                   ...)
  # Creating the test statistic
  test_statistic <- test.gen(formula = formula,
                             metric = metric,
                             data = data,
                             method = method,
                             nperm = 1,
                             poly = poly,
                             degree = degree,
                             interaction = interaction,
                             nrounds = nrounds,
                             p = p,
                             permutation = FALSE,
                             mlfunc = mlfunc,
                             metricfunc = metricfunc,
                             subsample = subsample,
                             progress = progress,
                             k = k,
                             center = center,
                             scale. = scale,
                             eps = eps,
                             positive = positive,
                             kernel = kernel,
                             distance = distance,
                             ...)


  if (metric %in% c("Kappa", "LogLoss")) {
    tail <- "right"
  } else if (metric == "RMSE") {
    tail <- "left"
  } else if (is.na(tail)) {
    stop("Please specify the tail direction for the metric.")
  }

  metric_label <- if (!is.null(metricfunc)) {
    deparse(substitute(metricfunc))
  } else {
    metric
  }
  
  p.value <- get_pvalues(unlist(dist), unlist(test_statistic), parametric, tail)

  status <- "Complete"

  additional_args <- list(...)

  # Gather everything in "obj"
  obj <- list(status = status,
              MLfunc = method,
              data = data,
              formula = formula,
              p = p,
              poly = poly,
              interaction = interaction,
              degree = degree,
              nperm = nperm,
              nrounds = nrounds,
              subsample = subsample,
              train_test_ratio = p,
              metric = metric_label,
              parametric = parametric,
              null.distribution = dist,
              test.statistic = test_statistic,
              tail = tail,
              p.value =  p.value,
              additional_args = additional_args
              )

  class(obj) <- c("CCI", "htest")
  return(obj)
}
