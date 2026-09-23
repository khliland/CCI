#' Permutation Test for Conditional Independence
#'
#' @param formula Model formula or DAGitty object specifying the relationship between dependent and independent variables.
#' @param data A data frame containing the variables specified in the formula.
#' @param p Proportion of data to use for training the model. Default is 0.5.
#' @param nperm Number of permutations to perform. Default is 160.
#' @param MC_sample Numeric between 0 and 1. The share of the data used in each Monte Carlo sample (a new random sample each time). Default is 1 (all data).
#' @param subsample Deprecated, use `MC_sample`.
#' @param metric Type of metric: "RMSE", "Kappa" or "LogLoss". Default is 'RMSE'.
#' @param method The machine learning method to use for the learner. Supported methods include "rf", "xgboost", "KNN" and "svm". Default is "rf".
#' @param nrounds Number of rounds (trees) for methods 'xgboost' and 'rf'. Default is 600.
#' @param mtry Number of variables to possibly split at in each node for method 'rf'. Default is NULL (sqrt of number of variables).
#' @param parametric Logical. If TRUE, a parametric p-value is calculated instead of an empirical p-value. Default is FALSE.
#' @param tail Specifies whether the test is one-tailed ("left" or "right") or two-tailed. Default is NA.
#' @param robust Logical. If TRUE and the conditioning set Z contains any categorical variables (factor, character or logical), X is permuted within the groups defined by the categorical variables in Z (stratified permutation). This keeps the relationship between X and the categorical part of Z under the null hypothesis. If FALSE, X is always permuted over all observations. Default is TRUE.
#' @param metricfunc Optional custom performance metric: a function \code{function(actual, predictions, ...)} returning a single number. Set \code{tail} to "right" if higher values mean better predictions and "left" if lower values do. \code{actual} is numeric for a numeric Y and a factor for a categorical Y. For a numeric Y, \code{predictions} is numeric. For a categorical Y, \code{predictions} are the predicted classes (a factor) for methods "rf", "svm" and "KNN", and class probabilities for "xgboost": the probability of the second class level for two classes, or an n x K matrix with the class levels as column names for more classes. The \code{...} arguments are only passed on if the function accepts them. Default is NULL.
#' @param mlfunc An optional custom machine learning function to use instead of the predefined methods. Default is NULL.
#' @param nthread Integer. The number of threads to use for parallel processing for method 'rf' and 'xgboost'. Default is 1.
#' @param progress Logical. If TRUE, a progress bar is displayed during the permutation process. Default is TRUE.
#' @param k Integer. The number of nearest neighbors for the "KNN" method. Default is 15.
#' @param center Logical. If TRUE, the data is centered before model fitting. Default is TRUE.
#' @param scale Logical. If TRUE, the data is scaled before model fitting. Default is TRUE.
#' @param eps Numeric. A small value added to avoid division by zero. Default is 1e-15.
#' @param positive Character vector. Specifies which levels of a factor variable should be treated as positive class in classification tasks. Default is NULL.
#' @param kernel Character string specifying the kernel type for method option "KNN" . Possible choices are "rectangular" (which is standard unweighted knn), "triangular", "epanechnikov" (or beta(2,2)), "biweight" (or beta(3,3)), "triweight" (or beta(4,4)), "cos", "inv", "gaussian" and "optimal". Default is "optimal".
#' @param distance Numeric. Parameter of Minkowski distance for the "KNN" method. Default is 2.
#' @param ... Additional arguments to pass to the machine learning model fitting function.
#'
#' @return An object of class 'CCI' containing the null distribution, observed test statistic, p-values, the machine learning model used, and the data. The element \code{settings} holds all arguments passed to \code{\link{test.gen}} (including custom \code{metricfunc}/\code{mlfunc} and model parameters), so that \code{\link{QQplot}} can repeat the test with the same settings.
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
                      p = 0.5,
                      nperm = 160,
                      MC_sample = 1,
                      metric = 'RMSE',
                      method = "rf",
                      nrounds = 600,
                      mtry = NULL,
                      parametric = FALSE,
                      tail = NA,
                      robust = TRUE,
                      metricfunc = NULL,
                      mlfunc = NULL,
                      nthread = 1,
                      progress = TRUE,
                      k = 15,
                      center = TRUE,
                      scale = TRUE,
                      eps = 1e-15,
                      positive = NULL,
                      kernel = "optimal",
                      distance = 2,
                      subsample = NULL,
                      ...) {

  MC_sample <- deprecated_arg(MC_sample, subsample, "subsample", "MC_sample")


  # All model and data settings, used for the null distribution and the test statistic,
  # and stored in the result so that QQplot() can repeat the test with the same settings
  settings <- c(list(metric = metric,
                     method = method,
                     mtry = mtry,
                     nrounds = nrounds,
                     nthread = nthread,
                     p = p,
                     robust = robust,
                     mlfunc = mlfunc,
                     metricfunc = metricfunc,
                     MC_sample = MC_sample,
                     k = k,
                     center = center,
                     scale = scale,
                     eps = eps,
                     positive = positive,
                     kernel = kernel,
                     distance = distance),
                list(...))

  # Creating the null distribution
  dist <- do.call(test.gen, c(list(formula = formula, data = data, nperm = nperm,
                                   permutation = TRUE, progress = progress),
                              settings))
  # Creating the test statistic
  test_statistic <- do.call(test.gen, c(list(formula = formula, data = data, nperm = 1,
                                             permutation = FALSE, progress = progress),
                                        utils::modifyList(settings, list(robust = FALSE))))


  # Under dependence the test statistic is better than the null: higher for Kappa, lower for RMSE and LogLoss
  if (metric == "Kappa") {
    tail <- "right"
  } else if (metric %in% c("RMSE", "LogLoss")) {
    tail <- "left"
  } else if (is.na(tail)) {
    stop("Please specify the tail direction for the metric.")
  }

  metric_label <- metric
  if (!is.null(metricfunc)) {
    # Use the function name; an anonymous function (or one passed through do.call) keeps the metric argument
    func_label <- deparse(substitute(metricfunc))
    if (length(func_label) == 1 && nchar(func_label) <= 60) metric_label <- func_label
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
              nperm = nperm,
              nrounds = nrounds,
              MC_sample = MC_sample,
              train_test_ratio = p,
              metric = metric_label,
              parametric = parametric,
              null.distribution = dist,
              test.statistic = test_statistic,
              tail = tail,
              p.value =  p.value,
              robust = robust,
              additional_args = additional_args,
              settings = settings
              )

  class(obj) <- c("CCI", "htest")
  return(obj)
}
