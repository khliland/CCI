#' Permutation Test for Conditional Independence
#'
#' @param formula Model formula or DAGitty object specifying the relationship between dependent and independent variables.
#' @param data A data frame containing the variables specified in the formula.
#' @param p Proportion of data to use for training the model. Default is 0.825.
#' @param subsampling The proportion of the data to be used for subsampling. Default is 1 (no subsampling).
#' @param nperm Number of permutations to perform. Default is 500.
#' @param metric Type of metric: "RMSE", "Kappa" or "Custom". Default is 'RMSE'.
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
#' @param dag A DAGitty object specifying the directed acyclic graph for the variables. Default is NA.
#' @param dag_n A character string specifying the name of the node in the DAGitty object to be used for conditional independence testing. Default is NA.
#' @param num_class Integer. The number of classes for categorical data (used in xgboost). Default is NULL.
#'
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
#' perm.test(y ~ x1 | x2 + x3 + x4, data = dat)

perm.test <- function(formula,
                      data,
                      p = 0.7,
                      nperm = 600,
                      subsampling = 1,
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
                      dag = NA,
                      dag_n  = NA,
                      num_class = NULL,
                      ...) {


  # Creating the null distribution
  dist <- test.gen(formula = formula, metric = metric, data = data, method, nperm = nperm, poly = poly, interaction = interaction, nrounds = nrounds, p = p, permutation = TRUE, mlfunc = mlfunc, metricfunc = metricfunc, num_class = num_class, ...)
  # Creating the test statistic
  test_statistic <- test.gen(formula = formula, metric = metric, data = data, method, nperm = 1, poly = poly, interaction = interaction, nrounds = nrounds, p = p, permutation = FALSE, mlfunc = mlfunc, metricfunc = metricfunc, num_class = num_class, ...)

  if (is.na(tail)) {
    if (metric == "Kappa") {
      tail <- "right"
    } else if (metric == "RMSE") {
      tail <- "left"
    }
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
              train_test_ratio = p,
              metric = metric,
              parametric = parametric,
              null.distribution = dist,
              test.statistic = test_statistic,
              tail = tail,
              p.value =  p.value,
              dag = dag,
              dag_n = dag_n,
              additional_args = additional_args
              )

  class(obj) <- c("CCI", "list")
  return(obj)
}
