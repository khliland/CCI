#' Permutation Test for Conditional Independence
#'
#' @param formula Model formula or DAGitty object specifying the relationship between dependent and independent variables.
#' @param data A data frame containing the variables specified in the formula.
#' @param p Proportion of data to use for training the model. Default is 0.825.
#' @param nperm Number of permutations to perform. Default is 500.
#' @param dag An optional DAGitty object for specifying a Directed Acyclic Graph (DAG). Default is NA.
#' @param dag_n If a DAGitty object is provided, specifies which conditional independence test to perform. Default is NA.
#' @param data_type Type of data: "continuous", "binary", or "categorical". Default is "continuous".
#' @param method The machine learning method to use. Supported methods include "lm", "rf", "xgboost", etc. Default is "rf".
#' @param nrounds Number of rounds (trees) for methods such as xgboost and random forest. Default is 120.
#' @param parametric Logical. If TRUE, a parametric p-value is calculated in addition to the empirical p-value. Default is FALSE.
#' @param poly Logical. If TRUE, polynomial terms of the conditional variables are included in the model. Default is TRUE.
#' @param interaction Logical. If TRUE, interaction terms of the conditional variables are included in the model. Default is TRUE.
#' @param degree The degree of polynomial terms to include if poly is TRUE. Default is 3.
#' @param family The family object for glm, specifying the distribution and link function to use. Default is gaussian().
#' @param tail Specifies whether the test is one-tailed ("left" or "right") or two-tailed. Default is NA.
#' @param metricfunc An optional custom function to calculate the performance metric based on the model's predictions. Default is NULL.
#' @param mlfunc An optional custom machine learning function to use instead of the predefined methods. Default is NULL.
#' @param nthread Integer. The number of threads to use for parallel processing. Default is 1.
#' @param ... Additional arguments to pass to the machine learning model fitting function.
#'
#' @return An object of class 'CCI' containing the null distribution, observed test statistic, p-values, the machine learning model used, and the data.
#' @importFrom stats lm rnorm predict as.formula
#' @importFrom dagitty impliedConditionalIndependencies
#' @importFrom dplyr mutate
#' @importFrom utils flush.console
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
                      nperm = 100,
                      dag = NA,
                      dag_n = NA,
                      data_type = "continuous",
                      method = "rf",
                      nrounds = 120,
                      parametric = FALSE,
                      poly = TRUE,
                      interaction = TRUE,
                      degree = 3,
                      family = gaussian(),
                      tail = NA,
                      metricfunc = NULL,
                      mlfunc = NULL,
                      nthread = 1,
                      ...) {


  if ((!is.null(metricfunc) | !is.null(mlfunc)) && is.na(tail)) {
    stop("tail parameter must be either 'left' or 'right'")
  }

  if (is.na(tail)) {
    if (data_type %in% c("binary", "categorical")) {
      tail <- "right"
    } else if (data_type == "continuous") {
      tail <- "left"
    }
  }

  if (is.null(data)) {
    status <- "Error: data is missing"
    stop("Please provide some data")
  }

  if (is.null(formula) & is.na(dag)) {
    status <- "Error: Formula and DAG are missing"
    stop("Formula and dag object is missing")
  }

  if (!is.na(dag) & !inherits(dag, "dagitty")) {
    stop("DAG needs to be of class dagitty.")
  }

  if (!is.na(dag)) {
    if (!is.null(formula)) {
      formula = as.formula(formula)
    } else if (is.null(formula)) {
      ci_statement <- dagitty::impliedConditionalIndependencies(dag)[dag_n]
      names(ci_statement)[names(ci_statement) == dag_n] <- "CI"
      if (length(ci_statement$CI$Z) == 0) {
        warning("The formula indicates an unconditional independence statement. Are you sure that you don't need conditioning variables.")
      }
      formula <- as.formula(paste(ci_statement$CI$Y, " ~ ", ci_statement$CI$X, "|", paste(ci_statement$CI$Z, collapse = "+ ")))
    }
  }
  formula <- clean_formula(formula)
  check_formula(formula, data)

  # Creating the null distribution
  dist <- test.gen(formula = formula, data_type = data_type, data = data, method, nperm = nperm, poly = poly, interaction = interaction, nrounds = nrounds, p = p, permutation = TRUE, family = family, mlfunc = mlfunc, metricfunc = metricfunc, ...)
  # Creating the test statistic
  test_statistic <- test.gen(formula = formula, data_type = data_type, data = data, method, nperm = 1, poly = poly, interaction = interaction, nrounds = nrounds, p = p, permutation = FALSE, family = family, mlfunc = mlfunc, metricfunc = metricfunc, ...)

  p.value <- get_pvalues(unlist(dist), unlist(test_statistic), parametric, tail)

  status <- "Complete"

  metric <- if (!is.null(metricfunc)) {
          deparse(substitute(metricfunc))
  } else if (!is.null(mlfunc) && is.null(metricfunc)) {
          deparse(substitute(mlfunc))
  } else {
              if (data_type == "continuous") {
                "RMSE"
              } else {
                "Kappa Score"
              }
  }

  additional_args <- list(...)

  # Gather everything in "obj"
  obj <- list(status = status,
              MLfunc = method,
              metric = metric,
              data = data,
              formula = formula,
              p = p,
              dag = dag,
              dag_n = dag_n,
              poly = poly,
              interaction = interaction,
              degree = degree,
              nperm = nperm,
              nrounds = nrounds,
              train_test_ratio = p,
              family = family,
              data_type = data_type,
              parametric = parametric,
              null.distribution = dist,
              test.statistic = test_statistic,
              tail = tail,
              p.value =  p.value,
              additional_args = additional_args
              )

  class(obj) <- c("CCI", "list")
  return(obj)
}
