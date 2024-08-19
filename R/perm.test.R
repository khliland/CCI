#' Permutation test for conditional independence
#'
#' @param formula Model formula or DAGitty object
#' @param data Data frame
#' @param MLfunc Model fitting function similar to \code{lm} (default)
#' @param nperm Number of permutations
#' @param dag_n Which test to perform if using a DAGitty object
#' @param ... Additional arguments to pass to \code{MLfunc}
#'
#' @return An object of class 'CCI' containing a null distribution,
#' observed value, p-values, the ML model, and the data.
#' @importFrom stats lm rnorm predict
#' @importFrom dagitty impliedConditionalIndependencies
#' @import dplyr
#' @export
#' @seealso \code{\link{print.CCI}}, \code{\link{summary.CCI}},
#' \code{\link{plot.CCI}}, \code{\link{QQplot}}
#'
#' @examples
#' set.seed(123)
#' dat <- data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100), x4 = rnorm(100), y = rnorm(100))
#' perm.test(y ~ x1 | x2 + x3 + x4, data = dat)

perm.test <- function(formula,
                      data,
                      p = 0.825,
                      nperm = 500,
                      dag = NA,
                      dag_n = NA,
                      data_type = "continuous",
                      method = "rf",
                      nrounds = 120,
                      parametric = FALSE,
                      poly = TRUE,
                      degree = 3,
                      family = gaussian(),
                      objective = "reg:squarederror",
                      probability = FALSE,
                      tail = NA,
                      metricfunc = NULL,
                      mlfunc = NULL,
                      seed = NULL,
                      ...) {

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

  if (!is.na(dag) & class(dag) != 'dagitty') {
    stop("DAG needs to be of class dagitty.")
  }

  if (!is.na(dag)) {
    if (!is.null(formula)) {
      formula = as.formula(formula)
    } else if (is.null(formula)) {
      ci_statement <- dagitty::impliedConditionalIndependencies(dag)[dag_n]
      names(ci_statement)[names(ci_statement) == dag_n] <- "CI" # Rename list element
      formula <- as.formula(paste(ci_statement$CI$Y, " ~ ", ci_statement$CI$X, "|", paste(ci_statement$CI$Z, collapse = "+ ")))
    }
  }
  formula <- clean_formula(formula)
  check_formula(formula, data) # Check that all variables are found in the data

  dependent1 <- formula[[2]]
  dependent2 <- formula[[3]][[2]]
  conditioning <- unlist(strsplit(deparse(formula[[3]][[3]]), split = " \\+ "))

  # Creating the null distribution
  dist <- test.gen(Y = dependent1, X = dependent2, Z = conditioning, data_type = data_type, data = data, method, nperm = nperm, nrounds = nrounds, p = p, permutation = TRUE, family = family, mlfunc = mlfunc, ...)
  # Creating the test statistic
  test_statistic <- test.gen(Y = dependent1, X = dependent2, Z = conditioning, data_type = data_type, data = data, method, nperm = 1, p = p, permutation = FALSE, family = family, mlfunc = mlfunc, ...)

  p.value <- get_pvalues(unlist(dist), unlist(test_statistic), parametric, tail)

  status <- "Complete"
  metric <- if (!is.null(metricfunc)) {
                metricfunc
  } else if (!is.null(mlfunc) && is.null(metricfunc)) {
                mlfunc
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
