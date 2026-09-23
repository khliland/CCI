#' CCI tuner function for CCI test
#'
#' The `CCI.pretuner` function performs a (random) grid search over hyperparameters for the
#' machine learning models used by \code{\link{CCI.test}}. Each parameter combination is evaluated
#' with the same model wrappers as used in \code{CCI.test} (\code{\link{wrapper_ranger}},
#' \code{\link{wrapper_xgboost}} and \code{\link{wrapper_svm}}), and all combinations are evaluated
#' on the same resampling splits, so differences in performance reflect the parameters and not the
#' random splits.
#'
#' By default the model is tuned for predicting Y from the conditioning set Z only
#' (\code{include_explanatory = FALSE}), i.e. under the null hypothesis.
#'
#' @param formula Model formula of the form \code{Y ~ X | Z1 + Z2} (or \code{Y ~ X + Z1 + Z2}).
#' @param data A data frame containing the variables specified in the formula.
#' @param method Character. The machine learning method to tune: random forest \code{"rf"}, extreme gradient boosting \code{"xgboost"} or Support Vector Machine \code{"svm"}. Default is \code{"rf"}.
#' @param metric Character. The performance metric to optimize: \code{"RMSE"} (continuous outcome), \code{"Kappa"} or \code{"LogLoss"} (categorical outcome). Default is \code{"RMSE"}.
#' @param validation_method Character. The resampling method. One of \code{"cv"} (k-fold cross-validation), \code{"LGOCV"} (leave-group-out / Monte Carlo cross-validation) or \code{"boot"} (bootstrap, evaluated on the out-of-bag observations). Default is \code{"cv"}.
#' @param folds Integer. Number of folds (\code{"cv"}) or resamples (\code{"LGOCV"}, \code{"boot"}). Default is 4.
#' @param training_share Numeric. Training share for \code{validation_method = "LGOCV"}. Default is 0.7.
#' @param tune_length Deprecated and ignored. Use \code{samples} to control the number of parameter combinations tried.
#' @param random_grid Logical. If TRUE, \code{samples} combinations are drawn at random from the full grid. If FALSE, the full grid is evaluated. Default is TRUE.
#' @param samples Integer. The number of random combinations to draw from the grid when \code{random_grid = TRUE}. Default is 35.
#' @param poly Logical. If TRUE, polynomial terms of the conditioning variables are added. Set to FALSE if \code{data} already contains them (as when called from \code{CCI.test}). Default is TRUE.
#' @param degree Integer. The degree of polynomial terms to include if \code{poly} is TRUE. Default is 3.
#' @param interaction Logical. If TRUE, pairwise interaction terms of the conditioning variables are added. Default is TRUE.
#' @param verboseIter Deprecated and ignored.
#' @param include_explanatory Logical. If TRUE, given the condition Y _||_ X | Z, the explanatory variable X is included when tuning the model for Y. Default is FALSE.
#' @param verbose Logical. If TRUE, prints details about the tuning process. Default is FALSE.
#' @param progress Logical. If TRUE, shows a progress bar. Default is TRUE.
#' @param parallel Deprecated and ignored. Use \code{nthread} for multithreaded model fitting.
#' @param nthread Integer. Number of threads used when fitting rf and xgboost models. Default is 1.
#' @param num_trees Integer. Number of trees used when tuning random forest. Default is 500.
#' @param mtry Integer vector. Candidate values for the number of variables sampled at each split in random forest. Default is \code{NULL}, which uses \code{1:min(p, max(5, ceiling(p / 2)))} where p is the number of predictors.
#' @param nrounds Integer vector. Candidate numbers of boosting rounds for xgboost. Default is \code{seq(100, 1000, by = 100)}.
#' @param eta Numeric vector. Candidate learning rates for xgboost. Default is \code{seq(0.01, 0.3, by = 0.05)}.
#' @param max_depth Integer vector. Candidate maximum tree depths for xgboost. Default is \code{2:6}.
#' @param gamma Numeric vector. Candidate minimum loss reductions for xgboost. Default is \code{c(0, 1, 2, 3)}.
#' @param colsample_bytree Numeric vector. Candidate column subsample ratios for xgboost. Default is \code{c(0.8, 0.9, 1)}.
#' @param min_child_weight Numeric vector. Candidate minimum child weights for xgboost. Default is \code{c(1, 3)}.
#' @param subsample Deprecated and ignored.
#' @param sigma Numeric vector. Candidate RBF kernel widths for svm (passed as \code{gamma} to \code{e1071::svm}). Default is \code{seq(0.1, 2, by = 0.3)}.
#' @param C Numeric vector. Candidate cost (regularization) values for svm (passed as \code{cost} to \code{e1071::svm}). Default is \code{seq(0.1, 2, by = 0.5)}.
#' @param ... Additional arguments passed to the model wrapper. A custom grid can be supplied as \code{tuneGrid}, a data frame with one column per parameter (\code{mtry} for rf; \code{nrounds}, \code{eta}, \code{max_depth}, \code{gamma}, \code{colsample_bytree}, \code{min_child_weight} for xgboost; \code{sigma}, \code{C} for svm).
#'
#' @importFrom dplyr %>%
#' @importFrom stats model.matrix var cor sd
#' @import progress
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{best_param}: A one-row data frame with the best parameters, their mean performance and its standard deviation across resamples, and the \code{method}.
#'   \item \code{tuning_result}: A data frame with all tested parameter combinations and their performance, best first.
#'   \item \code{warnings}: A character vector of warnings and errors issued during tuning.
#' }
#' @aliases tuner
#' @export
#'
#' @seealso \code{\link{CCI.test}}, \code{\link{get_tuned_params}}, \code{\link{perm.test}}
#'
#' @examples
#' set.seed(123)
#' data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100), y = rnorm(100))
#' # Tune random forest parameters
#' result <- CCI.pretuner(formula = y ~ x1 | x2 + x3,
#'                        data = data,
#'                        samples = 5,
#'                        folds = 3,
#'                        method = "rf",
#'                        progress = FALSE)
#' result$best_param

CCI.pretuner <- function(formula,
                         data,
                         method = "rf",
                         metric = "RMSE",
                         validation_method = "cv",
                         folds = 4,
                         training_share = 0.7,
                         tune_length = NULL,
                         random_grid = TRUE,
                         samples = 35,
                         poly = TRUE,
                         degree = 3,
                         interaction = TRUE,
                         verboseIter = NULL,
                         include_explanatory = FALSE,
                         verbose = FALSE,
                         progress = TRUE,
                         parallel = NULL,
                         nthread = 1,
                         num_trees = 500,
                         mtry = NULL,
                         nrounds = seq(100, 1000, by = 100),
                         eta = seq(0.01, 0.3, by = 0.05),
                         max_depth = 2:6,
                         gamma = c(0, 1, 2, 3),
                         colsample_bytree = c(0.8, 0.9, 1),
                         min_child_weight = c(1, 3),
                         subsample = NULL,
                         sigma = seq(0.1, 2, by = 0.3),
                         C = seq(0.1, 2, by = 0.5),
                         ...) {

  # ---- Input checks ----
  if (!metric %in% c("RMSE", "Kappa", "LogLoss")) {
    stop("metric must be one of 'RMSE', 'Kappa' or 'LogLoss'.")
  }
  if (!is.data.frame(data) || nrow(data) == 0) {
    stop("The 'data' argument must be a non-empty data frame.")
  }
  if (!method %in% c("rf", "xgboost", "svm")) {
    stop("method must be one of 'rf', 'xgboost' or 'svm'.")
  }
  if (!validation_method %in% c("cv", "LGOCV", "boot")) {
    stop("validation_method must be one of 'cv', 'LGOCV' or 'boot'.")
  }
  if (!is.numeric(folds) || folds < 2) {
    stop("folds must be an integer of at least 2.")
  }
  if (!is.numeric(samples) || samples < 1) {
    stop("samples must be a positive integer.")
  }
  if (poly && degree < 1) {
    stop("Degree of 0 or less is not allowed")
  }
  deprecated <- c(tune_length = !is.null(tune_length), verboseIter = !is.null(verboseIter),
                  parallel = !is.null(parallel), subsample = !is.null(subsample))
  if (any(deprecated)) {
    warning("Deprecated and ignored argument(s): ", paste(names(deprecated)[deprecated], collapse = ", "), ".")
  }

  formula <- clean_formula(formula)
  check_formula(formula, data)
  formula_vars <- all.vars(formula)
  outcome <- formula_vars[1]

  if (metric == "RMSE") {
    if (!is.numeric(data[[outcome]])) {
      stop("metric 'RMSE' requires a numeric response. Use 'Kappa' or 'LogLoss' for a categorical response.")
    }
    if (stats::var(data[[outcome]]) < 1e-10) {
      warning("Response variable has near-zero variance (", stats::var(data[[outcome]]), "). Tuning results may be unreliable.")
    }
  } else if (length(unique(data[[outcome]])) < 2) {
    stop("The response variable must have at least two classes for classification metrics.")
  }

  predictors <- formula_vars[-1]
  nzv <- caret::nearZeroVar(data[predictors], saveMetrics = TRUE)
  if (any(nzv$nzv)) {
    warning("Predictors with near-zero variance detected: ",
            paste(predictors[nzv$nzv], collapse = ", "))
  }

  # ---- Build model formula ----
  X_name <- all.vars(formula[[3]][[2]])
  Z <- setdiff(formula_vars, c(outcome, X_name))

  poly_result <- add_poly_terms(data, Z, degree = degree, poly = poly)
  data <- poly_result$data

  interaction_terms <- NULL
  if (interaction) {
    interaction_result <- add_interaction_terms(data, Z)
    data <- interaction_result$data
    interaction_terms <- interaction_result$interaction_terms
  }

  formula <- build_formula(formula, poly_result$new_terms, interaction_terms)
  rhs <- all.vars(formula[[3]])
  if (!include_explanatory) {
    rhs <- setdiff(rhs, X_name)
  }
  if (length(rhs) == 0) {
    stop("No conditioning variables to tune on. Use include_explanatory = TRUE when Z is empty.")
  }
  formula <- stats::as.formula(paste(outcome, "~", paste(rhs, collapse = " + ")))
  check_formula(formula, data)

  # ---- Parameter grid ----
  dots <- list(...)
  tuneGrid <- dots$tuneGrid
  dots$tuneGrid <- NULL

  if (is.null(tuneGrid)) {
    if (method == "rf" && is.null(mtry)) {
      mtry <- seq_len(min(length(rhs), max(5, ceiling(length(rhs) / 2))))
    }
    tuneGrid <- switch(method,
                       rf = expand.grid(mtry = unique(pmin(mtry, length(rhs)))),
                       xgboost = expand.grid(nrounds = nrounds,
                                             eta = eta,
                                             max_depth = max_depth,
                                             gamma = gamma,
                                             colsample_bytree = colsample_bytree,
                                             min_child_weight = min_child_weight),
                       svm = expand.grid(sigma = sigma, C = C))
  } else if (!is.data.frame(tuneGrid) && !is.matrix(tuneGrid)) {
    stop("Custom tuneGrid must be a data frame or matrix.")
  }
  tuneGrid <- as.data.frame(tuneGrid, stringsAsFactors = FALSE)

  if (random_grid) {
    total <- nrow(tuneGrid)
    sample_n <- min(samples, total)
    if (verbose) {
      cat("Total combinations in grid:", total, "\n")
      cat("Randomly sampling", sample_n, "combinations...\n\n")
    }
    tuneGrid <- tuneGrid[sample(seq_len(total), sample_n), , drop = FALSE]
  }
  rownames(tuneGrid) <- NULL

  # ---- Resampling: fixed splits shared by all candidates ----
  Y <- data[[outcome]]
  if (metric != "RMSE") Y <- factor(Y)
  train_sets <- switch(validation_method,
                       cv    = caret::createFolds(Y, k = folds, returnTrain = TRUE),
                       LGOCV = caret::createDataPartition(Y, p = training_share, times = folds),
                       boot  = caret::createResample(Y, times = folds))
  test_sets <- lapply(train_sets, function(train) setdiff(seq_len(nrow(data)), train))

  fit_one <- function(params, train_indices, test_indices) {
    args <- c(list(formula = formula,
                   data = data,
                   train_indices = train_indices,
                   test_indices = test_indices,
                   metric = metric),
              switch(method,
                     rf = list(mtry = params$mtry, num.trees = num_trees, nthread = nthread),
                     xgboost = list(nrounds = params$nrounds, eta = params$eta,
                                    max_depth = params$max_depth, gamma = params$gamma,
                                    colsample_bytree = params$colsample_bytree,
                                    min_child_weight = params$min_child_weight,
                                    nthread = nthread),
                     svm = list(gamma = params$sigma, cost = params$C)),
              dots)
    wrapper <- switch(method, rf = wrapper_ranger, xgboost = wrapper_xgboost, svm = wrapper_svm)
    as.numeric(do.call(wrapper, args))
  }

  # ---- Tuning ----
  warning_log <- character()
  if (progress) {
    pb <- progress::progress_bar$new(
      format = "tuning [:bar] :percent eta: :eta",
      total = nrow(tuneGrid),
      clear = FALSE,
      width = 60
    )
  }

  scores <- vapply(seq_len(nrow(tuneGrid)), function(i) {
    if (progress) pb$tick()
    params <- as.list(tuneGrid[i, , drop = FALSE])
    label <- paste(names(params), unlist(params), sep = "=", collapse = ", ")
    if (verbose) {
      cat("Training model", i, "of", nrow(tuneGrid), "with parameters:", label, "\n")
    }
    values <- vapply(seq_along(train_sets), function(j) {
      tryCatch(
        withCallingHandlers(
          fit_one(params, train_sets[[j]], test_sets[[j]]),
          warning = function(w) {
            warning_log <<- c(warning_log, paste0("Warning for parameters ", label, ": ", conditionMessage(w)))
            invokeRestart("muffleWarning")
          }
        ),
        error = function(e) {
          warning_log <<- c(warning_log, paste0("Error for parameters ", label, ": ", conditionMessage(e)))
          NA_real_
        }
      )
    }, numeric(1))
    if (verbose) {
      cat("Mean", metric, "for model", i, ":", mean(values, na.rm = TRUE), "\n")
    }
    if (all(is.na(values))) c(NA_real_, NA_real_) else c(mean(values, na.rm = TRUE), stats::sd(values, na.rm = TRUE))
  }, numeric(2))

  results_df <- tuneGrid
  results_df[[metric]] <- scores[1, ]
  results_df[[paste0(metric, "SD")]] <- scores[2, ]

  if (all(is.na(results_df[[metric]]))) {
    stop("No models were successfully trained in pretuning. Check parameter ranges and data.\n",
         paste(utils::head(unique(warning_log), 5), collapse = "\n"))
  }
  maximize <- metric == "Kappa"
  results_df <- results_df[order(results_df[[metric]], decreasing = maximize, na.last = TRUE), , drop = FALSE]
  rownames(results_df) <- NULL

  best <- results_df[1, , drop = FALSE]
  best$method <- method

  if (length(warning_log) > 0) {
    warning("Tuning completed with ", length(warning_log), " warnings. Check $warnings for details.")
  }
  if (verbose) {
    cat("Best parameters found:\n")
    print(best)
  }

  list(best_param = best, tuning_result = results_df, warnings = warning_log)
}
