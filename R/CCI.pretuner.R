#' CCI tuner function for CCI test
#'
#' The `CCI.tuner` function performs a grid search over parameters for a conditional independence test using machine learning model supported by CCI.test. The tuner use the caret package for tuning.
#'
#' @param formula Model formula specifying the relationship between dependent and independent variables.
#' @param data A data frame containing the variables specified in the formula.
#' @param tune_length Integer. The number of parameter combinations to try during the tuning process. Default is 10.
#' @param method Character. Specifies the machine learning method to use. Supported methods are random forest "rf", extreme gradient boosting "xgboost" and Support Vector Machine "svm".
#' @param validation_method Character. Specifies the resampling method. Default is "cv".
#' @param training_share Numeric. For leave-group out cross-validation: the training percentage. Default is 0.7.
#' @param random_grid Logical. If TRUE, a random grid search is performed. If FALSE, a full grid search is performed. Default is TRUE.
#' @param samples Integer. The number of random samples to take from the grid. Default is 30.
#' @param poly Logical. If TRUE, polynomial terms of the conditional variables are included in the model. Default is TRUE.
#' @param degree Integer. The degree of polynomial terms to include if poly is TRUE. Default is 3.
#' @param interaction Logical. If TRUE, interaction terms of the conditional variables are included in the model. Default is TRUE.
#' @param folds Integer. The number of folds for cross-validation during the tuning process. Default is 10.
#' @param verboseIter Logical. If TRUE, the function will print the tuning process. Default is FALSE.
#' @param include_explanatory Logical. If TRUE, given the condition Y _||_ X |  Z, the function will include explanatory variable X in the model for Y. Default is FALSE
#' @param verbose Logical. If TRUE, the function will print the tuning process. Default is FALSE..
#' @param parallel Logical. If TRUE, the function will use parallel processing. Default is TRUE.
#' @param mtry Integer. The number of variables randomly sampled as candidates at each split for random forest. Default is 1:5.
#' @param nrounds Integer. The number of rounds (trees) for methods such as xgboost and random forest. Default is seq(50, 200, by = 25).
#' @param eta Numeric. The learning rate for xgboost. Default is seq(0.01, 0.3, by = 0.05).
#' @param max_depth Integer. The maximum depth of the tree for xgboost. Default is 1:6.
#' @param subsample Numeric. The subsample ratio of the training instances for xgboost. Default is seq(0.5, 1, by = 0.1).
#' @param gamma Numeric. The minimum loss reduction required to make a further partition on a leaf node for xgboost. Default is seq(0, 5, by = 1).
#' @param colsample_bytree Numeric. The subsample ratio of columns when constructing each tree for xgboost. Default is seq(0.5, 1, by = 0.1).
#' @param min_child_weight Integer. The minimum sum of instance weight (hessian) needed in a child for xgboost. Default is 1:5.
#' @param sigma Numeric. The standard deviation of the Gaussian kernel for Gaussian Process Regression. Default is seq(0.1, 2, by = 0.3).
#' @param C Numeric. The regularization parameter for Support Vector Machine. Default is seq(0.1, 2, by = 0.5).
#' @param ... Additional arguments to pass to the \code{CCI.tuner} function.
#'
#' @importFrom caret train trainControl nearZeroVar
#' @importFrom lightgbm lgb.Dataset lgb.cv
#' @importFrom dplyr %>%
#' @importFrom pbapply pblapply
#' @importFrom stats model.matrix var
#' @import progress
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{best_param}: A data frame with the best parameters.
#'   \item \code{tuning_result}: A data frame with all tested parameter combinations and their performance metrics.
#'   \item \code{warnings}: A character vector of warnings issued during tuning.
#' }
#' @aliases tuner
#' @export
#'
#' @seealso \code{\link{CCI.test} \link{perm.test}}, \code{\link{print.summary.CCI}}, \code{\link{plot.CCI}}, \code{\link{QQplot}}
#'
#' @examples
#' set.seed(123)
#' data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = rnorm(100))
#' # Tune random forest parameters
#' result <- CCI.pretuner(formula = y ~ x1 | x2,
#' data = data,
#' samples = 5,
#' folds = 3,
#' method = "rf")
#' # Returns a list with best parameters and tuning results
#' if (requireNamespace("xgboost", quietly = TRUE)) {
#'   # Tune xgboost parameters
#'   result_xgb <- CCI.pretuner(formula = y ~ x1 | x2,
#'   data = data,
#'   samples = 5,
#'   folds = 3,
#'   method = "xgboost")
#' }


CCI.pretuner <- function(formula,
                         data,
                         method = "rf",
                         metric = "RMSE",
                         validation_method = 'cv',
                         folds = 4,
                         training_share = 0.7,
                         tune_length = 4,
                         random_grid = TRUE,
                         samples = 35,
                         data_type = "continuous",
                         poly = TRUE,
                         degree = 3,
                         interaction = TRUE,
                         verboseIter = FALSE,
                         include_explanatory = FALSE,
                         verbose = FALSE,
                         parallel = FALSE,
                         mtry = 1:10,
                         nrounds = c(50, 100, 200, 300, 400),
                         eta = seq(0.01, 0.3, by = 0.05),
                         max_depth = 2:6,
                         gamma = c(0,1,2,3),
                         colsample_bytree = c(0.8, 0.9, 1),
                         min_child_weight = c(1, 3),
                         subsample = c(0.8,0.9,1),
                         sigma = seq(0.1, 2, by = 0.3),
                         C = seq(0.1, 2, by = 0.5),
                         ...) {

  if (metric != "RMSE") {
    stop("Pre tuning does not currently work for the other than the RMSE metric. Will be fixed soon.")
  }
  if (!is.data.frame(data) || nrow(data) == 0) {
    stop("The 'data' argument must be a non-empty data frame.")
  }
  if (!is.numeric(tune_length) || tune_length < 1) {
    stop("tune_length must be a positive integer.")
  }
  if (!method %in% c("rf", "xgboost", "svm")) {
    stop("method must be one of 'rf', 'xgboost' or 'svm'.")
  }

  if (poly && degree < 1) {
    stop("Degree of 0 or less is not allowed")
  }
  formula_vars <- all.vars(formula)
  if (any(sapply(data[formula_vars], function(x) !is.numeric(x) && !is.factor(x)))) {
    stop("All formula variables must be numeric or factors.")
  }

  if (data_type == "continuous" && stats::var(data[[formula_vars[1]]]) < 1e-10) {
    warning("Response variable has near-zero variance (", stats::var(data[[formula_vars[1]]]), "). R-squared may be unreliable.")
  }
  nzv <- caret::nearZeroVar(data[formula_vars[-1]], saveMetrics = TRUE)
  if (any(nzv$nzv)) {
    warning("Predictors with near-zero variance detected: ",
            paste(names(data)[formula_vars[-1]][nzv$nzv], collapse = ", "))
  }

  org_formula <- formula # Store the original formula for later use
  formula <- clean_formula(formula)

  Y = formula[[2]]
  X = formula[[3]][[2]]
  vars <- all.vars(formula)
  Z <- setdiff(vars, c(deparse(Y), deparse(X)))

  poly_result <- add_poly_terms(data, Z, degree = degree, poly = poly)
  data <- poly_result$data
  poly_terms <- poly_result$new_terms

  if (interaction) {
    interaction_result <- add_interaction_terms(data, Z)
    data <- interaction_result$data
    interaction_terms <- interaction_result$interaction_terms
  } else {
    interaction_terms <- NULL
  }

  formula <- build_formula(formula, poly_terms, interaction_terms)

  n_predictors <- length(all.vars(formula[[3]]))

  if (n_predictors > 100) {
    warning("Formula includes ", n_predictors, " predictors including polynomials and interactions. Tuning may be slow. Consider reducing folds or samples.")
  }

  if (method == "rf") {
    mtry <- seq(1, min(n_predictors, max(5, ceiling(n_predictors / 2))), by = 1)
  }
  tuneGrid <- NULL
  dots <- list(...)

  if (!is.null(dots$tuneGrid)) {
    tuneGrid <- dots$tuneGrid
    if (!is.data.frame(tuneGrid) && !is.matrix(tuneGrid)) {
      stop("Custom tuneGrid must be a data frame or matrix.")
    }
  }


  if (random_grid) {
    search <- "random"
  } else {
    search <- "grid"
  }

  outcome_name <- all.vars(formula)[1]


  if (include_explanatory) {
    formula <- formula
  } else {
    formula <- as.formula(paste(all.vars(formula)[1], "~", paste(all.vars(formula[[3]])[-1], collapse = " + ")))
  }

  check_formula(formula, data)
  X <- model.matrix(formula, data = data)[, -1, drop = FALSE]
  Y <- data[[all.vars(formula)[1]]]
  if (ncol(X) == 0) {
    stop("The formula produced an empty design matrix. Check variable types and formula specification.")
  }


  warning_log <- character()

  caret_method <- switch(method,
                         rf = "rf",
                         xgboost = "xgbTree",
                         svm = "svmRadial",
                         stop("Unsupported method"))


  ctrl <- caret::trainControl(method = validation_method,
                              p = training_share,
                              number = folds,
                              search = search,
                              verboseIter = verboseIter,
                              allowParallel = parallel,
                              summaryFunction = if (metric == "RMSE") {
                                function(data, lev = NULL, model = NULL) {
                                  obs <- data$obs
                                  pred <- data$pred
                                  if (verbose) {
                                    cat("Summary function called. Variance of predictions:", var(pred), "\n")
                                    cat("Variance of observed:", var(obs), "\n")
                                    cat("Correlation:", base::cor(obs, pred, use = "pairwise.complete.obs"), "\n")
                                  }
                                  if (stats::var(pred) < 1e-10 || stats::var(obs) < 1e-10) {
                                    return(c(RMSE = sqrt(mean((obs - pred)^2)), Rsquared = NA, MAE = mean(abs(obs - pred))))
                                  }
                                  out <- caret::defaultSummary(data, lev, model)
                                  if (is.nan(out["Rsquared"]) || is.na(out["Rsquared"])) {
                                    out["Rsquared"] <- NA
                                  }
                                  out
                                }
                              } else caret::defaultSummary
  )

  if (is.null(tuneGrid)) {
  tuneGrid <- switch(method,
                     rf   = expand.grid(mtry = mtry),
                     xgboost = expand.grid(
                       nrounds = nrounds,
                       eta = eta,
                       max_depth = max_depth,
                       colsample_bytree = colsample_bytree,
                       min_child_weight = min_child_weight,
                       subsample = subsample,
                       gamma = gamma
                     ),
                     svm = expand.grid(sigma = sigma, C = C),
                     stop("Unsupported method for pretuning.")
  )
  }


  if (random_grid) {
    total <- nrow(tuneGrid)
    sample_n <- min(samples, total)
    cat("Total combinations in grid:", total, "\n")
    cat("Randomly sampling", sample_n, "combinations...\n\n")
    tuneGrid <- tuneGrid[sample(seq_len(total), sample_n), , drop = FALSE]
  }

  warning_log <- character()
  pb <- progress::progress_bar$new(
    format = "  tuning [:bar] :percent eta: :eta",
    total = nrow(tuneGrid),
    clear = FALSE,
    width = 60
  )
  results <- lapply(seq_len(nrow(tuneGrid)), function(i) {
    pb$tick()
    row <- tuneGrid[i, , drop = FALSE]
    if (verbose) {
      cat("Training model", i, "of", nrow(tuneGrid), "with parameters:",
          paste(names(row), row, sep = "=", collapse = ", "), "\n")
    }

    train_args <- list(
      x = X,
      y = Y,
      method = caret_method,
      trControl = ctrl,
      tuneGrid = row,
      metric = metric,
      ...
    )

    model <- tryCatch(
      {
        withCallingHandlers(
          do.call(caret::train, train_args),
          warning = function(w) {
            warning_log <<- c(warning_log, paste("Warning for parameters ",
                                                 paste(names(row), row, sep = "=", collapse = ", "),
                                                 ": ", conditionMessage(w)))
            invokeRestart("muffleWarning")
          }
        )
      },
      error = function(e) {
        warning_log <<- c(warning_log, paste("Error for parameters ",
                                             paste(names(row), row, sep = "=", collapse = ", "),
                                             ": ", conditionMessage(e)))
        return(NULL)
      }
    )

    if (is.null(model)) return(NULL)

    res <- model$results[1, ]
    res[] <- lapply(res, function(x) if (is.numeric(x)) replace(x, is.nan(x), NA) else x)

    if (verbose) {
      cat("Results for model", i, ":", paste(names(res), res, sep = "=", collapse = ", "), "\n")
    }

    cbind(row, res)
  })

  # if (!is.null(last_error)) { # For debugging
  #   message("Last error: ", last_error)
  # }
  results <- results[!sapply(results, is.null)]
  if (length(results) == 0) {

    warning("No models were successfully trained in pretuning. Check parameter ranges and data.")
  }

  results_df <- do.call(rbind, results)

  best_idx <- which.min(results_df$RMSE)
  best <- results_df[best_idx, ]
  best$method <- method
  formula <- org_formula # Restore the original formula

  if (length(warning_log) > 0) {
    warning("Tuning completed with ", length(warning_log), " warnings. Check $warnings for details.")
  }

  cat("\n Tuning complete. Best model found.\n")
  return(list(best_param = best, tuning_result = results_df, warnings = warning_log))
}
