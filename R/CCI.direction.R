#' Choose Direction for testing for the CCI test
#'
#' This function selects the best direction for the CCI test based on cross validation. For the condition Y _||_ X | Z, the function return the recommended
#' formula either Y ~ X | Z or X ~ Y | Z .
#'
#' Both directions are evaluated with k-fold cross-validation, using the same learners as
#' \code{\link{CCI.test}} (\code{\link{wrapper_ranger}}, \code{\link{wrapper_xgboost}},
#' \code{\link{wrapper_svm}} or \code{\link{wrapper_knn}}) and the same folds for both directions.
#' The numeric variables are standardized (mean 0, standard deviation 1) before the models are
#' fitted, so the cross-validated RMSEs of the two directions are comparable and the choice does not
#' depend on the units of the variables. The direction with the lowest standardized RMSE, i.e. the
#' variable that is easiest to predict, is chosen.
#'
#' @param formula A formula of the form \code{Y ~ X | Z1 + Z2} (or \code{Y ~ X + Z1 + Z2}, or \code{Y ~ X | 1} without conditioning variables).
#' @param data A data frame containing the variables specified in the formula.
#' @param method Character. The learner: \code{"rf"} (random forest, ranger), \code{"xgboost"}, \code{"svm"} (e1071) or \code{"KNN"} (kknn). Default is \code{"rf"}.
#' @param folds Integer. The number of folds for cross-validation. Default is 4.
#' @param nrounds Integer. The number of trees for \code{"rf"} and boosting rounds for \code{"xgboost"}. Default is 600.
#' @param max_depth Integer. The maximum depth of the trees for xgboost. Default is 6.
#' @param eta Numeric. The learning rate for xgboost. Default is 0.3.
#' @param gamma Numeric. The minimum loss reduction required to make a further partition on a leaf node for xgboost. Default is 0.
#' @param colsample_bytree Numeric. The subsample ratio of columns when constructing each tree for xgboost. Default is 1.
#' @param min_child_weight Numeric. The minimum sum of instance weight (hessian) needed in a child for xgboost. Default is 1.
#' @param MC_sample Numeric between 0 and 1. The share of the data to use (a random sample). Default is 1 (all data).
#' @param subsample Deprecated, use `MC_sample`.
#' @param mtry Integer. The number of variables tried at each split for \code{"rf"}. Default is \code{NULL} (ranger's default).
#' @param nthread Integer. The number of threads for \code{"rf"} and \code{"xgboost"}. Default is 1.
#' @param k Integer. The number of neighbours for \code{"KNN"}. Default is 15.
#' @param kernel Character. The kernel for \code{"KNN"}. Default is \code{"optimal"}.
#' @param distance Numeric. The Minkowski distance parameter for \code{"KNN"}. Default is 2.
#' @param poly Not used. Kept for backward compatibility; \code{CCI.test} adds polynomial terms before choosing the direction.
#' @param degree Not used. Kept for backward compatibility.
#' @param interaction Not used. Kept for backward compatibility.
#' @param verbose Logical. If TRUE, prints the cross-validated RMSE of both directions and the selected formula. Default is FALSE.
#' @param ... Additional arguments passed to the learner's wrapper function.
#'
#' @return The selected formula, either \code{Y ~ X | Z} or \code{X ~ Y | Z}. With
#'   \code{verbose = TRUE}, the cross-validated standardized RMSE of both directions is printed.
#' @export
#'
#' @examples
#' set.seed(1)
#' dat <- data.frame(Z1 = rnorm(200), Z2 = rnorm(200))
#' dat$X <- dat$Z1 + dat$Z2 + rnorm(200, sd = 0.1)   # X is easy to predict from Z
#' dat$Y <- dat$Z1 + rnorm(200)                      # Y is harder to predict
#' CCI.direction(Y ~ X | Z1 + Z2, data = dat, method = "xgboost", nrounds = 50)

CCI.direction <- function(formula,
                          data,
                          method = "rf",
                          folds = 4,
                          nrounds = 600,
                          max_depth = 6,
                          eta = 0.3,
                          gamma = 0,
                          colsample_bytree = 1,
                          min_child_weight = 1,
                          MC_sample = 1,
                          mtry = NULL,
                          nthread = 1,
                          k = 15,
                          kernel = "optimal",
                          distance = 2,
                          poly = TRUE,
                          degree = 3,
                          interaction = TRUE,
                          verbose = FALSE,
                          subsample = NULL,
                          ...) {
  MC_sample <- deprecated_arg(MC_sample, subsample, "subsample", "MC_sample")
  if (verbose) {
    cat("Deciding best direction, Y ~ X | Z or X ~ Y | Z...\n")
  }
  if (!method %in% c("rf", "xgboost", "svm", "KNN")) {
    stop("method must be one of 'rf', 'xgboost', 'svm' or 'KNN'.")
  }
  if (!is.numeric(folds) || folds < 2) {
    stop("folds must be an integer of at least 2.")
  }
  if (MC_sample <= 0 || MC_sample > 1) {
    stop("MC_sample must be between 0 and 1.")
  }

  formula <- clean_formula(stats::as.formula(formula))
  check_formula(formula, data)
  outcome_var <- all.vars(formula[[2]])
  rhs_vars <- all.vars(formula[[3]])
  X_var <- rhs_vars[1]
  Z_vars <- rhs_vars[-1]

  if (!is.numeric(data[[outcome_var]]) || !is.numeric(data[[X_var]])) {
    stop("When argument 'choose_direction = TRUE', both marginal variables must be numeric.")
  }

  # The two directions as regression formulas (the explicit 1 keeps a formula without Z valid)
  direction_formula <- function(outcome, predictor) {
    stats::as.formula(paste(outcome, "~", paste(c(predictor, Z_vars, if (length(Z_vars) == 0) "1"),
                                                collapse = " + ")))
  }
  formula_Y_XZ <- direction_formula(outcome_var, X_var)
  formula_X_YZ <- direction_formula(X_var, outcome_var)

  if (MC_sample < 1) {
    data <- data[sample(nrow(data), size = round(nrow(data) * MC_sample)), , drop = FALSE]
  }
  data <- characters_to_factors(data, all.vars(formula))

  # Standardize the numeric variables, so the two prediction errors are on the same scale.
  # Otherwise the direction would be chosen by the units of Y and X, not by how easy they are to predict.
  for (v in all.vars(formula)) {
    if (!is.numeric(data[[v]])) next
    v_sd <- stats::sd(data[[v]], na.rm = TRUE)
    if (!is.finite(v_sd) || v_sd == 0) {
      if (v %in% c(outcome_var, X_var)) {
        stop("Variable '", v, "' has zero variance, so the direction can not be chosen.")
      }
      next
    }
    data[[v]] <- (data[[v]] - mean(data[[v]], na.rm = TRUE)) / v_sd
  }

  # Same folds for both directions
  n <- nrow(data)
  if (folds > n) stop("folds can not be larger than the number of observations.")
  fold_id <- sample(rep_len(seq_len(folds), n))

  cv_rmse <- function(f) {
    rmse <- vapply(seq_len(folds), function(j) {
      train <- which(fold_id != j)
      test <- which(fold_id == j)
      args <- list(formula = f, data = data, train_indices = train, test_indices = test, metric = "RMSE")
      value <- switch(method,
        rf = do.call(wrapper_ranger, c(args, list(num.trees = nrounds, mtry = mtry, nthread = nthread), list(...))),
        xgboost = do.call(wrapper_xgboost, c(args, list(nrounds = nrounds, nthread = nthread, eta = eta,
                                                        max_depth = max_depth, gamma = gamma,
                                                        colsample_bytree = colsample_bytree,
                                                        min_child_weight = min_child_weight), list(...))),
        svm = do.call(wrapper_svm, c(args, list(...))),
        KNN = do.call(wrapper_knn, c(args, list(k = k, kernel = kernel, distance = distance), list(...))))
      as.numeric(value)
    }, numeric(1))
    mean(rmse)
  }

  metric1 <- cv_rmse(formula_Y_XZ)
  metric2 <- cv_rmse(formula_X_YZ)

  Z_part <- if (length(Z_vars) == 0) "1" else paste(Z_vars, collapse = " + ")  # 1 = unconditional test
  if (metric1 <= metric2) {
    final_formula <- stats::as.formula(paste(outcome_var, "~", X_var, "|", Z_part))
  } else {
    final_formula <- stats::as.formula(paste(X_var, "~", outcome_var, "|", Z_part))
  }
  if (verbose) {
    cat("Cross-validated standardized RMSE: ", outcome_var, " ~ ", X_var, ": ", round(metric1, 4), ", ",
        X_var, " ~ ", outcome_var, ": ", round(metric2, 4), "\n", sep = "")
    cat("Selected formula:", paste(deparse(final_formula), collapse = " "), "\n")
  }

  final_formula
}
