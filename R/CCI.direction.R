#' Choose Direction for testing for the CCI test
#'
#' This function selects the best direction for the CCI test based on cross validation. For the condition Y _||_ X | Z, the function return the recommended
#' formula either Y ~ X | Z or X ~ Y | Z .
#'
#' @param formula A formula object specifying the model to be fitted.
#' @param data A data frame containing the variables specified in the formula.
#' @param method A character string specifying the method to be used for model fitting. Options include "rf" (random forest), "xgboost" (XGBoost), "nnet" (neural network), "gpr" (Gaussian process regression), and "svm" (support vector machine).
#' @param folds An integer specifying the number of folds for cross-validation. Default is 4.
#' @param subsample Numeric. The proportion of the data to be used. Default is 1 (i.e., use all data).
#' @param nrounds Integer. The number of rounds (trees) for methods like xgboost, ranger, and lightgbm. Default is 600.
#' @param max_depth Integer. The maximum depth of the trees for methods like xgboost. Default is 6.
#' @param eta Numeric. The learning rate for methods like xgboost. Default is 0.3.
#' @param gamma Numeric. The minimum loss reduction required to make a further partition on a leaf node of the tree for methods like xgboost. Default is 0.
#' @param colsample_bytree Numeric. The subsample ratio of columns when constructing each tree for methods like xgboost. Default is 1.
#' @param min_child_weight Numeric. The minimum sum of instance weight (hessian) needed in a child for methods like xgboost. Default is 1.
#' @param subsample Numeric. The proportion of the data to be used for subsampling. Default is 1 (no subsampling).
#' @param poly Logical. If TRUE, polynomial terms of the conditioning variables are included in the model. Default is TRUE.
#' @param degree Integer. The degree of polynomial terms to include if \code{poly} is TRUE. Default is 3.
#' @param interaction Logical. If TRUE, interaction terms of the conditioning variables are included in the model. Default is TRUE.
#' @param verbose Logical. If TRUE, prints additional information during the execution. Default is FALSE.
#' @param ... Additional arguments to be passed to the model fitting function.
#'
#' @return A formula object specifying the selected model direction.
#' @import caret
#' @export




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
                          subsample = 1,
                          poly = TRUE,
                          degree = 3,
                          interaction = TRUE,
                          verbose = FALSE,
                          ...) {
  if (verbose) {
   cat("Deciding best direction, Y ~ X | Z or X ~ Y | Z...\n")
  }

  formula <- as.formula(formula)
  outcome_var <- all.vars(formula[[2]])
  rhs_vars <- all.vars(formula[[3]])

  if (length(rhs_vars) < 2) {
    stop("Formula must have at least two variables on the right-hand side (X and Z).")
  }

  X_var <- rhs_vars[1]
  Z_vars <- rhs_vars[-1]

  # stop if either Y or C is non numeric
  if (!is.numeric(data[[outcome_var]]) || !is.numeric(data[[X_var]])) {
    stop("When argument 'choose_direction = TRUE', both marginal variables must be numeric.")
  }
  
  # Two formulas
  formula_Y_XZ <- as.formula(
    paste(outcome_var, "~", paste(c(X_var, Z_vars), collapse = " + "))
  )
  formula_X_YZ <- as.formula(
    paste(X_var, "~", paste(c(outcome_var, Z_vars), collapse = " + "))
  )


  
  
  if ((subsample <= 0 || subsample > 1) && method != "xgboost") {
    stop("Subsample must be between 0 and 1.")
  } else if (subsample < 1) {
    data <- data[sample(nrow(data), size = round(nrow(data) * subsample)), ]
  } else  {
    data <- data
  }
  caret_method <- switch(method,
                         rf = "rf",
                         xgboost = "xgbTree",
                         svm = "svmRadial",
                         KNN = "kknn",
                         stop("Unsupported method"))


  ctrl <- suppressWarnings(caret::trainControl(method = "cv", number = folds))

  common_args <- list(
    trControl = ctrl,
    preProcess = c("center", "scale")
  )

  if (method == "xgboost") {
    # build numeric design matrices safely (handles factors)
    Xmat1 <- model.matrix(formula_Y_XZ, data)[, -1, drop = FALSE]
    y1 <- data[[outcome_var]]
    
    Xmat2 <- model.matrix(formula_X_YZ, data)[, -1, drop = FALSE]
    y2 <- data[[X_var]]
    
    d1 <- xgboost::xgb.DMatrix(data = Xmat1, label = y1)
    d2 <- xgboost::xgb.DMatrix(data = Xmat2, label = y2)
    
    params <- list(
      objective = "reg:squarederror",
      eta = eta,
      max_depth = max_depth,
      gamma = gamma,
      colsample_bytree = colsample_bytree,
      min_child_weight = min_child_weight,
      subsample = subsample,
      verbosity = 0
    )
    
    cv1 <- xgboost::xgb.cv(params = params, data = d1, nrounds = nrounds,
                           nfold = folds, metrics = "rmse", verbose = 0)
    cv2 <- xgboost::xgb.cv(params = params, data = d2, nrounds = nrounds,
                           nfold = folds, metrics = "rmse", verbose = 0)
    
    metric1 <- min(cv1$evaluation_log$test_rmse_mean)
    metric2 <- min(cv2$evaluation_log$test_rmse_mean)
  } else {

  model1 <- do.call(caret::train, c(list(
    form = formula_Y_XZ,
    data = data,
    method = caret_method
  ), common_args, list(...)))

  model2 <- do.call(caret::train, c(list(
    form = formula_X_YZ,
    data = data,
    method = caret_method
  ), common_args, list(...)))

  metric1 <- min(model1$results$RMSE, na.rm = TRUE)
  metric2 <- min(model2$results$RMSE, na.rm = TRUE)
  
}
  best_direction <- if (metric1 <= metric2) "Y ~ X | Z" else "X ~ Y | Z"

  # Using the original formula to present to user
  outcome_var <- all.vars(formula[[2]])
  rhs_vars <- all.vars(formula[[3]])
  X_var <- rhs_vars[1]
  Z_vars <- rhs_vars[-1]
  # Return the selected formula
  if (best_direction == "Y ~ X | Z") {
    final_formula <- as.formula(paste(outcome_var, "~", X_var, "|", paste(Z_vars, collapse = "+")))
  } else {
    final_formula <- as.formula(paste(X_var, "~", outcome_var, "|", paste(Z_vars, collapse = "+")))
  }
  if (verbose) {
    cat("Selected formula:", deparse(final_formula), "\n")
  }

  return(final_formula)
}
