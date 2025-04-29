#' Choose Direction for testing for the CCI test
#'
#' This function selects the best direction for the CCI test based on cross validation. For the condition Y _||_ X | Z, the function return the recommended
#' formula either Y ~ X | Z or X ~ Y | Z .
#'
#' @param formula A formula object specifying the model to be fitted.
#' @param data A data frame containing the variables specified in the formula.
#' @param method A character string specifying the method to be used for model fitting. Options include "rf" (random forest), "xgboost" (XGBoost), "nnet" (neural network), "gpr" (Gaussian process regression), and "svm" (support vector machine).
#' @param folds An integer specifying the number of folds for cross-validation. Default is 5.
#' @param data_type A character string specifying the type of data. Options include "continuous", "binary", and "categorical". Default is "continuous".
#' @param seed An integer specifying the seed for random number generation. Default is 1984.
#' @param poly Logical. If TRUE, polynomial terms of the conditioning variables are included in the model. Default is TRUE.
#' @param degree Integer. The degree of polynomial terms to include if \code{poly} is TRUE. Default is 3.
#' @param interaction Logical. If TRUE, interaction terms of the conditioning variables are included in the model. Default is TRUE.
#' @param ... Additional arguments to be passed to the model fitting function.
#'
#' @return A formula object specifying the selected model direction.
#' @export
#' @examples
#' # Example usage
#' # Load necessary libraries
#' dat <- sineGaussian_biv(N = 500, a = 2, d = 0)
#'
#' formula_init <- Y ~ X + Z1 + Z2
#'
#' chosen_formula <- choose_direction_CCI(
#'  formula = formula_init,
#'  data = dat,
#'  method = "xgboost",
#'  data_type = "continuous",
#'  folds = 5,
#'  seed = 1)
#'
#'chosen_formula


choose_direction_CCI <- function(formula,
                                 data,
                                 method = "rf",
                                 folds = 5,
                                 data_type = "continuous",
                                 seed = 1984,
                                 poly = TRUE,
                                 degree = 3,
                                 interaction = TRUE,
                                 ...) {
  set.seed(seed)

  formula <- as.formula(formula)
  outcome_var <- all.vars(formula[[2]])
  rhs_vars <- all.vars(formula[[3]])

  if (length(rhs_vars) < 2) {
    stop("Formula must have at least two variables on the right-hand side (X and Z).")
  }

  X_var <- rhs_vars[1]
  Z_vars <- rhs_vars[-1]

  # Two formulas
  formula_Y_XZ <- as.formula(
    paste(outcome_var, "~", paste(c(X_var, Z_vars), collapse = " + "))
  )
  formula_X_YZ <- as.formula(
    paste(X_var, "~", paste(c(outcome_var, Z_vars), collapse = " + "))
  )

  # Set caret method
  caret_method <- switch(method,
                         rf = "rf",
                         xgboost = "xgbTree",
                         nnet = "nnet",
                         gpr = "gaussprRadial",
                         svm = "svmRadial",
                         stop("Unsupported method"))

  ctrl <-  suppressWarnings(caret::trainControl(method = "cv", number = folds))

  model1 <- suppressWarnings(caret::train(formula_Y_XZ, data = data, method = caret_method, trControl = ctrl, ...))
  model2 <- suppressWarnings(caret::train(formula_X_YZ, data = data, method = caret_method, trControl = ctrl, ...))

  # Choose based on metric
  if (data_type == "continuous") {
    metric1 <- min(model1$results$RMSE, na.rm = TRUE)
    metric2 <- min(model2$results$RMSE, na.rm = TRUE)
    best_direction <- if (metric1 <= metric2) "Y ~ X | Z" else "X ~ Y | Z"
  } else if (data_type %in% c("binary", "categorical")) {
    metric1 <- max(model1$results$Kappa, na.rm = TRUE)
    metric2 <- max(model2$results$Kappa, na.rm = TRUE)
    best_direction <- if (metric1 >= metric2) "Y ~ X | Z" else "X ~ Y | Z"
  } else {
    stop("Unsupported data type: must be 'continuous', 'binary', or 'categorical'.")
  }

  # Return the selected formula
  if (best_direction == "Y ~ X | Z") {
    final_formula <- as.formula(paste(outcome_var, "~", X_var, "|", paste(Z_vars, collapse = "+")))
  } else {
    final_formula <- as.formula(paste(X_var, "~", outcome_var, "|", paste(Z_vars, collapse = "+")))
  }

  cat("Best direction selected:", best_direction, "\n")
  return(final_formula)
}
