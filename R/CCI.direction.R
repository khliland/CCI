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
#' @param poly Logical. If TRUE, polynomial terms of the conditioning variables are included in the model. Default is TRUE.
#' @param degree Integer. The degree of polynomial terms to include if \code{poly} is TRUE. Default is 3.
#' @param interaction Logical. If TRUE, interaction terms of the conditioning variables are included in the model. Default is TRUE.
#' @param ... Additional arguments to be passed to the model fitting function.
#'
#' @return A formula object specifying the selected model direction.
#' @importFrom caret train trainControl randomForest xgboost svm
#' @export
#' @examples
#' # Example usage
#' dat <- SineGaussianBiv(N = 100, a = 2)
#'
#' formula_init <- Y ~ X + Z1 + Z2
#'
#' chosen_formula <- CCI.direction(
#'  formula = formula_init,
#'  data = dat,
#'  method = "rf",
#'  folds = 2,
#'  seed = 1)
#'
#' chosen_formula


CCI.direction <- function(formula,
                                 data,
                                 method = "rf",
                                 folds = 4,
                                 subsample = 1,
                                 poly = TRUE,
                                 degree = 3,
                                 interaction = TRUE,
                                 ...) {
  cat("Deciding best direction, Y ~ X | Z or X ~ Y | Z...\n")

  org_formula <- formula
  # Parse formula
  Y <- all.vars(formula)[1]
  X <- all.vars(formula[[3]])[1]
  Z <- all.vars(formula[[3]])[-1]
  if (length(Z) == 0) {
    Z <- NULL
  }
  if (!is.null(Z) && any(sapply(data[Z], is.factor))) {
    poly <- FALSE
  }

  # Add polynomial and interaction terms
  poly_result <- add_poly_terms(data, Z, degree = degree, poly = poly)
  data <- poly_result$data
  poly_terms <- poly_result$new_terms

  if (interaction && !is.null(Z)) {
    interaction_result <- add_interaction_terms(data, Z)
    data <- interaction_result$data
    interaction_terms <- interaction_result$interaction_terms
  } else {
    interaction_terms <- NULL
  }

  formula <- build_formula(formula, poly_terms, interaction_terms)

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

  # Make any data type into numeric
  data <- data.frame(lapply(data, function(x) {
    if (is.factor(x)) {
      as.numeric(as.character(x))
    } else {
      x
    }
  }))
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
                         stop("Unsupported method"))


  ctrl <-  suppressWarnings(caret::trainControl(method = "cv", number = folds))

  model1 <- caret::train(formula_Y_XZ, data = data, method = caret_method, trControl = ctrl,  verbosity = 0, subsample = subsample, ...)
  model2 <- caret::train(formula_X_YZ, data = data, method = caret_method, trControl = ctrl,  verbosity = 0, subsample = subsample, ...)

  metric1 <- min(model1$results$RMSE, na.rm = TRUE)
  metric2 <- min(model2$results$RMSE, na.rm = TRUE)
  best_direction <- if (metric1 <= metric2) "Y ~ X | Z" else "X ~ Y | Z"

  # Using the original formula to present to user
  outcome_var <- all.vars(org_formula[[2]])
  rhs_vars <- all.vars(org_formula[[3]])
  X_var <- rhs_vars[1]
  Z_vars <- rhs_vars[-1]
  # Return the selected formula
  if (best_direction == "Y ~ X | Z") {
    final_formula <- as.formula(paste(outcome_var, "~", X_var, "|", paste(Z_vars, collapse = "+")))
  } else {
    final_formula <- as.formula(paste(X_var, "~", outcome_var, "|", paste(Z_vars, collapse = "+")))
  }

  cat("Best direction selected:", best_direction, "\n")
  return(final_formula)
}
