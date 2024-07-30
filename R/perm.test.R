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
#' @export
#' @seealso \code{\link{print.CCI}}, \code{\link{summary.CCI}},
#' \code{\link{plot.CCI}}, \code{\link{QQplot}}
#'
#' @examples
#' set.seed(123)
#' data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100), x4 = rnorm(100), y = rnorm(100))
#' perm.test(y ~ x1 | x2, data = data)
#' formula <- "y ~ x1 | x2, x3, x4"

library(dplyr)
library(xgboost)
library(caret)
perm.test <- function(formula = NA, 
                      data, 
                      p = 0.825, 
                      nperm = 600, 
                      dag = NA, 
                      dag_n = 1, 
                      data_type = "continuous", 
                      ...) {
  
  if (is.null(data)) {
    stop("Please provide some data")
  }
  
  if (is.na(formula) & is.na(dag)) {
    stop("Formula and dag object is missing")
  } 
  
  if (is.na(formula) & class(dag) != 'dagitty') {
    stop("DAG needs to be of class dagitty.")
  } 
  
  if (class(dag) == 'dagitty' & is.na(formula)) {
    ci_statement <- impliedConditionalIndependencies(dag)[dag_n]
    names(ci_statement)[names(ci_statement) == dag_n] <- "CI" # Rename list element
    formula <- paste(ci_statement$CI$Y, " ~ ", ci_statement$CI$X, "|", paste(ci_statement$CI$Z, collapse = ", "))
    
  } else if (!is.na(formula)) {
    formula = gsub("\\s+", " ", formula)
  }
  formula <- clean_formula(formula)
  check_formula(formula)
  
  parts <- strsplit(formula, "\\|")[[1]]
  parts2 <- strsplit(parts, "\\~")[[1]]
  
  dependent1 <- parts2[1]
  dependent2 <- parts2[2]
  conditioning <- unlist(strsplit(parts[2], split = ","))
  
  # Create null distribution using Nullfunc
  preds <- Nullfunc(Y = dependent1, X = dependent2, Z = conditioning, data_type = data_type, data = data, method, ...)
  
  # Calculate empirical p-value
  # Calculate parametric p-value(s)
  
  
  # Gather everything in "obj"
  obj <- list(status  = "Not implemented yet!",
              MLfunc  = MLfunc,
              MLname  = deparse(substitute(MLfunc)),
              data    = data,
              formula = formula,
              dag_n   = dag_n,
              predictions = preds)
  class(obj) <- c("CCI", "list")
  obj
}

# NullFunc() is the null distribution generator function
Nullfunc <- function(Y, X, Z, data, data_type = "continuous", method = "lm", nperm = 500, p = 0.825, N = nrow(data), poly = TRUE, degree = 3, ...) {
    # Permutation of the "other" variable
    data <- data %>% mutate(!!X := sample(!!sym(X)))    
    
    # Create formula for the prediction 
    if (poly == TRUE & degree < 1) {
      stop("Degree of 0 or less is not allowed")
    }
    # Setting 'poly == true' creates nth degree terms of conditioned variables
    if (poly == TRUE & degree > 1){
      transformations <- lapply(2:degree, function(d) {
        eval(parse(text = paste0("~ .^", d)))
      })
      names(transformations) <- paste0("d_", 2:degree)
      
      data <- data %>%
        mutate(across(all_of(Z), transformations, .names = "{col}_{fn}"))
     
      new_terms <- unlist(lapply(Z, function(var) {
        sapply(2:degree, function(d) {
          paste0(var, "_d_", d)
        })
      }))
      formula <- as.formula(paste(Y, " ~ ", X, " + ", paste(Z, collapse = " + "), " + ",paste(new_terms, collapse = " + ")))
    } 
    else {
      formula <- as.formula(paste(Y, " ~ ", X, " + ", paste(Z, collapse = "+")))
    }
    
  # Create nperm number of Monte Carlo Cross Validation sets
  train_indices <- matrix(NA, nrow = nperm, ncol = round(p * N))
  for (i in 1:nperm) {
    if (data_type == "continuous") {
      inTraining <- sample(1:nrow(data), size = floor(p * N), replace = F)
      train_indices[i, ]  <- inTraining
    } else if (data_type %in% c('binary', 'categorical')) {
      inTraining <- caret::createDataPartition(y = factor(data[[Y]]), p = p, list = FALSE)
      train_indices[i, ]  <- inTraining
    }
  }
  # Initialize a matrix for storing of results
  null <- matrix(NA, nrow = nperm, ncol = 1)

  # If statement of all the ML methods one can use to to computational test 
  for (iteration in 1:nperm) {
    if (method == "lm" & data_type == "continuous") {
      model <- glm(formula = formula, data = data, family = gaussian(link = "identity"), subset = train_indices[iteration,])
      testing <- data[-train_indices[iteration,],]
      pred <- predict.glm(model, newdata = testing)
      actual <- testing[[all.vars(formula)[1]]]  
      null[iteration] <- sqrt(mean((pred - actual)^2)) # Storing RMSE as performance metric
      
    } 
    else if (method == "lm" & data_type == "binary") {
      model <- glm(formula, data = data, family = binomial(link = "logit"), subset = train_indices[iteration,])
      testing <- data[-train_indices[iteration,],]
      pred <- predict.glm(model, newdata = testing, type = "response")
      actual <- testing[[all.vars(formula)[1]]]
      
      # Convert probabilities to binary class predictions
      pred_class <- ifelse(pred > 0.5, 1, 0)
      
      # Calculate Kappa score
      cm <- caret::confusionMatrix(factor(pred_class), factor(actual))
      null[iteration] <- cm$overall["Kappa"] # Storing Kappa score as performance metric
      
    } 
    else if (method == "lm" & data_type == "multinomial") {
      model <- nnet::multinom(formula, data = data, subset = train_indices[iteration,])
      testing <- data[-train_indices[iteration,],]
      pred <- predict(model, newdata = testing)
      actual <- testing[[all.vars(formula)[1]]]
      
      # Calculate Kappa score
      cm <- caret::confusionMatrix(factor(pred), factor(actual))
      null[iteration] <- cm$overall["Kappa"] # Storing Kappa score as performance metric
    } else if (method == "xgboost" & data_type == "continuous") {
      
    }
  }
  
  return(null)
}

# Function to check the logic of statement
check_formula <- function(formula_str) {
  if (grepl("\\|", formula_str)) {
    parts <- strsplit(formula_str, "\\|")[[1]]
    if (length(parts) < 2 || nchar(trimws(parts[2])) == 0) {
      warning("The independence statement is unconditional.")
    } else {
      variables_after_pipe <- trimws(parts[2])
      if (variables_after_pipe == "") {
        warning("The independence statement is unconditional.")
      }
    }
  } else {
    warning("The formula does not contain the '|' symbol.")
  }
}

# Function to clean formula string
clean_formula <- function(formula_str) {
  formula_str <- gsub("\\s*~\\s*", "~", formula_str)
  formula_str <- gsub("\\s*\\|\\s*", "|", formula_str)
  formula_str <- gsub("\\s*,\\s*", ",", formula_str)
  formula_str <- gsub("\\s+", " ", formula_str)
  formula_str <- trimws(formula_str)
  return(formula_str)
}
