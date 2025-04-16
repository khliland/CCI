#' Check the formula statement
#'
#' This function verifies that all variables specified in the formula are present in the provided data frame.
#' If any variables are missing, the function will stop and return an error message listing the missing variables.
#'
#' @param formula Formula. The model formula that specifies the relationship between the dependent and independent variables.
#' @param data Data frame. The data frame in which to check for the presence of variables specified in the formula.
#'
#' @return NULL. The function is used for validation purposes and stops execution if any variables are missing.
#' @export

check_formula <- function(formula, data) {
  all_vars <- all.vars(formula)
  if (all(all_vars %in% colnames(data))) {
    # All variables are present, do nothing
  } else if (any(all_vars %in% colnames(data))) {
    # Some variables are present, but not all
    missing_vars <- all_vars[!all_vars %in% colnames(data)]
    warning("The following variables are missing from the data: ", paste(missing_vars, collapse = ", "))
  }
  else {
    # No variables are present
    stop("The formula contains variables that are not present in the data.")
  }
}

#' Clean and Reformat Formula String
#'
#' This function processes and reformats a formula string to ensure it is in the correct format for conditional independence testing.
#' The function checks if the formula uses the '+' operator for additive models and transforms it into a format that includes a conditioning variable separated by '|'.
#'
#' @param formula Formula. The model formula that specifies the relationship between the dependent and independent variables, and potentially the conditioning variables. The formula is expected to follow the format `Y ~ X + Z1 + Z2` or `Y ~ X | Z1 + Z2`.
#'
#' @return A reformatted formula in the correct format for conditional independence testing. The returned formula will either retain the original format or be transformed to include conditioning variables.
#'
#' @importFrom stats as.formula
#' @export
#' @examples
#' clean_formula(y ~ x | z + v)
#' clean_formula(y ~ x + z + v)
#' # Error: The formula is not of the right format
#' try(clean_formula(y ~ x))

clean_formula <- function(formula) {
  tryCatch({ if (formula[[3]][[1]] == "+") {
                        response <- as.character(formula[[2]])
                        predictors <- as.character(deparse(formula[[3]]))
                        split_predictors <- strsplit(predictors, " \\+ ")
                        split_predictors <- unlist(split_predictors)
                        new_formula <-  as.formula(paste(response, "~", split_predictors[1], "|", paste(split_predictors[-1], collapse = " + ")))
  } else if (formula[[3]][[1]] == "|") {
    new_formula <- formula
  } else if (formula[[3]][[1]] != "+" & formula[[3]][[1]] != "|") {
    stop("The formula is not of the right format")
  }
  return(new_formula)
  }, error = function(e) {
    stop("The formula indicate an unconditional independence statement. Ensure that you include conditioning variables.")
  })
}


#' P-value Calculation Based on Null Distribution and Test Statistic
#'
#' This function calculates p-values based on the comparison of a test statistic against a null distribution. It can perform either empirical or parametric p-value calculations and supports both left-tailed and right-tailed tests.
#'
#' @param dist Numeric vector. Represents the null distribution of the test statistic.
#' @param test_statistic Numeric. The observed test statistic for which the p-value is to be calculated.
#' @param parametric Logical. Indicates whether to calculate parametric p-values using a normal distribution. If FALSE, empirical p-values are calculated. Default is FALSE.
#' @param tail Character. Specifies whether to calculate left-tailed or right-tailed p-values. Must be either "left" or "right". Default is "left".
#'
#' @importFrom stats pnorm
#' @return Numeric. The calculated p-value.
#' @export
#'
#' @examples
#' set.seed(123)
#' null_dist <- rnorm(1000)
#' observed_stat <- 1.5
#' p_value <- get_pvalues(null_dist, observed_stat, parametric = FALSE, tail = "right")
#' print(p_value)

get_pvalues <- function(dist, test_statistic, parametric = FALSE, tail = c("left", "right")) {
  dist <- as.numeric(dist)
  test_statistic <- as.numeric(test_statistic)
  null_mean <- mean(dist)
  null_sd <- sd(dist)

  tail <- match.arg(tail)  # Ensure tail is either "left" or "right"

  pvalue <- if (parametric == FALSE) {
    if (tail == "left") {
      (sum(dist <= test_statistic) + 1) / (length(dist) + 1)
    } else if (tail == "right") {
      (sum(dist >= test_statistic) + 1) / (length(dist) + 1)
    }
  } else {
    z_value <- (test_statistic - null_mean) / null_sd
    if (tail == "right") {
      (1-stats::pnorm(z_value))
    } else if (tail == "left") {
      (stats::pnorm(z_value))
    }
  }

  return(pvalue)
}

#' Get the best parameters after tuning with CCI.tuner
#'
#'
#' @param tuned_model A model object returned from the CCI.pretuner function. This object contains the tuned parameters and other relevant information.
#'
#' @return list
#' @export
#'
#' @examples
#' set.seed(123)
#' data_generator <-  function(N){
#' Z1 <- rnorm(N,0,1)
#' Z2 <- rnorm(N,0,1)
#' X <- rnorm(N, Z1 + Z2, 1)
#' Y <- rnorm(N, Z1 + Z2, 1)
#' df <- data.frame(Z1, Z2, X, Y)
#' return(df)
#' }
#' dat <- data_generator(250)
#' tuned_model <- CCI.pretuner(formula = Y ~ X + Z1 + Z2,
#' data = dat,
#' method = 'xgboost')
#' tuned_params <- get_tuned_params(tuned_model$best_param)
#' print(tuned_params)
#'

get_tuned_params <- function(tuned_model) {
  if (tuned_model$method == 'rf') {
    return(list(mtry = tuned_model$mtry))
  } else if (tuned_model$method == 'xgboost') {
    return(list(eta = tuned_model$eta,
                max_depth = tuned_model$max_depth,
                subsample = tuned_model$subsample,
                gamma = tuned_model$gamma,
                colsample_bytree = tuned_model$colsample_bytree,
                min_child_weight = tuned_model$min_child_weight,
                nrounds = tuned_model$nrounds))
  } else if (tuned_model$method == 'nnet') {
    return(list(size = tuned_model$size,
                decay = tuned_model$decay))
  } else if (tuned_model$method == 'svm') {
    return(list(gamma = tuned_model$sigma,
                cost = tuned_model$C))
  } else if (tuned_model$method == 'gpr'){
    return(list(kpar = list(sigma = tuned_model$sigma)))
  } else {
    return(NULL)
  }
}

#' Creates polynomial terms for specified variables in a data frame
#'
#'
#' @param data Data frame. The data frame containing the variables for which polynomial terms are to be created.
#' @param Z Character vector. The names of the variables for which polynomial terms are to be created.
#' @param degree Integer. The maximum degree of polynomial terms to be created. Default is 3.
#' @param poly Logical. If TRUE, polynomial terms will be created. If FALSE, no polynomial terms will be created. Default is TRUE.
#'
#' @return list
#' @export
#'
#' @examples
#' set.seed(123)
#' data_generator <-  function(N){
#' Z1 <- rnorm(N,0,1)
#' Z2 <- rnorm(N,0,1)
#' X <- rnorm(N, Z1 + Z2, 1)
#' Y <- rnorm(N, Z1 + Z2, 1)
#' df <- data.frame(Z1, Z2, X, Y)
#' return(df)
#' }
#' dat <- data_generator(250)
#' poly_terms <- add_poly_terms(data = dat, Z = c("Z1", "Z2"), degree = 3, poly = TRUE)
#' print(poly_terms$new_terms)

add_poly_terms <- function(data, Z, degree = 3, poly = TRUE) {
  if (!poly || degree <= 1) {
    return(list(data = data, new_terms = character(0), poly = FALSE))
  }

  if (any(sapply(data[Z], is.factor))) {
    warning("Polynomial terms are not supported for categorical variables. Polynomial terms will not be included.")
    return(list(data = data, new_terms = character(0), poly = FALSE))
  }

  transformations <- lapply(2:degree, function(d) {
    eval(parse(text = paste0("~ .^", d)))
  })
  names(transformations) <- paste0("d_", 2:degree)

  data <- data %>%
    dplyr::mutate(dplyr::across(all_of(Z), transformations, .names = "{col}_{fn}"))

  new_terms <- unlist(lapply(Z, function(var) {
    sapply(2:degree, function(d) paste0(var, "_d_", d))
  }))

  return(list(data = data, new_terms = new_terms, poly = TRUE))
}

#' Creates interaction terms for specified variables in a data frame
#'
#'
#' @param data Data frame. The data frame containing the variables for which interaction terms are to be created.
#' @param Z Character vector. The names of the variables for which interaction terms are to be created.
#'
#' @return list
#' @export
#'
#' @examples
#' data_generator <-  function(N){
#' Z1 <- rnorm(N,0,1)
#' Z2 <- rnorm(N,0,1)
#' X <- rnorm(N, Z1 + Z2, 1)
#' Y <- rnorm(N, Z1 + Z2, 1)
#' df <- data.frame(Z1, Z2, X, Y)
#' return(df)
#' }
#' dat <- data_generator(250)
#' interaction_terms <- add_interaction_terms(data = dat, Z = c("Z1", "Z2"))
#' head(interaction_terms$data$Z1_int_Z2)
#'
add_interaction_terms <- function(data, Z) {
  interaction_terms <- character(0)

  if (length(Z) >= 2) {
    interaction_terms <- combn(Z, 2, FUN = function(x) {
      interaction_name <- paste0(x[1], "_int_", x[2])
      data[[interaction_name]] <<- data[[x[1]]] * data[[x[2]]]
      return(interaction_name)
    })
  }

  return(list(data = data, interaction_terms = interaction_terms))
}

#' Build an expanded formula with poly and interaction terms
#'
#' @param formula A base formula in the format Y ~ X | Z1 + Z2
#' @param poly_terms Character vector of polynomial term names
#' @param interaction_terms Character vector of interaction term names
#'
#' @return A formula object combining all terms
#' @export
#'
#' @examples
#' poly_terms <- c("Z1_d_2", "Z2_d_2")
#' interaction_terms <- c("Z1_int_Z2")
#' formula <- Y ~ X | Z1 + Z2
#' final_formula <- build_formula(formula, poly_terms, interaction_terms)
#' print(final_formula)

build_formula <- function(formula, poly_terms = NULL, interaction_terms = NULL) {
  # Clean up and parse formula
  formula <- clean_formula(formula)

  Y <- all.vars(formula)[1]
  X <- all.vars(formula[[3]][[2]])  # e.g., x1 in x1 | z1 + z2
  Z <- all.vars(formula[[3]][[3]])  # conditioning variables

  # Combine all RHS terms
  rhs_vars <- unique(c(X, Z, poly_terms, interaction_terms))
  formula_string <- paste(Y, "~", paste(rhs_vars, collapse = " + "))

  return(as.formula(formula_string))
}
