#' Check the logic of the statement
#'
#' @param formula_str Formula string
#'
#' @return NULL
#' @export
check_formula <- function(formula, data) {
  all_vars <- all.vars(formula)
  if (all(all_vars %in% colnames(data))) {
    cat("All variables are present in the data.\n")
  } else {
    missing_vars <- all_vars[!all_vars %in% colnames(data)]
    stop("The following variables are missing from the data: ", paste(missing_vars, collapse = ", "))
  }
  }


#' #' Clean formula string
#' #'
#' #' @param formula Formula
#' #'
#' #' @return Cleaned formula in the right format
#' #' @export

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


#' P-value calculation based on null distribution and test statistic
#'
#' @param dist A vector representing the null-distribution
#' @param test_statistic The test statistic
#' @param parametric Logical, whether the perform to calculate parametric p-values
#' @param tail Indicator, whether to calculate left or right tailed p-values, depends on the performance metric being used
#'
#' @return P-value
#' @export

get_pvalues <- function(dist, test_statistic, parametric = FALSE, tail = c("left", "right")) {
  dist <- as.numeric(dist)
  test_statistic <- as.numeric(test_statistic)
  null_mean <- mean(dist)
  null_sd <- sd(dist)

  tail <- match.arg(tail)  # Ensure tail is either "left" or "right"

  pvalue <- if (parametric == FALSE) {
    if (tail == "left") {
      (sum(dist <= test_statistic) + 1) / (length(dist) + 1)
    } else {
      (sum(dist >= test_statistic) + 1) / (length(dist) + 1)
    }
  } else {
    z_value <- (test_statistic - null_mean) / null_sd
    if (tail == "left") {
      pnorm(z_value)
    } else {
      (1 - pnorm(z_value))
    }
  }

  return(pvalue)
}

#' Calculation of log loss for categorical outcome
#'
#' @param actual The observed categorical (factor) outcome variable.
#' @param predicted The predicted probabilities for each category.
#' @param all_levels A vector of all possible levels (categories) of the outcome variable.
#' @return Log loss of classification model
#' @export

multi_class_log_loss <- function(actual, predicted, all_levels) {
  # Ensure actual is a factor
  actual <- factor(actual, levels = all_levels)
  actual_matrix <- model.matrix(~ actual - 1)
  predicted <- pmax(pmin(predicted, 1 - eps), eps)
  log_loss <- -sum(actual_matrix * log(predicted)) / nrow(predicted)
  return(log_loss)
}

