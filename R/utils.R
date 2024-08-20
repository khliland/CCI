#' Check the formula statement
#'
#' This function verifies that all variables specified in the formula are present in the provided data frame.
#' If any variables are missing, the function will stop and return an error message listing the missing variables.
#'
#' @param formula Formula. The model formula that specifies the relationship between the dependent and independent variables.
#' @param data Data frame. The data frame in which to check for the presence of variables specified in the formula.
#'
#' @import dplyr
#' @return NULL. The function is used for validation purposes and stops execution if any variables are missing.
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
#' clean_formula(y ~ x)

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
    if (tail == "right") {
      (sum(dist >= test_statistic) + 1) / (length(dist) + 1)
    } else if (tail == "left") {
      (sum(dist <= test_statistic) + 1) / (length(dist) + 1)
    }
  } else {
    z_value <- (test_statistic - null_mean) / null_sd
    if (tail == "right") {
      (1 - stats::pnorm(z_value))
    } else if (tail == "left") {
      (stats::pnorm(z_value))
    }
  }

  return(pvalue)
}


