#' Check the logic of the statement
#'
#' @param formula_str Formula string
#'
#' @import dplyr
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
#' @importFrom stats pnorm
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
      stats::pnorm(z_value)
    } else {
      (1 - stats::pnorm(z_value))
    }
  }

  return(pvalue)
}


