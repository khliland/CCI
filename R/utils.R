#' Check the logic of the statement
#'
#' @param formula_str Formula string
#'
#' @return NULL
#' @export
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

#' Clean formula string
#'
#' @param formula_str Formula string
#'
#' @return Cleaned formula string
#' @export
clean_formula <- function(formula_str) {
  formula_str <- gsub("\\s*~\\s*", "~", formula_str)
  formula_str <- gsub("\\s*\\|\\s*", "|", formula_str)
  formula_str <- gsub("\\s*,\\s*", ",", formula_str)
  formula_str <- gsub("\\s+", " ", formula_str)
  formula_str <- trimws(formula_str)
  return(formula_str)
}

#' P-value calculation based on null distribution and test statistic
#'
#' @param formula_str Formula string
#'
#' @return P-value
#' @export

get_pvalues <- function(dist, metric, type = c("Empirical", "Parametric")) {
  dist <- as.numeric(dist)
  metric <- as.numeric(metric)
  null_mean <- mean(dist)
  null_sd <- sd(dist)
  
  
  p_value1 <- (sum(NullDist1 <= mean_test1_metric) + 1) / (length(NullDist1) + 1)
  
  p_value2 <- if (objective %in% 'reg:squarederror') {
    (sum(NullDist2 <= mean_test2_metric) + 1) / (length(NullDist2) + 1)
  } else {
    (sum(NullDist2 >= mean_test2_metric) + 1) / (length(NullDist2) + 1)
  }
  means <- (p_value1 + p_value2)/2
  
  result <- c(p_value1, p_value2, means)
  names(result) <- c('p_value1', 'p_value2', 'mean')
  
  return(result)
}
