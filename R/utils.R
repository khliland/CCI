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
