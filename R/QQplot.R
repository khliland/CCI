#' QQ-plot for multiple testing in CCI
#'
#' @param object Object of class 'CCI'
#' @param n Number of samplings
#' @param ... Additional arguments to ggplot2
#'
#' @importFrom ggplot2 geom_qq
#' @return A QQ-plot of the p-values in ggplot2 format.
#' @seealso \code{\link{print.CCI}}, \code{\link{summary.CCI}},
#' \code{\link{plot.CCI}}, \code{\link{perm.test}}
#' @export
#'
#' @examples
#' dat <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = rnorm(100))
#' cci <- perm.test(y ~ x1 + x2, data = dat)
#' QQplot(cci, 1000)
QQplot <- function(object, ...) {

  if (!inherits(object, "CCI"))
    stop("Object must be of class 'CCI'")
  
  # All arguments from perm.test()
  null_dist <- object$null.distribution
  nperm <- object$nperm
  method <- object$method
  formula <- object$formula
  dag <- object$dag
  dag_n <- object$dag_n
  data <- object$data
  data_type <- object$data_type
  add_arguments <- object$additional_arguments
  tail <- object$tail
  
  if (!is.na(dag)) {
    if (!is.na(formula)) {
      formula = gsub("\\s+", " ", formula)
    } else if (is.na(formula)) {
      ci_statement <- impliedConditionalIndependencies(dag)[dag_n]
      names(ci_statement)[names(ci_statement) == dag_n] <- "CI" # Rename list element
      formula <- paste(ci_statement$CI$Y, " ~ ", ci_statement$CI$X, "|", paste(ci_statement$CI$Z, collapse = ", "))
    }
  } 
  
  formula <- clean_formula(formula)
  check_formula(formula)
  
  parts <- strsplit(formula, "\\|")[[1]]
  parts2 <- strsplit(parts, "\\~")[[1]]
  
  dependent1 <- parts2[1]
  dependent2 <- parts2[2]
  conditioning <- unlist(strsplit(parts[2], split = ","))
  
  test_result <- test.gen(Y = object$Y, X = object$X, Z = object$Z, data = data, 
                            data_type = data_type, method = method, nperm = nperm, permutation = FALSE, nrounds = nrounds)

  
  # Calculate p-values for each test statistic
  p_values <- sapply(test_stats, function(stat) {
    get_pvalues(unlist(null_dist), stat, parametric = object$parametric, tail = tail)
  })
  
  # Create QQ-plot
  qq_data <- data.frame(
    observed = -log10(sort(p_values)),
    expected = -log10(ppoints(length(p_values)))
  )
  
  ggobj <- ggplot(qq_data, aes(x = expected, y = observed)) +
    geom_point(color = "blue") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(title = "QQ Plot of P-values",
         x = "Expected -log10(p-value)",
         y = "Observed -log10(p-value)") +
    theme_minimal()
  
  return(ggobj)
}