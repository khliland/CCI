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
#' cci <- perm.test("y ~ x1 | x2", data = dat)
#' QQplot(cci)

QQplot <- function(object, ...) {
  if (!inherits(object, "CCI")) {
    stop("Object must be of class 'CCI'")
  }
  
  # Extracting parameters from the CCI object
  null_dist <- object$null.distribution
  nperm <- object$nperm
  nrounds <- object$nrounds
  method <- object$MLfunc
  formula <- object$formula
  dag <- object$dag
  dag_n <- object$dag_n
  data <- object$data
  N <- nrow(data)
  data_type <- object$data_type
  tail <- object$tail
  parametric <- object$parametric
  p <- object$p
  additional_args <- object$additional_args
  
  if (!is.na(dag)) {
    if (!is.na(formula)) {
      formula = gsub("\\s+", " ", formula)
    } else if (is.na(formula)) {
      ci_statement <- impliedConditionalIndependencies(dag)[dag_n]
      names(ci_statement)[names(ci_statement) == dag_n] <- "CI"  
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
  
  test_result <- do.call(test.gen, c(
    list(
      Y = dependent1,
      X = dependent2,
      Z = conditioning,
      data = data,
      permutation = FALSE,
      data_type = data_type,
      method = method,
      nperm = nperm,
      nrounds = nrounds,
      p = p,
      ...
    ),
    additional_args
  ))
  

  test_stats <- unlist(test_result$distribution)
  
   
  p_values <- data.frame(sapply(test_stats, function(stat) {
    get_pvalues(unlist(null_dist), stat, parametric = parametric, tail = tail)
  }))
  colnames(p_values) <- c("pvalues")
  
  ggobj <- ggplot(p_values, aes(sample = pvalues)) +
    geom_qq(distribution = stats::qunif, , size = 0.1)  +
    geom_abline(slope = 1, intercept = 0, color = "blue") +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles",
         title = paste0("QQPlot of p-values with ", nperm, " samples"))  +
    theme_minimal() +
    theme(text = element_text(size = 17), legend.position = 'none')
  
  return(ggobj)
}
