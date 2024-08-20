#' QQ-plot for multiple testing in CCI
#'
#' @param object Object of class 'CCI'
#'
#' @import ggplot2 dplyr
#' @return A QQ-plot of the p-values in ggplot2 format.
#' @seealso \code{\link{print.CCI}}, \code{\link{summary.CCI}},
#' \code{\link{plot.CCI}}, \code{\link{perm.test}}
#' @export
#'
#' @examples
#' dat <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = rnorm(100))
#' cci <- CCI.test("y ~ x1 | x2", data = dat)
#' QQplot(cci)

QQplot <- function(object) {
  if (!inherits(object, "CCI")) {
    stop("Object must be of class 'CCI'")
  }

  data <- object$data
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
      ci_statement <- dagitty::impliedConditionalIndependencies(dag)[dag_n]
      names(ci_statement)[names(ci_statement) == dag_n] <- "CI"
      formula <- paste(ci_statement$CI$Y, " ~ ", ci_statement$CI$X, "|", paste(ci_statement$CI$Z, collapse = ", "))
    }
  }

  formula <- clean_formula(formula)
  check_formula(formula, data)

  dependent1 <- formula[[2]]
  dependent2 <- formula[[3]][[2]]
  conditioning <- unlist(strsplit(deparse(formula[[3]][[3]]), split = " \\+ "))

  test_result <- test.gen(
      Y = dependent1,
      X = dependent2,
      Z = conditioning,
      data = data,
      permutation = FALSE,
      data_type = data_type,
      method = method,
      nperm = nperm,
      nrounds = nrounds,
      p = p)

  test_stats <- unlist(test_result$distribution)
  p_values <- data.frame(sapply(test_stats, function(stat) {
    get_pvalues(unlist(null_dist), stat, parametric = parametric, tail = tail)
  }))
  colnames(p_values) <- c("pvalues")

  ggobj <- ggplot2::ggplot(p_values, ggplot2::aes(sample = "pvalues")) +
    ggplot2::geom_qq(distribution = stats::qunif, , size = 0.1)  +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "blue") +
    ggplot2::labs(x = "Theoretical Quantiles", y = "Sample Quantiles",
         title = paste0("QQPlot of p-values with ", nperm, " samples"))  +
    ggplot2::theme_minimal() +
    ggplot2::theme(text = element_text(size = 17), legend.position = 'none')

  return(ggobj)
}
