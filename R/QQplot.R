#' QQ-plot for multiple testing in CCI
#'
#' @param object Object of class 'CCI'
#' @param axis.text.x Size of x-axis text
#' @param axis.text.y Size of y-axis text
#' @param strip.text.x Size of x-axis strip text
#' @param strip.text.y Size of y-axis strip text
#' @param legend.title Size of legend title
#' @param legend.text Size of legend text
#'
#' @param ... Additional arguments to pass to the \code{test.gen} function.
#'
#' @importFrom ggplot2 ggplot aes geom_qq geom_abline labs theme_minimal theme element_text
#' @importFrom stats qunif
#' @return A QQ-plot of the p-values in ggplot2 format.
#' @seealso \code{\link{print.CCI}}, \code{\link{summary.CCI}},
#' \code{\link{plot.CCI}}, \code{\link{perm.test}}
#' @export
#'
#' @examples
#' dat <- data.frame(x1 = rnorm(200), x2 = rnorm(200), y = rnorm(200))
#' cci <- CCI.test(y ~ x1 | x2,
#' data = dat,
#' nperm = 50,
#' interaction = FALSE)
#' QQplot(cci)

QQplot <- function(object, axis.text.x = 17, axis.text.y = 17, strip.text.x = 17, strip.text.y = 17, legend.text = 17, legend.title = 17, ...) {
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
  N <- nrow(data)
  metric <- object$metric
  subsample <- object$subsample
  tail <- object$tail
  parametric <- object$parametric
  p <- object$p
  degree <- object$degree
  poly <- object$poly
  interaction <- object$interaction
  additional_args <- object$additional_args

  # Ensure p and N are numeric
  if (!is.numeric(p) || !is.numeric(N)) {
    stop("p and N must be numeric values.")
  }

  if (!is.null(dag)) {
    if (!is.null(formula)) {
      formula = gsub("\\s+", " ", formula)
    } else if (is.null(formula)) {
      ci_statement <- dagitty::impliedConditionalIndependencies(dag)[dag_n]
      names(ci_statement)[names(ci_statement) == dag_n] <- "CI"
      formula <- paste(ci_statement$CI$Y, " ~ ", ci_statement$CI$X, "|", paste(ci_statement$CI$Z, collapse = ", "))
    }
  }

  formula <- clean_formula(formula)
  check_formula(formula, data)


  test_result <- test.gen(formula = formula,
                          data = data,
                          permutation = FALSE,
                          metric = metric,
                          method = method,
                          nperm = nperm,
                          subsample = subsample,
                          nrounds = nrounds,
                          N = nrow(data),
                          p = p,
                          degree = degree,
                          poly = poly,
                          interaction = interaction,
                          additional_args,
                          ...)

  test_stats <- unlist(test_result$distribution)
  p_values <- data.frame(sapply(test_stats, function(stat) {
    get_pvalues(unlist(null_dist), stat, parametric = parametric, tail = tail)
  }))
  colnames(p_values) <- c("pvalues")
  pvalues <- NULL # Dummy to avoid 'globals' warning in package test.

  ggobj <- ggplot2::ggplot(p_values, ggplot2::aes(sample = pvalues)) +
    ggplot2::geom_qq(distribution = stats::qunif, size = 0.1)  +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "blue") +
    ggplot2::labs(x = "Theoretical Quantiles", y = "Sample Quantiles",
         title = paste0("QQPlot of p-values with ", nperm, " samples"))  +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = element_text(size = axis.text.x),
                   axis.text.y = element_text(size = axis.text.y),
                   strip.text.x = element_text(size = strip.text.x),
                   strip.text.y = element_text(size = strip.text.y),
                   legend.text = element_text(size = legend.text),
                   legend.title = element_text(size = legend.title), legend.position = 'none')

  return(ggobj)
}
