#' QQ-plot for multiple testing in CCI
#'
#' @param object Object of class 'CCI'
#' @param title.size Size of the plot title
#' @param axis.text.x Size of x-axis text
#' @param axis.text.y Size of y-axis text
#' @param strip.text.x Size of x-axis strip text
#' @param strip.text.y Size of y-axis strip text
#' @param legend.title Size of legend title
#' @param legend.text Size of legend text
#' @param axis.title.x Size of x-axis title
#' @param axis.title.y Size of y-axis title
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
#' dat <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = rnorm(100))
#' cci <- CCI.test(y ~ x1 | x2,
#' data = dat,
#' nperm = 25,
#' interaction = FALSE)
#' QQplot(cci)

QQplot <- function(object, 
                   title.size = 14,
                   axis.text.x = 13, 
                   axis.text.y = 13, 
                   strip.text.x = 13, 
                   strip.text.y = 13, 
                   legend.text = 13, 
                   legend.title = 13,
                   axis.title.x = 13,
                   axis.title.y = 13,
                   ...) {
  if (!inherits(object, "CCI")) {
    stop("Object must be of class 'CCI'")
  }

  data <- object$data
  null_dist <- object$null.distribution
  nperm <- object$nperm
  nrounds <- object$nrounds
  method <- object$MLfunc
  formula <- object$formula
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

  formula = as.formula(formula)
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
    gggplot2::theme(axis.text.x = element_text(size = axis.text.x),
                    axis.text.y = element_text(size = axis.text.y),
                    strip.text.x = element_text(size = strip.text.x),
                    strip.text.y = element_text(size = strip.text.y),
                    legend.text = element_text(size = legend.text),
                    legend.title = element_text(size = legend.title),
                    axis.title.x = element_text(size = axis.title.x),
                    axis.title.y = element_text(size = axis.title.y),
                    plot.title = element_text(size = title.size, face = "bold"), 
                    legend.position = 'none')

  return(ggobj)
}
