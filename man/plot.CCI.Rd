% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot.CCI}
\alias{plot.CCI}
\title{Plot for CCI testing}
\usage{
\method{plot}{CCI}(
  x,
  fill_color = "lightblue",
  axis.text.x = 13,
  axis.text.y = 13,
  strip.text.x = 13,
  strip.text.y = 13,
  legend.text = 13,
  legend.title = 13,
  ...
)
}
\arguments{
\item{x}{Object of class 'CCI'}

\item{fill_color}{Color for the histogram fill}

\item{axis.text.x}{Size of x-axis text}

\item{axis.text.y}{Size of y-axis text}

\item{strip.text.x}{Size of x-axis strip text}

\item{strip.text.y}{Size of y-axis strip text}

\item{legend.text}{Size of legend text}

\item{legend.title}{Size of legend title}

\item{...}{Additional arguments to ggplot2}
}
\value{
A plot of the null distribution and the test statistic in ggplot2 format.
}
\description{
Plot for CCI testing
}
\examples{
dat <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = rnorm(100))
cci <- CCI.test(y ~ x1 + x2, data = dat, interaction = FALSE)
plot(cci)
}
\seealso{
\code{\link{print.CCI}}, \code{\link{summary.CCI}},
\code{\link{plot.CCI}}, \code{\link{perm.test}}
}
