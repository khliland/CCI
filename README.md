# CCI: Computational Conditional Independence testing

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/CCI)](https://CRAN.R-project.org/package=CCI)
<!-- badges: end -->

**CCI** is an R package for testing conditional independence, $Y _||_ X \mid Z$: does
$X$ carry information about $Y$ once $Z$ is known? It uses machine learning to answer this as a
prediction question. If a model for $Y$ predicts new data better with the real $X$ than with a
randomly permuted $X$, then $X$ carries information about $Y$ beyond $Z$. The null distribution is
built with permutation and Monte Carlo cross-validation, so the test needs no parametric assumptions
about the relationships between the variables.

- Works for continuous, binary and categorical variables, and mixed data.
- Detects non-linear and complex relationships.
- Built-in learners: random forest, XGBoost, support vector machines and k-nearest neighbours, or
  your own model and performance metric.
- Tools to judge the result: plots of the null distribution, QQ-plots of p-values, hyperparameter
  tuning and automatic choice of test direction.

The method is described in Thorjussen et al. (2024, [Algorithms](https://doi.org/10.3390/a17080323))
and the package in Thorjussen et al. (2026, [SoftwareX](https://doi.org/10.1016/j.softx.2026.102726)).

## Installation

From CRAN:

```r
install.packages("CCI")
```

The development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("khliland/CCI", build_vignettes = TRUE)
```

## A first test

We simulate data where $Y$ and $X$ both depend on $Z_1$ and $Z_2$, but not on each other. Then
$Y _||_ X \mid Z_1, Z_2$ is true, while $Y _||_ X \mid Z_1$ is false.

```r
library(CCI)

normal_data <- function(n) {
  Z1 <- rnorm(n)
  Z2 <- rnorm(n)
  X <- Z1 + Z2 + rnorm(n)
  Y <- Z1 + Z2 + rnorm(n)
  data.frame(Z1, Z2, X, Y)
}
set.seed(123)
dat <- normal_data(500)

summary(CCI.test(Y ~ X | Z1 + Z2, data = dat, seed = 1))
```

```
Computational Conditional Independence Test
--------------------------------------------
Method:    CCI test using rf 
Formula:   Y ~ X | Z1 + Z2 
Permutations:  160 
Metric:    RMSE 
Tail:      left 
Statistic: 1.143 
P-value:   0.7019 

MC sample:   1 
```

The p-value is large, so conditional independence is not rejected. Leaving out $Z_2$:

```r
summary(CCI.test(Y ~ X | Z1, data = dat, seed = 1))
```

```
Computational Conditional Independence Test
--------------------------------------------
Method:    CCI test using rf 
Formula:   Y ~ X | Z1 
Permutations:  160 
Metric:    RMSE 
Tail:      left 
Statistic: 1.492 
P-value:   0.006211 

MC sample:   1 
```

Here the null hypothesis is rejected. The p-value is the smallest possible with 160 Monte Carlo
samples, 1/161. `plot()` shows the null distribution together with the test statistic.

## Main features

**The formula.** `Y ~ X | Z1 + Z2` tests $Y _||_ X \mid Z_1, Z_2$. `Y ~ X + Z1 + Z2` is
the same. For an unconditional test of $Y _||_ X$, write `Y ~ X | 1` or `Y ~ X + 1`.

**Data types.** The metric is chosen from the type of $Y$: RMSE for a numeric $Y$, Cohen's Kappa
for a factor, character or logical $Y$. `metric = "LogLoss"` uses predicted class probabilities
instead. When $Z$ contains categorical variables, $X$ is permuted within their groups
(`robust = TRUE`).

**Learners.** `method = "rf"` (default), `"xgboost"`, `"svm"` or `"KNN"`. Model parameters are
passed directly, e.g. `CCI.test(Y ~ X | Z, data, method = "xgboost", eta = 0.1, max_depth = 3)`.

**Large data.** `MC_sample = "Auto"` (default) uses a share $(900/n)^{0.75}$ of the data in each Monte
Carlo sample when $n > 900$, and `method = "KNN"` is by far the fastest learner.

**Judging the result.**

```r
res <- CCI.test(Y ~ X | Z1, data = dat, parametric = TRUE)  # p-value from a normal approximation
plot(res)     # the null distribution should be unimodal
QQplot(res)   # p-values over new train/test splits
```

**Tuning and test direction.** `tune = TRUE` tunes the learner with `CCI.pretuner()` before testing,
and `choose_direction = TRUE` tests in the direction where the prediction is easiest.

**Your own model or metric.**

```r
r_squared <- function(actual, predictions) {
  1 - sum((actual - predictions)^2) / sum((actual - mean(actual))^2)
}
CCI.test(Y ~ X | Z1, data = dat, metricfunc = r_squared, tail = "right")  # higher R^2 is better

lm_rmse <- function(formula, data, train_indices, test_indices, ...) {
  fit <- lm(formula, data = data[train_indices, ])
  actual <- data[test_indices, all.vars(formula)[1]]
  sqrt(mean((predict(fit, data[test_indices, ]) - actual)^2))
}
CCI.test(Y ~ X | Z1, data = dat, mlfunc = lm_rmse, tail = "left")         # lower RMSE is better
```

## Learn more

The vignettes cover the package in detail:

| Vignette | Contents |
|---|---|
| [Getting started with CCI](vignettes/Testing-CI-with-CCI.Rmd) | The idea behind the test, reading the output, formulas, learners, data types and large data |
| [Diagnostics, tuning and test direction](vignettes/diagnostics-and-tuning.Rmd) | Null distribution plots, `nperm` and parametric p-values, `QQplot()`, tuning and `choose_direction`, with a checklist |
| [Custom models and performance metrics](vignettes/custom-models-and-metrics.Rmd) | Writing `metricfunc` and `mlfunc` functions, for regression and classification |
| [Applied examples](vignettes/applied-examples.Rmd) | Testing the conditional independencies implied by a causal DAG, and time series |

In R: `browseVignettes("CCI")`, or e.g. `vignette("Testing-CI-with-CCI", package = "CCI")`.

What has changed between versions is listed in [NEWS.md](NEWS.md).

## Citation

If you use CCI, please cite:

- Thorjussen, C. B. H., Liland, K. H., Solberg, L. E., & Måge, I. (2026). CCI: An R package for
  computational conditional independence testing. *SoftwareX*, 34, 102726.
  <https://doi.org/10.1016/j.softx.2026.102726>
- Thorjussen, C. B. H., Liland, K. H., Måge, I., & Solberg, L. E. (2024). Computational test for
  conditional independence. *Algorithms*, 17(8), 323. <https://doi.org/10.3390/a17080323>

`citation("CCI")` gives the references in BibTeX format.

## Bugs and contributions

Please report bugs and suggestions at <https://github.com/khliland/CCI/issues>. Contributions are
welcome.
