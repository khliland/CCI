Sys.setenv(TMPDIR = tempdir())
devtools::check()
devtools::check_win_devel()
devtools::document()
devtools::clean_dll()
devtools::build()
devtools::build_vignettes
# remove.packages("CCI")
devtools::install()

library(CCI)
library(dplyr)
library(CIsimdata)
# Testing of functions
debug(test.gen)
set.seed(123)
data <- NormalData(500)
result <- CCI.test(Y ~ X | Z1 + Z2,
                   data = data,
                   seed = 1)

summary(result)
result <- CCI.test(Y ~ X | Z1 ,
                   data = data,
                   seed = 1)
summary(result)
QQplot(result)
plot(result)

data <- NormalData(2500)
result <- CCI.test(Y ~ X | Z1,
                   data = data,
                   seed = 1,
                   method = "KNN")
summary(result)

result <- CCI.test(Y ~ X | Z1,
                   data = data,
                   seed = 1,
                   method = "KNN")
summary(result)

result <- CCI.test(Y ~ X | Z1 + Z2,
                   data = data,
                   seed = 1,
                   method = "KNN")
summary(result)

result <- CCI.test(Y ~ X | Z1,
                   data = data,
                   seed = 1,
                   method = "svm")
summary(result)

data <- BinaryData(500)
result <- CCI.test(Y ~ X | Z1 + Z2,
                   data = data,
                   seed = 1,
                   metric = "Kappa")
plot(result)
summary(result)

result <- CCI.test(Y ~ X | Z1 + Z2,
                   data = data,
                   seed = 1,
                   metric = "LogLoss")
plot(result)
summary(result)

result <- CCI.test(Y ~ X | Z1 + Z2,
                   data = data,
                   seed = 1,
                   method = 'KNN',
                   metric = "LogLoss")
plot(result)
summary(result)



result <- CCI.test(Y ~ X | Z1,
                   data = data,
                   seed = 1,
                   method = "xgboost",
                   metric = "LogLoss")
plot(result)
summary(result)

result <- CCI.test(Y ~ X | Z1,
                   data = data,
                   seed = 1,
                   method = "KNN",
                   metric = "Kappa")
plot(result)
summary(result)

result <- CCI.test(Y ~ X | Z1 + Z2,
                   data = data,
                   seed = 1,
                   method = "KNN",
                   metric = "Kappa")
summary(result)

result <- CCI.test(Y ~ X | Z1  + Z2,
                   data = data,
                   seed = 1,
                   method = "svm",
                   metric = "Kappa")
summary(result)
result <- CCI.test(Y ~ X | Z1,
                   data = data,
                   seed = 1,
                   method = "svm",
                   metric = "Kappa")
summary(result)

####################################################################
run_scenarios <- function(n = 500, seed_data = 123, seed_test = 1, method = NULL) {
  set.seed(seed_data)
  d0 <- NormalData(n)
  
  # Ensure we have enough Z's; create if missing
  if (!"Z1" %in% names(d0)) d0$Z1 <- rnorm(n)
  if (!"Z2" %in% names(d0)) d0$Z2 <- rnorm(n)
  if (!"Z3" %in% names(d0)) d0$Z3 <- rnorm(n)
  if (!"Z4" %in% names(d0)) d0$Z4 <- rnorm(n)
  
  scenarios <- list(
    
    # S1: baseline (your example)
    S1_CI_given_Z1Z2 = list(
      data = d0,
      formula = Y ~ X | Z1 + Z2
    ),
    
    # S2: unconditional independence (should NOT reject)
    S2_unconditional_independence = list(
      data = within(d0, { Y <- rnorm(n) }),
      formula = Y ~ X | 1
    ),
    
    # S3: unconditional dependence (should reject)
    S3_unconditional_dependence = list(
      data = within(d0, { Y <- 0.8 * X + rnorm(n, sd = 0.5) }),
      formula = Y ~ X | 1
    ),
    
    # S4: dependent but conditionally independent (classic: X <- Z -> Y)
    # After conditioning on Z1, X ⟂ Y | Z1 should hold
    S4_CI_after_conditioning_on_common_cause = list(
      data = within(d0, {
        X <- 0.9 * Z1 + rnorm(n, sd = 0.5)
        Y <- -0.7 * Z1 + rnorm(n, sd = 0.5)
      }),
      formula = Y ~ X | Z1
    ),
    
    # S5: dependence remains even after conditioning (direct effect)
    S5_not_CI_due_to_direct_effect = list(
      data = within(d0, {
        X <- 0.9 * Z1 + rnorm(n, sd = 0.5)
        Y <- 0.6 * X - 0.7 * Z1 + rnorm(n, sd = 0.5)
      }),
      formula = Y ~ X | Z1
    ),
    
    # S6: nonlinear dependence (should reject with flexible methods)
    S6_nonlinear_dependence = list(
      data = within(d0, {
        Y <- sin(X) + 0.2 * rnorm(n)
      }),
      formula = Y ~ X | 1
    ),
    
    # S7: nonlinear but conditionally independent via Z1 (X=Z+e, Y=f(Z)+e)
    S7_nonlinear_CI_given_Z1 = list(
      data = within(d0, {
        X <- Z1 + rnorm(n, sd = 0.3)
        Y <- sin(Z1) + rnorm(n, sd = 0.3)
      }),
      formula = Y ~ X | Z1
    ),
    
    # S8: "bad conditioning" / irrelevant Z’s (should behave like unconditional)
    S8_irrelevant_conditioning = list(
      data = within(d0, { Y <- 0.8 * X + rnorm(n, sd = 0.5) }),
      formula = Y ~ X | Z3 + Z4
    )
  )
  
  # Run
  out <- lapply(names(scenarios), function(nm) {
    sc <- scenarios[[nm]]
    if (is.null(method)) {
      res <- CCI.test(sc$formula, data = sc$data, seed = seed_test)
    } else {
      res <- CCI.test(sc$formula, data = sc$data, seed = seed_test, method = method)
    }
    list(name = nm, formula = deparse(sc$formula), result = res)
  })
  names(out) <- names(scenarios)
  out
}
# Example run
results_rf <- run_scenarios(method = "rf")
results_xgb <- run_scenarios(method = "xgboost")
results_knn <- run_scenarios(method = "KNN")
results_svm <- run_scenarios(method = "svm")

pvals_rf <- sapply(results_rf, function(x) x$result$p.value)
pvals_xgb <- sapply(results_xgb, function(x) x$result$p.value)
pvals_knn <- sapply(results_knn, function(x) x$result$p.value)
pvals_svm <- sapply(results_svm, function(x) x$result$p.value)

alpha <- 0.05
should_reject <- c("S3_unconditional_dependence",
                   "S5_not_CI_due_to_direct_effect",
                   "S6_nonlinear_dependence",
                   "S8_irrelevant_conditioning")

should_not <- setdiff(names(pvals_rf), should_reject)

data.frame(
  scenario = names(pvals_rf),
  p_value = as.numeric(pvals_rf),
  reject = pvals_rf < alpha,
  expected = ifelse(names(pvals_rf) %in% should_reject, "reject", "not reject")
)

should_not <- setdiff(names(pvals_xgb), should_reject)

data.frame(
  scenario = names(pvals_xgb),
  p_value = as.numeric(pvals_xgb),
  reject = pvals_xgb < alpha,
  expected = ifelse(names(pvals_xgb) %in% should_reject, "reject", "not reject")
)
should_not <- setdiff(names(pvals_knn), should_reject)
data.frame(
  scenario = names(pvals_knn),
  p_value = as.numeric(pvals_knn),
  reject = pvals_knn < alpha,
  expected = ifelse(names(pvals_knn) %in% should_reject, "reject", "not reject")
)
should_not <- setdiff(names(pvals_svm), should_reject)
data.frame(
  scenario = names(pvals_svm),
  p_value = as.numeric(pvals_svm),
  reject = pvals_svm < alpha,
  expected = ifelse(names(pvals_svm) %in% should_reject, "reject", "not reject")
)
# End of tests

####################################################################
run_scenarios_hardcase <- function(n = 500, seed_data = 123, seed_test = 1, method = NULL) {
  set.seed(seed_data)
  d0 <- HardCase(n)
  
  # Ensure we have enough Z's; create if missing
  if (!"Z1" %in% names(d0)) d0$Z1 <- rnorm(n)
  if (!"Z2" %in% names(d0)) d0$Z2 <- rnorm(n)
  if (!"Z3" %in% names(d0)) d0$Z3 <- rnorm(n)
  if (!"Z4" %in% names(d0)) d0$Z4 <- rnorm(n)
  
  scenarios <- list(
    
    # S1: baseline 
    S1_CI_given_Z1Z2 = list(
      data = d0,
      formula = Y ~ X | Z1 + Z2
    ),
    
    # S2: unconditional independence (should NOT reject)
    S2_unconditional_independence = list(
      data = within(d0, { Y <- rnorm(n) }),
      formula = Y ~ X | 1
    ),
    
    # S3: unconditional dependence (should reject)
    S3_unconditional_dependence = list(
      data = within(d0, { Y <- 0.8 * X + rnorm(n, sd = 0.5) }),
      formula = Y ~ X | 1
    ),
    
    # S4: dependent but conditionally independent (classic: X <- Z -> Y)
    # After conditioning on Z1, X ⟂ Y | Z1 should hold
    S4_CI_after_conditioning_on_common_cause = list(
      data = within(d0, {
        X <- 0.9 * Z1 + rnorm(n, sd = 0.5)
        Y <- -0.7 * Z1 + rnorm(n, sd = 0.5)
      }),
      formula = Y ~ X | Z1
    ),
    
    # S5: dependence remains even after conditioning (direct effect)
    S5_not_CI_due_to_direct_effect = list(
      data = within(d0, {
        X <- 0.9 * Z1 + rnorm(n, sd = 0.5)
        Y <- 0.6 * X - 0.7 * Z1 + rnorm(n, sd = 0.5)
      }),
      formula = Y ~ X | Z1
    ),
    
    # S6: nonlinear dependence (should reject with flexible methods)
    S6_nonlinear_dependence = list(
      data = within(d0, {
        Y <- sin(X) + 0.2 * rnorm(n)
      }),
      formula = Y ~ X | 1
    ),
    
    # S7: nonlinear but conditionally independent via Z1 (X=Z+e, Y=f(Z)+e)
    S7_nonlinear_CI_given_Z1 = list(
      data = within(d0, {
        X <- Z1 + rnorm(n, sd = 0.3)
        Y <- sin(Z1) + rnorm(n, sd = 0.3)
      }),
      formula = Y ~ X | Z1
    ),
    
    # S8: "bad conditioning" / irrelevant Z’s (should behave like unconditional)
    S8_irrelevant_conditioning = list(
      data = within(d0, { Y <- 0.8 * X + rnorm(n, sd = 0.5) }),
      formula = Y ~ X | Z3 + Z4
    )
  )
  
  # Run
  out <- lapply(names(scenarios), function(nm) {
    sc <- scenarios[[nm]]
    if (is.null(method)) {
      res <- CCI.test(sc$formula, data = sc$data, seed = seed_test)
    } else {
      res <- CCI.test(sc$formula, data = sc$data, seed = seed_test, method = method)
    }
    list(name = nm, formula = deparse(sc$formula), result = res)
  })
  names(out) <- names(scenarios)
  out
}
# Example run
results_rf <- run_scenarios_hardcase(method = "rf")
results_xgb <- run_scenarios_hardcase(method = "xgboost")
results_knn <- run_scenarios_hardcase(method = "KNN")
results_svm <- run_scenarios_hardcase(method = "svm")

pvals_rf <- sapply(results_rf, function(x) x$result$p.value)
pvals_xgb <- sapply(results_xgb, function(x) x$result$p.value)
pvals_knn <- sapply(results_knn, function(x) x$result$p.value)
pvals_svm <- sapply(results_svm, function(x) x$result$p.value)

alpha <- 0.05
should_reject <- c("S3_unconditional_dependence",
                   "S5_not_CI_due_to_direct_effect",
                   "S6_nonlinear_dependence",
                   "S8_irrelevant_conditioning")

should_not <- setdiff(names(pvals_rf), should_reject)

data.frame(
  scenario = names(pvals_rf),
  p_value = as.numeric(pvals_rf),
  reject = pvals_rf < alpha,
  expected = ifelse(names(pvals_rf) %in% should_reject, "reject", "not reject")
)

should_not <- setdiff(names(pvals_xgb), should_reject)

data.frame(
  scenario = names(pvals_xgb),
  p_value = as.numeric(pvals_xgb),
  reject = pvals_xgb < alpha,
  expected = ifelse(names(pvals_xgb) %in% should_reject, "reject", "not reject")
)
should_not <- setdiff(names(pvals_knn), should_reject)
data.frame(
  scenario = names(pvals_knn),
  p_value = as.numeric(pvals_knn),
  reject = pvals_knn < alpha,
  expected = ifelse(names(pvals_knn) %in% should_reject, "reject", "not reject")
)
should_not <- setdiff(names(pvals_svm), should_reject)
data.frame(
  scenario = names(pvals_svm),
  p_value = as.numeric(pvals_svm),
  reject = pvals_svm < alpha,
  expected = ifelse(names(pvals_svm) %in% should_reject, "reject", "not reject")
)
# End of tests

run_scenarios_SineGaussianNoise <- function(n = 700, seed_data = 123, seed_test = 1, method = NULL) {
  set.seed(seed_data)
  d0 <- SineGaussianNoise(n)
  
  # Ensure we have enough Z's; create if missing
  if (!"Z1" %in% names(d0)) d0$Z1 <- rnorm(n)
  if (!"Z2" %in% names(d0)) d0$Z2 <- rnorm(n)
  if (!"Z3" %in% names(d0)) d0$Z3 <- rnorm(n)
  if (!"Z4" %in% names(d0)) d0$Z4 <- rnorm(n)
  
  scenarios <- list(
    
    # S1: baseline 
    S1_CI_given_Z1Z2 = list(
      data = d0,
      formula = Y ~ X | Z1 + Z2
    ),
    
    # S2: unconditional independence (should NOT reject)
    S2_unconditional_independence = list(
      data = within(d0, { Y <- rnorm(n) }),
      formula = Y ~ X | 1
    ),
    
    # S3: unconditional dependence (should reject)
    S3_unconditional_dependence = list(
      data = within(d0, { Y <- 0.8 * X + rnorm(n, sd = 0.5) }),
      formula = Y ~ X | 1
    ),
    
    # S4: dependent but conditionally independent (classic: X <- Z -> Y)
    # After conditioning on Z1, X ⟂ Y | Z1 should hold
    S4_CI_after_conditioning_on_common_cause = list(
      data = within(d0, {
        X <- 0.9 * Z1 + rnorm(n, sd = 0.5)
        Y <- -0.7 * Z1 + rnorm(n, sd = 0.5)
      }),
      formula = Y ~ X | Z1
    ),
    
    # S5: dependence remains even after conditioning (direct effect)
    S5_not_CI_due_to_direct_effect = list(
      data = within(d0, {
        X <- 0.9 * Z1 + rnorm(n, sd = 0.5)
        Y <- 0.6 * X - 0.7 * Z1 + rnorm(n, sd = 0.5)
      }),
      formula = Y ~ X | Z1
    ),
    
    # S6: nonlinear dependence (should reject with flexible methods)
    S6_nonlinear_dependence = list(
      data = within(d0, {
        Y <- sin(X) + 0.2 * rnorm(n)
      }),
      formula = Y ~ X | 1
    ),
    
    # S7: nonlinear but conditionally independent via Z1 (X=Z+e, Y=f(Z)+e)
    S7_nonlinear_CI_given_Z1 = list(
      data = within(d0, {
        X <- Z1 + rnorm(n, sd = 0.3)
        Y <- sin(Z1) + rnorm(n, sd = 0.3)
      }),
      formula = Y ~ X | Z1
    ),
    
    # S8: "bad conditioning" / irrelevant Z’s (should behave like unconditional)
    S8_irrelevant_conditioning = list(
      data = within(d0, { Y <- 0.8 * X + rnorm(n, sd = 0.5) }),
      formula = Y ~ X | Z3 + Z4
    )
  )
  
  # Run
  out <- lapply(names(scenarios), function(nm) {
    sc <- scenarios[[nm]]
    if (is.null(method)) {
      res <- CCI.test(sc$formula, data = sc$data, seed = seed_test)
    } else {
      res <- CCI.test(sc$formula, data = sc$data, seed = seed_test, method = method)
    }
    list(name = nm, formula = deparse(sc$formula), result = res)
  })
  names(out) <- names(scenarios)
  out
}
# Example run
results_rf <- run_scenarios_SineGaussianNoise(method = "rf")
results_xgb <- run_scenarios_SineGaussianNoise(method = "xgboost")
results_knn <- run_scenarios_SineGaussianNoise(method = "KNN")
results_svm <- run_scenarios_SineGaussianNoise(method = "svm")

pvals_rf <- sapply(results_rf, function(x) x$result$p.value)
pvals_xgb <- sapply(results_xgb, function(x) x$result$p.value)
pvals_knn <- sapply(results_knn, function(x) x$result$p.value)
pvals_svm <- sapply(results_svm, function(x) x$result$p.value)

alpha <- 0.05
should_reject <- c("S3_unconditional_dependence",
                   "S5_not_CI_due_to_direct_effect",
                   "S6_nonlinear_dependence",
                   "S8_irrelevant_conditioning")

should_not <- setdiff(names(pvals_rf), should_reject)

data.frame(
  scenario = names(pvals_rf),
  p_value = as.numeric(pvals_rf),
  reject = pvals_rf < alpha,
  expected = ifelse(names(pvals_rf) %in% should_reject, "reject", "not reject")
)

should_not <- setdiff(names(pvals_xgb), should_reject)

data.frame(
  scenario = names(pvals_xgb),
  p_value = as.numeric(pvals_xgb),
  reject = pvals_xgb < alpha,
  expected = ifelse(names(pvals_xgb) %in% should_reject, "reject", "not reject")
)
should_not <- setdiff(names(pvals_knn), should_reject)
data.frame(
  scenario = names(pvals_knn),
  p_value = as.numeric(pvals_knn),
  reject = pvals_knn < alpha,
  expected = ifelse(names(pvals_knn) %in% should_reject, "reject", "not reject")
)
should_not <- setdiff(names(pvals_svm), should_reject)
data.frame(
  scenario = names(pvals_svm),
  p_value = as.numeric(pvals_svm),
  reject = pvals_svm < alpha,
  expected = ifelse(names(pvals_svm) %in% should_reject, "reject", "not reject")
)




run_scenarios_SineGaussianBiv <- function(n = 700, seed_data = 123, seed_test = 1, method = NULL) {
  set.seed(seed_data)
  d0 <- SineGaussianBiv(n)
  
  # Ensure we have enough Z's; create if missing
  if (!"Z1" %in% names(d0)) d0$Z1 <- rnorm(n)
  if (!"Z2" %in% names(d0)) d0$Z2 <- rnorm(n)
  if (!"Z3" %in% names(d0)) d0$Z3 <- rnorm(n)
  if (!"Z4" %in% names(d0)) d0$Z4 <- rnorm(n)
  
  scenarios <- list(
    
    # S1: baseline 
    S1_CI_given_Z1Z2 = list(
      data = d0,
      formula = Y ~ X | Z1 + Z2
    ),
    
    # S2: unconditional independence (should NOT reject)
    S2_unconditional_independence = list(
      data = within(d0, { Y <- rnorm(n) }),
      formula = Y ~ X | 1
    ),
    
    # S3: unconditional dependence (should reject)
    S3_unconditional_dependence = list(
      data = within(d0, { Y <- 0.8 * X + rnorm(n, sd = 0.5) }),
      formula = Y ~ X | 1
    ),
    
    # S4: dependent but conditionally independent (classic: X <- Z -> Y)
    # After conditioning on Z1, X ⟂ Y | Z1 should hold
    S4_CI_after_conditioning_on_common_cause = list(
      data = within(d0, {
        X <- 0.9 * Z1 + rnorm(n, sd = 0.5)
        Y <- -0.7 * Z1 + rnorm(n, sd = 0.5)
      }),
      formula = Y ~ X | Z1
    ),
    
    # S5: dependence remains even after conditioning (direct effect)
    S5_not_CI_due_to_direct_effect = list(
      data = within(d0, {
        X <- 0.9 * Z1 + rnorm(n, sd = 0.5)
        Y <- 0.6 * X - 0.7 * Z1 + rnorm(n, sd = 0.5)
      }),
      formula = Y ~ X | Z1
    ),
    
    # S6: nonlinear dependence (should reject with flexible methods)
    S6_nonlinear_dependence = list(
      data = within(d0, {
        Y <- sin(X) + 0.2 * rnorm(n)
      }),
      formula = Y ~ X | 1
    ),
    
    # S7: nonlinear but conditionally independent via Z1 (X=Z+e, Y=f(Z)+e)
    S7_nonlinear_CI_given_Z1 = list(
      data = within(d0, {
        X <- Z1 + rnorm(n, sd = 0.3)
        Y <- sin(Z1) + rnorm(n, sd = 0.3)
      }),
      formula = Y ~ X | Z1
    ),
    
    # S8: "bad conditioning" / irrelevant Z’s (should behave like unconditional)
    S8_irrelevant_conditioning = list(
      data = within(d0, { Y <- 0.8 * X + rnorm(n, sd = 0.5) }),
      formula = Y ~ X | Z3 + Z4
    )
  )
  
  # Run
  out <- lapply(names(scenarios), function(nm) {
    sc <- scenarios[[nm]]
    if (is.null(method)) {
      res <- CCI.test(sc$formula, data = sc$data, seed = seed_test)
    } else {
      res <- CCI.test(sc$formula, data = sc$data, seed = seed_test, method = method)
    }
    list(name = nm, formula = deparse(sc$formula), result = res)
  })
  names(out) <- names(scenarios)
  out
}
# Example run
results_rf <- run_scenarios_SineGaussianNoise(method = "rf")
results_xgb <- run_scenarios_SineGaussianNoise(method = "xgboost")
results_knn <- run_scenarios_SineGaussianNoise(method = "KNN")
results_svm <- run_scenarios_SineGaussianNoise(method = "svm")

pvals_rf <- sapply(results_rf, function(x) x$result$p.value)
pvals_xgb <- sapply(results_xgb, function(x) x$result$p.value)
pvals_knn <- sapply(results_knn, function(x) x$result$p.value)
pvals_svm <- sapply(results_svm, function(x) x$result$p.value)

alpha <- 0.05
should_reject <- c("S3_unconditional_dependence",
                   "S5_not_CI_due_to_direct_effect",
                   "S6_nonlinear_dependence",
                   "S8_irrelevant_conditioning")

should_not <- setdiff(names(pvals_rf), should_reject)

data.frame(
  scenario = names(pvals_rf),
  p_value = as.numeric(pvals_rf),
  reject = pvals_rf < alpha,
  expected = ifelse(names(pvals_rf) %in% should_reject, "reject", "not reject")
)

should_not <- setdiff(names(pvals_xgb), should_reject)

data.frame(
  scenario = names(pvals_xgb),
  p_value = as.numeric(pvals_xgb),
  reject = pvals_xgb < alpha,
  expected = ifelse(names(pvals_xgb) %in% should_reject, "reject", "not reject")
)
should_not <- setdiff(names(pvals_knn), should_reject)
data.frame(
  scenario = names(pvals_knn),
  p_value = as.numeric(pvals_knn),
  reject = pvals_knn < alpha,
  expected = ifelse(names(pvals_knn) %in% should_reject, "reject", "not reject")
)
should_not <- setdiff(names(pvals_svm), should_reject)
data.frame(
  scenario = names(pvals_svm),
  p_value = as.numeric(pvals_svm),
  reject = pvals_svm < alpha,
  expected = ifelse(names(pvals_svm) %in% should_reject, "reject", "not reject")
)






































  
  
  
