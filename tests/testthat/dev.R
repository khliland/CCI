Sys.setenv(TMPDIR = tempdir())
devtools::check()
devtools::check(cran = TRUE)
devtools::check_win_devel()
devtools::document()
devtools::clean_dll()
devtools::build()
devtools::build_vignettes(quiet = FALSE)
usethis::use_vignette("Testing-CI-with-CCI")
devtools::install()

library(CCI)
library(dplyr)
library(CIsimdata)
# Testing of functions
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

data <- BinaryData(5000)
result <- CCI.test(Y ~ X | Z1 ,
                   data = data,
                   seed = 1,
                   method = "KNN",
                   metric = "Kappa",
                   choose_direction = TRUE)
summary(result)

set.seed(1985)
data <- NormalData(80)
CCI_obj <- CCI.test(formula = Y ~ X | Z1 + Z2, data = data, nperm = 200, parametric = T)
QQplot(CCI_obj) 
# Testing a false Null
CCI_obj <- CCI.test(formula = Y ~ X | Z2, data = data, nperm = 200, parametric = T)
QQplot(CCI_obj) 

data <- PoissonNoise(500)
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

# End of tests


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
# Run
results_rf <- run_scenarios_SineGaussianBiv(method = "rf")
results_xgb <- run_scenarios_SineGaussianBiv(method = "xgboost")
results_knn <- run_scenarios_SineGaussianBiv(method = "KNN")
results_svm <- run_scenarios_SineGaussianBiv(method = "svm")

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

# Advanced testing script

# Scenario 1 continous data
run_scenarios <- function(n = 600, seed_data = 123, seed_test = 1, method = NULL) {
  set.seed(seed_data)
  d0 <- NonLinNormal(n)
  
  # Ensure we have enough Z's; create if missing

  if (!"Z3" %in% names(d0)) d0$Z3 <- rnorm(n)
  if (!"Z4" %in% names(d0)) d0$Z4 <- rnorm(n)
  
  scenarios <- list(
    
    # S1: baseline 
    S1_CI_given_Z1Z2 = list(
      data = d0,
      formula = Y ~ X | Z1 + Z2
    ),
    
    # S2: baseline false
    S2_CI_given_Z1 = list(
      data = d0,
      formula = Y ~ X | Z1
    ),
    
    # S3: unconditional independence (should NOT reject)
    S3_unconditional_independence = list(
      data = within(d0, { Y <- rnorm(n) }),
      formula = Y ~ X | 1
    ),
    
    # S4: unconditional dependence (should reject)
    S4_unconditional_dependence = list(
      data = within(d0, { Y <- 0.8 * X + rnorm(n, sd = 0.5) }),
      formula = Y ~ X | 1
    ),
    
    # S5: dependent but conditionally independent (classic: X <- Z -> Y)
    # After conditioning on Z1, X ⟂ Y | Z1 should hold
    S5_CI_after_conditioning_on_common_cause = list(
      data = within(d0, {
        X <- 0.9 * Z1 + rnorm(n, sd = 0.5)
        Y <- -0.7 * Z1 + rnorm(n, sd = 0.5)
      }),
      formula = Y ~ X | Z1 + Z2
    ),
    
    # S6: dependence remains even after conditioning (direct effect)
    S6_not_CI_due_to_direct_effect = list(
      data = within(d0, {
        X <- 0.9 * Z1 + rnorm(n, sd = 0.5)
        Y <- 0.6 * X - 0.7 * Z1 + rnorm(n, sd = 0.5)
      }),
      formula = Y ~ X | Z1 + Z2
    ),
    
    # S7: nonlinear dependence (should reject with flexible methods)
    S7_nonlinear_dependence = list(
      data = within(d0, {
        Y <- sin(X) + 0.2 * rnorm(n)
      }),
      formula = Y ~ X | 1
    ),
    
    # S8: nonlinear but conditionally independent via Z1 (X=Z+e, Y=f(Z)+e)
    S8_nonlinear_CI_given_Z1 = list(
      data = within(d0, {
        X <- Z1 + rnorm(n, sd = 0.3)
        Y <- sin(Z1) + rnorm(n, sd = 0.3)
      }),
      formula = Y ~ X | Z1 + Z2
    ),
    
    # S9: "bad conditioning" / irrelevant Z’s (should behave like unconditional)
    S9_irrelevant_conditioning = list(
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

# Run
results_rf <- run_scenarios(method = "rf")
results_xgb <- run_scenarios(method = "xgboost")
results_knn <- run_scenarios(method = "KNN")
results_svm <- run_scenarios(method = "svm")

pvals_rf <- sapply(results_rf, function(x) x$result$p.value)
pvals_xgb <- sapply(results_xgb, function(x) x$result$p.value)
pvals_knn <- sapply(results_knn, function(x) x$result$p.value)
pvals_svm <- sapply(results_svm, function(x) x$result$p.value)

alpha <- 0.05
should_reject <- c("S2_CI_given_Z1",
                   "S4_unconditional_dependence",
                   "S6_not_CI_due_to_direct_effect",
                   "S7_nonlinear_dependence",
                   "S9_irrelevant_conditioning")

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

# Asvanced test categorical data (NO finished)
run_cat_scenarios <- function(n = 600, seed_data = 123, seed_test = 1, method = NULL) {
  set.seed(seed_data)
  d0 <- QuadThresh(n)
  
  # Ensure we have enough Z's; create if missing
  d0$Y <- as.factor(d0$Y)
  
  
  if (!"Z3" %in% names(d0)) d0$Z3 <- rnorm(n)
  if (!"Z4" %in% names(d0)) d0$Z4 <- rnorm(n)
  
  scenarios <- list(
    
    # S1: baseline 
    S1_CI_given_Z1Z2 = list(
      data = d0,
      formula = Y ~ X | Z1 + Z2
    ),
    
    # S2: baseline false
    S2_CI_given_Z1 = list(
      data = d0,
      formula = Y ~ X | Z1
    ),
    
    # S3: unconditional independence (should NOT reject)
    S3_unconditional_independence = list(
      data = within(d0, { Y <- as.factor(sample(1:4, size = 1, prob = c(0.1, 0.2, 0.3, 0.4))) }),
      formula = Y ~ X | 1
    ),
    
    # S4: unconditional dependence (should reject)
    S4_unconditional_dependence = list(
      data = within(d0, { Y <- 0.8 * X + rnorm(n, sd = 0.5) }),
      formula = Y ~ X | 1
    ),
    
    
    # S5: dependence remains even after conditioning (direct effect)
    S5_not_CI_due_to_direct_effect = list(
      data = within(d0, {
        X <- 0.9 * Z1 + rnorm(n, sd = 0.5)
        Y <- 0.6 * X - 0.7 * Z1 + rnorm(n, sd = 0.5)
      }),
      formula = Y ~ X | Z1 + Z2
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
      formula = Y ~ X | Z1 + Z2
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
      res <- CCI.test(sc$formula, data = sc$data, metric = "Kappa", seed = seed_test)
    } else {
      res <- CCI.test(sc$formula, data = sc$data, seed = seed_test, metric = "Kappa", method = method)
    }
    list(name = nm, formula = deparse(sc$formula), result = res)
  })
  names(out) <- names(scenarios)
  out
}


results_rf <- run_cat_scenarios(method = "rf")
results_xgb <- run_cat_scenarios(method = "xgboost")
results_knn <- run_cat_scenarios(method = "KNN")
results_svm <- run_cat_scenarios(method = "svm")

pvals_rf <- sapply(results_rf, function(x) x$result$p.value)
pvals_xgb <- sapply(results_xgb, function(x) x$result$p.value)
pvals_knn <- sapply(results_knn, function(x) x$result$p.value)
pvals_svm <- sapply(results_svm, function(x) x$result$p.value)

alpha <- 0.05
should_reject <- c("S2_CI_given_Z1",
                   "S4_unconditional_dependence",
                   "S6_not_CI_due_to_direct_effect",
                   "S7_nonlinear_dependence",
                   "S9_irrelevant_conditioning")

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




simulate_discrete <- function(
    n = 1000,
    K = 10,          # number of strata (levels of Z)
    sigma_y = 1.0,
    sigma_x = 0.5,
    seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)
  
  Z <- sample.int(K, n, replace = TRUE)
  
  # g(Z): only depends on Z (so Y is independent of X given Z)
  gZ <- sin(2 * pi * Z / K) + 0.8 * (Z %% 3 == 0) - 0.4 * (Z %% 4 == 0)
  
  # X depends on Z -> creates marginal correlation between X and Y through Z
  # but no direct X->Y effect after conditioning on Z.
  X <- as.numeric(scale(gZ)[, 1] + rnorm(n, sd = sigma_x))
  
  # Y depends on Z only
  Y <- gZ + rnorm(n, sd = sigma_y)
  
  data.frame(Y = Y, X = X, Z = factor(Z))
}

results_list <- list()
for (i in c(1:200)) {
  data <- simulate_discrete(n = 700, K = 10)
  res <- CCI.test(Y ~ X | Z, data = data, seed = i)
  results_list[[i]] <- res$p.value
}
pvals <- unlist(results_list)
# pvals less tna 0.05
mean(pvals < 0.05)
hist(pvals, breaks = 10)

simulate_correlated_data <- function(n = 500, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  Z <- rnorm(n)
  X <- 0.95 * Z + rnorm(n, sd = 0.3)  
  Y <- 2 * Z + rnorm(n)
  return(data.frame(Y = Y, X = X, Z = Z))
}

results_list <- list()
for (i in c(1:100)) {
  data <- simulate_correlated_data(n = 500, seed = 1)
  res <- CCI.test(Y ~ X | Z, data = data, seed = i)
  results_list[[i]] <- res$p.value
}
pvals <- unlist(results_list)
# pvals less tna 0.05
mean(pvals < 0.05)
hist(pvals, breaks = 10)
simulate_independent_data <- function(n = 500, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  Z <- rnorm(n)
  X <- rnorm(n)  
  Y <- rnorm(n)
  return(data.frame(Y = Y, X = X, Z = Z))
}


















  
  
  
