# =============================================================================
# CCI package check script
# -----------------------------------------------------------------------------
# A manual check of the whole package, beyond the automatic unit tests in
# tests/testthat/test-*.R (which run with devtools::test()).
#
#   Part 1: Feature checks. Every feature is run once, and the p-value is
#           checked against what the data should give (reject / keep H0).
#           Takes a few minutes.
#   Part 2: Type I error and power on CIsimdata scenarios, as in Fig. 4 of the
#           SoftwareX article. Slow; set REPS and N_MC to fit your time.
#
# Run from the package folder, section by section or with source().
# This file is excluded from the package build (.Rbuildignore) and is not run
# by testthat (the file name does not start with "test").
# =============================================================================

devtools::load_all()
library(CIsimdata)

# ---- Settings ----------------------------------------------------------------
N      <- 400   # sample size in Part 1
NPERM  <- 40    # Monte Carlo samples per test in Part 1
ALPHA  <- 0.05

# ---- Helpers -------------------------------------------------------------------
check_results <- list()

# Runs one check and records the outcome.
#   expect = "reject": p-value should be <= ALPHA (H0 false)
#   expect = "keep":   p-value should be > ALPHA (H0 true)
#   expect = "runs":   only check that it runs and gives a valid result
check <- function(label, expr, expect = c("runs", "reject", "keep")) {
  expect <- match.arg(expect)
  warnings <- character()
  start <- Sys.time()
  value <- tryCatch(
    withCallingHandlers(expr, warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }),
    error = function(e) structure(conditionMessage(e), class = "check_error")
  )
  seconds <- as.numeric(difftime(Sys.time(), start, units = "secs"))

  p <- if (inherits(value, "CCI")) value$p.value else NA_real_
  status <- if (inherits(value, "check_error")) {
    "ERROR"
  } else if (inherits(value, "CCI") && is.na(p)) {
    "NA p-value"
  } else if (expect == "reject" && p > ALPHA) {
    "UNEXPECTED (kept H0)"
  } else if (expect == "keep" && p <= ALPHA) {
    "UNEXPECTED (rejected H0)"
  } else {
    "OK"
  }
  note <- if (inherits(value, "check_error")) unclass(value) else paste(unique(warnings), collapse = " | ")
  check_results[[label]] <<- data.frame(check = label, expect = expect, p_value = round(p, 4),
                                        status = status, seconds = round(seconds, 1),
                                        n_warnings = length(warnings), note = substr(note, 1, 120))
  cat(sprintf("%-55s %-24s p = %-7s (%.1fs)\n", label, status, format(round(p, 4)), seconds))
  invisible(value)
}

# Checks that an expression fails with an error matching `pattern`
check_error <- function(label, expr, pattern) {
  msg <- tryCatch({ expr; NA_character_ }, error = function(e) conditionMessage(e))
  ok <- !is.na(msg) && grepl(pattern, msg, fixed = TRUE)
  check_results[[label]] <<- data.frame(check = label, expect = "error", p_value = NA,
                                        status = if (ok) "OK" else "UNEXPECTED (no/other error)",
                                        seconds = NA, n_warnings = 0, note = substr(msg %||% "", 1, 120))
  cat(sprintf("%-55s %s\n", label, if (ok) "OK" else "UNEXPECTED (no/other error)"))
}

# Custom metric and ML functions
r2 <- function(actual, predictions) {
  1 - sum((actual - predictions)^2) / sum((actual - mean(actual))^2)
}
accuracy <- function(actual, predictions) mean(as.character(actual) == as.character(predictions))
lm_rmse <- function(formula, data, train_indices, test_indices, ...) {
  fit <- stats::lm(formula, data = data[train_indices, ])
  y <- data[test_indices, all.vars(formula)[1]]
  sqrt(mean((stats::predict(fit, data[test_indices, ]) - y)^2))
}

# =============================================================================
# PART 1: Feature checks
# =============================================================================
# In NormalData (and the other two-Z scenarios), Y _||_ X | Z1, Z2 is true,
# while Y _||_ X | Z1 is false.

set.seed(1)
cont   <- NormalData(N)                                          # continuous Y
cat4   <- QuadThresh(N)                                          # Y with 4 classes (character)
binary <- BinaryData(N)                                          # binary numeric Y (0/1)
catZ   <- simulate_cat_Z1_Z2_null(n = N, seed = 1)               # categorical Z, H0 true
uncond <- data.frame(X = rnorm(N)); uncond$Y <- uncond$X + rnorm(N); uncond$Y0 <- rnorm(N)

cat("\n---- 1.1 Methods, continuous Y ----\n")
for (m in c("rf", "xgboost", "svm", "KNN")) {
  check(paste0(m, ": Y ~ X | Z1 + Z2 (true)"),
        CCI.test(Y ~ X | Z1 + Z2, cont, method = m, nperm = NPERM, progress = FALSE, seed = 1), "keep")
  check(paste0(m, ": Y ~ X | Z1 (false)"),
        CCI.test(Y ~ X | Z1, cont, method = m, nperm = NPERM, progress = FALSE, seed = 1), "reject")
}

cat("\n---- 1.2 Categorical and binary Y ----\n")
for (m in c("rf", "xgboost", "svm", "KNN")) {
  check(paste0(m, ": 4 classes, Kappa (false)"),
        CCI.test(Y ~ X | Z1, cat4, method = m, nperm = NPERM, progress = FALSE, seed = 1), "reject")
  check(paste0(m, ": 4 classes, LogLoss (false)"),
        CCI.test(Y ~ X | Z1, cat4, method = m, metric = "LogLoss", nperm = NPERM, progress = FALSE, seed = 1), "reject")
}
check("rf: binary 0/1 Y, Kappa", CCI.test(Y ~ X | Z1 + Z2, binary, metric = "Kappa", nperm = NPERM, progress = FALSE, seed = 1))
check("xgboost: binary 0/1 Y, Kappa", CCI.test(Y ~ X | Z1 + Z2, binary, method = "xgboost", metric = "Kappa", nperm = NPERM, progress = FALSE, seed = 1))

cat("\n---- 1.3 Categorical Z (stratified permutation) ----\n")
check("rf: categorical Z, robust = TRUE (true)",
      CCI.test(Y ~ X | Z1 + Z2, catZ, nperm = NPERM, progress = FALSE, seed = 1), "keep")
check("rf: categorical Z, robust = FALSE",
      CCI.test(Y ~ X | Z1 + Z2, catZ, robust = FALSE, nperm = NPERM, progress = FALSE, seed = 1))

cat("\n---- 1.4 Unconditional tests ----\n")
check("rf: Y ~ X | 1 (dependent)", CCI.test(Y ~ X | 1, uncond, nperm = NPERM, progress = FALSE, seed = 1), "reject")
check("rf: Y0 ~ X + 1 (independent)", CCI.test(Y0 ~ X + 1, uncond, nperm = NPERM, progress = FALSE, seed = 1), "keep")
check_error("Y ~ X without | 1 gives an error", CCI.test(Y ~ X, uncond), "unconditional testing")

cat("\n---- 1.5 Custom metricfunc and mlfunc ----\n")
for (m in c("rf", "xgboost", "svm", "KNN")) {
  check(paste0(m, ": metricfunc = r2 (false)"),
        CCI.test(Y ~ X | Z1, cont, method = m, metricfunc = r2, tail = "right", nperm = NPERM, progress = FALSE, seed = 1), "reject")
}
check("rf: metricfunc = accuracy, categorical Y (false)",
      CCI.test(Y ~ X | Z1, cat4, metricfunc = accuracy, tail = "right", nperm = NPERM, progress = FALSE, seed = 1), "reject")
check("mlfunc = lm_rmse (false)",
      CCI.test(Y ~ X | Z1, cont, mlfunc = lm_rmse, tail = "left", nperm = NPERM, progress = FALSE, seed = 1), "reject")
check_error("metricfunc without tail gives an error",
            CCI.test(Y ~ X | Z1, cont, metricfunc = r2), "tail")

cat("\n---- 1.6 Options ----\n")
check("parametric = TRUE", CCI.test(Y ~ X | Z1, cont, parametric = TRUE, nperm = NPERM, progress = FALSE, seed = 1), "reject")
check("MC_sample = 'Yes', MC_sample_set = 0.5",
      CCI.test(Y ~ X | Z1, cont, MC_sample = "Yes", MC_sample_set = 0.5, nperm = NPERM, progress = FALSE, seed = 1), "reject")
check("MC_sample = 'No'", CCI.test(Y ~ X | Z1, cont, MC_sample = "No", nperm = NPERM, progress = FALSE, seed = 1), "reject")
check("poly = FALSE, interaction = FALSE",
      CCI.test(Y ~ X | Z1 + Z2, cont, poly = FALSE, interaction = FALSE, nperm = NPERM, progress = FALSE, seed = 1), "keep")
check("degree = 2", CCI.test(Y ~ X | Z1 + Z2, cont, degree = 2, nperm = NPERM, progress = FALSE, seed = 1), "keep")
check("p = 0.7", CCI.test(Y ~ X | Z1, cont, p = 0.7, nperm = NPERM, progress = FALSE, seed = 1), "reject")
check("formula Y ~ X + Z1 + Z2 (same as | Z1 + Z2)",
      CCI.test(Y ~ X + Z1 + Z2, cont, nperm = NPERM, progress = FALSE, seed = 1), "keep")
check("choose_direction = TRUE",
      CCI.test(Y ~ X | Z1 + Z2, cont, choose_direction = TRUE, nperm = NPERM, progress = FALSE, seed = 1), "keep")
check("xgboost with eta = 0.1, max_depth = 3",
      CCI.test(Y ~ X | Z1, cont, method = "xgboost", eta = 0.1, max_depth = 3, nperm = NPERM, progress = FALSE, seed = 1), "reject")

cat("\n---- 1.7 Tuning ----\n")
for (m in c("rf", "xgboost", "svm")) {
  check(paste0(m, ": tune = TRUE (false)"),
        CCI.test(Y ~ X | Z1, cont, method = m, tune = TRUE, samples = 5, folds = 3, nperm = NPERM, progress = FALSE, seed = 1), "reject")
}
check("rf: tune = TRUE, categorical Y (false)",
      CCI.test(Y ~ X | Z1, cat4, tune = TRUE, samples = 3, folds = 3, nperm = NPERM, progress = FALSE, seed = 1), "reject")
tuned <- check("CCI.pretuner directly", CCI.pretuner(Y ~ X | Z1 + Z2, cont, samples = 5, folds = 3, progress = FALSE))
print(tuned$best_param)

cat("\n---- 1.8 Lower-level functions ----\n")
check("perm.test directly", perm.test(Y ~ X | Z1, cont, nperm = NPERM, progress = FALSE), "reject")
check("test.gen directly", {
  out <- test.gen(Y ~ X | Z1 + Z2, cont, nperm = 10, permutation = TRUE, progress = FALSE)
  stopifnot(length(unlist(out$distribution)) == 10, !anyNA(unlist(out$distribution)))
  out
})
check("CCI.direction", CCI.direction(Y ~ X | Z1 + Z2, cont, method = "xgboost", nrounds = 50))

cat("\n---- 1.9 Summary and plots ----\n")
res_rf  <- CCI.test(Y ~ X | Z1, cont, nperm = NPERM, progress = FALSE, seed = 1)
res_xgb <- CCI.test(Y ~ X | Z1, cont, method = "xgboost", eta = 0.1, nperm = NPERM, progress = FALSE, seed = 1)
res_knn <- CCI.test(Y ~ X | Z1, cont, method = "KNN", metricfunc = r2, tail = "right", nperm = NPERM, progress = FALSE, seed = 1)
summary(res_rf)
check("plot()", { g <- plot(res_rf); print(g); g })
for (nm in c("res_rf", "res_xgb", "res_knn")) {
  check(paste0("QQplot(", nm, ")"), {
    g <- QQplot(get(nm), progress = FALSE)
    stopifnot(!anyNA(g$data$pvalues))
    print(g)
    g
  })
}

# ---- Part 1 summary ----
part1 <- do.call(rbind, check_results)
rownames(part1) <- NULL
cat("\n==== Part 1 summary ====\n")
print(table(part1$status))
not_ok <- part1[part1$status != "OK", ]
if (nrow(not_ok) > 0) {
  cat("\nChecks that need a look:\n")
  print(not_ok[, c("check", "expect", "p_value", "status", "note")], right = FALSE)
} else {
  cat("All checks OK.\n")
}

# =============================================================================
# PART 2: Type I error and power on CIsimdata scenarios
# =============================================================================
# For each scenario, REPS data sets are drawn, and a true and a false statement
# are tested. The rejection rate of the true statement is the type I error
# (should be close to ALPHA or below), and the rejection rate of the false
# statement is the power. The article used N = 1000 and 100 repetitions.

REPS    <- 20     # data sets per scenario (article: 100)
N_MC    <- 1000   # sample size (article: 1000)
METHODS <- c("rf", "xgboost")

# true / false: formulas for the true and the false statement.
# d_true / d_false: for scenarios with a dependence parameter d, the same
# formula is used and d = 0 gives H0 true, d = 0.5 gives H0 false.
scenarios <- list(
  NormalData       = list(gen = NormalData,       true = Y ~ X | Z1 + Z2, false = Y ~ X | Z1),
  NonLinNormal     = list(gen = NonLinNormal,     true = Y ~ X | Z1 + Z2, false = Y ~ X | Z1),
  UniformNoise     = list(gen = UniformNoise,     true = Y ~ X | Z1 + Z2, false = Y ~ X | Z1),
  PoissonNoise     = list(gen = PoissonNoise,     true = Y ~ X | Z1 + Z2, false = Y ~ X | Z1),
  ExponentialNoise = list(gen = ExponentialNoise, true = Y ~ X | Z1 + Z2, false = Y ~ X | Z1),
  HardCase         = list(gen = HardCase,         true = Y ~ X | Z1 + Z2, false = Y ~ X | Z1),
  QuadThresh       = list(gen = QuadThresh,       true = Y ~ X | Z1 + Z2, false = Y ~ X | Z1),
  PolyData         = list(gen = PolyData,         true = Y ~ X | Z1 + Z2, false = Y ~ X | Z1),
  SineGaussian     = list(gen = SineGaussian,     true = Y ~ X | Z,       d = TRUE),
  NonLinearCategorization = list(gen = NonLinearCategorization, true = Y ~ X | Z, d = TRUE)
)

run_scenario <- function(name, sc, method, reps = REPS, n = N_MC) {
  p_true <- p_false <- rep(NA_real_, reps)
  for (r in seq_len(reps)) {
    set.seed(1000 + r)
    if (isTRUE(sc$d)) {
      data_true  <- sc$gen(n, d = 0)
      data_false <- sc$gen(n, d = 0.5)
      f_true <- f_false <- sc$true
    } else {
      data_true <- data_false <- sc$gen(n)
      f_true <- sc$true
      f_false <- sc$false
    }
    p_true[r]  <- suppressWarnings(CCI.test(f_true, data_true, method = method, progress = FALSE, seed = r)$p.value)
    p_false[r] <- suppressWarnings(CCI.test(f_false, data_false, method = method, progress = FALSE, seed = r)$p.value)
    cat(sprintf("\r%-25s %-8s %d/%d", name, method, r, reps))
  }
  cat("\n")
  data.frame(scenario = name, method = method,
             type_I_error = mean(p_true <= ALPHA, na.rm = TRUE),
             power = mean(p_false <= ALPHA, na.rm = TRUE),
             n_na = sum(is.na(c(p_true, p_false))))
}

part2 <- do.call(rbind, lapply(METHODS, function(m) {
  do.call(rbind, lapply(names(scenarios), function(nm) run_scenario(nm, scenarios[[nm]], m)))
}))

cat("\n==== Part 2 summary (alpha =", ALPHA, ", reps =", REPS, ", n =", N_MC, ") ====\n")
print(part2, row.names = FALSE)
# With REPS repetitions, a type I error rate above
# ALPHA + 2 * sqrt(ALPHA * (1 - ALPHA) / REPS) is worth a closer look:
cat("Flag type I error above:", round(ALPHA + 2 * sqrt(ALPHA * (1 - ALPHA) / REPS), 3), "\n")
print(part2[part2$type_I_error > ALPHA + 2 * sqrt(ALPHA * (1 - ALPHA) / REPS) | part2$n_na > 0, ],
      row.names = FALSE)
