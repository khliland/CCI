# Bug fixes

Bugs found and fixed while improving `CCI.pretuner` (2026-09-23, in CCI 0.3.7; see also NEWS.md).
Every fix is covered by a test in `tests/testthat/`.

## 1. Tuned and user-supplied model parameters never reached the model

**File:** `R/CCI.test.R`

**Symptom:** `CCI.test(tune = TRUE)` tuned the parameters, but the test itself ran with the defaults.
xgboost arguments such as `eta`, `max_depth`, `gamma`, `colsample_bytree` and `min_child_weight` were also ignored in ordinary `CCI.test` calls.
Tracing the calls showed that `ranger` always got `mtry = NULL` and `xgb.train` only got `objective`, `eval_metric` and `nthread`.

**Cause:** `params` was passed to `perm.test` as an unnamed list inside `...`
(`perm.test(..., distance = distance, params, ...)`). The wrappers never saw it as named arguments, so it was silently dropped.

**Fix:** Build the `perm.test` arguments as a list, merge `params` into it with `utils::modifyList()`, and call `do.call(perm.test, c(perm_args, list(...)))`.
Tuned values now override the defaults, for example `nrounds`.

**Impact:** Results with default settings are unchanged, because the default `CCI.test` xgboost values equal xgboost's own defaults. Results change when tuning or non-default xgboost parameters were used.

**Tests:** "tuned parameters are passed on to the model in CCI.test" and "user given xgboost parameters reach the model in CCI.test".

## 2. Polynomial and interaction terms were expanded twice when tuning

**Files:** `R/CCI.test.R`, `R/CCI.pretuner.R`

**Symptom:** With `tune = TRUE`, tuning ran on a much larger and wrong set of predictors. `Y ~ X | Z1 + Z2` with `degree = 3` became 36 terms (`Z1_d_2_d_3`, `Z1_int_Z1_d_2`, …) instead of 7.

**Cause:** `CCI.test` passed the already expanded formula and data to `CCI.pretuner`, which by default (`poly = TRUE, interaction = TRUE`) expanded them again.

**Fix:** `CCI.test` now calls `CCI.pretuner(..., poly = FALSE, interaction = FALSE)`.

**Test:** "pretuner does not expand already expanded terms".

## 3. `wrapper_ranger` computed Kappa wrongly for binary outcomes

**File:** `R/wrappers.R`

**Symptom:**
- Factor outcome with labels other than 0/1 (e.g. `"high"`/`"low"`): every model fit failed, and the test statistic from `CCI.test` was `NaN`.
- Numeric 0/1 outcome with `metric = "Kappa"`: classes could be swapped, giving a negative Kappa (−0.57 in the reproduction) for a model that actually predicts well.

**Cause:** Predicted classes were coded 0/1 and matched against the outcome's factor levels. With other labels every prediction became `NA`, and `confusionMatrix()` failed inside `try()`. For numeric outcomes, ranger returns an unnamed probability matrix with columns in the order the classes first appear in the training data, not in sorted level order, so taking column 2 as the positive class could be wrong.

**Fix:** Take the class labels from the prediction column names, or from `model$forest$class.values` when the columns are unnamed. Pick the predicted class with `max.col()` and compare it with the outcome on a common set of levels. The same code now handles binary and multiclass outcomes.

**Test:** "wrapper_ranger computes Kappa for binary outcomes with any labels".

## 4. `CCI.pretuner` fixes

**File:** `R/CCI.pretuner.R`

| Bug | Fix |
|---|---|
| Any metric other than RMSE stopped with an error, so `CCI.test(tune = TRUE)` failed for categorical outcomes (Kappa) | Supports `"RMSE"`, `"Kappa"` and `"LogLoss"`. `CCI.test` falls back to RMSE or Kappa when a custom `metricfunc` is used |
| xgboost tuning failed for every combination with xgboost ≥ 3 (caret's `xgbTree` is incompatible with it) | Candidates are scored with CCI's own `wrapper_xgboost`, `wrapper_ranger` and `wrapper_svm` instead of `caret::train` |
| Random forest was tuned with `randomForest` (caret `"rf"`), while the test uses `ranger`; svm was tuned with `kernlab`. Neither package is a dependency | Tuned with the same wrappers as the test, so the tuned model is the tested model |
| A user-supplied `mtry` was always overwritten | `mtry = NULL` by default; user values are used and capped at the number of predictors |
| The near-zero-variance warning printed `NA` instead of the variable names (`names(data)[<character>]`) | Uses the predictor names directly |
| When no model trained, it warned and then crashed in `which.min()` on `NULL` | Stops with a clear error showing the first logged errors |
| Each candidate was evaluated on different random folds, so the comparisons were noisy | The folds are created once and shared by all candidates |
| The progress bar could not be turned off | New `progress` argument; `CCI.test` passes its own `progress` |
| `tune_length` was checked but never used | Deprecated with a warning (as are `verboseIter`, `parallel` and `subsample`) |
| The documented defaults did not match the code (e.g. `folds`) | Roxygen docs rewritten |

## 5. `get_tuned_params` overwrote `CCI.test`'s data subsampling

**File:** `R/utils.R`

**Cause:** It returned xgboost's row `subsample`, which has the same name as `CCI.test`'s `subsample` (the share of data used in each Monte Carlo cross-validation (MCCV) iteration). After fix 1 it would have overwritten that value.

**Fix:** `subsample` is no longer returned. The unreachable `nnet` branch was removed.

## 6. LogLoss p-values used the wrong tail

**File:** `R/perm.test.R`

**Symptom:** With `metric = "LogLoss"`, the test could never reject independence. In a simulation where Y depends strongly on X given Z (n = 400), the test statistic (LogLoss 0.26 with rf, 0.18 with xgboost) was far below the null distribution (mean 0.77 / 1.12), yet the p-value was 1.

**Cause:** `perm.test` set `tail = "right"` for both Kappa and LogLoss. All wrappers return LogLoss as a positive loss (`-mean(log(p))`), where lower is better. So under dependence the test statistic lies in the left tail, as it does for RMSE.

**Fix:** `tail = "right"` for Kappa only; `tail = "left"` for RMSE and LogLoss.

**Result (40 permutations):** p-value for dependent data went from 1 to 0.024 (the smallest possible, 1/41) for both rf and xgboost; independent data gives p ≈ 0.5.

**Impact:** All earlier LogLoss results are wrong. Their p-values are roughly 1 minus the correct one, so the conclusions should be rerun. `CCI` objects saved before the fix store `tail = "right"`, which `QQplot()` reuses.

**Tests:** "LogLoss uses the left tail, so dependence gives a small p-value" and "metrics get the correct tail direction" in `tests/testthat/test-perm-test.R`.

## Known, not yet fixed

- `wrapper_ranger` does not pass `...` to `ranger()` for RMSE, so extra ranger arguments are ignored for continuous outcomes.
- `wrapper_xgboost` only uses `subsample` when a custom `objective` is supplied.
