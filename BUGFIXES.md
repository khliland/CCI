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

## 7. One failed model fit made the p-value NA

**Files:** `R/utils.R`, `R/QQplot.R`

**Symptom:** If a single permutation failed, `CCI.test` reported `P-value: NA`. In a run with 50 permutations where a model failed in 9 of them, the whole test was lost. `QQplot` returned a normal-looking but empty plot when every fit failed (e.g. KNN, see the known issues).

**Cause:** `test.gen` turns an error in an iteration into a warning and `NA`, so the rest of the loop continues. `get_pvalues` then used `sum(dist <= test_statistic)` and `mean(dist)` without removing `NA`s, so one `NA` made the p-value `NA`.

**Fix:** `get_pvalues` removes missing values from the null distribution with one warning ("9 of 50 values in the null distribution are missing (failed model fits) and were removed. The p-value is based on the remaining 41."). It returns `NA` with a warning when the test statistic is missing or fewer than two valid null values remain. `QQplot` removes missing values once (so the warning is not repeated `nperm` times) and stops with an informative error if all test statistics are missing. Results without missing values are unchanged.

**Result:** The run above now gives p = 0.048 (1/42, the smallest possible with 41 valid null values) instead of `NA`.

**Tests:** `tests/testthat/test-pvalues.R`.

## 8. `wrapper_xgboost` bugs

**File:** `R/wrappers.R` (the function was rewritten; binary and continuous results are identical to before, checked on the same splits).

### 8a. Multiclass predictions were scrambled with xgboost 3.x

**Symptom:** For outcomes with three or more classes (Kappa or LogLoss), the model predicted at chance level. On a 3-class example, accuracy dropped from 0.57 to 0.33, and LogLoss was 2.60, worse than uniform guessing (log 3 = 1.10). The test therefore had almost no power for multiclass outcomes.

**Cause:** Since xgboost 3.x, `predict()` returns an n x K probability matrix for `multi:softprob`. The wrapper reshaped it with `matrix(predictions, ncol = num_class, byrow = TRUE)`, which assumes the older flat vector and mixes up rows and classes (rows no longer sum to 1).

**Fix:** The matrix is used as returned. A flat vector (older xgboost) is still reshaped.

**Impact:** Multiclass xgboost results computed with xgboost 3.x should be recomputed. Results computed with older xgboost versions were correct.

### 8b. Custom `metricfunc` failed for continuous outcomes

**Symptom:** `CCI.test(..., method = "xgboost", metricfunc = R2)` with a continuous Y gave `P-value: NA`; every model fit failed.

**Cause:** The type of task was decided from `metric`. With a custom metric, `metric` is the function name (e.g. `"R2"`), which fell through to the catch-all "categorical" branch. A continuous Y was fitted as a multiclass problem with one class per unique value, and the metric function got the actual values as a factor.

**Fix:** RMSE gives regression; Kappa and LogLoss give classification. For a custom metric, a numeric response gives regression and a factor, character or logical response gives classification. The metric function gets numeric actual values and predictions for regression. For classification it gets a factor, plus the probability of the second class (binary) or an n x K probability matrix with class names (multiclass).

### 8c. Numeric class labels had to be coded 0..K-1

**Symptom:** A numeric response coded 1/2 or 1/2/3 with `metric = "Kappa"` gave `P-value: NA`; xgboost stopped with "base_score must be in (0,1)" or an invalid label error.

**Cause:** Numeric labels were passed to xgboost unchanged. Only factors were converted to 0..K-1.

**Fix:** For classification, the response is always converted to a factor and encoded as 0..K-1. Predicted classes are mapped back to the original labels.

### 8d. Smaller fixes

- The design matrix was built separately for the training and test rows, so a factor level missing in one of them gave mismatched columns. It is now built once and split. Character predictors are also one-hot encoded.
- Non-finite predictions were removed without removing the matching actual values. Both are now removed.
- The custom-objective branch checked `names(args)`, but `args` was never defined (it resolved to `base::args`), so the branch was dead. A custom `objective` passed through `...` still works through `modifyList`.
- A `metricfunc` without a `...` argument failed because model parameters (e.g. `eta`) were passed to it. The new internal helper `call_metricfunc()` only passes them when the function accepts `...`.

**Tests:** `tests/testthat/test-wrapper-xgboost.R`.

## Known, not yet fixed

- `wrapper_ranger` does not pass `...` to `ranger()` for RMSE, so extra ranger arguments are ignored for continuous outcomes.
- `wrapper_xgboost` does not use xgboost's own row `subsample`; the name is taken by the data subsampling in `CCI.test`.
- Only `wrapper_xgboost` uses `call_metricfunc()`; the other wrappers still pass `...` to `metricfunc` unconditionally, and `wrapper_knn` stops for any custom metric.
