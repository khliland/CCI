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

## 9. `QQplot` did not repeat the test it was plotting

**Files:** `R/QQplot.R`, `R/perm.test.R`

**Symptom:** The article recommends `QQplot()` for borderline p-values (0.05–0.2), but:
- with xgboost, parameters such as `eta` and `max_depth` were dropped (traced: no `xgb.train` call got `eta`), so a different model was plotted than the one tested;
- with `method = "KNN"`, a custom `metricfunc` or a custom `mlfunc`, all p-values were `NA`, but a normal-looking plot was still returned;
- centering and scaling were turned off, unlike in the test (matters for svm and KNN);
- it failed for objects from `perm.test()`, which have no `ext_formula`.

**Cause:**
- `object$additional_args` was passed to `test.gen` unnamed (same bug class as #1), so it was dropped.
- The `CCI` object did not store `k`, `center`, `scale`, `eps`, `positive`, `kernel`, `distance`, `mtry`, `nthread`, `metricfunc` or `mlfunc`. `QQplot` read them as `NULL`: `k = NULL` broke KNN, and `center = NULL` turned scaling off. Without `metricfunc` and `mlfunc`, a custom metric name or method name was sent to the wrappers, which stopped with "Unsupported metric" or "Method chosen is not supported".

**Fix:** `perm.test` collects all settings passed to `test.gen` in one list. It uses the list for both the null distribution and the test statistic, and stores it in the result as `settings`. `QQplot` repeats the test with `do.call(test.gen, settings)`, and arguments given to `QQplot()` override the stored settings (e.g. `nperm = 50`). The formula falls back to `object$formula` when `ext_formula` is missing. Objects created before this version (no `settings`) use the stored fields with defaults for the rest. If such an object used a custom metric, `QQplot` stops with an explanation, since the function was never stored.

**Unchanged results:** `perm.test` gives identical null distributions and p-values to before for the same seed (checked for rf, KNN and svm).

**Tests:** `tests/testthat/test-qqplot.R`.

## 10. KNN failed with any custom metric; `metricfunc` without `...` failed

**File:** `R/wrappers.R`

**Symptom:** `CCI.test(..., method = "KNN", metricfunc = f)` gave `P-value: NA` (and `QQplot` failed), because every model fit stopped with "metric must be 'RMSE' (regression), 'Kappa' (classification), or 'LogLoss' (classification)." In the ranger, svm and KNN wrappers, a metric function without a `...` argument failed with "unused argument" whenever extra arguments were given (e.g. `min.node.size`, `cost`).

**Cause:** `wrapper_knn` decided the type of task only from `metric`, and with a custom metric `metric` is the function name, so it stopped. All wrappers except xgboost called `metricfunc(actual, predictions, ...)` unconditionally.

**Fix:** With a custom metric, `wrapper_knn` decides the task from the response, as `wrapper_xgboost` does: numeric gives regression (numeric predictions), and factor, character or logical gives classification (predicted classes as a factor). All wrappers call the metric through `call_metricfunc()`, which passes `...` only when the function accepts it. The `metricfunc` docs in `wrapper_ranger` described wrong arguments (`data`, `model`, `test_indices`) and were corrected.

**Tests:** `tests/testthat/test-metricfunc.R`.

## 11. `CCI.direction` depended on the units of Y and X

**File:** `R/CCI.direction.R`

**Symptom:** Multiplying Y by 100 changed the chosen direction from `Y ~ X | Z1 + Z2` to `X ~ Y | Z1 + Z2`, with the same data otherwise.

**Cause:** The function compared the cross-validated RMSE of predicting Y (from X, Z) with the RMSE of predicting X (from Y, Z). RMSE is in the units of the outcome, so the variable with the smaller variance tended to win, regardless of how well it could be predicted. The article describes the intention as choosing "the easiest prediction". caret's `preProcess = c("center", "scale")` only scales the predictors, not the outcome.

**Fix:** Y and X are standardized (mean 0, sd 1) before the two models are fitted, so the comparison is RMSE / sd(outcome), a unit-free measure of how hard each variable is to predict. A variable with zero variance gives an informative error.

**Result:** Rescaling Y by 0.01 or 100, or X by 100, now gives the same direction. A variable that is almost fully explained by Z is chosen as outcome even when the other variable has a much smaller scale.

**Impact:** `CCI.test(choose_direction = TRUE)` can choose a different direction than before when Y and X have different variances. The test itself is valid in both directions; the choice only affects power.

**Tests:** `tests/testthat/test-direction.R`.

## 12. Character and logical variables were not handled consistently

**Files:** `R/CCI.test.R`, `R/wrappers.R`, `R/utils.R`

**Found by:** `tests/testthat/check_package.R` (Part 1), on `CIsimdata::QuadThresh`, which returns Y as a character vector (as do several other CIsimdata scenarios).

**Symptom:**
- `method = "rf"` with a character Y or character X: every model fit failed with ranger's "Unsupported type of dependent variable", so the p-value was `NA`. This also broke rf with a custom metric and `tune = TRUE` ("No models were successfully trained in pretuning").
- A logical Y stopped with "Could not determine an appropriate metric automatically".
- A character Z stopped for every method with "non-numeric argument to binary operator": it was not recognised as categorical, so `add_poly_terms` computed `Z^2` on text.
- svm, xgboost and KNN already handled a character Y.

**Cause:** Only factors were treated as categorical. `CCI.test` chose Kappa for a character Y, but passed the character column on unchanged. ranger does not accept character variables, and the "is Z categorical?" check (which turns off polynomial terms) only looked for factors.

**Fix:** A new internal helper, `characters_to_factors()`, converts character and logical formula variables to factors. `CCI.test` uses it right after parsing the formula, so the metric, polynomial terms, stratified permutation and all wrappers treat them as categorical. `wrapper_ranger` and `wrapper_svm` also use it, so direct calls and `CCI.pretuner` work. Numeric and factor data are unchanged.

**Result:** The four failing checks on QuadThresh (rf with Kappa, LogLoss, a custom accuracy metric, and tuning) now reject the false statement with p = 1/41 (40 permutations); the true statement gives p = 0.22.

**Tests:** `tests/testthat/test-character.R`.

## 13. `CCI.direction` used other models than the test; printing gave nothing useful

**Files:** `R/CCI.direction.R`, `R/CCI.test.R`, `R/reports.R`

**Symptom:**
- `CCI.direction` (used by `choose_direction = TRUE`) trained its models through caret: `method = "rf"` used the randomForest package and `method = "svm"` the kernlab package. Neither is a dependency of CCI, so the direction choice could fail on a system without them, and the models differed from the ones used in the test (ranger and e1071). For xgboost, `xgb.cv()` was used, with the data subsample share (e.g. 0.3 for n = 5000) passed as xgboost's own row `subsample`.
- The article lists a `print.CCI()` method, but it did not exist: printing a result used `print.htest` and showed an almost empty printout.
- After `choose_direction = TRUE` switched the direction, `summary()` showed the formula as given, not the one that was tested.

**Fix:**
- `CCI.direction` cross-validates both directions with the package's own wrappers (`wrapper_ranger`, `wrapper_xgboost`, `wrapper_svm`, `wrapper_knn`) on the same folds (drawn in base R), with the settings from `CCI.test` (`nrounds`, xgboost parameters, `mtry`, `nthread`, `k`, `kernel`, `distance`). All numeric variables are standardized first. `subsample` only subsamples the data. It also handles character variables and unconditional formulas (`Y ~ X | 1`), and `verbose = TRUE` prints the RMSE of both directions.
- New `print.CCI()`: formula, learner, metric, number of permutations, statistic and p-value (marked "parametric" when `parametric = TRUE`).
- `CCI.test` stores the tested formula (without polynomial and interaction terms) in `tested_formula`. `print()` and `summary()` show it, and add the formula as given when the direction was changed.

**Tests:** `tests/testthat/test-direction.R` (all four learners, with `caret::train` mocked to fail) and `tests/testthat/test-reports.R`.

## 14. Remaining smaller issues

**Files:** `R/utils.R`, `R/CCI.test.R`, `R/wrappers.R`, `R/plot.R`, `R/perm.test.R`, `R/test.gen.R`, `development/TODO.txt`

- **Polynomial terms were turned off for all of Z if any Z was categorical.** `CCI.test` set `poly <- FALSE` when any Z was a factor, and `add_poly_terms` returned without terms. Now `add_poly_terms` only skips the non-numeric variables, so numeric Z variables get their polynomial terms also in mixed data. (Test: `test-misc.R`, and `test-character.R` checks that a character Z gets no terms while a numeric Z does.)
- **`wrapper_ranger` ignored `...` for continuous outcomes.** The RMSE branch called `ranger()` without `...`, so arguments such as `min.node.size` or `max.depth` had no effect for a numeric Y. They are now passed on, as in the classification branches. (Test: `max.depth = 1` now changes the RMSE.)
- **`plot.CCI` labelled the density axis "Freq.",** and extra ggplot2 layers in `...` never worked: unnamed arguments were matched by position to `fill_color`, `title.size`, etc., and if any argument was not a ggplot object, all were silently ignored. `...` now comes right after `x`, layers and themes are added, other arguments give a warning, and the y-axis is labelled "Density".
- **The `robust` documentation** in `test.gen` said stratified permutation was used when *all* conditioning variables are categorical; the code (and the SoftwareX article) use it when *any* is. The documentation in `CCI.test`, `perm.test` and `test.gen` now describes the behaviour.
- **The `metricfunc` documentation** did not say what the metric function receives. It now describes `actual` and `predictions` for each learner in `CCI.test`, `perm.test` and `test.gen`. The difference between xgboost (class probabilities) and rf, svm and KNN (predicted classes) is kept, since changing either would break existing metric functions; it is documented instead.
- **`development/TODO.txt`** started with the leftover merge-conflict markers `<<<<<<< HEAD` and `=======` (without a closing marker); they were removed.

## Known, not yet fixed

- `wrapper_xgboost` does not use xgboost's own row `subsample`; the name is taken by the data subsampling in `CCI.test`.
