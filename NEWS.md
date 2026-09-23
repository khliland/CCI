# CCI 0.3.7

## Bug fixes

* `metric = "LogLoss"` now uses a left-tailed test. Earlier versions used the
  right tail, so a test with LogLoss could never reject conditional
  independence. **P-values from LogLoss tests in earlier versions are wrong and
  should be recomputed.** `CCI` objects saved with earlier versions still store
  `tail = "right"`, which `QQplot()` reuses.

* A failed model fit in a single permutation no longer makes the p-value `NA`.
  `get_pvalues()` removes missing values from the null distribution with a
  warning saying how many were removed, and returns `NA` with a warning only
  when the test statistic itself is missing. `QQplot()` stops with an
  informative error when all model fits fail, instead of returning an empty
  plot.

* `wrapper_xgboost()` (`method = "xgboost"`):
  * Multiclass outcomes (three or more classes) with `metric = "Kappa"` or
    `"LogLoss"` gave predictions at chance level with xgboost 3.x, because the
    probability matrix returned by `predict()` was reshaped as if it were a
    vector. **Multiclass xgboost results computed with xgboost 3.x should be
    recomputed.** Binary and continuous outcomes are unaffected.
  * A custom `metricfunc` now works for continuous outcomes. Before, the
    response was treated as multiclass and every model fit failed. The metric
    function gets numeric actual values for regression, and a factor with class
    probabilities for classification.
  * Numeric class labels no longer need to be coded 0..K-1 (e.g. 1/2 or 1/2/3
    now work).
  * Factor predictors with a level missing in the training or test rows no
    longer give mismatched feature columns.
  * A custom `metricfunc` without a `...` argument now works; model parameters
    are only passed to it when it accepts `...`.

* Model parameters given to `CCI.test()` now reach the model. Earlier, tuned
  parameters (`tune = TRUE`) and xgboost parameters (`eta`, `max_depth`,
  `gamma`, `colsample_bytree`, `min_child_weight`) were silently ignored.
  Results with default settings are unchanged.

* `CCI.test(tune = TRUE)` no longer adds polynomial and interaction terms a
  second time before tuning.

* `wrapper_ranger()` (`method = "rf"`) computes Kappa correctly for binary
  outcomes. Factor outcomes with labels other than 0/1 gave `NaN`, and numeric
  0/1 outcomes could have their classes swapped.

* `get_tuned_params()` no longer returns xgboost's `subsample`, which would
  overwrite the data subsampling in `CCI.test()`.

## Changes to `CCI.pretuner()`

* Candidates are now evaluated with the same model wrappers as `CCI.test()`
  (ranger, xgboost and e1071) instead of `caret::train()`. Tuning therefore uses
  the same models as the test, works with xgboost 3.x, and no longer needs the
  randomForest or kernlab packages.

* All candidates are evaluated on the same resampling splits.

* Supports `metric = "Kappa"` and `"LogLoss"`, so `CCI.test(tune = TRUE)` works
  for categorical outcomes.

* A user-supplied `mtry` is no longer overwritten.

* New arguments `progress`, `nthread` and `num_trees`.

* `validation_method` accepts `"cv"`, `"LGOCV"` and `"boot"`.

* Results in `tuning_result` are sorted best first and include the standard
  deviation across resamples.

* Stops with an informative error when no model can be trained.

* `tune_length`, `verboseIter`, `parallel` and `subsample` are deprecated and
  ignored, with a warning. `tune_length` in `CCI.test()` is likewise deprecated;
  use `samples` to set the number of combinations tried.

## Internal changes

* Added a testthat test suite.
