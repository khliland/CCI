# CCI 0.3.7

## New features

* Unconditional tests of Y ⊥ X (no conditioning variables) are now supported.
  Write the formula as `Y ~ X | 1` or `Y ~ X + 1`. A formula without
  conditioning variables and without the explicit `1`, such as `Y ~ X`, gives
  an error explaining the syntax. This works in `CCI.test()`, `perm.test()`,
  `CCI.pretuner()`, `CCI.direction()` and `QQplot()`.

* New `print()` method for `CCI` objects, giving a short overview of the test
  (formula, learner, metric, statistic and p-value). Before, printing a result
  gave an almost empty printout.

* `print()` and `summary()` show the formula that was tested. When
  `choose_direction = TRUE` changed the direction, the formula as given is
  shown as well. Test results store the tested formula in `tested_formula`.

* The argument `subsample` is renamed to `MC_sample`, and `subsample_set` to
  `MC_sample_set`, in `CCI.test()`, `perm.test()`, `test.gen()` and
  `CCI.direction()`: they set the share of the data used in each Monte Carlo
  sample. The old names still work, with a deprecation warning. Results store
  the share in `MC_sample`, and `summary()` shows it as "MC sample".
  `MC_sample = "Yes"` without a valid `MC_sample_set` now gives an informative
  error.

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

* `QQplot()` now repeats the test with exactly the settings that created the
  `CCI` object. Before, it silently lost model parameters (e.g. xgboost `eta`
  and `max_depth`), turned off centering and scaling, and gave only missing
  p-values for `method = "KNN"`, a custom `metricfunc` or a custom `mlfunc`.
  `CCI` objects now store these settings in `settings`. Arguments given to
  `QQplot()` override them, e.g. `QQplot(result, nperm = 50)`. `QQplot()` also
  works on objects from `perm.test()`.

* `method = "KNN"` now works with a custom `metricfunc`; before, every model
  fit stopped with "metric must be 'RMSE' ...". A numeric response gives
  regression and a factor, character or logical response gives
  classification.

* In all wrappers, a custom `metricfunc` without a `...` argument now works.
  Additional arguments (e.g. model parameters) are only passed to the metric
  function when it accepts `...`.

* `CCI.direction()` (and `CCI.test(choose_direction = TRUE)`) no longer depends
  on the units of Y and X. It compared the raw RMSE of predicting Y with that
  of predicting X, so rescaling a variable (e.g. Y * 100) could switch the
  chosen direction. Y and X are now standardized before the comparison, so the
  variable that is easiest to predict is chosen, as described in the article.
  The chosen direction can differ from earlier versions when Y and X have
  different variances.

* `CCI.direction()` now uses the same learners as the test (ranger, xgboost,
  e1071 and kknn through the package's wrappers) with the same settings,
  instead of models from caret. Before, `method = "rf"` used the randomForest
  package and `method = "svm"` the kernlab package (neither is a dependency of
  CCI), and xgboost used `xgb.cv()` with the data subsample share as xgboost's
  row `subsample`. New arguments `mtry`, `nthread`, `k`, `kernel` and
  `distance`; `poly`, `degree` and `interaction` were never used and are
  documented as such. `verbose = TRUE` prints the cross-validated RMSE of both
  directions.

* Character and logical variables are now treated as categorical (factors)
  everywhere. Before:
  * `method = "rf"` failed in every iteration (p-value `NA`) when Y or X was a
    character vector, e.g. the categorical scenarios in the CIsimdata package.
  * A logical Y stopped with "Could not determine an appropriate metric".
  * A character Z stopped with "non-numeric argument to binary operator",
    because polynomial terms were computed for it.

* Polynomial terms are now added for the numeric conditioning variables even
  when Z also contains categorical variables. Before, one factor in Z turned
  off polynomial terms for all of Z.

* `method = "rf"` now passes additional arguments (e.g. `min.node.size`) to
  `ranger::ranger()` also for continuous outcomes; they were silently ignored.

* `plot()` for `CCI` objects labels the y-axis "Density" (it showed a density,
  labelled "Freq."). Additional ggplot2 layers and themes can be given directly,
  e.g. `plot(result, ggplot2::labs(title = "My test"))`; before, they were
  matched to the size arguments by position. Other arguments are ignored with
  a warning. `fill_color` and the size arguments must now be named.

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

## Documentation

* Four vignettes:
  * "Getting started with CCI" (rewritten): the idea behind the test, reading
    the output, the formula (including unconditional tests), learners, data
    types and large data sets.
  * "Diagnostics, tuning and test direction": null distribution plots,
    `nperm` and parametric p-values, `QQplot()`, tuning with
    `CCI.pretuner()`, and `choose_direction`.
  * "Custom models and performance metrics": `metricfunc` and `mlfunc`, with
    examples for regression and classification.
  * "Applied examples: causal models and time series": testing the
    conditional independencies implied by a DAG (with dagitty), and
    Granger-type tests with lagged variables.
* `citation("CCI")` now includes the SoftwareX article describing the package.
* The documentation of `robust` now says what it does: X is permuted within
  the groups of the categorical variables in Z whenever Z contains any
  categorical variable (it said "all").
* The documentation of `metricfunc` now describes what the metric function
  receives for each learner (predicted classes from rf, svm and KNN, class
  probabilities from xgboost).
* The README is rewritten as a short overview that points to the vignettes.

## Internal changes

* Added a testthat test suite.
* Removed unused caret imports (`import(caret)`, `train`, `trainControl`). caret is
  still a dependency and is called with `caret::` for data splitting,
  `nearZeroVar()` and `confusionMatrix()`.
