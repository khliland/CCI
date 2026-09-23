# CCI design decisions

A log of the design decisions behind the CCI package, with the reasons for them. It is written for
the maintainers and for AI coding agents working on the package (see `AGENTS.md`), so that
decisions are kept consistent and not undone by accident.

Each decision has a **status**:

- **Article**: follows from the method as published (Thorjussen et al. 2024, *Algorithms*; 2026,
  *SoftwareX*). Changing it changes the method.
- **Confirmed**: decided or confirmed by the maintainer (Christian Thorjussen).
- **Implementation**: chosen while implementing a fix or feature. Reasonable, but open for the
  maintainer to revisit.

To add a decision, give it the next number, a status, a date, and say *what* was decided, *why*, and
*where* in the code it lives. Do not renumber existing decisions.

---

## The test

### D1. One split for the test statistic, many for the null distribution

- **Status:** Article.
- **Decision:** `perm.test()` computes the test statistic with `test.gen(permutation = FALSE, nperm = 1)`,
  i.e. one random train/test split with the real X. The null distribution uses `nperm` splits, each
  with a newly permuted X.
- **Why:** This is the algorithm in the SoftwareX article (Section 2.1). The variation of the p-value
  over splits is shown with `QQplot()`.
- **Where:** `R/perm.test.R`.

### D2. Exchangeability is not enforced

- **Status:** Article.
- **Decision:** For continuous Z, X is permuted over all observations, which also breaks the relation
  between X and Z. CCI does not try approximate conditional permutation (e.g. binning of Z).
- **Why:** Exact conditional permutation is only possible for categorical Z. Approximate methods do
  not keep exchangeability either. Under H0 both f(X, Z) and f(X*, Z) converge to E(Y | Z), and
  repeated out-of-sample evaluation keeps the type I error under control; the test is conservative
  (SoftwareX, Section 2.2). A simulation on 2026-09-23 (rf, n = 300, cor(X, Z) up to 1) gave
  rejection rates of 2.5–5 % at α = 0.05.
- **Where:** `R/test.gen.R`.
- **Open:** a CRT-based variant (conditional randomization test) is on the maintainer's wish list
  (`development/TODO.txt`).

### D3. `robust = TRUE` stratifies when *any* Z is categorical

- **Status:** Confirmed (2026-09-23).
- **Decision:** If Z contains at least one categorical variable (factor, character or logical), X is
  permuted within the groups formed by the categorical Z variables. Continuous Z variables are not
  used for the groups. `robust = FALSE` always permutes freely.
- **Why:** Stratifying on the categorical part keeps at least part of the X–Z relation, which is
  closer to exchangeability than a free permutation. The SoftwareX article (Section 2.4) describes
  "whenever there are categorical or factor variables in Z". The alternative ("only when all Z are
  categorical") was considered and rejected by the maintainer.
- **Where:** `R/test.gen.R`, `is_categorical_Z_any()` and `make_strata_from_categorical_Z()` in
  `R/utils.R`.

### D4. Tail of the test

- **Status:** Article / Implementation (LogLoss fixed 2026-09-23).
- **Decision:** Kappa is right-tailed (higher is better). RMSE and LogLoss are left-tailed (lower is
  better). A custom `metricfunc` or `mlfunc` requires `tail` from the user.
- **Why:** Under dependence, the model with the real X predicts better than the null models. For
  LogLoss this means a *lower* value; before 0.3.7 it was wrongly right-tailed (BUGFIXES.md #6).
- **Where:** `R/perm.test.R`.

### D5. Empirical p-value and failed model fits

- **Status:** Implementation (2026-09-23).
- **Decision:** The empirical p-value is `(#{null at least as extreme} + 1) / (n_null + 1)`. Missing
  values in the null distribution (failed model fits) are removed with a warning that says how many;
  if the test statistic is missing, the p-value is `NA` with a warning.
- **Why:** The `+ 1` gives a valid p-value that is never 0. Before 0.3.7, a single failed fit made
  the p-value `NA` (BUGFIXES.md #7).
- **Where:** `get_pvalues()` in `R/utils.R`; `QQplot()` removes missing values once before looping.

## Formula and data

### D6. Unconditional tests need an explicit `1`

- **Status:** Confirmed (2026-09-23).
- **Decision:** Y ⊥ X without conditioning variables is written `Y ~ X | 1` or `Y ~ X + 1`. `Y ~ X`
  gives an error that explains the syntax.
- **Why:** Makes an unconditional test a deliberate choice, so a forgotten conditioning set is not
  silently tested as an unconditional hypothesis.
- **Where:** `clean_formula()` and `has_explicit_one()` in `R/utils.R`. `build_formula()` keeps the
  `+ 1` when there are no Z terms, so the formula survives `clean_formula()` later.

### D7. Metric chosen from the type of Y; character and logical are categorical

- **Status:** Article / Implementation (character handling 2026-09-23).
- **Decision:** `metric = "Auto"` gives RMSE for a numeric Y and Kappa for a factor, character or
  logical Y. Character and logical formula variables are converted to factors at the start of
  `CCI.test()`, and in `wrapper_ranger`/`wrapper_svm`.
- **Why:** One consistent notion of "categorical" in every step (metric, polynomial terms,
  stratification, learners). ranger does not accept character variables (BUGFIXES.md #12).
- **Where:** `characters_to_factors()` in `R/utils.R`.

### D8. Polynomial and interaction terms only for numeric Z, added once

- **Status:** Article / Implementation (2026-09-23).
- **Decision:** `CCI.test()` adds polynomial terms (`Z1_d_2`, ...) and pairwise products
  (`Z1_int_Z2`) of the numeric Z variables, never of X. Categorical Z variables are skipped, but do
  not turn off terms for the numeric ones. The terms are added once, before direction choice and
  tuning; `CCI.pretuner()` is called with `poly = FALSE, interaction = FALSE`.
- **Why:** The terms help the learners model smooth Z–Y relations. Adding them twice created
  polynomials of polynomials (BUGFIXES.md #2); turning them off for all Z when one Z was a factor lost
  power in mixed data (BUGFIXES.md #14).
- **Where:** `add_poly_terms()`, `add_interaction_terms()`, `build_formula()` in `R/utils.R`.

### D9. `MC_sample` is the share of data per Monte Carlo sample

- **Status:** Article (the rule) / Confirmed (the name, 2026-09-23).
- **Decision:** `MC_sample = "Auto"` uses the share (900/n)^0.75 when n > 900; `"Yes"` uses
  `MC_sample_set`; `"No"` uses all data. The argument is called `MC_sample` (and `MC_sample_set`) in
  `CCI.test`, `perm.test`, `test.gen` and `CCI.direction`, and results store it in `MC_sample`. The
  old names `subsample` and `subsample_set` (used in the SoftwareX article) are deprecated aliases
  that still work with a warning. Through `CCI.test`, xgboost's own row subsampling is not exposed,
  since `subsample` is the deprecated alias; `wrapper_xgboost` called directly passes `subsample` in
  `...` to xgboost.
- **Why:** Runtime for large data (SoftwareX, Section 3.4). The maintainer renamed the argument to
  `MC_sample` so the name says what it controls; deprecated aliases keep published code and existing
  scripts working.
- **Where:** `R/CCI.test.R`, `R/perm.test.R`, `R/test.gen.R`, `R/CCI.direction.R`,
  `deprecated_arg()` in `R/utils.R`.

## Learners and tuning

### D10. Model parameters are passed as named arguments

- **Status:** Implementation (2026-09-23).
- **Decision:** Parameters for the learners (user-given or tuned) reach the wrappers as named
  arguments: `CCI.test()` merges them into the `perm.test()` arguments with `utils::modifyList()` and
  calls `do.call()`.
- **Why:** Passing them as an unnamed list in `...` silently dropped all tuned and xgboost parameters
  before 0.3.7 (BUGFIXES.md #1).
- **Where:** `R/CCI.test.R`, `R/perm.test.R`.

### D11. Tuning and direction choice use the package's own learners

- **Status:** Implementation (2026-09-23).
- **Decision:** `CCI.pretuner()` and `CCI.direction()` evaluate candidates with `wrapper_ranger`,
  `wrapper_xgboost`, `wrapper_svm` and `wrapper_knn`, not with `caret::train()`. caret is only used for
  data splitting (`createDataPartition`, `createFolds`, `createResample`), `nearZeroVar()` and
  `confusionMatrix()`.
- **Why:** The tuned or compared model is then the same model as in the test. caret's `"rf"` and
  `"svmRadial"` need randomForest and kernlab, which are not dependencies, and caret's `xgbTree` does
  not work with xgboost 3.x (BUGFIXES.md #4, #13).
- **Where:** `R/CCI.pretuner.R`, `R/CCI.direction.R`.

### D12. Tuning under the null hypothesis, on shared folds

- **Status:** Implementation (2026-09-23).
- **Decision:** By default (`include_explanatory = FALSE`) the learner is tuned for predicting Y from
  Z only. All parameter combinations are evaluated on the same folds. For unconditional tests, X is
  included (there is nothing else to tune on).
- **Why:** Tuning without X does not favour a rejection. Shared folds make the comparison between
  candidates depend on the parameters, not on the random splits.
- **Where:** `R/CCI.pretuner.R`.

### D13. `CCI.direction` compares standardized RMSE

- **Status:** Implementation (2026-09-23), following the article's intention.
- **Decision:** Numeric variables are standardized before both directions are cross-validated, and
  the direction with the lower RMSE is chosen. Only numeric Y and X are supported.
- **Why:** The article says the "easiest" prediction should be chosen; raw RMSE depended on the units
  (Y * 100 flipped the choice, BUGFIXES.md #11).
- **Where:** `R/CCI.direction.R`.

### D14. What a custom `metricfunc` receives

- **Status:** Implementation (2026-09-23), open for the maintainer to revisit.
- **Decision:** `metricfunc(actual, predictions, ...)`. For a numeric Y both are numeric. For a
  categorical Y, rf, svm and KNN give predicted classes (a factor) and xgboost gives class
  probabilities (probability of the second level, or an n x K matrix). `...` is only passed on if the
  function accepts it (`call_metricfunc()`).
- **Why:** Making the learners consistent would break existing metric functions on one side or the
  other, so the difference is documented instead (in `CCI.test`, `perm.test`, `test.gen` and the
  custom-models vignette).
- **Where:** `R/wrappers.R`, `call_metricfunc()` in `R/utils.R`.

## Results and output

### D15. The `CCI` object stores its settings

- **Status:** Implementation (2026-09-23).
- **Decision:** `perm.test()` stores every argument passed to `test.gen()` (including custom functions
  and model parameters) in `object$settings`. `QQplot()` repeats the test with these settings; its own
  arguments override them. Objects from before 0.3.7 fall back to the stored fields.
- **Why:** `QQplot()` used to repeat a different test than the one it plotted (BUGFIXES.md #9).
- **Where:** `R/perm.test.R`, `R/QQplot.R`.

### D16. What `print()` and `summary()` show

- **Status:** Confirmed (no expanded formula, 2026-09-23) / Implementation (the rest).
- **Decision:** `print()` gives a short overview; `summary()` a longer one. Both show the tested
  formula as the user wrote it (without polynomial terms), plus the formula as given when
  `choose_direction` swapped Y and X. `CCI.test(verbose = TRUE)` does not print the expanded formula
  with all polynomial and interaction terms.
- **Where:** `R/reports.R`, `tested_formula` in `R/CCI.test.R`.

## Documentation and development

### D17. Vignettes and tests are CRAN-safe

- **Status:** Implementation (2026-09-23).
- **Decision:** Vignettes define their data-generating functions inline (no CIsimdata), need no
  internet, build in about 4–5 minutes in total, use `summary()`/inline values rather than hard-coded
  numbers, and treat optional packages (dagitty) with `eval = requireNamespace(...)`. Slow tests use
  `skip_on_cran()`.
- **Why:** CRAN time limits and reproducibility; ranger is multithreaded and not exactly
  reproducible.

### D18. Backward compatibility

- **Status:** Implementation (2026-09-23).
- **Decision:** Arguments that are no longer used stay in the signature and give a deprecation warning
  (`tune_length`, `verboseIter`, `parallel`, `subsample` in `CCI.pretuner`; `tune_length` in
  `CCI.test`) or are documented as unused (`poly`, `degree`, `interaction` in `CCI.direction`).
  Renamed arguments keep their old name as a deprecated alias that still works, with a warning
  (`subsample` and `subsample_set`, now `MC_sample` and `MC_sample_set`; see D9), using the internal
  helper `deprecated_arg()`.
- **Why:** Existing user code keeps working after an update from CRAN.
