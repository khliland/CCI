# CCI: guide for coding agents

This file tells AI coding agents (and new contributors) how the CCI package is built, how to work
in it, and which design decisions must be kept. The reasons behind the decisions are in
[development/DECISIONS.md](development/DECISIONS.md). Read that file before changing anything
listed under "Design decisions" below.

## The package

CCI tests conditional independence Y ⊥ X | Z with machine learning: a model for Y is trained with X
and with permuted X*, and out-of-sample performance is compared using Monte Carlo cross-validation
(MCCV). The method is described in Thorjussen et al. (2024, *Algorithms* 17(8):323) and the package
in Thorjussen et al. (2026, *SoftwareX* 34:102726). The package is on CRAN; maintainers are
Christian Thorjussen and Kristian Hovde Liland.

### How a test runs

```
CCI.test()                 parse formula, choose metric, add polynomial/interaction terms of Z,
  |                        optional CCI.direction() and CCI.pretuner(), then:
  v
perm.test()                builds `settings`, calls test.gen() twice, chooses tail, get_pvalues()
  |  null distribution:    test.gen(permutation = TRUE, nperm = nperm)
  |  test statistic:       test.gen(permutation = FALSE, nperm = 1)
  v
test.gen()                 center/scale, then per iteration: draw MC_sample, permute X (stratified by
  |                        categorical Z if robust), random train/test split (share p), call wrapper
  v
wrapper_ranger / wrapper_xgboost / wrapper_svm / wrapper_knn / user mlfunc
                           fit on train rows, return one metric value on test rows
```

| File | Contents |
|---|---|
| `R/CCI.test.R` | Main user function |
| `R/perm.test.R` | Null distribution + test statistic + p-value; returns the `CCI` object |
| `R/test.gen.R` | The MCCV/permutation loop |
| `R/wrappers.R` | The four built-in learners and their metrics |
| `R/CCI.pretuner.R` | Hyperparameter tuning (uses the wrappers, not `caret::train`) |
| `R/CCI.direction.R` | Chooses `Y ~ X \| Z` or `X ~ Y \| Z` (uses the wrappers, not caret) |
| `R/QQplot.R`, `R/plot.R`, `R/reports.R` | Diagnostics, `print`/`summary` methods |
| `R/utils.R` | Formula helpers, polynomial/interaction terms, p-values, stratification, small internal helpers |
| `R/datasets.R` | Documentation of the simulated data sets in `data/` |

## How to work in this repository

- **Environment:** Windows, R 4.5 at `C:/Program Files/R/R-4.5.1/bin/Rscript.exe` (not on PATH).
- **Load and test:** `devtools::load_all()`, `devtools::test()`, `devtools::document()` after
  changing roxygen comments. Never edit `man/*.Rd` or `NAMESPACE` by hand.
- **Full `R CMD check`:** takes 7–12 minutes with vignettes. **Only run it when the maintainer asks.**
  For day-to-day work, run the unit tests.
- **Do not install the package** into the user's R library (`devtools::install()`) unless asked:
  replacing the installed package while the user has an R session open corrupts that session's
  lazy-load database. Use `load_all()` instead.
- **Manual scripts, not run by testthat** (both excluded from the build):
  - `tests/testthat/dev.R`: the maintainer's personal scratch script. **Never modify or overwrite it.**
  - `tests/testthat/check_package.R`: feature checks (Part 1) and type I error/power on CIsimdata
    scenarios (Part 2, slow). Uses the maintainer's GitHub-only package CIsimdata.
- **Vignettes** need pandoc; outside RStudio set
  `Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools")`.

### Tool gotchas in this environment

- The Bash tool turns `\\` into `\` inside commands and heredocs. Writing `\code{` through `sed`
  produced a hidden control character (`\c`). Edit R and Rd text with the file editor, or build
  backslashes in R with `intToUtf8(92)`. After shell edits, scan for control characters in R
  (`grep -P` does not work reliably in this shell's locale), e.g. for each file:
  `x <- readBin(f, "raw", file.size(f)); any(x < as.raw(32) & !(x %in% as.raw(c(9, 10, 13))))`.
- `Rscript -e "..."` with `|` (e.g. a test `filter = "a|b"`) breaks on Windows: put the code in a
  script file.
- ranger runs on 2 threads by default (`nthread = 2` in `CCI.test`), so results are not exactly
  reproducible even with `seed`. Vignettes and tests must not depend on exact p-values: use inline
  R values in vignette text and robust thresholds in tests.

## Conventions for changes

For every bug fix or behaviour change:

1. Reproduce it first (a short R script), then fix it.
2. Add a test in `tests/testthat/test-*.R`. Use `skip_on_cran()` for tests that fit many models.
   Tests must not use CIsimdata (not on CRAN).
3. Update `NEWS.md` (user-facing, under the current version), `BUGFIXES.md` (symptom, cause, fix,
   tests; numbered entries) and tick the item in `TODO.md`.
4. Keep backward compatibility where possible: deprecate arguments with a warning and keep them in
   the signature (see `tune_length`, `verboseIter`, `parallel`, `subsample` in `CCI.pretuner`).
5. Check that results that were correct before are unchanged (compare against `git show HEAD:<file>`
   on the same seed) and say so in `BUGFIXES.md`.

Other rules:

- **Dependencies:** do not add randomForest, kernlab or other model packages; the learners are
  ranger, xgboost, e1071 and kknn. caret is only used for data splitting (`createDataPartition`,
  `createFolds`, `createResample`), `nearZeroVar()` and `confusionMatrix()`, never to train models.
  New packages for examples go in `Suggests` and must be optional in vignettes
  (`eval = requireNamespace(...)`).
- **CRAN:** vignettes must build in a few minutes in total, need no internet, and cannot use
  CIsimdata (define data-generating functions inline). Files that are not part of the package go in
  `.Rbuildignore`.
- **Model arguments** must reach the wrappers as *named* arguments. Pass lists of parameters with
  `do.call()` / `utils::modifyList()`, never as an unnamed list in `...` (this bug silently dropped
  all tuned and xgboost parameters before 0.3.7).
- **Style:** match the surrounding code; roxygen2 with markdown; user-facing messages in plain
  English that say what to do.

## Design decisions (do not change without the maintainer)

Short list; reasons and sources in [development/DECISIONS.md](development/DECISIONS.md).

1. The test statistic comes from one random train/test split; the null distribution from `nperm`
   splits with permuted X. (Article.)
2. Exchangeability is not enforced: for continuous Z, X is permuted freely. The test is conservative
   by design. (Article.)
3. `robust = TRUE` stratifies the permutation of X by the categorical variables in Z whenever **any**
   Z variable is categorical. (Confirmed by the maintainer.)
4. Tails: Kappa is right-tailed; RMSE and LogLoss are left-tailed; custom metrics require `tail`.
5. `metric = "Auto"`: numeric Y gives RMSE; factor, character or logical Y gives Kappa. Character
   and logical variables are converted to factors (`characters_to_factors()`).
6. Unconditional tests must be written `Y ~ X | 1` or `Y ~ X + 1`; `Y ~ X` is an error.
   (Maintainer's decision.)
7. Polynomial and interaction terms are added to numeric Z only, never to X, once, in `CCI.test`
   (the pretuner gets `poly = FALSE, interaction = FALSE`).
8. Tuning and direction choice use the package's own wrappers, so they use the same models as the
   test. Tuning is done under H0 (Y from Z only), with the same folds for all candidates.
9. `CCI.direction` compares RMSE after standardizing the variables, so the choice does not depend on
   units.
10. `perm.test` stores all settings in `object$settings`; `QQplot()` repeats the test with them.
11. Failed model fits give `NA` in the null distribution; `get_pvalues()` drops them with a warning.
12. A custom `metricfunc` gets predicted classes from rf/svm/KNN but class probabilities from xgboost
    (kept for backward compatibility, documented). `...` is passed to it only if it accepts `...`.
13. `MC_sample` (formerly `subsample`, still accepted with a deprecation warning) is the share of the
    data per Monte Carlo sample; xgboost's own row
    subsampling is not exposed.
14. No `verbose` printout of the expanded formula in `CCI.test`. (Maintainer's decision.)
