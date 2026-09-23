# TODO

- [x] 1. Improve `CCI.pretuner`
  - Tunes with CCI's own wrappers (ranger, xgboost, e1071) instead of `caret::train`; same folds for all candidates
  - Supports RMSE, Kappa and LogLoss; fixed double poly/interaction expansion from `CCI.test`
  - Tuned parameters now actually reach the model in `CCI.test`
  - NEWS.md added and version bumped to 0.3.7
- [ ] 2. Make CCI project-specific decisions available to agents
- [ ] 3. Finish improving vignettes and README
- [ ] 4. Code improvements
  - [x] `perm.test` used `tail = "right"` for LogLoss; now `"left"` (see BUGFIXES.md #6)
  - `wrapper_ranger` drops `...` for RMSE, so extra ranger arguments are ignored for continuous outcomes
  - `wrapper_xgboost` uses `subsample` only when a custom `objective` is given
