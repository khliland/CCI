# testing testing

dat <- sineGaussian_biv(N = 500, a = 2, d = 1)

formula_init <- Y ~ X + Z1 + Z2

chosen_formula <- choose_direction_CCI(
 formula = formula_init,
 data = dat,
 method = "xgboost",
 data_type = "continuous",
 folds = 5,
 seed = 1)
