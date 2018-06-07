# title : gbm5_bayesian_w_stoppingRules
# author : jacob
# desc : https://a-ghorbani.github.io/2016/11/24/data-science-with-h2o


# bayesGridSearch
set.seed(1234)
bayesGridSearch <- rBayesianOptimization::BayesianOptimization(
  FUN = bayes_gbm,
  bounds = lapply(grid_options(algo="gbm"), range),
  init_points = 10,
  n_iter = 5,
  acq = "ucb",
  kappa = 2.576,
  eps = 0.0,
  verbose = TRUE
)

# gbm5_bayesian_w_stoppingRules
gbm5_bayesian_w_stoppingRules <- h2o.gbm(
  x = x,
  y = y,
  seed = 1234,
  training_frame = train_hex,
  validation_frame = valid_hex,
  max_depth = as.numeric(bayesGridSearch$Best_Par["max_depth"]),
  learn_rate = as.numeric(bayesGridSearch$Best_Par["learn_rate"]),
  sample_rate = as.numeric(bayesGridSearch$Best_Par["sample_rate"]),
  col_sample_rate = as.numeric(bayesGridSearch$Best_Par["col_sample_rate"]),
  stopping_rounds = stopping_rules$stopping_rounds,
  stopping_metric = stopping_rules$stopping_metric,
  stopping_tolerance = stopping_rules$stopping_tolerance,
  score_each_iteration = stopping_rules$score_each_iteration,
  ntrees = stopping_rules$ntrees,
  model_id = "gbm5",
  verbose = TRUE
)

cat(">> gbm5 done! \n")
