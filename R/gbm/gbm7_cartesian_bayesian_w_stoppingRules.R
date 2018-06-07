# title : gbm7_cartesian_bayesian_w_stoppingRules
# author : jacob
# desc : https://a-ghorbani.github.io/2016/11/24/data-science-with-h2o


# proceed cartesian grid search
grid <- h2o.grid(
  algorithm = "gbm",
  grid_id = "gbm7_cartesian",
  x = x,
  y = y,
  training_frame = train_hex,
  validation_frame = valid_hex,
  seed = 1234,
  stopping_rounds = stopping_rules$stopping_rounds,
  stopping_metric = stopping_rules$stopping_metric,
  stopping_tolerance = stopping_rules$stopping_tolerance,
  score_each_iteration = stopping_rules$score_each_iteration,
  ntrees = stopping_rules$ntrees,
  hyper_params = grid_options(algo="gbm")[1],
  search_criteria = list(strategy = "Cartesian")
)

# find optimal range for max_depth
grid_sorted <- h2o.getGrid(grid_id="gbm7_cartesian", sort_by="logloss", decreasing=FALSE)
topDepths <- grid_sorted@summary_table$max_depth[1:3]
minDepth <- min(as.numeric(topDepths))
maxDepth <- max(as.numeric(topDepths))

# update max_depth options
BayesGridOptions <- grid_options(algo="gbm")
BayesGridOptions$max_depth <- as.integer(seq(from=minDepth, to=maxDepth, by=1))

# bayesGridSearch
set.seed(1234)
bayesGridSearch <- rBayesianOptimization::BayesianOptimization(
  FUN = bayes_gbm,
  bounds = lapply(BayesGridOptions, range),
  init_points = 10,
  n_iter = 5,
  acq = "ucb",
  kappa = 2.576,
  eps = 0,
  verbose = TRUE
)

# gbm7_cartesian_bayesian_w_stoppingRules
gbm7_cartesian_bayesian_w_stoppingRules <- h2o.gbm(
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
  model_id = "gbm7",
  verbose = TRUE
)

cat(">> gbm7 done! \n")

