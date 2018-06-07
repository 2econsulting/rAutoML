# title : gbm6_cartesian_random_w_stoppingRules
# author : jacob
# desc :


# proceed cartesian grid search
grid <- h2o.grid(
  algorithm = "gbm",
  grid_id = "gbm6_cartesian",
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
grid_sorted <- h2o.getGrid(grid_id="gbm6_cartesian", sort_by="logloss", decreasing=FALSE)
topDepths <- grid_sorted@summary_table$max_depth[1:3]
minDepth <- min(as.numeric(topDepths))
maxDepth <- max(as.numeric(topDepths))

# update max_depth options
RandomGridOptions <- grid_options(algo="gbm")
RandomGridOptions$max_depth <- seq(from=minDepth, to=maxDepth, by=1)

# proceed random grid search
search_criteria <- list(
  strategy = "RandomDiscrete",
  max_runtime_secs = 60*60,
  max_models = 60,
  seed = 1234
)
grid <- h2o.grid(
  algorithm = "gbm",
  grid_id = "gbm6_random",
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
  hyper_params = RandomGridOptions,
  search_criteria = search_criteria
)

grid_sorted <- h2o.getGrid(grid_id="gbm6_random", sort_by="logloss", decreasing=FALSE)
bayesGridSearch <- h2o.getModel(grid_sorted@model_ids[[1]])

gbm6_cartesian_random_w_stoppingRules <- h2o.gbm(
  x = x,
  y = y,
  seed = 1234,
  training_frame = train_hex,
  validation_frame = valid_hex,
  max_depth = bayesGridSearch@allparameters$max_depth,
  learn_rate = bayesGridSearch@allparameters$learn_rate,
  sample_rate = bayesGridSearch@allparameters$sample_rate,
  col_sample_rate = bayesGridSearch@allparameters$col_sample_rate,
  stopping_rounds = stopping_rules$stopping_rounds,
  stopping_metric = stopping_rules$stopping_metric,
  stopping_tolerance = stopping_rules$stopping_tolerance,
  score_each_iteration = stopping_rules$score_each_iteration,
  ntrees = stopping_rules$ntrees,
  model_id = "gbm6",
  verbose = TRUE
)

cat(">> gbm6 done! \n")

