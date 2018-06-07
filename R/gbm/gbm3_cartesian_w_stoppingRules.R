# title : gbm3_cartesian_w_stoppingRules
# author : jacob
# desc :

cat(">> stopping_rules")
cat(">> cartesian grid search for [learn_rate]... \n")
cat(">> cartesian grid search for [categorical_encoding]... \n")
cat(">> cartesian grid search for [max_depth]... \n")
cat(">> random grid search for [sample_rate, col_sample_rate, nbins, min_rows]... \n")
cat(">> train with full dataset using [nfolds]... \n")



search_criteria <- list(
  strategy = "RandomDiscrete",
  max_runtime_secs = 60*60,
  max_models = 60,
  seed = 1234
)

stoppingRulesOptions <- list(
  min_rows = c(10, 20, 30),
  stopping_tolerance = c(0.01, 0.05, 0.001),
  categorical_encoding = c("AUTO", "Enum", "OneHotExplicit", "Binary", "Eigen", "LabelEncoder", "SortByResponse", "EnumLimited")
)

h2o.rm("gbm_stoppingRules")
grid <- h2o.grid(
  algorithm = "gbm",
  grid_id = "gbm_stoppingRules",
  x = x,
  y = y,
  training_frame = h2o.rbind(train_hex, valid_hex),
  nfolds = 3,
  seed = 1234,
  stopping_metric = "logloss",
  ntrees = 10000,
  score_each_iteration = TRUE,
  stopping_rounds = 3,
  hyper_params = stoppingRulesOptions,
  search_criteria = search_criteria
)


grid_sorted <- h2o.getGrid(grid_id="gbm_stoppingRules", sort_by="AUC", decreasing=TRUE)
stopping_rules <- h2o.getModel(grid_sorted@model_ids[[1]])


grid <- h2o.grid(
  algorithm = "gbm",
  grid_id = "gbm3_cartesian",
  x = x,
  y = y,
  training_frame = train_hex,
  validation_frame = valid_hex,
  seed = 1234,

  categorical_encoding = stopping_rules@allparameters$categorical_encoding,
  stopping_rounds = stopping_rules@allparameters$stopping_rounds,
  min_rows = stopping_rules@allparameters$min_rows,
  stopping_metric = stopping_rules@allparameters$stopping_metric,
  stopping_tolerance = stopping_rules@allparameters$stopping_tolerance,
  score_each_iteration = stopping_rules@allparameters$score_each_iteration,
  ntrees = stopping_rules@allparameters$ntrees,

  hyper_params = grid_options(algo="gbm")[1],
  search_criteria = list(strategy = "Cartesian")
)

grid_sorted <- h2o.getGrid(grid_id="gbm3_cartesian", sort_by="logloss", decreasing=FALSE)
bayesGridSearch <- h2o.getModel(grid_sorted@model_ids[[1]])

gbm3_cartesian_w_stoppingRules <- h2o.gbm(
  x = x,
  y = y,
  seed = 1234,
  training_frame = train_hex,
  validation_frame = valid_hex,
  max_depth = bayesGridSearch@allparameters$max_depth,
  learn_rate = bayesGridSearch@allparameters$learn_rate,
  sample_rate = bayesGridSearch@allparameters$sample_rate,
  col_sample_rate = bayesGridSearch@allparameters$col_sample_rate,

  categorical_encoding = stopping_rules@allparameters$categorical_encoding,
  stopping_rounds = stopping_rules@allparameters$stopping_rounds,
  min_rows = stopping_rules@allparameters$min_rows,
  stopping_metric = stopping_rules@allparameters$stopping_metric,
  stopping_tolerance = stopping_rules@allparameters$stopping_tolerance,
  score_each_iteration = stopping_rules@allparameters$score_each_iteration,
  ntrees = stopping_rules@allparameters$ntrees,

  model_id = "gbm3",
  verbose = TRUE
)

cat(">> gbm3 done! \n")

