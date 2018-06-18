# title : H2OGBM_Random
# author : jacob
# desc :
h2o.rm("H2OGBM_Random")

search_criteria <- list(
  strategy = "RandomDiscrete",
  max_runtime_secs = max_runtime_secs,
  max_models = max_models,
  seed = 1234
)

hyper_params <- list(
  categorical_encoding = autoGBM_BestParams['TOP3_categorical_encoding'][[1]],
  max_depth = autoGBM_BestParams['TOP3_max_depth'][[1]],
  learn_rate = c(0.1, 0.5, 0.01),
  sample_rate = seq(from=0.3, to=1, by=0.1),
  col_sample_rate = seq(from=0.3, to=1, by=0.1),
  min_rows = seq(from=5, to=30, by=5)
)

grid <- h2o.grid(
  algorithm = "gbm",
  grid_id = "H2OGBM_Random",
  x = x, y = y, seed = 1234,
  training_frame = h2o.rbind(train_hex, valid_hex),
  nfolds = 3,
  score_each_iteration = TRUE,
  stopping_metric = "logloss",
  ntrees = 10000,
  stopping_rounds = autoGBM_BestParams$stopping_rounds,
  stopping_tolerance = autoGBM_BestParams$stopping_tolerance,
  hyper_params = hyper_params,
  search_criteria = search_criteria
)

cat(">> H2OGBM_Random done! \n")
