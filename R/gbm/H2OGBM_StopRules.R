# title : H2OGBM_StopRules
# author : jacob
# desc :
h2o.rm("H2OGBM_StopRules")

hyper_params <- list(
  stopping_rounds = c(1, 2, 3, 5),
  stopping_tolerance = c(0.01, 0.05, 0.001)
)

grid <- h2o.grid(
  algorithm = "gbm",
  grid_id = "H2OGBM_StopRules",
  x = x, y = y, seed = 1234,
  training_frame = train_hex,
  validation_frame = valid_hex,
  score_each_iteration = TRUE,
  stopping_metric = "logloss",
  ntrees = 10000,
  hyper_params = hyper_params,
  search_criteria = list(strategy = "Cartesian")
)

cat(">> H2OGBM_StopRules done! \n")
