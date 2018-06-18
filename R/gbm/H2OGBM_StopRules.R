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

grid_sorted <- h2o.getGrid(grid_id="H2OGBM_StopRules", sort_by="logloss", decreasing=FALSE)
autoGBM_Models["H2OGBM_StopRules"] <- list(h2o.getModel(grid_sorted@model_ids[[1]]))
h2o.auc(h2o.performance(autoGBM_Models["H2OGBM_StopRules"][[1]], newdata = test_hex))
saveRDS(autoGBM_Models['H2OGBM_StopRules'], file.path(model_path, "H2OGBM_StopRules.rda"))
autoGBM_BestParams['stopping_rounds'] <- as.numeric(grid_sorted@summary_table$stopping_rounds[1])
autoGBM_BestParams['stopping_tolerance'] <- as.numeric(grid_sorted@summary_table$stopping_tolerance[1])

cat(">> H2OGBM_StopRules done! \n")
