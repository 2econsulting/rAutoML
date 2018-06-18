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

grid_sorted <- h2o.getGrid(grid_id="H2OGBM_Random", sort_by="logloss", decreasing=FALSE)
autoGBM_Models["H2OGBM_Random"] <- list(h2o.getModel(grid_sorted@model_ids[[1]]))
h2o.auc(h2o.performance(autoGBM_Models["H2OGBM_Random"][[1]], newdata = test_hex))
saveRDS(autoGBM_Models['H2OGBM_Random'], file.path(model_path, "H2OGBM_Random.rda"))
autoGBM_BestParams['Random_categorical_encoding'] <- grid_sorted@summary_table$categorical_encoding[1]
autoGBM_BestParams['Random_max_depth'] <- as.numeric(grid_sorted@summary_table$max_depth[1])
autoGBM_BestParams['Random_learn_rate'] <- as.numeric(grid_sorted@summary_table$learn_rate[1])
autoGBM_BestParams['Random_sample_rate'] <- as.numeric(grid_sorted@summary_table$sample_rate[1])
autoGBM_BestParams['Random_col_sample_rate'] <- as.numeric(grid_sorted@summary_table$col_sample_rate[1])
autoGBM_BestParams['Random_min_rows'] <- as.numeric(grid_sorted@summary_table$min_rows[1])

cat(">> H2OGBM_Random done! \n")
