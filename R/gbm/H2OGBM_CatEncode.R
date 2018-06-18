# title : H2OGBM_CatEncode
# author : jacob
# desc :
h2o.rm("H2OGBM_CatEncode")

hyper_params <- list(
  categorical_encoding = c(
    "enum",
    "one_hot_explicit",
    "binary",
    "eigen",
    "label_encoder",
    "sort_by_response",
    "enum_limited"
  )
)

grid <- h2o.grid(
  algorithm = "gbm",
  grid_id = "H2OGBM_CatEncode",
  x = x, y = y, seed = 1234,
  training_frame = h2o.rbind(train_hex, valid_hex),
  nfolds = 3,
  score_each_iteration = TRUE,
  stopping_metric = "logloss",
  ntrees = 10000,
  stopping_rounds = autoGBM_BestParams$stopping_rounds,
  stopping_tolerance = autoGBM_BestParams$stopping_tolerance,
  hyper_params = hyper_params,
  search_criteria = list(strategy = "Cartesian")
)

grid_sorted <- h2o.getGrid(grid_id="H2OGBM_CatEncode", sort_by="logloss", decreasing=FALSE)
autoGBM_Models["H2OGBM_CatEncode"] <- list(h2o.getModel(grid_sorted@model_ids[[1]]))
h2o.auc(h2o.performance(autoGBM_Models["H2OGBM_CatEncode"][[1]], newdata = test_hex))
saveRDS(autoGBM_Models['H2OGBM_CatEncode'], file.path(model_path, "H2OGBM_CatEncode.rda"))
autoGBM_BestParams['TOP3_categorical_encoding'] <- list(grid_sorted@summary_table$categorical_encoding[1:3])

cat(">> H2OGBM_CatEncode done! \n")
