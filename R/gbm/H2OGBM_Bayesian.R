# title : H2OGBM_Bayesian
# author : jacob
# desc :
h2o.rm("H2OGBM_Bayesian.R")

# bayesGridFun
bayesGridFun <- function(max_depth, min_rows, sample_rate, col_sample_rate){
  gbm <- h2o.gbm(
    x = x, y = y, seed = 1234,
    training_frame = h2o.rbind(train_hex, valid_hex),
    nfolds = 3,
    score_each_iteration = TRUE,
    stopping_metric = "logloss",
    ntrees = 10000,
    stopping_rounds = autoGBM_BestParams$stopping_rounds,
    stopping_tolerance = autoGBM_BestParams$stopping_tolerance,
    categorical_encoding = autoGBM_BestParams$Random_categorical_encoding,
    learn_rate = autoGBM_BestParams$Random_learn_rate,
    max_depth = max_depth,
    min_rows = min_rows,
    sample_rate = sample_rate,
    col_sample_rate = col_sample_rate
  )
  score <- h2o.auc(gbm, xval = T)
  list(Score = score, Pred  = 0)
}

# bayesGridOptions
max_depth <- autoGBM_BestParams$Random_max_depth
sample_rate <- autoGBM_BestParams$Random_sample_rate
col_sample_rate <- autoGBM_BestParams$Random_col_sample_rate
min_rows <- autoGBM_BestParams$Random_min_rows
bayesGridOptions <- list(
  max_depth = as.integer(c(max(2, max_depth-1), max_depth+1)),
  min_rows  = as.integer(c(max(1, min_rows-5), min_rows+5)),
  sample_rate = c(sample_rate-0.1, min(sample_rate+0.1, 1)),
  col_sample_rate = c(col_sample_rate-0.1, min(col_sample_rate+0.1, 1))
)

# bayesGridSearch
set.seed(1234)
bayesGridSearch <- rBayesianOptimization::BayesianOptimization(
  FUN = bayesGridFun,
  bounds = bayesGridOptions,
  init_points = init_points,
  n_iter = n_iter,
  acq = "ucb",
  kappa = 2.576,
  eps = 0.0,
  verbose = TRUE
)

# H2OGBM_Bayesian
H2OGBM_Bayesian <- h2o.gbm(
  x = x, y = y, seed = 1234,
  model_id = "H2OGBM_Bayesian",
  training_frame = train_hex,
  validation_frame = valid_hex,
  score_each_iteration = TRUE,
  stopping_metric = "logloss",
  ntrees = 10000,
  stopping_rounds = autoGBM_BestParams$stopping_rounds,
  stopping_tolerance = autoGBM_BestParams$stopping_tolerance,
  categorical_encoding = autoGBM_BestParams$Random_categorical_encoding,
  learn_rate = autoGBM_BestParams$Random_learn_rate,
  max_depth = as.numeric(bayesGridSearch$Best_Par["max_depth"]),
  min_rows = as.numeric(bayesGridSearch$Best_Par["min_rows"]),
  sample_rate = as.numeric(bayesGridSearch$Best_Par["sample_rate"]),
  col_sample_rate = as.numeric(bayesGridSearch$Best_Par["col_sample_rate"])
)

autoGBM_Models["H2OGBM_Bayesian"] <- list(h2o.getModel("H2OGBM_Bayesian"))
h2o.auc(h2o.performance(autoGBM_Models["H2OGBM_Bayesian"][[1]], newdata = test_hex))
saveRDS(autoGBM_Models['H2OGBM_Bayesian'], file.path(model_path, "H2OGBM_Bayesian.rda"))
autoGBM_BestParams['Bayes_max_depth'] <- as.numeric(bayesGridSearch$Best_Par["max_depth"])
autoGBM_BestParams['Bayes_min_rows'] <- as.numeric(bayesGridSearch$Best_Par["min_rows"])
autoGBM_BestParams['Bayes_sample_rate'] <- as.numeric(bayesGridSearch$Best_Par["sample_rate"])
autoGBM_BestParams['Bayes_col_sample_rate'] <- as.numeric(bayesGridSearch$Best_Par["col_sample_rate"])

cat(">> H2OGBM_Bayesian done! \n")
