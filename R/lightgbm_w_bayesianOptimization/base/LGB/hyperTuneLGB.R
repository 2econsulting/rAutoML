# title : hyperTuneLGB
# author : jacob

hyperTuneLGB <- function(nrounds,max_depth, subsample, colsample_bytree, num_leaves, min_data, min_gain_to_split, lambda_l1, lambda_l2){
  
  set.seed(1)
  ml_lgb <- lgb.cv(
    params = list(
      max_depth=max_depth,
      subsample=subsample,
      colsample_bytree=colsample_bytree,
      num_leaves=num_leaves,
      min_data=min_data,
      min_gain_to_split=min_gain_to_split,
      lambda_l1=lambda_l1,
      lambda_l2=lambda_l2
    ),
    data = dtrain,
    nfold = kfolds,
    objective = "binary",
    eval = "auc", 
    nrounds = nrounds,
    verbose = -1,
    record = TRUE,
    eval_freq = 10,
    learning_rate = learning_rate,
    num_threads = num_threads,
    early_stopping_rounds = early_stopping_rounds
  )
  
  ml_lgb$best_score
}

