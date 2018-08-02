# title : cvpredictLGB
# author : jacob

cvpredictLGB <- function(data, test, k, y, params, seed){
  
  if(k<2) stop(">> k is very small \n")

  data <- as.data.frame(data)
  test <- as.data.frame(test)
  
  # convert char to factor(no need if use lgb.prepare_rules)

  data_y <- data[,y]
  data_x <- data[,which(colnames(data)!=y)]

  # ...
  rules <- lgb.prepare_rules(data = data_x)$rules
  target_idx   <- which(colnames(data)==y)
  cat_features <- names(which(sapply(data[,-target_idx], is.factor)))
  
  set.seed(1)
  KFolds <- createFolds(1:nrow(data), k = k, list = TRUE, returnTrain = FALSE)        
  
  oof_preds <- rep(NA, nrow(data))
  oof_score <- list()
  sub_preds <- list()
  for(i in 1:k){
    
    train_idx = unlist(KFolds[-i])
    valid_idx = unlist(KFolds[i])
    
    # dtrain
    dtrain <- lgb.Dataset(
      data = as.matrix(lgb.prepare_rules(data = data_x[train_idx,],  rules = rules)[[1]]), 
      label = data_y[train_idx], 
      colnames = colnames(data_x),
      categorical_feature = cat_features
    )
    
    # dvalid
    dvalid <- lgb.Dataset(
      data = as.matrix(lgb.prepare_rules(data = data_x[valid_idx,],  rules = rules)[[1]]), 
      label = data_y[valid_idx], 
      colnames = colnames(data_x),
      categorical_feature = cat_features
    )
    
    set.seed(1001)
    ml_lgb <- lgb.train(
      params = params,
      data = dtrain,
      valids = list(test = dvalid),
      objective = "binary",
      eval = "auc", 
      nrounds = iterations,
      verbosity = -1, # verbose verbosity
      record = TRUE,
      eval_freq = 10,
      learning_rate = learning_rate,
      num_threads = num_threads,
      early_stopping_rounds = early_stopping_rounds,
      seed = seed
    )
    
    mvalid <- as.matrix(lgb.prepare_rules(data=data_x[valid_idx,], rules=rules)[[1]])
    oof_preds[valid_idx] = predict(ml_lgb, data=mvalid, n=ml_lgb$best_iter)
    oof_score[[i]] = auc(data_y[valid_idx], oof_preds[valid_idx])
    cat(">> oof_score :", oof_score[[i]], "\n")

    mtest <- as.matrix(lgb.prepare_rules(data=test, rules=rules)[[1]])
    sub_preds[[i]] = predict(ml_lgb, data=mtest, n=ml_lgb$best_iter)
  }
  score = auc(data_y, oof_preds)
  cat(">> score :", score, "\n")
  
  pred = expm1(rowMeans(do.call(cbind, sub_preds)))
  
  return(list(ztable=oof_preds, pred=pred, score=score))
}

