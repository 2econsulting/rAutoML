# title : cvpredictXGB
# author : jacob

cvpredictXGB <- function(data, test, k, y, params){
  
  if(k<2) stop(">> k is very small \n")

  data <- as.data.frame(data)
  test <- as.data.frame(test)
  
  # convert char to factor
  if(sum(sapply(data, function(x) is.character(x)))>0){
    char_idx <- which(sapply(data, function(x) is.character(x)))
    data[char_idx] <- lapply(data[char_idx], as.factor)
  }
  
  # convert char to factor
  if(sum(sapply(test, function(x) is.character(x)))>0){
    char_idx <- which(sapply(test, function(x) is.character(x)))
    test[char_idx] <- lapply(test[char_idx], as.factor)
  }
  
  data_y = data[,y]
  data_x = data[,which(colnames(data)!=y)]
  
  # need for xgboost
  colnames(data_x) <- paste0(rep("X",ncol(data_x)),1:ncol(data_x))
  colnames(test) <- paste0(rep("X",ncol(test)),1:ncol(test))

  set.seed(1)
  KFolds <- createFolds(1:nrow(data), k = k, list = TRUE, returnTrain = FALSE)        
  
  oof_preds <- rep(NA, nrow(data))
  oof_score <- list()
  sub_preds <- list()
  for(i in 1:k){
    
    train_idx = unlist(KFolds[-i])
    valid_idx = unlist(KFolds[i])
    
    # one-hot encoding
    sparse_matrix_train <- sparse.model.matrix(~.-1, data = data_x[train_idx,])
    dtrain <- xgb.DMatrix(data = sparse_matrix_train, label = data_y[train_idx])
    sparse_matrix_valid <- sparse.model.matrix(~.-1, data = data_x[valid_idx,])
    dvalid <- xgb.DMatrix(data = sparse_matrix_valid, label = data_y[valid_idx])
    
    set.seed(1)
    ml_xgb <- xgb.train(
      data = dtrain, 
      eval_metric = "logloss",
      maximize = FALSE,
      watchlist = list(eval = dvalid),
      objective = "binary:logistic",
      nround = iterations, 
      early_stopping_rounds = early_stopping_rounds,
      nthread = num_threads,
      print_every_n = 10,
      tree_method = "hist",
      grow_policy = "lossguide",
      params = params
    )
    
    oof_preds[valid_idx] = predict(ml_xgb, newdata=dvalid, ntreelimit = ml_xgb$best_iteration)   
    oof_score[[i]] = auc(data_y[valid_idx], oof_preds[valid_idx])
    cat(">> oof_score :", oof_score[[i]], "\n")
    ml_xgb$feature_names
    colnames(test)
    
    sparse_matrix_test <- sparse.model.matrix(~.-1, data = test)
    dtest <- xgb.DMatrix(data = sparse_matrix_test)
    sub_preds[[i]] = predict(ml_xgb, newdata=dtest, ntreelimit = ml_xgb$best_iteration)
  }
  score = auc(data_y, oof_preds)
  cat(">> score : ", score, "\n")
  
  pred = expm1(rowMeans(do.call(cbind, sub_preds)))
  
  return(list(ztable=oof_preds, pred=pred, score=score))
}

