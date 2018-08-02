# title : tuneCAT
# author : jacob

tuneCAT <- function(data, y, k, max_model, params){
  
  if(k<2) stop(">> k is very small \n")

  data <- as.data.frame(data)
  
  # shuffle params
  if(ncol(params)==1){
    params = params
    cat(">> cartesian grid search, params not shuffled! \n")
    Sys.sleep(3)
  }else{
    set.seed(1)
    params = params[sample(nrow(params)),]
    cat(">> random grid search, params shuffled! \n")
    Sys.sleep(3)
  }
  
  # convert char to factor
  if(sum(sapply(data, function(x) is.character(x)))>0){
    char_idx <- which(sapply(data, function(x) is.character(x)))
    data[char_idx] <- lapply(data[char_idx], as.factor)
  }

  # max_model 
  if(is.numeric(max_model)){
    max_model <- max_model
  }else{
    max_model <- nrow(params)
  }

  if(nrow(params)<max_model) stop(">> max_model is lower than nrow(params) \n")
  
  output <- list()
  for(i in 1:max_model){
    ml_cat <- catboost_cv(data, y, params=as.list(sapply(as.list(params),"[",i)), k)
    output$scores[i] <- ml_cat$best_score
  }
  output$scores <- data.frame(head(params, max_model), auc=output$scores)
  output$scores <- output$scores[order(output$scores$auc, decreasing = T), ]
  return(output)
}



catboost_cv <- function(data, y, params, k){
  
  # make x and y 
  data_y <- data[,y]
  data_x <- data[,which(colnames(data)!=y)]

  # ...
  target_idx <- which(colnames(data)==y)
  cat_features <- which(sapply(data[,-target_idx], is.factor))

  set.seed(1)
  KFolds <- createFolds(1:nrow(data), k = k, list = TRUE, returnTrain = FALSE)

  oof_preds <- rep(NA, nrow(data))
  oof_score <- list()
  for(i in 1:k){

    params$use_best_model <- TRUE
    params$random_seed <- 1
    params$loss_function <- 'Logloss'
    params$eval_metric <- 'AUC'  
    params$od_type = "Iter"
    params$od_wait = early_stopping_rounds
    params$thread_count = num_threads
    params$iterations = iterations

    train_idx = unlist(KFolds[-i])
    valid_idx = unlist(KFolds[i])

    train_pool <- catboost.load_pool(data = data_x[train_idx,], label = data_y[train_idx], cat_features = cat_features)
    valid_pool <- catboost.load_pool(data = data_x[valid_idx,], label = data_y[valid_idx], cat_features = cat_features)

    ml_cat <- catboost.train(
      learn_pool = train_pool, 
      test_pool = valid_pool,
      params = params
    )
    
    oof_preds[valid_idx] = catboost.predict(ml_cat, valid_pool, prediction_type="Probability")
    oof_score[[i]] = auc(data_y[valid_idx], oof_preds[valid_idx])
    cat(">> oof_score :", oof_score[[i]], "\n")
  }
  
  score = auc(data_y, oof_preds)
  cat(">> score :", score, "\n")
  return(data.frame(best_score=score))
}





