# title : tuneLGM
# author : jacob

tuneLGB <- function(data, y, params, k, max_model=NULL){
  
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

  # convert char to factor(no need if use lgb.prepare_rules)
  
  # make x and y 
  data_y <- data[,y]
  data_x <- data[,which(colnames(data)!=y)]
  
  # ...
  rules <- lgb.prepare_rules(data = data_x)$rules
  target_idx   <- which(colnames(data)==y)
  cat_features <- names(which(sapply(data[,-target_idx], is.factor)))
  
  # lgb.Dataset
  ddata <- lgb.Dataset(
    data = as.matrix(lgb.prepare_rules(data = data_x,  rules = rules)[[1]]), 
    label = data_y, 
    colnames = colnames(data_x),
    categorical_feature = cat_features
  )
  
  if(is.numeric(max_model)){
    max_model <- max_model
  }else{
    max_model <- nrow(params)
  }
  
  if(nrow(params)<max_model) stop(">> max_model is lower than nrow(params) \n")
  
  output <- list()
  for(i in 1:max_model){
    
    set.seed(1)
    ml_lgb = lgb.cv(
      params = as.list(sapply(as.list(params),"[",i)),
      data = ddata,
      objective = "binary",
      eval = "auc", # binary_logloss
      nrounds = iterations,
      nfold = k,
      verbosity = -1,
      record = TRUE,
      eval_freq = 10,
      learning_rate = learning_rate,
      num_threads = num_threads,
      early_stopping_rounds = early_stopping_rounds
    )
    output$scores[i] <- ml_lgb$best_score
    output$models[i] <- list(ml_lgb)
  }
  output$scores <- data.frame(head(params, max_model), auc=output$scores)
  output$scores <- output$scores[order(output$scores$auc, decreasing = T), ]
  return(output)
}



