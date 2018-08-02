# title : tuneXGB
# author : jacob

tuneXGB <- function(data, y, params, k, max_model=NULL){
  
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
  
  data_y = data[,y]
  data_x = data[,which(colnames(data)!=y)]
  
  # need for xgboost
  colnames(data_x) <- paste0(rep("X",ncol(data_x)),1:ncol(data_x))

  sparse_matrix_train <- sparse.model.matrix(~.-1, data = data_x)
  ddata <- xgb.DMatrix(data = sparse_matrix_train, label = data_y) 
  
  if(is.numeric(max_model)){
    max_model <- max_model
  }else{
    max_model <- nrow(params)
  }
  
  if(nrow(params)<max_model) stop(">> max_model is lower than nrow(params) \n")
  
  output <- list()
  for(i in 1:max_model){
    
    set.seed(1)
    ml_xgb <- xgb.cv(
      data = ddata,
      eval_metric  = "logloss", 
      maximize = FALSE, 
      objective = "binary:logistic",
      nrounds = iterations, 
      early_stopping_rounds = early_stopping_rounds,
      nthread = num_threads,
      print_every_n = 10,
      tree_method = "hist",
      grow_policy = "lossguide",
      nfold = k, 
      stratified = TRUE, 
      params = as.list(sapply(as.list(params),"[",i)),
      prediction = TRUE # cvpredict 
    )
    output$scores[i] <- min(ml_xgb$evaluation_log$test_logloss_mean)
    output$models[i] <- list(ml_xgb)
  }
  output$scores <- data.frame(head(params, max_model), logloss=output$scores)
  output$scores <- output$scores[order(output$scores$logloss, decreasing = F), ]
  return(output)
}



