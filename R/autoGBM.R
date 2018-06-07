#' @title autoGBM
#' @description auto train GBM using pre-defined strategies
#' @param data_hex H2ODataFrame
#' @param x independent variables
#' @param y dependent variable
#' @examples
#' library(rAutoML)
#' library(h2o)
#' h2o.init()
#' data(churn, package = "rAutoML")
#' data_hex <- as.h2o(churn)
#' split_hex <- h2o.splitFrame(data = data_hex, ratios = c(0.5,0.3), seed = 1234)
#' train_hex <- split_hex[[1]]
#' valid_hex <- split_hex[[2]]
#' test_hex <- split_hex[[3]]
#' y = "Churn."
#' x = colnames(data_hex)[colnames(data_hex)!=y]
#' autoGBM_list <- autoGBM(training_frame=train_hex, validation_frame_valid_hex, x=x, y=y)
#' @export
autoGBM <- function(training_frame, validation_frame, x, y){
  models = list.files('R/gbm')
  knitr::kable(models, format = "pandoc", caption = "GBM models in rAutoML library")
  sysinf = Sys.info()
  pbtype <- ifelse(as.character(sysinf['sysname']) == "Windows", "win", "timer")
  pbapply::pboptions(type = pbtype, style = 3, char = ".")
  system.time(pbapply::pblapply(1:length(models), function(i) source(file.path('R/gbm', models[i]))))
  model_list <- lapply(1:length(models), function(i) h2o.getModel(paste0("gbm",i)))
  return(model_list)
}


#' @title autoGBM.performance
#' @description auto train GBM using pre-defined strategies
#' @param data_hex H2ODataFrame
#' @param x independent variables
#' @param y dependent variable
#' @examples
#' library(rAutoML)
#' library(h2o)
#' h2o.init()
#' data(churn, package = "rAutoML")
#' data_hex <- as.h2o(churn)
#' split_hex <- h2o.splitFrame(data = data_hex, ratios = c(0.5,0.3), seed = 1234)
#' train_hex <- split_hex[[1]]
#' valid_hex <- split_hex[[2]]
#' test_hex <- split_hex[[3]]
#' y = "Churn."
#' x = colnames(data_hex)[colnames(data_hex)!=y]
#' autoGBM_list <- autoGBM(training_frame=train_hex, validation_frame_valid_hex, x=x, y=y)
#' autoGBM.performance(model_list=autoGBM_list, test_hex=test_hex)
#' @export
autoGBM.performance <- function(model_list, test_hex){
  model_report <- data.frame(
    "model" = list.files('R/gbm'),
    "train" = sapply(model_list, function(x) h2o.performance(x, train=TRUE)@metrics$AUC),
    "valid" = sapply(model_list, function(x) h2o.performance(x, valid=TRUE)@metrics$AUC),
    "test"  = sapply(model_list, function(x) h2o.performance(x, newdata=test_hex)@metrics$AUC)
  )
  return(model_report)
}


#' @title bayes_gbm
#' @description gbm function for bayesian grid search
#' @param max_depth max_depth
#' @param learn_rate learn_rate
#' @param sample_rate sample_rate
#' @param col_sample_rate col_sample_rate
#' @export
bayes_gbm <- function(max_depth, learn_rate, sample_rate, col_sample_rate){
  gbm <- h2o.gbm(
    x = x,
    y = y,
    training_frame = train_hex,
    validation_frame = valid_hex,
    stopping_rounds = stopping_rules$stopping_rounds,
    stopping_metric = stopping_rules$stopping_metric,
    stopping_tolerance = stopping_rules$stopping_tolerance,
    score_each_iteration = stopping_rules$score_each_iteration,
    ntrees = stopping_rules$ntrees,
    seed = 1234,
    max_depth = max_depth,
    learn_rate = learn_rate,
    sample_rate = sample_rate,
    col_sample_rate = col_sample_rate
  )
  score <- h2o.auc(gbm, valid = T)
  list(Score = score, Pred  = 0)
}

