#' @title autoGBM
#' @description auto train GBM using pre-defined strategies
#' @param data_hex H2ODataFrame
#' @param x independent variables
#' @param y dependent variable
#' @examples
#' library(h2o)
#' library(rAutoML)
#' model_path <- c("C:/tmp")
#' h2o.init()
#' data(churn, package = "rAutoML")
#' churn_hex <- as.h2o(churn)
#' split_hex <- h2o.splitFrame(data = churn_hex, ratios = c(0.5,0.3), seed = 1234)
#' train_hex <- split_hex[[1]]
#' valid_hex <- split_hex[[2]]
#' test_hex  <- split_hex[[3]]
#' y <- "Churn."
#' x <- setdiff(names(churn_hex),  c(y))
#' autoGBM(x, y, train_hex, valid_hex, test_hex, model_path)
#' @export
autoGBM <- function(x,y,train_hex,valid_hex,test_hex, model_path="./output", max_runtime_secs=60*60,max_models=60,init_points=40,n_iter=20){

  path = 'R/gbm'
  model_path <<- model_path
  autoGBM_Models <<- list()
  autoGBM_BestParams <<- list()
  source(file.path(path, "H2OGBM_Default.R"))
  source(file.path(path, "H2OGBM_StopRules.R"))
  source(file.path(path, "H2OGBM_CatEncode.R"))
  source(file.path(path, "H2OGBM_MaxDepth.R"))
  max_runtime_secs <<- max_runtime_secs
  max_models <<- max_models
  source(file.path(path, "H2OGBM_Random.R"))
  init_points <<- init_points
  n_iter <<- n_iter
  source(file.path(path, "H2OGBM_Bayesian.R"))

  # summmary ----
  m = autoGBM_Models
  autoGBM_eval <- data.frame(
    models = names(m),
    auc = sapply(1:length(m), function(x) h2o.auc(h2o.performance(m[names(m)[x]][[1]], newdata = test_hex))),
    logloss = sapply(1:length(m), function(x) h2o.logloss(h2o.performance(m[names(m)[x]][[1]], newdata = test_hex)))
  )
  return(autoGBM_eval)
}

















