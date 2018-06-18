#' @title autoGBM
#' @description auto train GBM using pre-defined strategies
#' @param data_hex H2ODataFrame
#' @param x independent variables
#' @param y dependent variable
#' @examples
#' library(rBayesianOptimization)
#' library(h2o)
#' library(rAutoML)
#' library(knitr)
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
#' knitr::kable(autoGBM_eval, format = "markdown")
#' knitr::kable(autoGBM_eval, format = "pandoc")
#' models                    auc     logloss
#' -----------------  ----------  ----------
#' H2OGBM_Default      0.9136564   0.2006527
#' H2OGBM_StopRules    0.8995600   0.2097254
#' H2OGBM_CatEncode    0.9154616   0.1566602
#' H2OGBM_MaxDepth     0.9166058   0.1754864
#' H2OGBM_Random       0.9225640   0.1522216
#' H2OGBM_Bayesian     0.9262237   0.1591523
#' @export
autoGBM <- function(x, y, train_hex, valid_hex, test_hex, model_path="./"){

  path = 'R/gbm'
  autoGBM_Models <- list()
  autoGBM_BestParams <- list()

  # 0.1 make baseline default model
  source(file.path(path, "H2OGBM_Default.R"))
  autoGBM_Models['H2OGBM_Default'] <- list(h2o.getModel("H2OGBM_Default"))
  h2o.auc(h2o.performance(H2OGBM_Default, newdata = test_hex))
  saveRDS(autoGBM_Models['H2OGBM_Default'], file.path(model_path, "H2OGBM_Default.rda"))

  # 0.2 choose stopping rules
  source(file.path(path, "H2OGBM_StopRules.R"))
  grid_sorted <- h2o.getGrid(grid_id="H2OGBM_StopRules", sort_by="logloss", decreasing=FALSE)
  autoGBM_Models["H2OGBM_StopRules"] <- list(h2o.getModel(grid_sorted@model_ids[[1]]))
  h2o.auc(h2o.performance(autoGBM_Models["H2OGBM_StopRules"][[1]], newdata = test_hex))
  saveRDS(autoGBM_Models['H2OGBM_StopRules'], file.path(model_path, "H2OGBM_StopRules.rda"))
  autoGBM_BestParams['stopping_rounds'] <- as.numeric(grid_sorted@summary_table$stopping_rounds[1])
  autoGBM_BestParams['stopping_tolerance'] <- as.numeric(grid_sorted@summary_table$stopping_tolerance[1])

  # 1.1 cartesian top3 categorical_encoding
  source(file.path(path, "H2OGBM_CatEncode.R"))
  grid_sorted <- h2o.getGrid(grid_id="H2OGBM_CatEncode", sort_by="logloss", decreasing=FALSE)
  autoGBM_Models["H2OGBM_CatEncode"] <- list(h2o.getModel(grid_sorted@model_ids[[1]]))
  h2o.auc(h2o.performance(autoGBM_Models["H2OGBM_CatEncode"][[1]], newdata = test_hex))
  saveRDS(autoGBM_Models['H2OGBM_CatEncode'], file.path(model_path, "H2OGBM_CatEncode.rda"))
  autoGBM_BestParams['TOP3_categorical_encoding'] <- list(grid_sorted@summary_table$categorical_encoding[1:3])

  # 1.2 cartesian top3 max depth
  source(file.path(path, "H2OGBM_MaxDepth.R"))
  grid_sorted <- h2o.getGrid(grid_id="H2OGBM_MaxDepth", sort_by="logloss", decreasing=FALSE)
  autoGBM_Models["H2OGBM_MaxDepth"] <- list(h2o.getModel(grid_sorted@model_ids[[1]]))
  h2o.auc(h2o.performance(autoGBM_Models["H2OGBM_MaxDepth"][[1]], newdata = test_hex))
  saveRDS(autoGBM_Models['H2OGBM_MaxDepth'], file.path(model_path, "H2OGBM_MaxDepth.rda"))
  autoGBM_BestParams['TOP3_max_depth'] <- list(as.numeric(grid_sorted@summary_table$max_depth[1:3]))

  # 2.1 random grid search
  max_runtime_secs = 60*60
  max_models = 60
  source(file.path(path, "H2OGBM_Random.R"))
  grid_sorted <- h2o.getGrid(grid_id="H2OGBM_Random", sort_by="logloss", decreasing=FALSE)
  autoGBM_Models["H2OGBM_Random"] <- list(h2o.getModel(grid_sorted@model_ids[[1]]))
  h2o.auc(h2o.performance(autoGBM_Models["H2OGBM_Random"][[1]], newdata = test_hex))
  saveRDS(autoGBM_Models['H2OGBM_Random'], file.path(model_path, "H2OGBM_Random.rda"))
  autoGBM_BestParams['Random_categorical_encoding'] <- grid_sorted@summary_table$categorical_encoding[1]
  autoGBM_BestParams['Random_max_depth'] <- as.numeric(grid_sorted@summary_table$max_depth[1])
  autoGBM_BestParams['Random_learn_rate'] <- as.numeric(grid_sorted@summary_table$learn_rate[1])
  autoGBM_BestParams['Random_sample_rate'] <- as.numeric(grid_sorted@summary_table$sample_rate[1])
  autoGBM_BestParams['Random_col_sample_rate'] <- as.numeric(grid_sorted@summary_table$col_sample_rate[1])
  autoGBM_BestParams['Random_min_rows'] <- as.numeric(grid_sorted@summary_table$min_rows[1])

  # 2.2 bayesian grid search
  init_points = 40
  n_iter = 20
  source(file.path(path, "H2OGBM_Bayesian.R"))
  autoGBM_Models["H2OGBM_Bayesian"] <- list(h2o.getModel("H2OGBM_Bayesian"))
  h2o.auc(h2o.performance(autoGBM_Models["H2OGBM_Bayesian"][[1]], newdata = test_hex))
  saveRDS(autoGBM_Models['H2OGBM_Bayesian'], file.path(model_path, "H2OGBM_Bayesian.rda"))
  autoGBM_BestParams['Bayes_max_depth'] <- as.numeric(bayesGridSearch$Best_Par["max_depth"])
  autoGBM_BestParams['Bayes_min_rows'] <- as.numeric(bayesGridSearch$Best_Par["min_rows"])
  autoGBM_BestParams['Bayes_sample_rate'] <- as.numeric(bayesGridSearch$Best_Par["sample_rate"])
  autoGBM_BestParams['Bayes_col_sample_rate'] <- as.numeric(bayesGridSearch$Best_Par["col_sample_rate"])

  # summmary ----
  m = autoGBM_Models
  autoGBM_eval <- data.frame(
    models = names(m),
    auc = sapply(1:length(m), function(x) h2o.auc(h2o.performance(m[names(m)[x]][[1]], newdata = test_hex))),
    logloss = sapply(1:length(m), function(x) h2o.logloss(h2o.performance(m[names(m)[x]][[1]], newdata = test_hex)))
  )
  return(autoGBM_eval)
}

















