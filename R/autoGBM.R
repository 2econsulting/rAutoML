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
autoGBM <- function(x, y, train_hex, valid_hex, test_hex, model_path="./output", max_runtime_secs=60*60, max_models=60, init_points=40, n_iter=20){

  model_path <<- model_path
  autoGBM_Models <<- list()
  autoGBM_BestParams <<- list()

  # title : H2OGBM_Default
  # author : jacob
  h2o.rm("H2OGBM_Default")

  H2OGBM_Default <- h2o.gbm(
    x = x,
    y = y,
    training_frame = train_hex,
    validation_frame = valid_hex,
    seed = 1234,
    model_id = "H2OGBM_Default",
    verbose = TRUE
  )

  autoGBM_Models['H2OGBM_Default'] <- list(h2o.getModel("H2OGBM_Default"))
  h2o.auc(h2o.performance(H2OGBM_Default, newdata = test_hex))
  saveRDS(autoGBM_Models['H2OGBM_Default'], file.path(model_path, "H2OGBM_Default.rda"))

  cat(">> H2OGBM_Default done! \n")

  # title : H2OGBM_StopRules
  # author : jacob
  h2o.rm("H2OGBM_StopRules")

  hyper_params <- list(
    stopping_rounds = c(1, 2, 3, 5),
    stopping_tolerance = c(0.01, 0.05, 0.001)
  )

  grid <- h2o.grid(
    algorithm = "gbm",
    grid_id = "H2OGBM_StopRules",
    x = x, y = y, seed = 1234,
    training_frame = train_hex,
    validation_frame = valid_hex,
    score_each_iteration = TRUE,
    stopping_metric = "logloss",
    ntrees = 10000,
    hyper_params = hyper_params,
    search_criteria = list(strategy = "Cartesian")
  )

  grid_sorted <- h2o.getGrid(grid_id="H2OGBM_StopRules", sort_by="logloss", decreasing=FALSE)
  autoGBM_Models["H2OGBM_StopRules"] <- list(h2o.getModel(grid_sorted@model_ids[[1]]))
  h2o.auc(h2o.performance(autoGBM_Models["H2OGBM_StopRules"][[1]], newdata = test_hex))
  saveRDS(autoGBM_Models['H2OGBM_StopRules'], file.path(model_path, "H2OGBM_StopRules.rda"))
  autoGBM_BestParams['stopping_rounds'] <- as.numeric(grid_sorted@summary_table$stopping_rounds[1])
  autoGBM_BestParams['stopping_tolerance'] <- as.numeric(grid_sorted@summary_table$stopping_tolerance[1])

  cat(">> H2OGBM_StopRules done! \n")

  # title : H2OGBM_CatEncode
  # author : jacob
  h2o.rm("H2OGBM_CatEncode")

  hyper_params <- list(
    categorical_encoding = c(
      "enum",
      "one_hot_explicit",
      "binary",
      "eigen",
      "label_encoder",
      "sort_by_response",
      "enum_limited"
    )
  )

  grid <- h2o.grid(
    algorithm = "gbm",
    grid_id = "H2OGBM_CatEncode",
    x = x, y = y, seed = 1234,
    training_frame = h2o.rbind(train_hex, valid_hex),
    nfolds = 3,
    score_each_iteration = TRUE,
    stopping_metric = "logloss",
    ntrees = 10000,
    stopping_rounds = autoGBM_BestParams$stopping_rounds,
    stopping_tolerance = autoGBM_BestParams$stopping_tolerance,
    hyper_params = hyper_params,
    search_criteria = list(strategy = "Cartesian")
  )

  grid_sorted <- h2o.getGrid(grid_id="H2OGBM_CatEncode", sort_by="logloss", decreasing=FALSE)
  autoGBM_Models["H2OGBM_CatEncode"] <- list(h2o.getModel(grid_sorted@model_ids[[1]]))
  h2o.auc(h2o.performance(autoGBM_Models["H2OGBM_CatEncode"][[1]], newdata = test_hex))
  saveRDS(autoGBM_Models['H2OGBM_CatEncode'], file.path(model_path, "H2OGBM_CatEncode.rda"))
  autoGBM_BestParams['TOP3_categorical_encoding'] <- list(grid_sorted@summary_table$categorical_encoding[1:3])

  cat(">> H2OGBM_CatEncode done! \n")

  # title : H2OGBM_MaxDepth
  # author : jacob
  h2o.rm("H2OGBM_MaxDepth")

  hyper_params <- list(
    max_depth = c(2,3,4,5,6,7,8,9,10)
  )

  grid <- h2o.grid(
    algorithm = "gbm",
    grid_id = "H2OGBM_MaxDepth",
    x = x, y = y, seed = 1234,
    training_frame = h2o.rbind(train_hex, valid_hex),
    nfolds = 3,
    score_each_iteration = TRUE,
    stopping_metric = "logloss",
    ntrees = 10000,
    stopping_rounds = autoGBM_BestParams$stopping_rounds,
    stopping_tolerance = autoGBM_BestParams$stopping_tolerance,
    hyper_params = hyper_params,
    search_criteria = list(strategy = "Cartesian")
  )

  grid_sorted <- h2o.getGrid(grid_id="H2OGBM_MaxDepth", sort_by="logloss", decreasing=FALSE)
  autoGBM_Models["H2OGBM_MaxDepth"] <- list(h2o.getModel(grid_sorted@model_ids[[1]]))
  h2o.auc(h2o.performance(autoGBM_Models["H2OGBM_MaxDepth"][[1]], newdata = test_hex))
  saveRDS(autoGBM_Models['H2OGBM_MaxDepth'], file.path(model_path, "H2OGBM_MaxDepth.rda"))
  autoGBM_BestParams['TOP3_max_depth'] <- list(as.numeric(grid_sorted@summary_table$max_depth[1:3]))

  cat(">> H2OGBM_MaxDepth done! \n")

  # title : H2OGBM_Random
  # author : jacob
  h2o.rm("H2OGBM_Random")

  max_runtime_secs <<- max_runtime_secs
  max_models <<- max_models

  search_criteria <- list(
    strategy = "RandomDiscrete",
    max_runtime_secs = max_runtime_secs,
    max_models = max_models,
    seed = 1234
  )

  hyper_params <- list(
    categorical_encoding = autoGBM_BestParams['TOP3_categorical_encoding'][[1]],
    max_depth = autoGBM_BestParams['TOP3_max_depth'][[1]],
    learn_rate = c(0.1, 0.5, 0.01),
    sample_rate = seq(from=0.3, to=1, by=0.1),
    col_sample_rate = seq(from=0.3, to=1, by=0.1),
    min_rows = seq(from=5, to=30, by=5)
  )

  grid <- h2o.grid(
    algorithm = "gbm",
    grid_id = "H2OGBM_Random",
    x = x, y = y, seed = 1234,
    training_frame = h2o.rbind(train_hex, valid_hex),
    nfolds = 3,
    score_each_iteration = TRUE,
    stopping_metric = "logloss",
    ntrees = 10000,
    stopping_rounds = autoGBM_BestParams$stopping_rounds,
    stopping_tolerance = autoGBM_BestParams$stopping_tolerance,
    hyper_params = hyper_params,
    search_criteria = search_criteria
  )

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

  cat(">> H2OGBM_Random done! \n")

  # title : H2OGBM_Bayesian
  # author : jacob
  tryCatch({

  h2o.rm("H2OGBM_Bayesian.R")

  init_points <<- init_points
  n_iter <<- n_iter

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

  }, error = function(e) print(">> error! skip this process! \n"))

  # summmary
  m = autoGBM_Models
  autoGBM_eval <- data.frame(
    models = names(m),
    auc = sapply(1:length(m), function(x) h2o.auc(h2o.performance(m[names(m)[x]][[1]], newdata = test_hex))),
    logloss = sapply(1:length(m), function(x) h2o.logloss(h2o.performance(m[names(m)[x]][[1]], newdata = test_hex)))
  )

  print(autoGBM_eval)
  return(list(autoGBM_Models=autoGBM_Models, autoGBM_eval=autoGBM_eval))

}

