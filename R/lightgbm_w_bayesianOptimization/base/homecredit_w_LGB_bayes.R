# title : ztable maker 
# author : jacob 

# library 
options(scipen = 999)
rm(list=ls())
gc(reset=TRUE)
library(data.table)
library(e1071)
library(caret)
library(Metrics)
require(Matrix)
require(lightgbm)  
library(xgboost)
library(catboost)
library(reticulate)
library(rBayesianOptimization)

# path 
path_code   = "~/GitHub/2econsulting/Kaggle/competition/homecredit/base"
path_output = "~/GitHub/2econsulting/Kaggle_data/homecredit/output" 
path_input  = "~/GitHub/2econsulting/Kaggle_data/homecredit/input"

# tuning code
source(file.path(path_code,"LGB/tuneLGB.R"))
source(file.path(path_code,"LGB/cvpredictLGB.R"))
source(file.path(path_code,"LGB/bayesTuneLGB.R"))

# train options 
y = "TARGET"
kfolds = 5
early_stopping_rounds = 100
iterations = 10000
num_threads = 8
learning_rate = 0.02
init_points = 100      
n_iter = 100  

# bayesian search 
table_nm = "olivier"

# set file
file_data = file.path(table_nm,paste0(table_nm,"_train.csv"))
file_test = file.path(table_nm,paste0(table_nm,"_test.csv"))

# read data
data = fread(file.path(path_input, file_data))
test = fread(file.path(path_input, file_test))
submit = fread(file.path(path_input, 'sample_submission.csv'))

# ..
data$SK_ID_CURR <- NULL
test$SK_ID_CURR <- NULL
names <- which(sapply(data, class) != "numeric")
data[, (names) := lapply(.SD, as.numeric), .SDcols = names]

# data.frame
data <- as.data.frame(data)

# convert char to factor
if(sum(sapply(data, function(x) is.character(x)))>0){
  char_idx <- which(sapply(data, function(x) is.character(x)))
  data[char_idx] <- lapply(data[char_idx], as.factor)
}

# sampling 
set.seed(1)
data <- dplyr::sample_n(data, 5000, replace=F)

# dtrain
data_y <- data[,y]
data_x <- data[,which(colnames(data)!=y)]
cat_features <- names(which(sapply(data_x, is.factor)))
dtrain <- lgb.Dataset(
  data = as.matrix(data_x), 
  label = data_y, 
  colnames = colnames(data_x),
  categorical_feature = cat_features
)


# params
params = list(
  max_depth = c(3L, 9L),
  subsample = c(0.6, 1),
  colsample_bytree = c(0.6, 1),
  num_leaves = c(15L, 100L) 
)


# train options
kfolds = 3L
early_stopping_rounds = 10L
iterations = 100L
num_threads = 8
learning_rate = 0.02
init_points = 5L      
n_iter = 2L  


# BayesianOptimization using python
start = Sys.time()
reticulate::source_python("../homecredit/bayesOptimization.py")
best_params1 = bayesOptimization(X=data_x, y=data_y, init_round=init_points, opt_round=n_iter, num_iterations=iterations)
Sys.time() - start


# BayesianOptimization using R
start = Sys.time()
best_params2 <- rBayesianOptimization::BayesianOptimization(
  FUN = function(...){bayesTuneLGB(data=data, k=kfolds, ...)},
  bounds = params, 
  init_points = init_points, 
  n_iter = n_iter,
  acq = "ucb", 
  kappa = 2.576, 
  eps = 0.0, 
  verbose = TRUE
)
Sys.time() - start


# compare
best_params1
best_params2$History
best_params2$Best_Value

