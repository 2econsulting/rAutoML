# title : home credit 
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
library(rBayesianOptimization)

# path 
path_code   = "~/GitHub/2econsulting/Kaggle/competition/homecredit/base"
path_output = "~/GitHub/2econsulting/Kaggle_data/homecredit/output" 
path_input  = "~/GitHub/2econsulting/Kaggle_data/homecredit/input"

# train options 
y = "TARGET"
sample_rate = 1
kfolds = 5
early_stopping_rounds = 100
iterations = 10000
num_threads = 8
learning_rate = 0.02

# tuning code
source(file.path(path_code,"LGB/tuneLGB.R"))
source(file.path(path_code,"LGB/cvpredictLGB.R"))

# table_nm
table_nm = "olivier"

# set file
file_data = file.path(table_nm,paste0(table_nm,"_train.csv"))
file_test = file.path(table_nm,paste0(table_nm,"_test.csv"))

# read data
data = fread(file.path(path_input, file_data))
test = fread(file.path(path_input, file_test))
submit = fread(file.path(path_input, 'sample_submission.csv'))

# sampling
set.seed(1)
sample_num =round(nrow(data)*sample_rate)

# ..
data$SK_ID_CURR <- NULL
test$SK_ID_CURR <- NULL
names <- which(sapply(data, class) != "numeric")
data[, (names) := lapply(.SD, as.numeric), .SDcols = names]

# LGB
params = list(
  learning_rate = 0.02,
  num_leaves = 20,
  colsample_bytree = 0.9497036,
  subsample = 0.8715623,
  subsample_freq = 1, 
  max_depth = 8,
  reg_alpha = 0.041545473,
  reg_lambda = 0.0735294,
  min_split_gain = 0.0222415,
  min_child_weight = 60, # 39.3259775
  seed = 0,
  verbose = -1,
  metric = "auc"
)
output <- cvpredictLGB(data, test, k=kfolds*2, y=y, params=params)
cat(">> cv_score :", output$score)





