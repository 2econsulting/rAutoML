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
library(rBayesianOptimization)

# path 
path_fe     = "~/GitHub/2econsulting/Kaggle/competition/homecredit/fe"
path_code   = "~/GitHub/2econsulting/Kaggle/competition/homecredit/base"
path_output = "~/GitHub/2econsulting/Kaggle_data/homecredit/output" 
path_input  = "~/GitHub/2econsulting/Kaggle_data/homecredit/input"

# tuning code
source(file.path(path_code,"LGB/tuneLGB.R"))
source(file.path(path_code,"LGB/cvpredictLGB2.R"))
source(file.path(path_code,"LGB/bayesTuneLGB.R"))

# train options 
y = "TARGET"

# bayesian search 
table_nm = "olivier"

# set file
file_data = file.path(table_nm,paste0(table_nm,"_train.csv"))
file_test = file.path(table_nm,paste0(table_nm,"_test.csv"))

# read data
data = fread(file.path(path_input, file_data))
test = fread(file.path(path_input, file_test))
submit = fread(file.path(path_input, 'sample_submission.csv'))

# eda_externalsource
source(file.path(path_fe,"eda_externalsource.R"))
vi = c("EXT_SOURCE_1","EXT_SOURCE_3")
data <- eda_externalsource(data, vi = vi)
test <- eda_externalsource(test, vi = vi)

# woe
# source(file.path(path_fe,"cvNumtoCat_WoE.R"))
# vi = c("EXT_SOURCE_1","EXT_SOURCE_3")
# binning = cvNumtoCat_WoE_fit(data, vi=vi, y="TARGET", bin=20, k=5)
# data <- rAutoFE::cvNumtoCat_WoE_transform(data, binning)
# test <- rAutoFE::cvNumtoCat_WoE_transform(test, binning)

# ..
data$SK_ID_CURR <- NULL
test$SK_ID_CURR <- NULL
names <- which(sapply(data, class) != "numeric")
data[, (names) := lapply(.SD, as.numeric), .SDcols = names]

data <- as.data.frame(data)
data_y <- data[,y]
data_x <- data[,which(colnames(data)!=y)]

# library(reticulate)
# source_python("bayes_parameter_opt_lgb.py")
# bayes_parameter_opt_lgb(X=data_x, y=data_y)
# sys = import('sys')
# sys$stdout$flush()

# ------------------------
# cvpredict catboost 
# ------------------------
kfolds = 5
early_stopping_rounds = 200
iterations = 10000
num_threads = 8
learning_rate = 0.02
params = list(
  num_leaves = 20,
  colsample_bytree = 0.9497036,
  subsample = 0.8715623,
  subsample_freq = 1,
  max_depth = 8,
  reg_alpha = 0.041545473,
  reg_lambda = 0.0735294,
  min_split_gain = 0.0222415,
  min_child_weight = 60,
  verbose = -1,
  metric = "auc"
)

output <- cvpredictLGB2(data, test, k=kfolds, y=y, params=params, seed=0)
cat(">> cv_score :", output$score)

# save ztable
file_ztable = paste0("ZTABLE_LGB",round(output$score,3)*10^3,table_nm,".csv")
fwrite(data.frame(ztable=output$ztable), file.path(path_output, file_ztable))
cat(">> ztable saved! \n")

# save submit
file_pred = paste0("SUBMIT_LGB",round(output$score,3)*10^3,table_nm,".csv")
submit[,y] <- ifelse(output$pred>1, 1, output$pred)
fwrite(submit, file.path(path_output, file_pred))
cat(">> submit saved! \n")



