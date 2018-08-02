# title : homecredit_w_LGB_bayes
# author : jacob

# tuning code
source(file.path(path_code,"LGB/tuneLGB.R"))
source(file.path(path_code,"LGB/cvpredictLGB.R"))
source(file.path(path_code,"LGB/hyperTuneLGB.R"))

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

# data.frame
data <- as.data.frame(data)

# convert char to factor
if(sum(sapply(data, function(x) is.character(x)))>0){
  char_idx <- which(sapply(data, function(x) is.character(x)))
  data[char_idx] <- lapply(data[char_idx], as.factor)
}

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
  max_depth = c(6L, 9L),
  subsample = c(0.6, 1),
  colsample_bytree = c(0.6, 1),
  num_leaves = c(15L, 254L), 
  min_data = c(10L, 100L),
  min_gain_to_split = c(0, 1),
  lambda_l1 = c(0, 1),
  lambda_l2 = c(0, 1)
)

# hyperband search 
optimalParams <- rHyperband::Hyperband(
  FUN = hyperTuneLGB,
  bounds = params,
  maximize = TRUE,
  R = 100,
  R_unit = 10000L,
  eta = 3, 
  verbose = TRUE
)
optimalParams

# cvpredict catboost 
params = as.list(optimalParams[1,names(params),with=F])
output <- cvpredictLGB(data, test, k=kfolds*2, y=y, params=params)
cat(">> cv_score :", output$score)

# save param
file_param = paste0("PARAM_LGBbayes",round(output$score,3)*10^3,table_nm,".Rda")
saveRDS(optimalParams$scores, file.path(path_output, file_param))
cat(">> best params saved! \n")

# save ztable
file_ztable = paste0("ZTABLE_LGBbayes",round(output$score,3)*10^3,table_nm,".csv")
fwrite(data.frame(ztable=output$ztable), file.path(path_output, file_ztable))
cat(">> ztable saved! \n")

# save submit
file_pred = paste0("SUBMIT_LGBbayes",round(output$score,3)*10^3,table_nm,".csv")
submit[,y] <- ifelse(output$pred>1, 1, output$pred)
fwrite(submit, file.path(path_output, file_pred))
cat(">> submit saved! \n")


