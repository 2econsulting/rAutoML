# title : homecredit_w_XGB 
# authro : jacob 

# tuning code
source(file.path(path_code,"XGB/tuneXGB.R"))
source(file.path(path_code,"XGB/cvpredictXGB.R"))

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

# ..
data[is.na(data)] <- -9999
test[is.na(test)] <- -9999

# ------------------------
#  optimal Depth Range
# ------------------------
params <- expand.grid(
  max_depth =  seq(from=2, to=15, by=1)
)
optimalDepthRange <- tuneXGB(head(data, sample_num), y=y, params=params, k=kfolds, max_model=nrow(params))

# ------------------------
# optimal hyper-params
# ------------------------
params <- expand.grid(
  max_depth = head(optimalDepthRange$scores$max_depth, 3),
  eta = learning_rate,
  subsample = seq(from=0.4, to=1, by=0.01),
  colsample_bytree = seq(from=0.4, to=1, by=0.01),
  min_child_weight = c(20, 1, 2, 3, 5, 10, 15, 40),
  lambda  = c(1, 2, 3)
)
optimalParams <- tuneXGB(head(data, sample_num), y=y, params=params, k=kfolds, max_model=max_model)

# ------------------------
# cvpredict catboost 
# ------------------------
params = as.list(head(optimalParams$scores[names(params)],1))
output <- cvpredictXGB(data, test, k=kfolds*2, y=y, params=params)
cat(">> cv_score :", output$score)

# save param
file_param = paste0("PARAM_XGB",round(output$score,3)*10^3,table_nm,".Rda")
saveRDS(optimalParams$scores, file.path(path_output, file_param))
cat(">> best params saved! \n")

# save ztable
file_ztable = paste0("ZTABLE_XGB",round(output$score,3)*10^3,table_nm,".csv")
fwrite(data.frame(ztable=output$ztable), file.path(path_output, file_ztable))
cat(">> ztable saved! \n")

# save submit
file_pred = paste0("SUBMIT_XGB",round(output$score,3)*10^3,table_nm,".csv")
submit[,y] <- ifelse(output$pred>1, 1, output$pred)
fwrite(submit, file.path(path_output, file_pred))
cat(">> submit saved! \n")




