# title : homecredit_w_CAT
# author : jacob

# tuning code
source(file.path(path_code,"CAT/tuneCAT.R"))
source(file.path(path_code,"CAT/cvpredictCAT.R"))

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

# na NOT allowed in catboost
data[is.na(data)] <- -9999
test[is.na(test)] <- -9999

# ------------------------
#  optimal Depth Range
# ------------------------
params <- expand.grid(
  depth = seq(from=2, to=15, by=1)
)
optimalDepthRange <- tuneCAT(head(data, sample_num), y=y, k=kfolds, params=params, max_model=nrow(params))

# ------------------------
# optimal hyper-params
# ------------------------
params <- expand.grid(
  depth = head(optimalDepthRange$scores$depth,3),
  learning_rate = learning_rate,
  rsm = seq(from=0.4, to=1, by=0.01),
  l2_leaf_reg = c(3 ,1, 2 ,6),
  border_count = c(32, 64, 128)
)
optimalParams <- tuneCAT(head(data, sample_num), y=y, k=kfolds, params=params, max_model=max_model)

# ------------------------
# cvpredict catboost 
# ------------------------
params = as.list(head(optimalParams$scores[names(params)],1))
output <- cvpredictCAT(data, test, k=kfolds*2, y=y, params=params)
cat(">> cv_score :", output$score)

# save param
file_param = paste0("PARAM_CAT",round(output$score,3)*10^3,table_nm,".Rda")
saveRDS(optimalParams$scores, file.path(path_output, file_param))
cat(">> best params saved! \n")

# save ztable
file_ztable = paste0("ZTABLE_CAT",round(output$score,3)*10^3,table_nm,".csv")
fwrite(data.frame(ztable=output$ztable), file.path(path_output, file_ztable))
cat(">> ztable saved! \n")

# save submit
file_pred = paste0("SUBMIT_CAT",round(output$score,3)*10^3,table_nm,".csv")
submit[,y] <- ifelse(output$pred>1, 1, output$pred)
fwrite(submit, file.path(path_output, file_pred))
cat(">> submit saved! \n")

